{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Handler.Tokens
  ( getTokensGmailR, postTokensGmailR
  , postTokensGmailClearR
  , getTokensGmailHookR
  , getTokensVapidR, postTokensVapidR
  , postTokensVapidClearR
  , getTokensVapidHookR
  , fetchRefreshToken, fetchAccessToken, fetchVapidKeys
  ) where

import ClassyPrelude (readMay)
import Control.Exception.Safe
    ( tryAny, SomeException (SomeException), Exception (fromException)
    )
import qualified Control.Lens as L ((^.), (?~), (^?))
import Control.Monad ((<=<), void)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (object, (.=))
import Data.Aeson.Lens (key, AsValue (_String))
import qualified Data.ByteString.Base64 as B64 (encode)
import Data.ByteString.Lazy (toStrict)
import Data.Function ((&))
import qualified Data.List.Safe as LS (last)
import Data.Text (Text, pack, unpack, splitOn)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, Value (unValue)
    , (^.), (==.)
    )
import Database.Persist
    ( Entity(Entity, entityVal)
    , PersistStoreWrite (delete)
    , PersistUniqueWrite (upsert)
    )
import qualified Database.Persist as P ((=.))

import Foundation
    ( App (appSettings), Form, Handler, widgetSnackbar, widgetTopbar
    , Route (DataR)
    , DataR
      ( TokensGmailR, TokensGmailHookR, TokensGmailClearR, TokensVapidR
      , TokensVapidClearR, TokensVapidHookR
      )
    , AppMessage
      ( MsgTokens, MsgInitialize, MsgUserSession, MsgDatabase
      , MsgStoreType, MsgInvalidStoreType, MsgRecordEdited, MsgClearSettings
      , MsgRecordDeleted, MsgInvalidFormData, MsgCleared, MsgEmailAddress
      , MsgGmail, MsgVapid, MsgGoogleSecretManager, MsgVapidGenerationWarning
      , MsgUserAccountThroughWhichMailWillBeSent, MsgSelectLocationToStoreRefreshToken
      , MsgGenerate
      )
    )

import Material3 (md3radioField)

import Model
    ( keyAccessTokenGmail, keyApiGmail
    , secretRefreshTokenGmail, secretVolumeRefreshTokenGmail
    , StoreType
      ( StoreTypeDatabase, StoreTypeSession, StoreTypeGoogleSecretManager )
    , Store (Store), Token (Token, tokenStore)
    , EntityField (StoreVal, TokenStore, TokenApi, StoreToken, StoreKey)
    , keySendby, msgSuccess, msgError, keyAccessTokenGmailExpiresIn
    , keyApiVapid, secretVapid, secretRefreshTokenVapid, secretVolumeVapid
    , secretVolumeRefreshTokenVapid
    )

import Network.HTTP.Client
    ( HttpException(HttpExceptionRequest)
    , HttpExceptionContent (StatusCodeException)
    )
import Network.Wreq
    ( post, FormParam ((:=)), responseBody, defaults, auth, oauth2Bearer
    , postWith, getWith
    )
import Network.Wreq.Lens (statusCode, responseStatus)

import Settings
    ( widgetFile, AppSettings (appGoogleApiConf, appGcloudConf)
    , GoogleApiConf (googleApiConfClientSecret, googleApiConfClientId)
    , GcloudConf (gcloudProjectId)
    )

import System.Directory (doesFileExist)
import System.IO (readFile')

import Text.Blaze.Html (preEscapedToHtml, toHtml)
import Text.Hamlet (Html)
import Text.Read (readMaybe)
import Text.Shakespeare.Text (st)

import Web.WebPush
    ( generateVAPIDKeys, VAPIDKeysMinDetails (VAPIDKeysMinDetails)
    , VAPIDKeys
    )

import Yesod.Core
    ( Yesod(defaultLayout), whamlet, SomeMessage (SomeMessage), getYesod
    , getUrlRender, deleteSession, getMessageRender, getMessages, logWarn
    , addMessage, setUltDestCurrent, newIdent, lookupSession
    )
import Yesod.Core.Handler (redirect, addMessageI, setSession)
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist (YesodPersist(runDB))
import Yesod.Form.Input (ireq, runInputGet)
import Yesod.Form.Fields (optionsPairs, textField, emailField)
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form.Types
    ( FormResult (FormSuccess), FieldView (fvInput, fvLabel, fvRequired)
    , FieldSettings (FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    )


fetchVapidKeys :: Handler (Maybe VAPIDKeys)
fetchVapidKeys = do

    tokenVapid <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val keyApiVapid
        return x

    (readMay =<<) <$> case tokenVapid of
      Just (Entity _ (Token _ StoreTypeSession)) -> lookupSession secretVapid
          
      Just (Entity tid (Token _ StoreTypeDatabase)) ->
          (unValue <$>) <$> runDB ( selectOne $ do
              x <- from $ table @Store
              where_ $ x ^. StoreToken ==. val tid
              where_ $ x ^. StoreKey ==. val secretVapid
              return $ x ^. StoreVal )
          
      Just (Entity _ (Token _ StoreTypeGoogleSecretManager)) ->
          Just . pack <$> liftIO ( readFile' (unpack secretVolumeVapid) )
          
      Nothing -> return Nothing


fetchAccessToken :: Text -> Handler (Maybe Text)
fetchAccessToken rtoken = do
    settings <- appSettings <$> getYesod

    r <- liftIO $ post "https://oauth2.googleapis.com/token"
             [ "refresh_token" := rtoken
             , "client_id" := (googleApiConfClientId . appGoogleApiConf $ settings)
             , "client_secret" := (googleApiConfClientSecret . appGoogleApiConf $ settings)
             , "grant_type" := ("refresh_token" :: Text)
             ]

    return $ r L.^? responseBody . key "access_token" . _String


fetchRefreshToken :: Handler (Maybe Text, Maybe Text)
fetchRefreshToken = do

    tokenInfo <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val keyApiGmail
        return x

    secretExists <- liftIO $ doesFileExist $ unpack secretVolumeRefreshTokenGmail

    (rtoken,sender) <- case (tokenInfo,secretExists) of
      (Just (Entity tid (Token _ StoreTypeDatabase)),_) -> do
          refresh <- (unValue <$>) <$> runDB ( selectOne $ do
              x <- from $ table @Store
              where_ $ x ^. StoreToken ==. val tid
              where_ $ x ^. StoreKey ==. val secretRefreshTokenGmail
              return $ x ^. StoreVal )
          sender <- (unValue <$>) <$> runDB ( selectOne $ do
              x <- from $ table @Store
              where_ $ x ^. StoreToken ==. val tid
              where_ $ x ^. StoreKey ==. val keySendby
              return $ x ^. StoreVal )
          return (refresh,sender)

      (Just (Entity _ (Token _ StoreTypeSession)),_) -> do
            refresh <- lookupSession secretRefreshTokenGmail
            sender <- lookupSession keySendby
            return (refresh,sender)

      (Just (Entity tid (Token _ StoreTypeGoogleSecretManager)),True) -> do

          refresh <- liftIO $ readFile' $ unpack secretVolumeRefreshTokenGmail

          sender <- (unValue <$>) <$> runDB ( selectOne $ do
              x <- from $ table @Store
              where_ $ x ^. StoreToken ==. val tid
              where_ $ x ^. StoreKey ==. val keySendby
              return $ x ^. StoreVal )

          return (Just (pack refresh),sender)

      (_,True) -> do
          refresh <- liftIO $ readFile' $ unpack secretVolumeRefreshTokenGmail
          return (Just (pack refresh),Just "me")

      _otherwise -> return (Nothing,Nothing)

    return (rtoken,sender)


getTokensVapidHookR :: Handler Html
getTokensVapidHookR = do
    rndr <- getUrlRender
    app <- appSettings <$> getYesod
    let googleClientId = googleApiConfClientId . appGoogleApiConf $ app
    let googleClientSecret = googleApiConfClientSecret . appGoogleApiConf $ app

    code <- runInputGet $ ireq textField "code"
    state <- readMaybe .  unpack <$> runInputGet (ireq textField "state")

    r <- liftIO $ post "https://oauth2.googleapis.com/token"
         [ "code" := code
         , "redirect_uri" := rndr (DataR TokensVapidHookR)
         , "client_id" := googleClientId
         , "client_secret" := googleClientSecret
         , "grant_type" := ("authorization_code" :: Text)
         ]

    let _status = r L.^. responseStatus . statusCode
    let _tokenType = r L.^. responseBody . key "token_type" . _String
    let _scope = r L.^. responseBody . key "scope" . _String

    let accessToken = r L.^. responseBody . key "access_token" . _String
    let refreshToken = r L.^. responseBody . key "refresh_token" . _String
    let _expiresIn = r L.^. responseBody . key "expires_in" . _String

    -- save refresh token
    case state of
      Just t@StoreTypeGoogleSecretManager -> do

          let project = gcloudProjectId . appGcloudConf $ app

          let opts = defaults & auth L.?~ oauth2Bearer (encodeUtf8 accessToken)
          response <- liftIO $ tryAny $ postWith opts
              (unpack [st|#{smProjects}/#{project}/secrets/#{secretRefreshTokenVapid}:addVersion|])
              (object [ "payload" .= object [ "data" .= decodeUtf8 (B64.encode (encodeUtf8 refreshToken)) ]])

          -- destroy previous version
          case response of
            Right res -> do

                let prev :: Maybe Int
                    prev = (fmap (\y -> y - 1) . readMaybe . unpack) <=< (LS.last . splitOn "/")
                        $ res L.^. responseBody . key "name" . _String

                case prev of
                  Just v | v > 0 -> do
                      void $ liftIO $ tryAny $ postWith opts
                          (unpack [st|#{smProjects}/#{project}/secrets/#{secretRefreshTokenVapid}/versions/#{v}:destroy|])
                          (object [])

                         | otherwise -> return ()
                  Nothing -> return ()

            Left e@(SomeException _) -> case fromException e of
              Just (HttpExceptionRequest _ (StatusCodeException _ bs)) -> do
                  $(logWarn) $ pack $ show e
                  let msg = bs L.^. key "error" . key "message" . _String
                  addMessage msgError (toHtml msg)
                  redirect $ DataR TokensVapidR

              _otherwise -> do
                  let msg = pack $ show e
                  $(logWarn) msg
                  addMessage msgError (toHtml msg)
                  redirect $ DataR TokensVapidR

          void $ runDB $ upsert (Token keyApiVapid t) [TokenStore P.=. t]

          -- save vapid          
          VAPIDKeysMinDetails s xc yc <- liftIO generateVAPIDKeys
          let triplet = pack $ show (s,xc,yc)
          
          resp <- liftIO $ tryAny $ postWith opts
              (unpack [st|#{smProjects}/#{project}/secrets/#{secretVapid}:addVersion|])
              (object [ "payload" .= object [ "data" .= decodeUtf8 (B64.encode (encodeUtf8 triplet)) ]])

          -- destroy previous version
          case resp of
            Right res -> do

                let prev :: Maybe Int
                    prev = (fmap (\y -> y - 1) . readMaybe . unpack) <=< (LS.last . splitOn "/")
                        $ res L.^. responseBody . key "name" . _String

                case prev of
                  Just v | v > 0 -> do
                      void $ liftIO $ tryAny $ postWith opts
                          (unpack [st|#{smProjects}/#{project}/secrets/#{secretVapid}/versions/#{v}:destroy|])
                          (object [])
                         | otherwise -> return ()
                  Nothing -> return ()

            Left e -> do
                let msg = pack $ show e
                addMessage msgError (toHtml msg)
                $(logWarn) msg

          _ <- runDB $ upsert (Token keyApiVapid t) [TokenStore P.=. t]

          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR TokensVapidR

      _otherwise -> do
          addMessageI msgError MsgInvalidStoreType
          redirect $ DataR TokensVapidR


postTokensVapidClearR :: Handler Html
postTokensVapidClearR = do

    tokenVapid <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val keyApiVapid
        return x

    ((fr,fwClear),etClear) <- runFormPost formTokensClear

    case (fr,tokenVapid) of
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeSession))) -> do
          deleteSession secretVapid
          runDB $ delete tid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR TokensVapidR
          
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeDatabase))) -> do
          runDB $ delete tid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR TokensVapidR
          
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeGoogleSecretManager))) -> do
          
          rtoken <- liftIO $ readFile' (unpack secretVolumeRefreshTokenVapid)

          app <- appSettings <$> getYesod

          refreshResponse <- liftIO $ post "https://oauth2.googleapis.com/token"
              [ "refresh_token" := rtoken
              , "client_id" := (googleApiConfClientId . appGoogleApiConf $ app)
              , "client_secret" := (googleApiConfClientSecret . appGoogleApiConf $ app)
              , "grant_type" := ("refresh_token" :: Text)
              ]

          let atoken = refreshResponse L.^. responseBody . key "access_token" . _String

          let project = gcloudProjectId . appGcloudConf $ app
          let opts = defaults & auth L.?~ oauth2Bearer (encodeUtf8 atoken)

          res <- liftIO $ getWith opts
              (unpack [st|#{smProjects}/#{project}/secrets/#{secretVapid}/versions/latest|])

          let ver :: Maybe Int
              ver = (readMaybe . unpack) <=< (LS.last . splitOn "/") $ res L.^. responseBody . key "name" . _String

          case ver of
            Just v -> void $ liftIO $ tryAny $ postWith opts
                       (unpack [st|#{smProjects}/#{project}/secrets/#{secretVapid}/versions/#{v}:destroy|])
                       (object [])

            Nothing -> return ()

          runDB $ delete tid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR TokensVapidR

      (FormSuccess (),Nothing) -> do
          addMessageI msgSuccess MsgCleared
          redirect $ DataR TokensVapidR
          
      _otherwise -> do
          (fw,et) <- generateFormPost $ formVapid tokenVapid
          msgr <- getMessageRender
          addMessageI msgError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgTokens
              idOverlay <- newIdent
              idFormVapid <- newIdent
              idFormClear <- newIdent
              $(widgetFile "data/tokens/vapid")


postTokensVapidR :: Handler Html
postTokensVapidR = do

    ((fr,_),_) <- runFormPost $ formVapid Nothing

    case fr of
      FormSuccess t@StoreTypeSession -> do
          VAPIDKeysMinDetails s xc yc <- liftIO generateVAPIDKeys
          let triplet = pack $ show (s,xc,yc)
          setSession secretVapid triplet
          void $ runDB $ upsert (Token keyApiVapid t) [TokenStore P.=. t]
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR TokensVapidR
          
      FormSuccess t@StoreTypeDatabase -> do
          VAPIDKeysMinDetails s xc yc <- liftIO generateVAPIDKeys
          let triplet = pack $ show (s,xc,yc)          
          Entity tid _ <- runDB $ upsert (Token keyApiVapid t) [TokenStore P.=. t]
          void $ runDB $ upsert (Store tid secretVapid triplet) [StoreVal P.=. triplet]
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR TokensVapidR

      FormSuccess t@StoreTypeGoogleSecretManager -> do
          app <- appSettings <$> getYesod
          rndr <- getUrlRender

          response <- liftIO $ post "https://accounts.google.com/o/oauth2/v2/auth"
              [ "redirect_uri" := rndr (DataR TokensVapidHookR)
              , "response_type" := ("code" :: Text)
              , "prompt" := ("consent" :: Text)
              , "client_id" := (googleApiConfClientId . appGoogleApiConf $ app)
              , "scope" := ("https://www.googleapis.com/auth/cloud-platform" :: Text)
              , "access_type" := ("offline" :: Text)
              , "state" := pack (show t)
              ]

          return $ preEscapedToHtml $ decodeUtf8 $ toStrict (response L.^. responseBody)
                
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR TokensVapidR


getTokensVapidR :: Handler Html
getTokensVapidR = do

    tokenVapid <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val keyApiVapid
        return x

    (fw,et) <- generateFormPost $ formVapid tokenVapid
    (fwClear,etClear) <- generateFormPost formTokensClear

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgTokens
        idOverlay <- newIdent
        idFormVapid <- newIdent
        idFormClear <- newIdent
        $(widgetFile "data/tokens/vapid")


getTokensGmailHookR :: Handler Html
getTokensGmailHookR = do
    rndr <- getUrlRender
    app <- appSettings <$> getYesod
    let googleClientId = googleApiConfClientId . appGoogleApiConf $ app
    let googleClientSecret = googleApiConfClientSecret . appGoogleApiConf $ app

    code <- runInputGet $ ireq textField "code"
    state <- readMaybe .  unpack <$> runInputGet (ireq textField "state")

    r <- liftIO $ post "https://oauth2.googleapis.com/token"
         [ "code" := code
         , "redirect_uri" := rndr (DataR TokensGmailHookR)
         , "client_id" := googleClientId
         , "client_secret" := googleClientSecret
         , "grant_type" := ("authorization_code" :: Text)
         ]

    let _status = r L.^. responseStatus . statusCode
    let _tokenType = r L.^. responseBody . key "token_type" . _String
    let _scope = r L.^. responseBody . key "scope" . _String

    let accessToken = r L.^. responseBody . key "access_token" . _String
    let refreshToken = r L.^. responseBody . key "refresh_token" . _String
    let _expiresIn = r L.^. responseBody . key "expires_in" . _String

    case state of
      Just (email,x@StoreTypeSession) -> do
          setSession keyAccessTokenGmail accessToken
          setSession secretRefreshTokenGmail refreshToken
          setSession keySendby email
          
          _ <- runDB $ upsert (Token keyApiGmail x) [TokenStore P.=. x]
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR TokensGmailR

      Just (email,x@StoreTypeDatabase) -> do
          Entity tid _ <- runDB $ upsert (Token keyApiGmail x) [TokenStore P.=. x]
          void $ runDB $ upsert (Store tid keyAccessTokenGmail accessToken) [StoreVal P.=. accessToken]
          void $ runDB $ upsert (Store tid secretRefreshTokenGmail refreshToken) [StoreVal P.=. refreshToken]
          void $ runDB $ upsert (Store tid keySendby email) [StoreVal P.=. email]
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR TokensGmailR

      Just (email,x@StoreTypeGoogleSecretManager) -> do
          let project = gcloudProjectId . appGcloudConf $ app

          let opts = defaults & auth L.?~ oauth2Bearer (encodeUtf8 accessToken)
          response <- liftIO $ tryAny $ postWith opts
              (unpack [st|#{smProjects}/#{project}/secrets/#{secretRefreshTokenGmail}:addVersion|])
              (object [ "payload" .= object [ "data" .= decodeUtf8 (B64.encode (encodeUtf8 refreshToken)) ]])

          -- destroy previous version
          case response of
            Right res -> do

                let prev :: Maybe Int
                    prev = (fmap (\y -> y - 1) . readMaybe . unpack) <=< (LS.last . splitOn "/")
                        $ res L.^. responseBody . key "name" . _String

                case prev of
                  Just v | v > 0 -> do
                      void $ liftIO $ tryAny $ postWith opts
                          (unpack [st|#{smProjects}/#{project}/secrets/#{secretRefreshTokenGmail}/versions/#{v}:destroy|])
                          (object [])

                         | otherwise -> return ()
                  Nothing -> return ()

            Left e@(SomeException _) -> case fromException e of
              Just (HttpExceptionRequest _ (StatusCodeException _ bs)) -> do
                  $(logWarn) $ pack $ show e
                  let msg = bs L.^. key "error" . key "message" . _String
                  addMessage msgError (toHtml msg)
                  redirect $ DataR TokensGmailR

              _otherwise -> do
                  let msg = pack $ show e
                  $(logWarn) msg
                  addMessage msgError (toHtml msg)
                  redirect $ DataR TokensGmailR

          Entity tid _ <- runDB $ upsert (Token keyApiGmail x) [TokenStore P.=. x]
          void $ runDB $ upsert (Store tid keySendby email) [StoreVal P.=. email]
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR TokensGmailR

      Nothing -> do
          addMessageI msgError MsgInvalidStoreType
          redirect $ DataR TokensGmailR


postTokensGmailClearR :: Handler Html
postTokensGmailClearR = do

    tokenGmail <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val keyApiGmail
        return x

    ((fr,fwClear),etClear) <- runFormPost formTokensClear
    case (fr,tokenGmail) of
      (FormSuccess (),Just (Entity tid (Token _ StoreTypeSession))) -> do
          deleteSession keyAccessTokenGmail
          deleteSession secretRefreshTokenGmail
          deleteSession keySendby
          runDB $ delete tid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR TokensGmailR

      (FormSuccess (),Just (Entity tid (Token _ StoreTypeDatabase))) -> do
          runDB $ delete tid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR TokensGmailR

      (FormSuccess (),Just (Entity tid (Token _ StoreTypeGoogleSecretManager))) -> do
          app <- appSettings <$> getYesod
          -- 1. read refresh token from mounted volume
          refreshToken <- liftIO $ readFile' (unpack secretVolumeRefreshTokenGmail)

          -- 2. get access token from googleapi
          refreshResponse <- liftIO $ post "https://oauth2.googleapis.com/token"
              [ "refresh_token" := refreshToken
              , "client_id" := (googleApiConfClientId . appGoogleApiConf $ app)
              , "client_secret" := (googleApiConfClientSecret . appGoogleApiConf $ app)
              , "grant_type" := ("refresh_token" :: Text)
              ]

          let newAccessToken = refreshResponse L.^. responseBody . key "access_token" . _String

          let opts = defaults & auth L.?~ oauth2Bearer (encodeUtf8 newAccessToken)

          let project = gcloudProjectId . appGcloudConf $ app

          res <- liftIO $ getWith opts
              (unpack [st|#{smProjects}/#{project}/secrets/#{secretRefreshTokenGmail}/versions/latest|])

          let ver :: Maybe Int
              ver = (readMaybe . unpack) <=< (LS.last . splitOn "/") $ res L.^. responseBody . key "name" . _String

          case ver of
            Just v -> do

                void $ liftIO $ tryAny $ postWith opts
                    (unpack [st|#{smProjects}/#{project}/secrets/#{secretRefreshTokenGmail}/versions/#{v}:destroy|])
                    (object [])

            Nothing -> return ()

          runDB $ delete tid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR TokensGmailR

      (FormSuccess (),Nothing) -> do
          deleteSession keyAccessTokenGmail
          deleteSession secretRefreshTokenGmail
          deleteSession keyAccessTokenGmailExpiresIn
          deleteSession keySendby
          addMessageI msgSuccess MsgCleared
          redirect $ DataR TokensGmailR

      _otherwise -> do
          (fw,et) <- generateFormPost $ formGmail tokenGmail
          msgr <- getMessageRender
          addMessageI msgError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgTokens
              idOverlay <- newIdent
              idFormTokensGmail <- newIdent
              idFormTokensGmailClear <- newIdent
              $(widgetFile "data/tokens/tokens")


formTokensClear :: Form ()
formTokensClear extra = return (FormSuccess (),[whamlet|#{extra}|])


postTokensGmailR :: Handler Html
postTokensGmailR = do

    tokenGmail <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val keyApiGmail
        return x

    ((fr,fw),et) <- runFormPost $ formGmail tokenGmail
    case fr of
      FormSuccess x@(_,StoreTypeGoogleSecretManager) -> do
          app <- appSettings <$> getYesod
          rndr <- getUrlRender

          r <- liftIO $ post "https://accounts.google.com/o/oauth2/v2/auth"
              [ "redirect_uri" := rndr (DataR TokensGmailHookR)
              , "response_type" := ("code" :: Text)
              , "prompt" := ("consent" :: Text)
              , "client_id" := (googleApiConfClientId . appGoogleApiConf $ app)
              , "scope" := ("https://www.googleapis.com/auth/gmail.send https://www.googleapis.com/auth/cloud-platform" :: Text)
              , "access_type" := ("offline" :: Text)
              , "state" := pack (show x)
              ]

          return $ preEscapedToHtml $ decodeUtf8 $ toStrict (r L.^. responseBody)
          
      FormSuccess x -> do
          app <- appSettings <$> getYesod
          rndr <- getUrlRender

          r <- liftIO $ post "https://accounts.google.com/o/oauth2/v2/auth"
              [ "redirect_uri" := rndr (DataR TokensGmailHookR)
              , "response_type" := ("code" :: Text)
              , "prompt" := ("consent" :: Text)
              , "client_id" := (googleApiConfClientId . appGoogleApiConf $ app)
              , "scope" := ("https://www.googleapis.com/auth/gmail.send" :: Text)
              , "access_type" := ("offline" :: Text)
              , "state" := pack (show x)
              ]

          return $ preEscapedToHtml $ decodeUtf8 $ toStrict (r L.^. responseBody)

      _otherwise -> do
          (fwClear,etClear) <- generateFormPost formTokensClear
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgTokens
              idOverlay <- newIdent
              idFormTokensGmail <- newIdent
              idFormTokensGmailClear <- newIdent
              $(widgetFile "data/tokens/tokens")


getTokensGmailR :: Handler Html
getTokensGmailR = do

    tokenGmail <- runDB $ selectOne $ do
        x <- from $ table @Token
        where_ $ x ^. TokenApi ==. val keyApiGmail
        return x

    (fw,et) <- generateFormPost $ formGmail tokenGmail
    (fwClear,etClear) <- generateFormPost formTokensClear

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setUltDestCurrent
        setTitleI MsgTokens
        idOverlay <- newIdent
        idFormTokensGmail <- newIdent
        idFormTokensGmailClear <- newIdent
        $(widgetFile "data/tokens/tokens")


formVapid :: Maybe (Entity Token) -> Form StoreType
formVapid token extra = do
    
    let storeOptions = [ (MsgUserSession, StoreTypeSession)
                       , (MsgDatabase, StoreTypeDatabase)
                       , (MsgGoogleSecretManager, StoreTypeGoogleSecretManager)
                       ]
                       
    (storeR,storeV) <- mreq (md3radioField (optionsPairs storeOptions)) FieldSettings
        { fsLabel = SomeMessage MsgStoreType
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = [("style","display:flex;flex-direction:column;row-gap:1rem")]
        } (tokenStore . entityVal <$> token)
        
    return ( storeR
           , [whamlet|
               #{extra}
               <fieldset.shape-medium>
                 <legend.body-medium>_{MsgStoreType}<sup>*
                 ^{fvInput storeV}
             |]
           )


formGmail :: Maybe (Entity Token)-> Form (Text,StoreType)
formGmail token extra = do

    let storeOptions = [ (MsgUserSession, StoreTypeSession)
                       , (MsgDatabase, StoreTypeDatabase)
                       , (MsgGoogleSecretManager, StoreTypeGoogleSecretManager)
                       ]

    (emailR,emailV) <- mreq emailField FieldSettings
        { fsLabel = SomeMessage MsgEmailAddress
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just "ciukstar@gmail.com")

    (storeR,storeV) <- mreq (md3radioField (optionsPairs storeOptions)) FieldSettings
        { fsLabel = SomeMessage MsgStoreType
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = [("style","display:flex;flex-direction:column;row-gap:1rem")]
        } (tokenStore . entityVal <$> token)

    return ( (,) <$> emailR <*> storeR
           , [whamlet|
               #{extra}

               <div.field.label.border.round>
                 ^{fvInput emailV}
                 <label>
                   #{fvLabel emailV}
                   $if fvRequired emailV
                     <sup>*
                 <span.helper style="line-height:1">
                   _{MsgUserAccountThroughWhichMailWillBeSent}


               <fieldset.shape-medium style="margin-top:3rem">
                 <legend.body-medium>
                   _{MsgStoreType}<sup>*
                 <p.small-text>
                   _{MsgSelectLocationToStoreRefreshToken}

                 ^{fvInput storeV}
             |]
           )


smProjects :: Text
smProjects = "https://secretmanager.googleapis.com/v1/projects"
