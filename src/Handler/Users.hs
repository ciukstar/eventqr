{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Users
  ( getUsersR, postUsersR
  , getUserR, postUserR
  , getUserPhotoR
  , postUserDeleR
  , getUserEditR
  , getUserNewR
  , getUserCardsR
  , getUserCardR
  , getCardQrCodeR
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, where_, val, update, set
    , (^.), (==.), (=.), (:&)((:&))
    , Value (unValue), orderBy, asc, innerJoin, on
    )
    
import Database.Persist
    ( Entity (Entity), entityVal, insert, insert_, delete, upsert)
import qualified Database.Persist as P ((=.))
import Database.Persist.Sql (fromSqlKey)


import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR, StaticR)
    , DataR
      ( UserPhotoR, UsersR, UserR, UserNewR, UserEditR, UserDeleR
      , UserCardsR, UserCardR, CardQrCodeR
      )
    , AppMessage
      ( MsgUsers, MsgPhoto, MsgUser, MsgAdministrator, MsgEmail, MsgName
      , MsgDeleteAreYouSure, MsgDele, MsgConfirmPlease, MsgCancel, MsgYes
      , MsgNo, MsgAttribution, MsgPassword, MsgSave, MsgAlreadyExists
      , MsgRecordAdded, MsgInvalidFormData, MsgRecordDeleted, MsgChangePassword
      , MsgDetails, MsgCards, MsgCard
      )
    )
    
import Model
    ( msgSuccess, msgError
    , UserId, User(User, userName, userEmail, userPassword, userAdmin)
    , UserPhoto (UserPhoto)
    , Card (Card)
    , EntityField
      ( UserPhotoUser, UserId, UserPhotoAttribution, UserEmail, UserPhotoPhoto
      , UserPhotoMime, UserName, UserAdmin, CardIssued, CardUser, CardId), CardId
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, getMessages
    , TypedContent (TypedContent), ToContent (toContent), redirect, whamlet
    , FileInfo (fileContentType), SomeMessage (SomeMessage), MonadHandler (liftHandler)
    , addMessageI, fileSourceByteString, notFound
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Fields
    ( emailField, textField, fileField, passwordField, htmlField
    , checkBoxField
    )
import Yesod.Form.Types
    ( Field
    , FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvErrors, fvInput, fvRequired, fvLabel, fvId ), FormResult (FormSuccess)
    )
import Yesod.Form.Functions (generateFormPost, mreq, mopt, checkM, runFormPost)
import Yesod.Auth.Email (saltPass)

import qualified Codec.QRCode as QR 
import Codec.QRCode.Data.QRCodeOptions (defaultQRCodeOptions)
import Codec.QRCode.Data.ErrorLevel as ErrorLevel (ErrorLevel (M))
import Codec.QRCode.JuicyPixels (toImage)
import Codec.Picture (encodeBitmap)
import Codec.QRCode (TextEncoding(Iso8859_1OrUtf8WithoutECI))


getCardQrCodeR :: CardId -> Handler TypedContent
getCardQrCodeR cid = do
    
    let input = show (fromSqlKey cid)

    case QR.encode (defaultQRCodeOptions ErrorLevel.M) Iso8859_1OrUtf8WithoutECI input of
      Just qrcode -> return $ TypedContent "image/bmp" $ toContent $ encodeBitmap $ toImage 0 1 qrcode
      Nothing -> notFound
    


getUserCardR :: UserId -> CardId -> Handler Html
getUserCardR uid cid = do

    card <- runDB $ selectOne $ do
        x :& u <- from $ table @Card
            `innerJoin` table @User `on` (\(c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. CardId ==. val cid
        return (x,u)
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCard
        idOverlay <- newIdent
        $(widgetFile "data/users/cards/card")


getUserCardsR :: UserId -> Handler Html
getUserCardsR uid = do

    cards <- runDB $ select $ do
        x :& u <- from $ table @Card
            `innerJoin` table @User `on` (\(c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. CardUser ==. val uid
        orderBy [asc (x ^. CardIssued)]
        return (x,u)
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUser
        idOverlay <- newIdent
        $(widgetFile "data/users/cards/cards")


postUserDeleR :: UserId -> Handler Html
postUserDeleR uid = do
    ((fr,_),_) <- runFormPost formUserDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete uid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR UsersR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ UserR uid


getUserEditR :: UserId -> Handler Html
getUserEditR uid = do

    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x

    (fw,et) <- generateFormPost $ formUserEdit user

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUser
        idOverlay <- newIdent
        $(widgetFile "data/users/edit")


getUserNewR :: Handler Html
getUserNewR = do

    (fw,et) <- generateFormPost $ formUser Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUser
        idOverlay <- newIdent
        $(widgetFile "data/users/new")


postUserR :: UserId -> Handler Html
postUserR uid = do

    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
    
    ((fr,fw),et) <- runFormPost $ formUserEdit user

    case fr of
      FormSuccess (User email _ name admin,(Just fi,attrib)) -> do
          void $ runDB $ update $ \x -> do
            set x [ UserEmail =. val email
                  , UserName =. val name
                  , UserAdmin =. val admin
                  ]
            where_ $ x ^. UserId ==. val uid
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (UserPhoto uid (fileContentType fi) bs attrib)
                    [ UserPhotoMime P.=. fileContentType fi
                    , UserPhotoPhoto P.=. bs
                    , UserPhotoAttribution P.=. attrib
                    ]
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR UsersR
          
      FormSuccess (User email _ name admin,(Nothing,attrib)) -> do
          void $ runDB $ update $ \x -> do
            set x [ UserEmail =. val email
                  , UserName =. val name
                  , UserAdmin =. val admin
                  ]
            where_ $ x ^. UserId ==. val uid
          void $ runDB $ update $ \x -> do
              set x [UserPhotoAttribution =. val attrib]
              where_ $ x ^. UserPhotoUser ==. val uid
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR UsersR

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/users/new")


getUserR :: UserId -> Handler Html
getUserR uid = do

    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x

    (fw0,et0) <- generateFormPost formUserDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUser
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/users/user")


formUserDelete :: Form ()
formUserDelete extra = return (pure (), [whamlet|#{extra}|])


postUsersR :: Handler Html
postUsersR = do

    ((fr,fw),et) <- runFormPost $ formUser Nothing

    case fr of
      FormSuccess (r@(User _ (Just pass) _ _),(Just fi,attrib)) -> do
          password <- liftIO $ saltPass pass
          uid <- runDB $ insert r { userPassword = Just password }
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (UserPhoto uid (fileContentType fi) bs attrib)
                    [ UserPhotoMime P.=. fileContentType fi
                    , UserPhotoPhoto P.=. bs
                    , UserPhotoAttribution P.=. attrib
                    ]
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR UsersR
          
      FormSuccess (r@(User _ (Just pass) _ _),(Nothing,attrib)) -> do
          password <- liftIO $ saltPass pass
          uid <- runDB $ insert r { userPassword = Just password }
          void $ runDB $ update $ \x -> do
              set x [UserPhotoAttribution =. val attrib]
              where_ $ x ^. UserPhotoUser ==. val uid
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR UsersR

      FormSuccess (r,_) -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR UsersR

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/users/new")


getUsersR :: Handler Html
getUsersR = do

    users <- runDB $ select $ do
        x <- from $ table @User
        orderBy [asc (x ^. UserName), asc (x ^. UserEmail), asc (x ^. UserId)]
        return x
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUsers
        idOverlay <- newIdent
        $(widgetFile "data/users/users")


formUser :: Maybe (Entity User) -> Form (User,(Maybe FileInfo,Maybe Html))
formUser user extra = do

    (emailR,emailV) <- mreq uniqueEmailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (userEmail . entityVal <$> user)

    (passR,passV) <- mopt passwordField FieldSettings
        { fsLabel = SomeMessage MsgPassword
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (userPassword . entityVal <$> user)

    (nameR,nameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (userName . entityVal <$> user)

    (adminR,adminV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAdministrator
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (userAdmin . entityVal <$> user)

    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    attrib <- (unValue =<<) <$> case user of
      Just (Entity uid _) -> liftHandler $ runDB $ selectOne $ do
          x <- from $ table @UserPhoto
          where_ $ x ^. UserPhotoUser ==. val uid
          return $ x ^. UserPhotoAttribution
      Nothing -> return Nothing

    (attribR,attribV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just attrib)

    let r = (,) <$> (User <$> emailR <*> passR <*> nameR <*> adminR)
                <*> ((,) <$> photoR <*> attribR)

    idPhotoContainer <- newIdent
    idFigurePhoto <- newIdent
    idLabelPhoto <- newIdent
    idImgPhoto <- newIdent

    let w = $(widgetFile "data/users/form") 
    return (r,w)
  where
      uniqueEmailField :: Field Handler Text
      uniqueEmailField = checkM uniqueEmail emailField

      uniqueEmail :: Text -> Handler (Either AppMessage Text)
      uniqueEmail email = do
          x <- runDB $ selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserEmail ==. val email
              return x
          return $ case x of
            Nothing -> Right email
            Just (Entity rid _) -> case user of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right email
                                   | otherwise -> Left MsgAlreadyExists


formUserEdit :: Maybe (Entity User) -> Form (User,(Maybe FileInfo,Maybe Html))
formUserEdit user extra = do

    (emailR,emailV) <- mreq uniqueEmailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (userEmail . entityVal <$> user)

    (nameR,nameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (userName . entityVal <$> user)

    (adminR,adminV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgAdministrator
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (userAdmin . entityVal <$> user)

    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    attrib <- (unValue =<<) <$> case user of
      Just (Entity uid _) -> liftHandler $ runDB $ selectOne $ do
          x <- from $ table @UserPhoto
          where_ $ x ^. UserPhotoUser ==. val uid
          return $ x ^. UserPhotoAttribution
      Nothing -> return Nothing

    (attribR,attribV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (Just attrib)

    let r = (,) <$> (User <$> emailR <*> pure Nothing <*> nameR <*> adminR)
                <*> ((,) <$> photoR <*> attribR)

    idPhotoContainer <- newIdent
    idFigurePhoto <- newIdent
    idLabelPhoto <- newIdent
    idImgPhoto <- newIdent

    let w = $(widgetFile "data/users/form-edit")
    return (r,w)
  where
      uniqueEmailField :: Field Handler Text
      uniqueEmailField = checkM uniqueEmail emailField

      uniqueEmail :: Text -> Handler (Either AppMessage Text)
      uniqueEmail email = do
          x <- runDB $ selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserEmail ==. val email
              return x
          return $ case x of
            Nothing -> Right email
            Just (Entity rid _) -> case user of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right email
                                   | otherwise -> Left MsgAlreadyExists


getUserPhotoR :: UserId -> Handler TypedContent
getUserPhotoR uid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @UserPhoto
        where_ $ x ^. UserPhotoUser ==. val uid
        return x
    case photo of
      Just (Entity _ (UserPhoto _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
