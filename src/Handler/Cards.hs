{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Handler.Cards
  ( getUserCardsR, postUserCardsR
  , getUserCardR, postUserCardR
  , getUserCardNewR
  , postUserCardsNewFieldR
  , getUserCardEditR
  , postUserCardNewFieldR
  , postUserCardDeleR
  , getCardQrImageR
  , getCardPhotoR
  ) where


import qualified Codec.QRCode as QR 
import Codec.QRCode.Data.QRCodeOptions (defaultQRCodeOptions)
import Codec.QRCode.Data.ErrorLevel as ErrorLevel (ErrorLevel (M))
import Codec.QRCode.JuicyPixels (toImage)
import Codec.Picture (encodeBitmap)
import Codec.QRCode (TextEncoding(Iso8859_1OrUtf8WithoutECI))

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(first, second))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)

import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, where_, val
    , (^.), (==.), (:&)((:&))
    , orderBy, asc, innerJoin, on
    , delete
    )
    
import Database.Persist
    ( Entity (Entity), entityVal, insert, insertMany_, replace)
import qualified Database.Persist as P (delete)
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, App, widgetTopbar, widgetSnackbar
    , Route (DataR, StaticR)
    , DataR
      ( UserPhotoR, UsersR, UserR
      , UserCardsR, UserCardR, CardQrImageR, UserCardNewR
      , UserCardsNewFieldR, UserCardEditR, UserCardNewFieldR
      , UserCardDeleR
      )
    , AppMessage
      ( MsgPhoto, MsgUser, MsgName
      , MsgDeleteAreYouSure, MsgDele, MsgConfirmPlease, MsgCancel
      , MsgSave, MsgCardDoesNotContainAdditionalInfo
      , MsgRecordAdded, MsgInvalidFormData, MsgRecordDeleted, MsgNewField
      , MsgDetails, MsgCards, MsgCard, MsgIssueDate, MsgAdd, MsgClose
      , MsgQrCode, MsgCardNumber, MsgCardholder
      , MsgValue, MsgNewFieldNameRequired, MsgNoFieldsForThisCardYet
      , MsgUserHasNoCardsYet, MsgRecordEdited
      )
    )

import Material3 (md3widget, md3textareaWidget)
    
import Model
    ( msgSuccess, msgError
    , UserId, User(User)
    , Card (Card)
    , CardId, Info (Info)
    , EntityField
      ( UserId, CardIssued, CardUser, CardId, InfoId, InfoCard, PhotoCard
      ), Photo (Photo)
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, getMessages
    , TypedContent (TypedContent), ToContent (toContent), redirect, whamlet
    , SomeMessage (SomeMessage), notFound
    , addMessageI, toHtml
    , getPostParams
    )
import Yesod.Form.Fields
    ( textField, htmlField
    )
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost)
import Yesod.Form.Types
    ( FormResult (FormSuccess)
    , FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postUserCardDeleR :: UserId -> CardId -> Handler Html
postUserCardDeleR  uid cid = do
    ((fr,_),_) <- runFormPost formCardDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete cid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ UserCardsR uid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ UserCardR uid cid


postUserCardNewFieldR :: UserId -> CardId -> Handler Html
postUserCardNewFieldR uid cid = do

    fields <- (second toHtml <$>) . filter paramsOut <$> getPostParams

    card <- runDB $ selectOne $ do
        x <- from $ table @Card
        where_ $ x ^. CardId ==. val cid
        return x

    let route = DataR $ UserCardNewFieldR uid cid
    
    ((fr,fw),et) <- runFormPost $ formCard route uid card fields
    
    case fr of
      FormSuccess ((_, attrs),(Just name,Just value)) -> do
          (fw,et) <- generateFormPost $ formCard route uid Nothing (attrs <> [(name,value)])

          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/edit")
              
      FormSuccess ((_, attrs),(Just name,Nothing)) -> do
          (fw,et) <- generateFormPost $ formCard route uid Nothing (attrs <> [(name,"")])

          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/edit")
              
      FormSuccess ((_, attrs),(Nothing,_)) -> do
          (fw,et) <- generateFormPost $ formCard route uid Nothing attrs

          msgr <- getMessageRender
          addMessageI msgError MsgNewFieldNameRequired
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/edit")

      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/edit")


postUserCardR :: UserId -> CardId -> Handler Html
postUserCardR uid cid = do

    fields <- (second toHtml <$>) . filter paramsOut <$> getPostParams

    card <- runDB $ selectOne $ do
        x <- from $ table @Card
        where_ $ x ^. CardId ==. val cid
        return x

    ((fr,fw),et) <- runFormPost $ formCard (DataR $ UserCardNewFieldR uid cid) uid card fields

    case fr of
      FormSuccess ((c,attrs),_) -> do
          runDB $ replace cid c
          runDB $ delete $ do
              x <- from $ table @Info
              where_ $ x ^. InfoCard ==. val cid
          runDB $ insertMany_ $ uncurry (Info cid) <$> attrs
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ UserCardR uid cid
          
      _otherwise -> do
          msgr <- getMessageRender
          addMessageI msgError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/edit")


getUserCardEditR :: UserId -> CardId -> Handler Html
getUserCardEditR uid cid = do

    card <- runDB $ selectOne $ do
        x <- from $ table @Card
        where_ $ x ^. CardId ==. val cid
        return x

    attrs <- ((\(Entity _ (Info _ name value)) -> (name, value)) <$>) <$> runDB (select $ do
        x <- from $ table @Info
        where_ $ x ^. InfoCard ==. val cid
        orderBy [asc (x ^. InfoId)]
        return x )
    
    (fw,et) <- generateFormPost $ formCard (DataR $ UserCardNewFieldR uid cid) uid card attrs

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCard 
        idOverlay <- newIdent
        $(widgetFile "data/users/cards/edit")


postUserCardsNewFieldR :: UserId -> Handler Html
postUserCardsNewFieldR uid = do

    fields <- (second toHtml <$>) . filter paramsOut <$> getPostParams
    
    let route = DataR $ UserCardsNewFieldR uid
    
    ((fr,fw),et) <- runFormPost $ formCard route uid Nothing fields
    
    case fr of
      FormSuccess ((_, attrs),(Just name,Just value)) -> do
          (fw,et) <- generateFormPost $ formCard route uid Nothing (attrs <> [(name,value)])

          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/new")
              
      FormSuccess ((_, attrs),(Just name,Nothing)) -> do
          (fw,et) <- generateFormPost $ formCard route uid Nothing (attrs <> [(name,"")])

          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/new")
              
      FormSuccess ((_, attrs),(Nothing,_)) -> do
          (fw,et) <- generateFormPost $ formCard route uid Nothing attrs

          msgr <- getMessageRender
          addMessageI msgError MsgNewFieldNameRequired
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/new")

      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/new")


postUserCardsR :: UserId -> Handler Html
postUserCardsR uid = do

    fields <- (second toHtml <$>) . filter paramsOut <$> getPostParams

    ((fr,fw),et) <- runFormPost $ formCard (DataR $ UserCardsNewFieldR uid) uid Nothing fields

    case fr of
      FormSuccess ((c,attrs),_) -> do
          cid <- runDB $ insert c
          runDB $ delete $ do
              x <- from $ table @Info
              where_ $ x ^. InfoCard ==. val cid
          runDB $ insertMany_ $ uncurry (Info cid) <$> attrs
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ UserCardsR uid
          
      _otherwise -> do
          msgr <- getMessageRender
          addMessageI msgError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/new")


getUserCardNewR :: UserId -> Handler Html
getUserCardNewR uid = do

    (fw,et) <- generateFormPost $ formCard (DataR $ UserCardsNewFieldR uid) uid Nothing []

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCard 
        idOverlay <- newIdent
        $(widgetFile "data/users/cards/new")


formCard :: Route App -> UserId -> Maybe (Entity Card) -> [(Text, Html)]
         -> Form ((Card,[(Text,Html)]),(Maybe Text,Maybe Html))
formCard route uid card fields extra = do
    now <- liftIO getCurrentTime

    attrs <- forM fields $ \(name, value) -> mreq htmlField FieldSettings
        { fsLabel = SomeMessage name
        , fsName = Just name
        , fsId = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just value) >>= \x -> return (first ((name,) <$>) x)
    
    (nameR,nameV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsName = Just paramFieldNewName
        , fsId = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
        
    (valR,valV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgValue
        , fsName = Just paramFieldNewValue
        , fsId = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing

    let r = (,) <$> ( (,) (maybe (Card uid "" now) entityVal card) <$> traverse fst attrs )
                <*> ( (,) <$> nameR <*> valR )
    let w = do
            idDetailsNewField <- newIdent
            $(widgetFile "data/users/cards/form")
    return (r,w)


paramFieldNewName :: Text
paramFieldNewName = "newName"

paramFieldNewValue :: Text
paramFieldNewValue = "newValue"

paramFieldToken :: Text
paramFieldToken = "_token"

paramsOut :: (Text, Text) -> Bool
paramsOut (p,_) = (p /= paramFieldToken) && (p /= paramFieldNewName) && (p /= paramFieldNewValue)


getUserCardR :: UserId -> CardId -> Handler Html
getUserCardR uid cid = do

    card <- runDB $ selectOne $ do
        x :& u <- from $ table @Card
            `innerJoin` table @User `on` (\(c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. CardId ==. val cid
        return (x,u)

    attrs <- runDB $ select $ do
        x <- from $ table @Info
        where_ $ x ^. InfoCard ==. val cid
        orderBy [asc (x ^. InfoId)]
        return x

    (fw0,et0) <- generateFormPost formCardDelete
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCard
        idOverlay <- newIdent
        idButtonShowDialogQrCode <- newIdent
        idDialogQrCode <- newIdent
        idButtonCloseDialogQrCode <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/users/cards/card")


formCardDelete :: Form ()
formCardDelete extra = return (pure (),[whamlet|^{extra}|])


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


getCardQrImageR :: CardId -> Handler TypedContent
getCardQrImageR cid = do
    
    let input = show (fromSqlKey cid)

    case QR.encode (defaultQRCodeOptions ErrorLevel.M) Iso8859_1OrUtf8WithoutECI input of
      Just qrcode -> return $ TypedContent "image/bmp" $ toContent $ encodeBitmap $ toImage 4 7 qrcode
      Nothing -> notFound


getCardPhotoR :: CardId -> Handler TypedContent
getCardPhotoR cid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @Photo
        where_ $ x ^. PhotoCard ==. val cid
        return x
    case photo of
      Just (Entity _ (Photo _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
