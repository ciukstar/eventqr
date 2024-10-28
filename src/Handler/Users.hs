{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Handler.Users
  ( getUsersR, postUsersR
  , getUserR, postUserR
  , getUserPhotoR
  , postUserDeleR
  , getUserEditR
  , getUserNewR
  , getUserCardsR, postUserCardsR
  , getUserCardR
  , getUserCardNewR
  , getCardQrImageR
  ) where


import qualified Codec.QRCode as QR 
import Codec.QRCode.Data.QRCodeOptions (defaultQRCodeOptions)
import Codec.QRCode.Data.ErrorLevel as ErrorLevel (ErrorLevel (M))
import Codec.QRCode.JuicyPixels (toImage)
import Codec.Picture (encodeBitmap)
import Codec.QRCode (TextEncoding(Iso8859_1OrUtf8WithoutECI))

import Control.Monad (void, forM)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(first))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)

import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, where_, val, update, set
    , (^.), (==.), (=.), (:&)((:&))
    , Value (unValue), orderBy, asc, innerJoin, on
    , delete, PersistStoreWrite (insertMany_)
    )
    
import Database.Persist
    ( Entity (Entity), entityVal, insert, insert_, upsert)
import qualified Database.Persist as P ((=.), delete)
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR, StaticR)
    , DataR
      ( UserPhotoR, UsersR, UserR, UserNewR, UserEditR, UserDeleR
      , UserCardsR, UserCardR, CardQrImageR, UserCardNewR
      )
    , AppMessage
      ( MsgUsers, MsgPhoto, MsgUser, MsgAdministrator, MsgEmail, MsgName
      , MsgDeleteAreYouSure, MsgDele, MsgConfirmPlease, MsgCancel, MsgYes
      , MsgNo, MsgAttribution, MsgPassword, MsgSave, MsgAlreadyExists
      , MsgRecordAdded, MsgInvalidFormData, MsgRecordDeleted, MsgNewField
      , MsgDetails, MsgCards, MsgCard, MsgIssueDate, MsgAddField, MsgAdd
      , MsgChangePassword, MsgClose, MsgQrCode, MsgCardNumber, MsgCardholder, MsgValue
      )
    )
    
import Model
    ( msgSuccess, msgError
    , UserId, User(User, userName, userEmail, userPassword, userAdmin)
    , UserPhoto (UserPhoto)
    , Card (Card)
    , CardId, Info (Info)
    , EntityField
      ( UserPhotoUser, UserId, UserPhotoAttribution, UserEmail, UserPhotoPhoto
      , UserPhotoMime, UserName, UserAdmin, CardIssued, CardUser, CardId, InfoCard, InfoId)
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)

import Yesod.Auth.Email (saltPass)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, getMessages
    , TypedContent (TypedContent), ToContent (toContent), redirect, whamlet
    , FileInfo (fileContentType), SomeMessage (SomeMessage), notFound
    , MonadHandler (liftHandler), addMessageI, fileSourceByteString
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Fields
    ( emailField, textField, fileField, passwordField, htmlField
    , checkBoxField
    )
import Yesod.Form.Types
    ( Field, FormResult (FormSuccess)
    , FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvErrors, fvInput, fvRequired, fvLabel, fvId )
    )
import Yesod.Form.Functions (generateFormPost, mreq, mopt, checkM, runFormPost)


postUserCardsR :: UserId -> Handler Html
postUserCardsR uid = do

    ((fr,fw),et) <- runFormPost $ formCard uid Nothing [("field 1","value 1")]

    case fr of
      FormSuccess (c,attrs) -> do          
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
              idButtonAddField <- newIdent
              idDialogAddField <- newIdent
              idButtonCloseDialogAddField <- newIdent
              $(widgetFile "data/users/cards/new")


getUserCardNewR :: UserId -> Handler Html
getUserCardNewR uid = do

    (fw,et) <- generateFormPost $ formCard uid Nothing []

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCard 
        idOverlay <- newIdent
        idButtonAddField <- newIdent
        idDialogAddField <- newIdent
        idButtonCloseDialogAddField <- newIdent
        $(widgetFile "data/users/cards/new")


formCard :: UserId -> Maybe (Entity Card) -> [(Text, Text)] -> Form (Card,[(Text,Text)])
formCard uid card fields extra = do
    now <- liftIO getCurrentTime

    attrs <- forM fields $ \(name, value) -> mreq textField FieldSettings
        { fsLabel = SomeMessage name
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (Just value) >>= \x -> return (first ((name,) <$>) x,name)
    
    let r = (,) (Card uid "" now) <$> traverse (fst . fst) attrs
    let w = [whamlet|
                    ^{extra}
                    $forall ((_,v), name) <- attrs
                      <div.field.label.round.border>
                        ^{fvInput v}
                        <label for=#{fvId v}>
                          #{name}
                    |]
    return (r,w)


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
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCard
        idOverlay <- newIdent
        idButtonShowDialogQrCode <- newIdent
        idDialogQrCode <- newIdent
        idButtonCloseDialogQrCode <- newIdent
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
          runDB $ P.delete uid
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


getCardQrImageR :: CardId -> Handler TypedContent
getCardQrImageR cid = do
    
    let input = show (fromSqlKey cid)

    case QR.encode (defaultQRCodeOptions ErrorLevel.M) Iso8859_1OrUtf8WithoutECI input of
      Just qrcode -> return $ TypedContent "image/bmp" $ toContent $ encodeBitmap $ toImage 0 1 qrcode
      Nothing -> notFound
