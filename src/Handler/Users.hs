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
  , getUserSettingsR
  , postUserSubscriptionsR
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (toJSON)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, where_, val, update, set
    , (^.), (==.), (=.)
    , Value (unValue), orderBy, asc
    )
    
import Database.Persist
    ( Entity (Entity), entityVal, insert, insert_, upsert, upsertBy, delete, deleteBy)
import qualified Database.Persist as P ((=.), delete)

import Foundation
    ( App (appSettings), Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR, StaticR, HomeR)
    , DataR
      ( UserPhotoR, UsersR, UserR, UserNewR, UserEditR, UserDeleR
      , UserCardsR, UserSettingsR, UserSubscriptionsR
      )
    , AppMessage
      ( MsgUsers, MsgPhoto, MsgUser, MsgAdministrator, MsgEmail, MsgName
      , MsgDeleteAreYouSure, MsgDele, MsgConfirmPlease, MsgCancel, MsgYes
      , MsgNo, MsgAttribution, MsgPassword, MsgSave, MsgAlreadyExists
      , MsgRecordAdded, MsgInvalidFormData, MsgRecordDeleted, MsgSettings
      , MsgDetails, MsgCards, MsgChangePassword, MsgRecordEdited
      , MsgSubscribeToPushNotifications, MsgVapidNotInitializedProperly
      , MsgSubscriptionSuccessful, MsgNotificationsHaveBeenDisabled
      , MsgEnableNotificationsPlease, MsgUnsubscribeSuccessful
      )
    )

import Material3 (md3switchWidget)
    
import Model
    ( msgSuccess, msgError
    , UserId, User(User, userName, userEmail, userPassword, userAdmin)
    , UserPhoto (UserPhoto)
    , Unique (UniquePushSubscription)
    , PushSubscription
      ( PushSubscription, pushSubscriptionEndpoint, pushSubscriptionP256dh
      , pushSubscriptionAuth
      )
    , EntityField
      ( UserPhotoUser, UserId, UserPhotoAttribution, UserEmail, UserPhotoPhoto
      , UserPhotoMime, UserName, UserAdmin, PushSubscriptionUser
      , PushSubscriptionEndpoint, PushSubscriptionP256dh, PushSubscriptionAuth
      )
    )

import Settings (widgetFile, AppSettings (appVAPIDKeys))
import Settings.StaticFiles
    ( img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)

import Web.WebPush (VAPIDKeys, vapidPublicKeyBytes)

import Yesod.Auth.Email (saltPass)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, getMessages
    , TypedContent (TypedContent), ToContent (toContent), redirect, whamlet
    , FileInfo (fileContentType), SomeMessage (SomeMessage)
    , MonadHandler (liftHandler), addMessageI, fileSourceByteString, getYesod
    )
import Yesod.Form.Fields
    ( emailField, textField, fileField, passwordField, htmlField
    , checkBoxField, hiddenField
    )
import Yesod.Form.Functions (generateFormPost, mreq, mopt, checkM, runFormPost)
import Yesod.Form.Types
    ( Field, FormResult (FormSuccess)
    , FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvErrors, fvInput, fvRequired, fvLabel, fvId )
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postUserSubscriptionsR :: UserId -> Handler Html
postUserSubscriptionsR uid = do

    vapid <- appVAPIDKeys . appSettings <$> getYesod

    case vapid of
      Nothing -> do
          msgr <- getMessageRender
          defaultLayout $ do
              setTitleI MsgSettings 
              idOverlay <- newIdent
              $(widgetFile "data/account/novapid")
              
      Just vapidKeys -> do

          subscription <- runDB $ selectOne $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionUser ==. val uid
              return x

          ((fr,_),_) <- runFormPost $ formSubscribe vapidKeys uid subscription 

          case fr of
            FormSuccess (True, ps@(PushSubscription uid' endpoint' keyP256dh' keyAuth')) -> do
                void $ runDB $ upsertBy (UniquePushSubscription endpoint') ps
                    [ PushSubscriptionUser P.=. uid'
                    , PushSubscriptionP256dh P.=. keyP256dh'
                    , PushSubscriptionAuth P.=. keyAuth'
                    ]
                addMessageI msgSuccess MsgSubscriptionSuccessful
                redirect $ DataR $ UserSettingsR uid
                
            FormSuccess (False, PushSubscription _ endpoint' _ _) -> do
                void $ runDB $ deleteBy (UniquePushSubscription endpoint')
                addMessageI msgSuccess MsgUnsubscribeSuccessful
                redirect $ DataR $ UserSettingsR uid

            _otherwise -> do           
                addMessageI msgError MsgInvalidFormData
                redirect $ DataR $ UserSettingsR uid


getUserSettingsR :: UserId -> Handler Html
getUserSettingsR uid = do

    vapid <- appVAPIDKeys . appSettings <$> getYesod

    case vapid of
      Nothing -> do
          msgr <- getMessageRender
          defaultLayout $ do
              setTitleI MsgSettings 
              idOverlay <- newIdent
              $(widgetFile "data/account/novapid")
              
      Just vapidKeys -> do

          subscription <- runDB $ selectOne $ do
              x <- from $ table @PushSubscription
              where_ $ x ^. PushSubscriptionUser ==. val uid
              return x

          (fw,et) <- generateFormPost $ formSubscribe vapidKeys uid subscription

          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgSettings
              idOverlay <- newIdent
              idFormSubscription <- newIdent
              $(widgetFile "data/account/push/subscriptions")


formSubscribe :: VAPIDKeys -> UserId -> Maybe (Entity PushSubscription) -> Form (Bool,PushSubscription)
formSubscribe vapidKeys uid subscription extra = do

    (res,view) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgSubscribeToPushNotifications
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } ( pure (isJust subscription) )

    (endpointR,endpointV) <- mreq hiddenField "" (pushSubscriptionEndpoint . entityVal <$> subscription)
    (p256dhR,p256dhV) <- mreq hiddenField "" (pushSubscriptionP256dh . entityVal <$> subscription)
    (authR,authV) <- mreq hiddenField "" (pushSubscriptionAuth . entityVal <$> subscription)

    let applicationServerKey = vapidPublicKeyBytes vapidKeys
    let r = (,) <$> res <*> (PushSubscription uid <$> endpointR <*> p256dhR <*> authR)
    let w = $(widgetFile "data/account/push/form")
    return (r,w)


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
          addMessageI msgSuccess MsgRecordEdited
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
          addMessageI msgSuccess MsgRecordEdited
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
