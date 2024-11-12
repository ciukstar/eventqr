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
  , getUserNotificationsR, getUserNotificationR
  , postUserNotificationDeleR
  , getUserSettingsR
  , postUserSubscriptionsR, postUserUnsubscribeR
  ) where

import ClassyPrelude (readMay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (toJSON)
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Clock (getCurrentTime)

import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, where_, val, update, set
    , (^.), (==.), (!=.), (=.), (:&) ((:&))
    , Value (unValue), orderBy, asc, innerJoin, on
    )
    
import Database.Persist
    ( Entity (Entity), entityVal, insert, insert_, upsert, upsertBy, deleteBy)
import qualified Database.Persist as P ((=.), delete)
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, widgetTopbar, widgetSnackbar
    , Route (DataR, StaticR, HomeR)
    , DataR
      ( UserPhotoR, UsersR, UserR, UserNewR, UserEditR, UserDeleR
      , UserCardsR, UserNotificationsR, UserNotificationR, UserSettingsR
      , UserSubscriptionsR, UserNotificationDeleR, UserUnsubscribeR
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
      , MsgSuperuser, MsgNotifications, MsgNoNotificationsForYouAtTheMoment
      , MsgRead, MsgUnread, MsgFrom, MsgNotification, MsgMessage, MsgSent
      , MsgMessageSubject, MsgVapidRequiredToPushNotifications
      , MsgVapidCanBeGeneratedByAdmin, MsgYourOtherSubscriptions, MsgUnsubscribe
      , MsgUnknownDevice, MsgUnsubscribeAreYouSure, MsgManager
      )
    )

import Handler.Tokens (fetchVapidKeys)

import Material3 (md3widget, md3switchWidget)
    
import Model
    ( msgSuccess, msgError
    , UserId
    , User(User, userName, userEmail, userPassword, userAdmin, userManager)
    , UserPhoto (UserPhoto)
    , Unique (UniquePushSubscription)
    , PushSubscriptionId
    , PushSubscription
      ( PushSubscription, pushSubscriptionEndpoint, pushSubscriptionP256dh
      , pushSubscriptionAuth
      )
    , NotificationId, Notification (Notification)
    , NotificationStatus (NotificationStatusRead, NotificationStatusUnread)
    , EntityField
      ( UserPhotoUser, UserId, UserPhotoAttribution, UserEmail, UserPhotoPhoto
      , UserPhotoMime, UserName, UserAdmin, PushSubscriptionUser, UserManager
      , PushSubscriptionP256dh, PushSubscriptionAuth, NotificationRecipient
      , NotificationPublisher, NotificationStatus, PushSubscriptionEndpoint
      , NotificationId, PushSubscriptionTime, PushSubscriptionUserAgent
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Hamlet (Html)

import Web.WebPush (VAPIDKeys, vapidPublicKeyBytes)

import Yesod.Auth.Email (saltPass)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, getMessages
    , TypedContent (TypedContent), ToContent (toContent), redirect, whamlet
    , FileInfo (fileContentType), SomeMessage (SomeMessage)
    , MonadHandler (liftHandler), addMessageI, fileSourceByteString
    , YesodRequest (reqGetParams), getRequest, lookupHeader
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
import Yesod.Form.Input (runInputGet, iopt)


postUserUnsubscribeR :: UserId -> PushSubscriptionId -> Handler Html
postUserUnsubscribeR uid sid = do
    stati <- reqGetParams <$> getRequest
    ((fr0,_),_) <- runFormPost formUserDelete
    case fr0 of
      FormSuccess () -> do
          runDB $ P.delete sid
          addMessageI msgSuccess MsgUnsubscribeSuccessful
          redirect (DataR $ UserSettingsR uid, stati)
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect (DataR $ UserSettingsR uid, stati)


postUserSubscriptionsR :: UserId -> Handler Html
postUserSubscriptionsR uid = do

    vapid <- fetchVapidKeys

    subscription <- runDB $ selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionUser ==. val uid
        return x

    ((fr,_),_) <- runFormPost $ formSubscribe uid vapid subscription 

    case fr of
      FormSuccess (True, ps@(PushSubscription uid' endpoint' keyP256dh' keyAuth' time' au')) -> do
          void $ runDB $ upsertBy (UniquePushSubscription endpoint') ps
              [ PushSubscriptionUser P.=. uid'
              , PushSubscriptionP256dh P.=. keyP256dh'
              , PushSubscriptionAuth P.=. keyAuth'
              , PushSubscriptionTime P.=. time'
              , PushSubscriptionUserAgent P.=. au'
              ]
          addMessageI msgSuccess MsgSubscriptionSuccessful
          redirect (DataR $ UserSettingsR uid,[("endpoint",endpoint')])

      FormSuccess (False, PushSubscription _ endpoint' _ _ _ _) -> do
          void $ runDB $ deleteBy (UniquePushSubscription endpoint')
          addMessageI msgSuccess MsgUnsubscribeSuccessful
          redirect $ DataR $ UserSettingsR uid

      _otherwise -> do           
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ UserSettingsR uid


getUserSettingsR :: UserId -> Handler Html
getUserSettingsR uid = do
    stati <- reqGetParams <$> getRequest
    endpoint <- runInputGet $ iopt textField "endpoint"
    
    vapid <- fetchVapidKeys

    subscription <- case endpoint of
      Nothing -> return Nothing
      Just ep -> runDB $ selectOne $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionUser ==. val uid
        where_ $ x ^. PushSubscriptionEndpoint ==. val ep
        return x

    (fw,et) <- generateFormPost ( formSubscribe uid vapid subscription )
    
    subscriptions <- case endpoint of
      Nothing -> return []
      Just ep -> runDB $ select $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionUser ==. val uid
        where_ $ x ^. PushSubscriptionEndpoint !=. val ep
        return x

    (fw0,et0) <- generateFormPost formUnsubscribe

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSettings 
        idDialogUnsubscribe <- newIdent
        idOverlay <- newIdent
        idFormSubscription <- newIdent
        $(widgetFile "data/account/push/subscriptions")


formUnsubscribe :: Form ()
formUnsubscribe extra = return (pure (), [whamlet|#{extra}|])


postUserNotificationDeleR  :: UserId -> NotificationId -> Handler Html
postUserNotificationDeleR uid nid = do
    ((fr0,_),_) <- runFormPost formUserNotificationDelete
    case fr0 of
      FormSuccess () -> do
          runDB $ P.delete nid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ UserNotificationsR uid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ UserNotificationR uid nid


getUserNotificationR :: UserId -> NotificationId -> Handler Html
getUserNotificationR uid nid = do

    stati <- reqGetParams <$> getRequest


    notification <- runDB $ selectOne $ do
        x :& p <- from $ table @Notification
            `innerJoin` table @User `on` (\(x :& p) -> x ^. NotificationPublisher ==. p ^. UserId)
        where_ $ x ^. NotificationId ==. val nid
        return (x,p)

    runDB $ update $ \x -> do
      set x [ NotificationStatus =. val NotificationStatusRead ]
      where_ $ x ^. NotificationId ==. val nid

    (fw0,et0) <- generateFormPost formUserNotificationDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgNotification 
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/account/notification")


formUserNotificationDelete :: Form ()
formUserNotificationDelete extra = return (pure (), [whamlet|#{extra}|])


getUserNotificationsR :: UserId -> Handler Html
getUserNotificationsR uid = do

    status <- (readMay =<<) <$> runInputGet ( iopt textField "status" )

    notifications <- runDB $ select $ do
        x :& p <- from $ table @Notification
            `innerJoin` table @User `on` (\(x :& p) -> x ^. NotificationPublisher ==. p ^. UserId)
        where_ $ x ^. NotificationRecipient ==. val uid
        case status of
          Just s -> where_ $ x ^. NotificationStatus ==. val s
          Nothing -> return ()
        return (x,p)

    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgNotifications
        idAnchorSettings <- newIdent
        idFormSearch <- newIdent
        $(widgetFile "data/account/notifications")


formSubscribe :: UserId -> Maybe VAPIDKeys -> Maybe (Entity PushSubscription) -> Form (Bool,PushSubscription)
formSubscribe uid vapidKeys subscription extra = do

    (res,view) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgSubscribeToPushNotifications
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } ( pure (isJust subscription) )

    (endpointR,endpointV) <- mreq hiddenField "" (pushSubscriptionEndpoint . entityVal <$> subscription)
    (p256dhR,p256dhV) <- mreq hiddenField "" (pushSubscriptionP256dh . entityVal <$> subscription)
    (authR,authV) <- mreq hiddenField "" (pushSubscriptionAuth . entityVal <$> subscription)

    now <- liftIO getCurrentTime
    ua <- (decodeUtf8 <$>) <$> lookupHeader "User-Agent"
    let r = (,) <$> res <*> (PushSubscription uid <$> endpointR <*> p256dhR <*> authR <*> pure now <*> pure ua)

    let applicationServerKey = maybe [] vapidPublicKeyBytes vapidKeys
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
      FormSuccess (User email _ name _ admin manager,(Just fi,attrib)) -> do
          void $ runDB $ update $ \x -> do
            set x [ UserEmail =. val email
                  , UserName =. val name
                  , UserAdmin =. val admin
                  , UserManager =. val manager
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
          
      FormSuccess (User email _ name _ admin manager,(Nothing,attrib)) -> do
          void $ runDB $ update $ \x -> do
            set x [ UserEmail =. val email
                  , UserName =. val name
                  , UserAdmin =. val admin
                  , UserManager =. val manager
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
      FormSuccess (r@(User _ (Just pass) _ _ _ _),(Just fi,attrib)) -> do
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
          
      FormSuccess (r@(User _ (Just pass) _ _ _ _),(Nothing,attrib)) -> do
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

    (managerR,managerV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgManager
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (userManager . entityVal <$> user)

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

    let r = (,) <$> (User <$> emailR <*> passR <*> nameR <*> pure False <*> adminR <*> managerR)
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

    (managerR,managerV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgManager
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = []
        } (userManager . entityVal <$> user)

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

    let r = (,) <$> (User <$> emailR <*> pure Nothing <*> nameR <*> pure False <*> adminR <*> managerR)
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
