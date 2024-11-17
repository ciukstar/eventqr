{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Accounts
  ( getAccountProfileR, getAccountSettingsR
  , getAccountEventScheduleR, getAccountEventR
  , getAccountNotificationsR, getAccountNotificationR
  , postAccountNotificationDeleR
  , getAccountPushSettingsR
  , postUserSubscriptionsR, postUserUnsubscribeR
  , postAccountEventUnregisterR
  , getAccountEventAttendeesR
  ) where

import ClassyPrelude (readMay, isJust)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (toJSON)
import Data.Bifunctor (Bifunctor(second))
import Data.Maybe (fromMaybe)
import Data.Text (pack) 
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (unValue), selectOne, select, from, table, where_, val
    , (^.), (==.), (:&) ((:&)), (>=.), (=.), (!=.)
    , innerJoin, on, subSelectCount, orderBy, asc
    , update, set, delete, in_, subSelectList
    )
import Database.Persist (Entity (Entity), entityVal, upsertBy, deleteBy)
import qualified Database.Persist as P (delete, (=.))
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, widgetSnackbar, widgetTopbar
    , Route (HomeR, DataR, EventPosterR)
    , DataR
      ( AccountEventScheduleR, AccountEventR, AccountSettingsR, UserPhotoR
      , AccountPushSettingsR, AccountNotificationR, AccountNotificationsR
      , AccountNotificationDeleR, UserUnsubscribeR, UserSubscriptionsR
      , AccountProfileR, AccountEventUnregisterR, AccountEventAttendeesR
      )
    , AppMessage
      ( MsgUserAccount, MsgYes, MsgNo, MsgSettings, MsgProfile, MsgName
      , MsgAdministrator, MsgManager, MsgPhoto, MsgTheme, MsgMyVisitingSchedule
      , MsgSchedule, MsgAttendees, MsgYouDoNotHaveEventsToAttendYet, MsgPoster
      , MsgUpcoming, MsgAll, MsgNotifications, MsgNoNotificationsForYouAtTheMoment
      , MsgUnread, MsgRead, MsgNotification, MsgInvalidFormData, MsgRecordDeleted
      , MsgFrom, MsgCancel, MsgDele, MsgDeleteAreYouSure, MsgConfirmPlease
      , MsgMessage, MsgMessageSubject, MsgSent, MsgSubscribeToPushNotifications
      , MsgVapidRequiredToPushNotifications, MsgVapidNotInitializedProperly
      , MsgVapidCanBeGeneratedByAdmin, MsgYourOtherSubscriptions, MsgUnknownDevice
      , MsgUnsubscribeAreYouSure, MsgUnsubscribeSuccessful, MsgSubscriptionSuccessful
      , MsgUnsubscribe, MsgEnableNotificationsPlease, MsgNotificationsHaveBeenDisabled
      , MsgEvent, MsgNumberOfAttendees, MsgEventStartTime, MsgDetails, MsgDescription
      , MsgRegistrationDate
      )
    )

import Handler.Tokens (fetchVapidKeys)

import Material3 (md3switchWidget)

import Model
    ( keyThemeMode, msgSuccess, msgError
    , UserId, User (User)
    , Attendee (Attendee)
    , EventId, Event (Event), Card
    , NotificationId, Notification (Notification)
    , NotificationStatus (NotificationStatusUnread, NotificationStatusRead)
    , PushSubscription
      ( PushSubscription, pushSubscriptionEndpoint, pushSubscriptionP256dh
      , pushSubscriptionAuth
      )
    , PushSubscriptionId, Unique (UniquePushSubscription)
    , EntityField
      ( UserId, AttendeeEvent, EventId, EventTime, AttendeeCard, CardId
      , CardUser, NotificationPublisher, NotificationRecipient
      , NotificationStatus, PushSubscriptionUser, PushSubscriptionEndpoint
      , PushSubscriptionP256dh, PushSubscriptionAuth, PushSubscriptionTime
      , PushSubscriptionUserAgent, NotificationId
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Web.WebPush (VAPIDKeys, vapidPublicKeyBytes)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, getMessageRender
    , YesodRequest (reqGetParams), getRequest, addMessageI, redirect
    , SomeMessage (SomeMessage), lookupHeader
    )
import Yesod.Core.Widget (whamlet)
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields (boolField, textField, hiddenField, checkBoxField)
import Yesod.Form.Functions (generateFormPost, runFormPost, mreq)
import Yesod.Form.Types
    ( FormResult(FormSuccess)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvId, fvInput)
    )


getAccountEventAttendeesR :: UserId -> EventId -> Handler Html
getAccountEventAttendeesR uid eid = do

    attendees <- runDB $ select $ do
        x :& c :& u <- from $ table @Attendee
            `innerJoin` table @Card `on` (\(x :& c) -> x ^. AttendeeCard ==. c ^. CardId)
            `innerJoin` table @User `on` (\(_ :& c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. AttendeeEvent ==. val eid
        return (x,c,u)

    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgAttendees
        idOverlay <- newIdent
        $(widgetFile "data/account/attendees/attendees") 


postAccountEventUnregisterR :: UserId -> EventId -> Handler Html
postAccountEventUnregisterR uid eid = do
    ((fr0,_),_) <- runFormPost formEventUserUnregister
    case fr0 of
      FormSuccess () -> do
          runDB $ delete $ do
              x <- from $ table @Attendee
              where_ $ x ^. AttendeeEvent ==. val eid
              where_ $ x ^. AttendeeCard `in_` subSelectList ( do
                  c <- from $ table @Card
                  where_ $ c ^. CardUser ==. val uid
                  return $ c ^. CardId )

          addMessageI msgSuccess MsgUnsubscribeSuccessful
          redirect $ DataR $ AccountEventScheduleR uid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ AccountEventScheduleR uid


getAccountEventR :: UserId -> EventId -> Handler Html
getAccountEventR uid eid = do

    event <- (second unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId

        where_ $ x ^. EventId ==. val eid
        return (x,attendees) )

    (fw0,et0) <- generateFormPost formEventUserUnregister

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent
        idOverlay <- newIdent
        idActions <- newIdent
        idButtonUnsubscribe <- newIdent
        idDialogUnsubscribe <- newIdent
        idButtonCloseDialogUnsubscribe <- newIdent
        $(widgetFile "data/account/event")


formEventUserUnregister :: Form ()
formEventUserUnregister extra = return (pure (), [whamlet|^{extra}|])


getAccountEventScheduleR :: UserId -> Handler Html
getAccountEventScheduleR uid = do

    allEvents <- fromMaybe False <$> runInputGet ( iopt boolField "all" )

    now <- liftIO getCurrentTime
    
    events <- (second (second (second unValue)) <$>) <$> runDB ( select $ do
        x :& c :& e <- from $ table @Attendee
            `innerJoin` table @Card `on` (\(x :& c) -> x ^. AttendeeCard ==. c ^. CardId)
            `innerJoin` table @Event `on` (\(x :& _ :& e) -> x ^. AttendeeEvent ==. e ^. EventId)

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. e ^. EventId
                
        where_ $ c ^. CardUser ==. val uid

        unless allEvents $ where_ $ e ^. EventTime >=. val now
        
        orderBy [asc (e ^. EventTime)]
        return (x,(c,(e,attendees))) )
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgMyVisitingSchedule
        idFormFilter <- newIdent
        $(widgetFile "data/account/schedule/schedule")


getAccountSettingsR :: UserId -> Handler Html
getAccountSettingsR uid = do
        
    defaultLayout $ do
        setTitleI MsgSettings
        idInputThemeMode <- newIdent
        $(widgetFile "data/account/settings")


getAccountProfileR :: UserId -> Handler Html
getAccountProfileR uid = do
    
    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
        
    defaultLayout $ do
        setTitleI MsgUserAccount 
        $(widgetFile "data/account/profile")


postUserUnsubscribeR :: UserId -> PushSubscriptionId -> Handler Html
postUserUnsubscribeR uid sid = do
    stati <- reqGetParams <$> getRequest
    ((fr0,_),_) <- runFormPost formUnsubscribe
    case fr0 of
      FormSuccess () -> do
          runDB $ P.delete sid
          addMessageI msgSuccess MsgUnsubscribeSuccessful
          redirect (DataR $ AccountPushSettingsR uid, stati)
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect (DataR $ AccountPushSettingsR uid, stati)


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
          redirect (DataR $ AccountPushSettingsR uid,[("endpoint",endpoint')])

      FormSuccess (False, PushSubscription _ endpoint' _ _ _ _) -> do
          void $ runDB $ deleteBy (UniquePushSubscription endpoint')
          addMessageI msgSuccess MsgUnsubscribeSuccessful
          redirect $ DataR $ AccountPushSettingsR uid

      _otherwise -> do           
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ AccountPushSettingsR uid


getAccountPushSettingsR :: UserId -> Handler Html
getAccountPushSettingsR uid = do
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


postAccountNotificationDeleR  :: UserId -> NotificationId -> Handler Html
postAccountNotificationDeleR uid nid = do
    ((fr0,_),_) <- runFormPost formUserNotificationDelete
    case fr0 of
      FormSuccess () -> do
          runDB $ P.delete nid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ AccountNotificationsR uid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ AccountNotificationR uid nid


getAccountNotificationR :: UserId -> NotificationId -> Handler Html
getAccountNotificationR uid nid = do

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


getAccountNotificationsR :: UserId -> Handler Html
getAccountNotificationsR uid = do

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
