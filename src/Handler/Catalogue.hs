{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Catalogue
  ( getDataEventsR, getDataEventNewR, postDataEventsR
  , getDataEventR, postDataEventR, getDataEventEditR, postDataEventDeleR
  , getDataEventAttendeesR, postDataEventAttendeesR, getDataEventAttendeeNewR
  , getDataEventAttendeeR, postDataEventAttendeeDeleR
  , getDataEventCalendarR
  , getDataEventCalendarEventsR, postDataEventCalendarEventsR
  , getDataEventCalendarEventR, postDataEventCalendarEventR
  , getDataEventScannerR
  , getDataEventRegistrationR, postDataEventRegistrationR
  , getDataEventCalendarEventAttendeesR
  , getDataEventCalendarEventAttendeeR
  , postDataEventCalendarEventAttendeeDeleR
  , getDataEventCalendarEventNewR, getDataEventCalendarEventEditR
  , postDataEventCalendarEventDeleR
  , getDataEventCalendarScannerR
  , getDataEventCalendarRegistrationR, postDataEventCalendarRegistrationR
  , postDataEventAttendeeNotifyR
  , getDataEventPosterR, postDataEventPosterR, postDataEventPosterDeleR
  , getDataEventCalendarEventPosterR, postDataEventCalendarEventPosterR
  , postDataEventCalendarEventPosterDeleR
  , postDataEventCalendarEventAttendeeNotifyR
  ) where


import Control.Applicative ((<|>))
import Control.Exception.Safe (tryAny)
import Control.Exception (SomeException(SomeException), Exception (fromException))
import Control.Lens ((.~), (?~))
import qualified Control.Lens as L ((^.))
import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (object, (.=))
import qualified Data.ByteString.Base64.Lazy as B64L (encode)
import Data.Either (isRight)
import qualified Data.List as L (find, length)
import Data.Map (Map, fromListWith)
import qualified Data.Map as M (lookup, foldr)
import Data.Maybe (fromMaybe, isJust)
import Data.Function ((&))
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy.Encoding as TLE (encodeUtf8)
import qualified Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar
    ( Day, addDays, toGregorian, weekFirstDay, DayOfWeek (Monday)
    , DayPeriod (periodFirstDay)
    )
import Data.Time.Calendar.Month (Month, pattern YearMonth, addMonths)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.LocalTime
    ( utcToLocalTime, localTimeToUTC, TimeOfDay (TimeOfDay)
    , LocalTime (LocalTime, localTimeOfDay, localDay)
    )

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, orderBy, desc, where_, val
    , (^.), (==.), (:&) ((:&)), (>=.), (<.), (=.)
    , innerJoin, on, asc, update, set
    )

import Database.Persist
    ( Entity (Entity, entityKey), delete, entityVal, replace, insert_, upsertBy)
import qualified Database.Persist as P ((=.))
import Database.Persist.Sql (toSqlKey)

import Foundation
    ( App (appSettings, appHttpManager), Handler, Form
    , widgetTopbar, widgetSnackbar, widgetScanner
    , Route (DataR, StaticR, EventR, EventPosterR)
    , DataR
      ( DataEventR, DataEventAttendeesR, DataEventsR, UserPhotoR, CardQrImageR
      , DataEventNewR, DataEventEditR, DataEventDeleR, DataEventAttendeeNewR
      , DataEventAttendeeR, DataEventAttendeeDeleR, DataEventCalendarR
      , DataEventCalendarEventsR, DataEventCalendarEventR
      , DataEventCalendarEventAttendeesR, DataEventCalendarEventAttendeeR
      , DataEventCalendarEventAttendeeDeleR, DataEventCalendarEventNewR
      , DataEventCalendarEventEditR, DataEventCalendarEventDeleR
      , DataEventScannerR, DataEventRegistrationR
      , DataEventCalendarScannerR, DataEventCalendarRegistrationR
      , DataEventAttendeeNotifyR, DataEventPosterDeleR, DataEventPosterR
      , DataEventCalendarEventPosterR, DataEventCalendarEventPosterDeleR
      , DataEventCalendarEventAttendeeNotifyR
      )
    , AppMessage
      ( MsgEventsCatalogue, MsgEvent, MsgDetails, MsgScanQrCodeAndLinkToEvent
      , MsgAttendees, MsgScanQrCode, MsgDescription, MsgTime, MsgName, MsgPhoto
      , MsgScanQrCodeAndLinkToEvent, MsgClose, MsgQrCode, MsgCancel, MsgDele
      , MsgDeleteAreYouSure, MsgConfirmPlease, MsgRecordDeleted, MsgInvalidFormData
      , MsgSave, MsgRecordAdded, MsgRecordEdited, MsgIssueDate, MsgRegistrationDate
      , MsgNoCardsRegisteredYet, MsgAttendee, MsgCalendar, MsgList
      , MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgNoEventsForThisMonth, MsgPrevious, MsgNext, MsgTotalEventsForThisMonth
      , MsgNoEventsForThisDayYet, MsgEvents, MsgNoAttendeesForThisEventYet
      , MsgNoEventsYet, MsgScanner, MsgRegistrationForEvent, MsgRegistration
      , MsgUserSuccessfullyRegisteredForEvent, MsgScanAgain, MsgRegister
      , MsgConfirmUserRegistrationForEventPlease, MsgNotifyUserAboutUpcomingEvent
      , MsgScanQrCodeAndLinkToThisEvent, MsgNotifyUser, MsgShowUserQrCode
      , MsgSend, MsgSendNotification, MsgSendEmail, MsgHeader, MsgMessage, MsgEmail
      , MsgEmailAddress, MsgNotificationSentToSubscribersN, MsgSendPushNotification
      , MsgItLooksLikeUserNotSubscribedToPushNotifications, MsgPoster, MsgUploadPoster
      , MsgAttribution, MsgNotificationSent, MsgUnableToObtainAccessToken
      , MsgRefreshTokenIsNotInitialized, MsgUnknownAccountForSendingEmail
      , MsgVapidNotInitializedProperly, MsgUnknownMessagePublisher
      , MsgUnknownMessageRecipient, MsgCheckAtLeastOneOptionPlease
      , MsgProvideRecipientEmailPlease
      )
    )
    
import Handler.Tokens (fetchRefreshToken, fetchAccessToken, fetchVapidKeys)

import Material3 (daytimeLocalField, md3textareaWidget, md3widget, md3checkboxWidget, md3fileWidget)

import Model
    ( msgSuccess, msgError, gmailSendEnpoint
    , EventId, Event(Event, eventName, eventTime, eventDescr)
    , CardId, Card (Card)
    , UserId, User (User, userEmail)
    , AttendeeId, Attendee (Attendee, attendeeRegDate, attendeeCard, attendeeEvent)
    , PushSubscription (PushSubscription)
    , Poster (Poster, posterAttribution), PosterId, Unique (UniquePoster)
    , Notification (Notification), NotificationStatus (NotificationStatusUnread)
    , EntityField
      ( EventTime, EventId, AttendeeCard, CardId, CardUser, AttendeeEvent
      , UserId, UserName, AttendeeId, PushSubscriptionUser, PosterEvent
      , PosterAttribution
      )
    )

import Network.Mail.Mime
    (renderMail', Mail (mailTo, mailHeaders, mailParts), Address (Address), emptyMail
    , Part (Part, partType, partEncoding, partDisposition, partContent, partHeaders)
    , Encoding (None), Disposition (DefaultDisposition), PartContent (PartContent)
    )
import Network.Wreq (auth, postWith, defaults, oauth2Bearer)
import qualified Network.Wreq.Lens as WL
import Network.HTTP.Client
    ( HttpException (HttpExceptionRequest)
    , HttpExceptionContent (StatusCodeException)
    )
import Network.HTTP.Types.URI (extractPath)

import Settings (widgetFile, AppSettings (appTimeZone))
import Settings.StaticFiles (img_logo_svg)

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (Html)
import Text.Julius (julius, RawJS (rawJS))

import Web.WebPush
    ( sendPushNotification, mkPushNotification, pushMessage, pushSenderEmail
    , pushExpireInSeconds, pushTopic, pushUrgency, PushTopic (PushTopic)
    , PushUrgency (PushUrgencyHigh), VAPIDKeys, PushNotificationError
    )

import Yesod.Auth (maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, getMessages
    , whamlet, redirect, addMessageI, SomeMessage (SomeMessage), getYesod
    , MonadHandler (liftHandler), handlerToWidget, YesodRequest (reqGetParams)
    , getRequest, toHtml, ToWidget (toWidget), getUrlRender, FileInfo (fileContentType)
    , fileSourceByteString
    )
import Yesod.Form.Fields
    ( textField, radioField, optionsPairs
    , Option (optionInternalValue, optionExternalValue), OptionList (olOptions)
    , timeField, hiddenField, intField, htmlField, checkBoxField, emailField, fileField
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, mreq, mopt)
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Types
    ( FormResult(FormSuccess), Field (fieldView), FieldView (fvInput, fvErrors, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postDataEventCalendarEventAttendeeNotifyR :: UserId -> Month -> Day -> EventId -> AttendeeId -> Handler Html
postDataEventCalendarEventAttendeeNotifyR uid month day eid aid = do

    publisher <- maybeAuth
    
    attendee <- runDB $ selectOne $ do
        x :& e :& c :& u <- from $ table @Attendee
            `innerJoin` table @Event `on` (\(x :& e) -> x ^. AttendeeEvent ==. e ^. EventId)
            `innerJoin` table @Card `on` (\(x :& _ :& c) -> x ^. AttendeeCard ==. c ^. CardId)
            `innerJoin` table @User `on` (\(_ :& _ :& c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. AttendeeId ==. val aid
        return (x,(e,(c,u)))

    (fw0,et0) <- generateFormPost formAttendeeRemove
    ((fr1,fw1),et1) <- runFormPost $ formAttendeeNotify ((\(_,(e,(_,u))) -> (e,u)) <$> attendee)
    
    case (fr1,(publisher,attendee)) of
      (FormSuccess ((subj, msg), ((True, True),(True,Just email))),(Just (Entity pid _),Just (_,(_,(_,Entity rid _))))) -> do
          postMessage eid attendee subj msg
          logNotification pid rid subj msg
          postEmail email subj msg
          redirect $ DataR $ DataEventCalendarEventAttendeeR uid month day eid aid
          
      (FormSuccess ((subj, msg), ((True, True),(False,_))),(Just (Entity pid _),Just (_,(_,(_,Entity rid _))))) -> do
          postMessage eid attendee subj msg
          logNotification pid rid subj msg
          redirect $ DataR $ DataEventCalendarEventAttendeeR uid month day eid aid
          
      (FormSuccess ((subj, msg), ((True, False),(True,Just email))),_) -> do
          postMessage eid attendee subj msg
          postEmail email subj msg
          redirect $ DataR $ DataEventCalendarEventAttendeeR uid month day eid aid
          
      (FormSuccess ((subj, msg), ((True, False),(False,_))),_) -> do
          postMessage eid attendee subj msg
          redirect $ DataR $ DataEventCalendarEventAttendeeR uid month day eid aid
          
      (FormSuccess ((subj, msg), ((False, True),(True,Just email))),(Just (Entity pid _),Just (_,(_,(_,Entity rid _))))) -> do
          logNotification pid rid subj msg
          postEmail email subj msg
          redirect $ DataR $ DataEventCalendarEventAttendeeR uid month day eid aid
          
      (FormSuccess ((subj, msg), ((False, True),(False,_))),(Just (Entity pid _),Just (_,(_,(_,Entity rid _))))) -> do
          logNotification pid rid subj msg
          redirect $ DataR $ DataEventCalendarEventAttendeeR uid month day eid aid
          
      (FormSuccess ((subj, msg), ((False, False),(True,Just email))),(Just (Entity pid _),Just (_,(_,(_,Entity rid _))))) -> do
          logNotification pid rid subj msg
          postEmail email subj msg
          redirect $ DataR $ DataEventCalendarEventAttendeeR uid month day eid aid

      (FormSuccess (_, ((False, False),(False,_))),_) -> do
          addMessageI msgError MsgCheckAtLeastOneOptionPlease
          redirect $ DataR $ DataEventCalendarEventAttendeeR uid month day eid aid

      (FormSuccess (_, (_,(True,Nothing))),_) -> do
          addMessageI msgError MsgProvideRecipientEmailPlease
          redirect $ DataR $ DataEventCalendarEventAttendeeR uid month day eid aid
          
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAttendee

              idOverlay <- newIdent
              idDialogDelete <- newIdent
              idButtonShowDialogQrCode <- newIdent
              idDialogQrCode <- newIdent
              idButtonCloseDialogQrCode <- newIdent

              idButtonShowDialogNotify <- newIdent
              idDialogNotifyAttendee <- newIdent
              idFormNotifyAttendee <- newIdent

              $(widgetFile "data/catalogue/calendar/events/attendees/attendee")


postDataEventCalendarEventAttendeeDeleR :: UserId -> Month -> Day -> EventId -> AttendeeId -> Handler Html
postDataEventCalendarEventAttendeeDeleR uid month day eid aid = do
    ((fr,_),_) <- runFormPost formAttendeeRemove
    case fr of
      FormSuccess () -> do
          runDB $ delete aid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ DataEventCalendarEventAttendeesR uid month day eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventCalendarEventAttendeeR uid month day eid aid


getDataEventCalendarEventAttendeeR :: UserId -> Month -> Day -> EventId -> AttendeeId -> Handler Html
getDataEventCalendarEventAttendeeR uid month day eid aid = do

    attendee <- runDB $ selectOne $ do
        x :& e :& c :& u <- from $ table @Attendee
            `innerJoin` table @Event `on` (\(x :& e) -> x ^. AttendeeEvent ==. e ^. EventId)
            `innerJoin` table @Card `on` (\(x :& _ :& c) -> x ^. AttendeeCard ==. c ^. CardId)
            `innerJoin` table @User `on` (\(_ :& _ :& c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. AttendeeId ==. val aid
        return (x,(e,(c,u)))

    (fw0,et0) <- generateFormPost formAttendeeRemove
    (fw1,et1) <- generateFormPost $ formAttendeeNotify ((\(_,(e,(_,u))) -> (e,u)) <$> attendee)

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAttendee

        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idButtonShowDialogNotify <- newIdent 
        idButtonShowDialogQrCode <- newIdent
        idDialogQrCode <- newIdent
        idButtonCloseDialogQrCode <- newIdent
        idDialogNotifyAttendee <- newIdent
        idFormNotifyAttendee <- newIdent

        $(widgetFile "data/catalogue/calendar/events/attendees/attendee")


getDataEventCalendarEventAttendeesR :: UserId -> Month -> Day -> EventId -> Handler Html
getDataEventCalendarEventAttendeesR uid month day eid = do

    attendees <- runDB $ select $ do
        x :& c :& u <- from $ table @Attendee
            `innerJoin` table @Card `on` (\(x :& c) -> x ^. AttendeeCard ==. c ^. CardId)
            `innerJoin` table @User `on` (\(_ :& c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. AttendeeEvent ==. val eid
        return (x,c,u)

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAttendees
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/calendar/events/attendees/attendees")


postDataEventCalendarEventPosterDeleR :: UserId -> Month -> Day -> EventId -> PosterId -> Handler Html
postDataEventCalendarEventPosterDeleR uid month day eid pid = do
    ((fr0,_),_) <- runFormPost formPosterDelete
    case fr0 of
      FormSuccess () -> do
          runDB $ delete pid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ DataEventCalendarEventPosterR uid month day eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventCalendarEventPosterR uid month day eid


postDataEventCalendarEventPosterR :: UserId -> Month -> Day -> EventId -> Handler Html
postDataEventCalendarEventPosterR uid month day eid = do

    poster <- runDB $ selectOne $ do
        x <- from $ table @Poster
        where_ $ x ^. PosterEvent ==. val eid
        return x

    (fw0,et0) <- generateFormPost formPosterDelete

    idImgPoster <- newIdent
    ((fr,fw),et) <- runFormPost $ formPoster idImgPoster poster

    case fr of
      FormSuccess (Just fi,attrib) -> do
          bs <- fileSourceByteString fi
          void $ runDB $ upsertBy (UniquePoster eid) (Poster eid (fileContentType fi) bs attrib)
              [PosterAttribution P.=. attrib]
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ DataEventCalendarEventPosterR uid month day eid

      FormSuccess (Nothing,attrib) -> do
          void $ runDB $ update $ \x -> do
              set x [ PosterAttribution =. val attrib ]
              where_ $ x ^. PosterEvent ==. val eid
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ DataEventCalendarEventPosterR uid month day eid

      _otherwise -> do
          msgr <- getMessageRender
          addMessageI msgError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPoster
              idOverlay <- newIdent
              idDialogDelete <- newIdent
              $(widgetFile "data/catalogue/calendar/events/poster/poster")


getDataEventCalendarEventPosterR :: UserId -> Month -> Day -> EventId -> Handler Html
getDataEventCalendarEventPosterR uid month day eid = do

    poster <- runDB $ selectOne $ do
        x <- from $ table @Poster
        where_ $ x ^. PosterEvent ==. val eid
        return x

    (fw0,et0) <- generateFormPost formPosterDelete

    idImgPoster <- newIdent
    (fw,et) <- generateFormPost $ formPoster idImgPoster poster

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPoster
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/catalogue/calendar/events/poster/poster")


postDataEventCalendarRegistrationR :: UserId -> Month -> Day -> EventId -> Handler Html
postDataEventCalendarRegistrationR uid month day eid = do

    ((fr,_),_) <- runFormPost $ formRegistration Nothing Nothing

    case fr of
      FormSuccess (eid',cid') -> do
          now <- liftIO getCurrentTime
          runDB $ insert_ Attendee { attendeeEvent = eid'
                                   , attendeeCard = cid'
                                   , attendeeRegDate = now
                                   }
          addMessageI msgSuccess MsgUserSuccessfullyRegisteredForEvent
          redirect $ DataR $ DataEventCalendarEventR uid month day eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventCalendarRegistrationR uid month day eid


getDataEventCalendarRegistrationR :: UserId -> Month -> Day -> EventId -> Handler Html
getDataEventCalendarRegistrationR uid month day eid = do

    cid <- toSqlKey <$> runInputGet (ireq intField "cid")

    card <- runDB $ selectOne $ do
        x :& u <- from $ table @Card
            `innerJoin` table @User `on` (\(x :& u) -> x ^. CardUser ==. u ^. UserId)
        where_ $ x ^. CardId ==. val cid
        return (x,u)

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    (fw,et) <- generateFormPost $ formRegistration event (fst <$> card)

    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgRegistration
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/calendar/events/registration/registration")


getDataEventCalendarScannerR :: UserId -> Month -> Day -> EventId -> Handler Html
getDataEventCalendarScannerR uid month day eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgScanner
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/calendar/events/scanner/scanner")


postDataEventCalendarEventDeleR :: UserId -> Month -> Day -> EventId -> Handler Html
postDataEventCalendarEventDeleR uid month day eid = do
    ((fr,_),_) <- runFormPost formEventDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete eid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ DataEventCalendarEventsR uid month day
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventCalendarEventR uid month day eid


postDataEventCalendarEventR :: UserId -> Month -> Day -> EventId -> Handler Html
postDataEventCalendarEventR uid month day eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    ((fr,fw),et) <- runFormPost $ formEventDay uid day event

    case fr of
      FormSuccess r -> do
          runDB $ replace eid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ DataEventCalendarEventR uid month day eid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/catalogue/calendar/events/edit")


getDataEventCalendarEventEditR :: UserId -> Month -> Day -> EventId -> Handler Html
getDataEventCalendarEventEditR uid month day eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    (fw,et) <- generateFormPost $ formEventDay uid day event

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/calendar/events/edit")


getDataEventCalendarEventR :: UserId -> Month -> Day -> EventId -> Handler Html
getDataEventCalendarEventR uid month day eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    (fw0,et0) <- generateFormPost formEventDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idActionQrScan <- newIdent
        idButtonQrScan <- newIdent
        $(widgetFile "data/catalogue/calendar/events/event")


postDataEventCalendarEventsR :: UserId -> Month -> Day -> Handler Html
postDataEventCalendarEventsR uid month day = do
    ((fr,fw),et) <- runFormPost $ formEventDay uid day Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ DataEventCalendarEventsR uid month day

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/catalogue/calendar/events/new")


getDataEventCalendarEventNewR :: UserId -> Month -> Day -> Handler Html
getDataEventCalendarEventNewR uid month day = do

    (fw,et) <- generateFormPost $ formEventDay uid day Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/calendar/events/new")


formEventDay :: UserId -> Day -> Maybe (Entity Event) -> Form Event
formEventDay mid day event extra = do

    tz <- appTimeZone . appSettings <$> getYesod

    (timeR,timeV) <- mreq timeField FieldSettings
        { fsLabel = SomeMessage MsgTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (localTimeOfDay . utcToLocalTime tz . eventTime . entityVal <$> event)

    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (eventName . entityVal <$> event)

    (descrR,descrV) <- mreq htmlField  FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (eventDescr . entityVal <$> event)

    let r = Event mid <$> (localTimeToUTC tz . LocalTime day <$> timeR) <*> nameR <*> descrR
    return (r,$(widgetFile "data/catalogue/calendar/events/form"))


getDataEventCalendarEventsR :: UserId -> Month -> Day -> Handler Html
getDataEventCalendarEventsR uid month day = do

    tz <- appTimeZone . appSettings <$> getYesod

    events <- runDB $ select $ do
        x <- from $ table @Event
        where_ $ x ^. EventTime >=. val (localTimeToUTC tz (LocalTime day (TimeOfDay 0 0 0)))
        where_ $ x ^. EventTime <.  val (localTimeToUTC tz (LocalTime (addDays 1 day) (TimeOfDay 0 0 0)))
        return x

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvents
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/calendar/events/events")


getDataEventCalendarR :: UserId -> Month -> Handler Html
getDataEventCalendarR uid month = do
    stati <- reqGetParams <$> getRequest

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month

    tz <- appTimeZone . appSettings <$> getYesod

    events <- groupByKey (localDay . utcToLocalTime tz . eventTime . entityVal) <$> runDB ( select $ do
        x <- from $ table @Event
        where_ $ x ^. EventTime >=. val (localTimeToUTC tz (LocalTime (periodFirstDay month) (TimeOfDay 0 0 0)))
        where_ $ x ^. EventTime <.  val (localTimeToUTC tz (LocalTime (periodFirstDay next) (TimeOfDay 0 0 0)))
        return x )

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCalendar
        idOverlay <- newIdent
        idCalendarPage <- newIdent
        idCalendarLegend <- newIdent
        $(widgetFile "data/catalogue/calendar/calendar")

  where

      total :: Map k [a] -> Int
      total = M.foldr (\xs a -> L.length xs + a) 0

      groupByKey :: Ord k => (v -> k) -> [v] -> Map k [v]
      groupByKey f = fromListWith (<>) . fmap (\x -> (f x,[x]))


postDataEventAttendeeDeleR :: UserId -> EventId -> AttendeeId -> Handler Html
postDataEventAttendeeDeleR uid eid aid = do
    ((fr,_),_) <- runFormPost formAttendeeRemove
    case fr of
      FormSuccess () -> do
          runDB $ delete aid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ DataEventAttendeesR uid eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventAttendeeR uid eid aid


getAccessToken :: Handler (Either AppMessage (Text,Text))
getAccessToken = do
    (rtoken,sender) <- fetchRefreshToken
    case (rtoken,sender) of       
      (Just rt, Just sendby) -> do
          at <- fetchAccessToken rt
          case at of
            Nothing -> return $ Left MsgUnableToObtainAccessToken
            Just atoken -> return $ Right (atoken,sendby)

      (Nothing,_) -> return $ Left MsgRefreshTokenIsNotInitialized

      (Just _,Nothing) -> return $ Left MsgUnknownAccountForSendingEmail
    


sendEmail :: Text -> Text -> Text -> Text -> Html -> Handler (Either Text ())
sendEmail atoken sendby email subject message = do
    let mail = (emptyMail $ Address Nothing "noreply")
               { mailTo = [Address Nothing email]
               , mailHeaders = [("Subject", subject)]
               , mailParts = [[textPart, htmlPart]]
               }
          where
            textPart = Part
                { partType = "text/plain; charset=utf-8"
                , partEncoding = None
                , partDisposition = DefaultDisposition
                , partContent = PartContent $ TLE.encodeUtf8 $ renderHtml message
                , partHeaders = []
                }
            htmlPart = Part
                { partType = "text/html; charset=utf-8"
                , partEncoding = None
                , partDisposition = DefaultDisposition
                , partContent = PartContent $ TLE.encodeUtf8 $ renderHtml message
                , partHeaders = []
                }


    raw <- liftIO $ TE.decodeUtf8 . BSL.toStrict . B64L.encode <$> renderMail' mail
    let opts = defaults & auth ?~ oauth2Bearer (TE.encodeUtf8 atoken)
    response <- liftIO $ tryAny $ postWith
            opts (gmailSendEnpoint $ unpack sendby) (object ["raw" .= raw])

    return $ case response of
          Right _ok -> Right ()
          Left e@(SomeException _) -> case fromException e of
            Just (HttpExceptionRequest _ (StatusCodeException r' _bs)) -> do
                Left $ pack $ show $ r' L.^. WL.responseBody
            _otherwise -> do
                Left $ pack $ show e


logNotification :: UserId -> UserId -> Text -> Html -> Handler ()
logNotification pid rid subject message = do
    now <- liftIO getCurrentTime
    runDB $ insert_ $ Notification pid rid now subject message NotificationStatusUnread
    addMessageI msgSuccess MsgNotificationSent


postDataEventAttendeeNotifyR :: UserId -> EventId -> AttendeeId -> Handler Html
postDataEventAttendeeNotifyR uid eid aid = do

    publisher <- maybeAuth
    
    attendee <- runDB $ selectOne $ do
        x :& e :& c :& u <- from $ table @Attendee
            `innerJoin` table @Event `on` (\(x :& e) -> x ^. AttendeeEvent ==. e ^. EventId)
            `innerJoin` table @Card `on` (\(x :& _ :& c) -> x ^. AttendeeCard ==. c ^. CardId)
            `innerJoin` table @User `on` (\(_ :& _ :& c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. AttendeeId ==. val aid
        return (x,(e,(c,u)))

    (fw0,et0) <- generateFormPost formAttendeeRemove
    ((fr1,fw1),et1) <- runFormPost $ formAttendeeNotify ((\(_,(e,(_,u))) -> (e,u)) <$> attendee)
    
    case (fr1,(publisher,attendee)) of
      (FormSuccess ((subj, msg), ((True, True),(True,Just email))),(Just (Entity pid _),Just (_,(_,(_,Entity rid _))))) -> do
          postMessage eid attendee subj msg
          logNotification pid rid subj msg
          postEmail email subj msg
          redirect $ DataR $ DataEventAttendeeR uid eid aid
          
      (FormSuccess ((subj, msg), ((True, True),(False,_))),(Just (Entity pid _),Just (_,(_,(_,Entity rid _))))) -> do
          postMessage eid attendee subj msg
          logNotification pid rid subj msg
          redirect $ DataR $ DataEventAttendeeR uid eid aid
          
      (FormSuccess ((subj, msg), ((True, False),(True,Just email))),_) -> do
          postMessage eid attendee subj msg
          postEmail email subj msg
          redirect $ DataR $ DataEventAttendeeR uid eid aid
          
      (FormSuccess ((subj, msg), ((True, False),(False,_))),_) -> do
          postMessage eid attendee subj msg
          redirect $ DataR $ DataEventAttendeeR uid eid aid
          
      (FormSuccess ((subj, msg), ((False, True),(True,Just email))),(Just (Entity pid _),Just (_,(_,(_,Entity rid _))))) -> do
          logNotification pid rid subj msg
          postEmail email subj msg
          redirect $ DataR $ DataEventAttendeeR uid eid aid
          
      (FormSuccess ((subj, msg), ((False, True),(False,_))),(Just (Entity pid _),Just (_,(_,(_,Entity rid _))))) -> do
          logNotification pid rid subj msg
          redirect $ DataR $ DataEventAttendeeR uid eid aid
          
      (FormSuccess ((subj, msg), ((False, False),(True,Just email))),(Just (Entity pid _),Just (_,(_,(_,Entity rid _))))) -> do
          logNotification pid rid subj msg
          postEmail email subj msg
          redirect $ DataR $ DataEventAttendeeR uid eid aid

      (FormSuccess (_, ((False, False),(False,_))),_) -> do
          addMessageI msgError MsgCheckAtLeastOneOptionPlease
          redirect $ DataR $ DataEventAttendeeR uid eid aid

      (FormSuccess (_, (_,(True,Nothing))),_) -> do
          addMessageI msgError MsgProvideRecipientEmailPlease
          redirect $ DataR $ DataEventAttendeeR uid eid aid
          
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgAttendee

              idOverlay <- newIdent
              idDialogDelete <- newIdent
              idButtonShowDialogQrCode <- newIdent
              idDialogQrCode <- newIdent
              idButtonCloseDialogQrCode <- newIdent

              idButtonShowDialogNotify <- newIdent
              idDialogNotifyAttendee <- newIdent
              idFormNotifyAttendee <- newIdent

              $(widgetFile "data/catalogue/attendees/attendee")


postMessage :: EventId -> Maybe (Entity Attendee, (Entity Event, (Entity Card, Entity User)))
            -> Text -> Html -> Handler ()
postMessage eid attendee subject message = do
    vapidKeys <- fetchVapidKeys
    user <- maybeAuth
    case (vapidKeys,user,attendee) of
      (Just vapid,Just publisher,Just (_,(_,(_,Entity rid _)))) -> do
          results <- pushNotifications vapid publisher rid eid subject message
          addMessageI msgSuccess (MsgNotificationSentToSubscribersN (length $ filter isRight results))

      (Nothing,_,_) -> do
          addMessageI msgError MsgVapidNotInitializedProperly

      (_,Nothing,_) -> do
          addMessageI msgError MsgUnknownMessagePublisher

      (_,_,Nothing) -> do
          addMessageI msgError MsgUnknownMessageRecipient


postEmail :: Text -> Text -> Html -> Handler ()
postEmail email subject message = do
    msgr <- getMessageRender
    at <- getAccessToken
    case at of
      Left msg -> do
          addMessageI msgError msg

      Right (atoken,sendby) -> do
          result <- sendEmail atoken sendby email subject message
          (status,msg) <- return $ case result of
                            Right () -> (msgSuccess, msgr MsgNotificationSent)
                            Left msg -> (msgError, msg)

          addMessageI status msg


pushNotifications :: VAPIDKeys -> Entity User -> UserId -> EventId -> Text -> Html
                  -> Handler [Either PushNotificationError ()]
pushNotifications vapid publisher rid eid subject message = do

    rndr <- getUrlRender

    subscriptions <- runDB $ select $ do
        x <- from $ table @PushSubscription
        where_ $ x ^. PushSubscriptionUser ==. val rid
        return x

    let expath = decodeUtf8 . extractPath . encodeUtf8 . rndr
    manager <- appHttpManager <$> getYesod

    let topic = "EventQrNotification"

    forM subscriptions $ \(Entity _ (PushSubscription _ endpoint' p256dh' auth' _ _)) -> do
        let notification = mkPushNotification endpoint' p256dh' auth'
                & pushMessage .~ object
                    [ "title" .= subject
                    , "icon" .= expath (StaticR img_logo_svg)
                    , "image" .= expath (EventPosterR eid)
                    , "body" .= renderHtml message
                    , "messageType" .= topic
                    , "eventHref" .= expath (EventR eid)
                    ]
                & pushSenderEmail .~ userEmail (entityVal publisher)
                & pushExpireInSeconds .~ 30 * 60
                & pushTopic ?~ PushTopic topic
                & pushUrgency ?~ PushUrgencyHigh

        sendPushNotification vapid manager notification


getDataEventAttendeeR :: UserId -> EventId -> AttendeeId -> Handler Html
getDataEventAttendeeR uid eid aid = do

    attendee <- runDB $ selectOne $ do
        x :& e :& c :& u <- from $ table @Attendee
            `innerJoin` table @Event `on` (\(x :& e) -> x ^. AttendeeEvent ==. e ^. EventId)
            `innerJoin` table @Card `on` (\(x :& _ :& c) -> x ^. AttendeeCard ==. c ^. CardId)
            `innerJoin` table @User `on` (\(_ :& _ :& c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. AttendeeId ==. val aid
        return (x,(e,(c,u)))

    (fw0,et0) <- generateFormPost formAttendeeRemove
    (fw1,et1) <- generateFormPost $ formAttendeeNotify ((\(_,(e,(_,u))) -> (e,u)) <$> attendee)

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAttendees

        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idButtonShowDialogQrCode <- newIdent
        idDialogQrCode <- newIdent
        idButtonCloseDialogQrCode <- newIdent

        idButtonShowDialogNotify <- newIdent
        idDialogNotifyAttendee <- newIdent
        idFormNotifyAttendee <- newIdent

        $(widgetFile "data/catalogue/attendees/attendee")


formAttendeeNotify :: Maybe (Entity Event, Entity User)
                   -> Form ((Text, Html),((Bool,Bool),(Bool,Maybe Text)))
formAttendeeNotify event extra = do
    (subjectR, subjectV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgHeader
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (eventName . entityVal . fst <$> event)

    (messageR, messageV) <- mreq htmlField FieldSettings
        { fsLabel = SomeMessage MsgMessage
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (toHtml . eventDescr . entityVal . fst <$> event)

    subscribed <- case event of
      Nothing -> return False
      Just (_,Entity uid _) -> liftHandler $ (isJust <$>) $ runDB $ selectOne $ do
          x <- from $ table @PushSubscription
          where_ $ x ^. PushSubscriptionUser ==. val uid
          return x

    (pushNotifR, pushNotifV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgSendPushNotification
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("disabled","") | not subscribed]
        } (pure subscribed)

    (sendNotifR, sendNotifV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgSendNotification
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (pure True)

    (sendEmailR, sendEmailV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgSendEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (pure False)

    (emailR, emailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (pure . userEmail . entityVal . snd <$> event)

    let open = case sendEmailR of
          FormSuccess True -> True
          _otherwise -> False

    let r = (,) <$> ((,) <$> subjectR <*> messageR)
            <*> ((,) <$> ((,) <$> pushNotifR <*> sendNotifR) <*> ((,) <$> sendEmailR <*> emailR))
    let w = do
            toWidget [julius|
                   document.getElementById(#{fvId sendEmailV}).addEventListener('change', function (e) {
                     if (e.target.checked) {
                       document.getElementById(`details#{rawJS $ fvId sendEmailV}`).setAttribute('open','');
                     } else {
                       document.getElementById(`details#{rawJS $ fvId sendEmailV}`).removeAttribute('open');
                     }
                   });
                   |]
            [whamlet|
                    ^{extra}
                    ^{md3widget subjectV}
                    ^{md3textareaWidget messageV}

                    ^{md3checkboxWidget pushNotifV}
                    $if not subscribed
                      <br>
                      <label.error-text for=#{fvId pushNotifV} style="display:inline-block;line-height:1">
                        _{MsgItLooksLikeUserNotSubscribedToPushNotifications}
                    <br>
                    ^{md3checkboxWidget sendNotifV}
                    <br>
                    ^{md3checkboxWidget sendEmailV}

                    <details :open:open #details#{fvId sendEmailV}>
                      <summary>_{MsgEmailAddress}
                      ^{md3widget emailV}
                    |]

    return (r,w)


formAttendeeRemove :: Form ()
formAttendeeRemove extra = return (pure (),[whamlet|^{extra}|])


postDataEventAttendeesR :: UserId -> EventId -> Handler Html
postDataEventAttendeesR uid eid = do
    ((fr,fw),et) <- runFormPost $ formAttendee eid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ DataEventAttendeesR uid eid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/catalogue/attendees/new")


getDataEventAttendeeNewR :: UserId -> EventId -> Handler Html
getDataEventAttendeeNewR uid eid = do

    (fw,et) <- generateFormPost $ formAttendee eid Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/attendees/new")


formAttendee :: EventId -> Maybe (Entity Attendee) -> Form Attendee
formAttendee eid attendee extra = do

    now <- liftIO getCurrentTime

    tz <- appTimeZone . appSettings <$> getYesod

    (timeR,timeV) <- mreq daytimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgRegistrationDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (utcToLocalTime tz <$> ((attendeeRegDate . entityVal <$> attendee) <|> Just now))

    cards <- liftHandler $ runDB $ select $ do
        x :& u <- from $ table @Card
            `innerJoin` table @User `on` (\(x :& u) -> x ^. CardUser ==. u ^. UserId)
        orderBy [asc (u ^. UserName)]
        return (x,u)

    (cidR,cidV) <- mreq (md3radioFieldList cards) FieldSettings
        { fsLabel = SomeMessage MsgIssueDate
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (attendeeCard . entityVal <$> attendee)

    let r = Attendee eid <$> cidR <*> (localTimeToUTC tz <$> timeR)
    return (r,$(widgetFile "data/catalogue/attendees/form"))

  where

      pairs (Entity cid _, Entity _ (User email _ name _ _)) = (fromMaybe email name, cid)

      md3radioFieldList :: [(Entity Card, Entity User)] -> Field Handler CardId
      md3radioFieldList cards = (radioField (optionsPairs (pairs <$> cards)))
          { fieldView = \theId name attrs x isReq -> do
                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs <$> cards))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findCard :: Option CardId -> [(Entity Card, Entity User)] -> Maybe (Entity Card, Entity User)
                    findCard opt = L.find (\(Entity cid' _,_) -> cid' == optionInternalValue opt)

                [whamlet|
$if null opts
    <figure style="text-align:center">
      <span style="font-size:4rem">&varnothing;
      <figcaption>
        _{MsgNoCardsRegisteredYet}.
$else
  <div *{attrs} style="max-height:60svh;overflow-y:auto">
    $forall (i,opt) <- opts
      $maybe (Entity _ (Card _ _ issued),Entity uid (User email _ uname _ _)) <- findCard opt cards
        <div.max.row.no-margin.padding.wave onclick="document.getElementById('#{theId}-#{i}').click()">

          <img.circle src=@{DataR $ UserPhotoR uid} alt=_{MsgPhoto} loading=lazy>

          <div.max>
            <h6.small>
              $maybe name <- uname
                #{name}
              $nothing
                #{email}
            <div.small>
              $with dt <- show issued
                <time.full-datetime datetime=#{dt}>
                  #{dt}

          <label.radio>
            <input type=radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
              :sel x opt:checked>
            <span>
|]
          }


getDataEventAttendeesR :: UserId -> EventId -> Handler Html
getDataEventAttendeesR uid eid = do

    attendees <- runDB $ select $ do
        x :& c :& u <- from $ table @Attendee
            `innerJoin` table @Card `on` (\(x :& c) -> x ^. AttendeeCard ==. c ^. CardId)
            `innerJoin` table @User `on` (\(_ :& c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. AttendeeEvent ==. val eid
        return (x,c,u)

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAttendees
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/attendees/attendees")


postDataEventPosterDeleR :: UserId -> EventId -> PosterId -> Handler Html
postDataEventPosterDeleR uid eid pid = do
    ((fr0,_),_) <- runFormPost formPosterDelete
    case fr0 of
      FormSuccess () -> do
          runDB $ delete pid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ DataEventPosterR uid eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventPosterR uid eid


postDataEventPosterR :: UserId -> EventId -> Handler Html
postDataEventPosterR uid eid = do

    poster <- runDB $ selectOne $ do
        x <- from $ table @Poster
        where_ $ x ^. PosterEvent ==. val eid
        return x

    (fw0,et0) <- generateFormPost formPosterDelete

    idImgPoster <- newIdent
    ((fr,fw),et) <- runFormPost $ formPoster idImgPoster poster

    case fr of
      FormSuccess (Just fi,attrib) -> do
          bs <- fileSourceByteString fi
          void $ runDB $ upsertBy (UniquePoster eid) (Poster eid (fileContentType fi) bs attrib)
              [PosterAttribution P.=. attrib]
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ DataEventPosterR uid eid

      FormSuccess (Nothing,attrib) -> do
          void $ runDB $ update $ \x -> do
              set x [ PosterAttribution =. val attrib ]
              where_ $ x ^. PosterEvent ==. val eid
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ DataEventPosterR uid eid

      _otherwise -> do
          msgr <- getMessageRender
          addMessageI msgError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPoster
              idOverlay <- newIdent
              idDialogDelete <- newIdent
              $(widgetFile "data/catalogue/poster/poster")


getDataEventPosterR :: UserId -> EventId -> Handler Html
getDataEventPosterR uid eid = do

    poster <- runDB $ selectOne $ do
        x <- from $ table @Poster
        where_ $ x ^. PosterEvent ==. val eid
        return x

    (fw0,et0) <- generateFormPost formPosterDelete

    idImgPoster <- newIdent
    (fw,et) <- generateFormPost $ formPoster idImgPoster poster

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPoster
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/catalogue/poster/poster")


formPoster :: Text -> Maybe (Entity Poster) -> Form (Maybe FileInfo, Maybe Html)
formPoster idImgPoster poster extra = do

    (posterR,posterV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgUploadPoster
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } Nothing

    (attribR,attribV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgAttribution
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (posterAttribution . entityVal <$> poster)

    let r = (,) <$> posterR <*> attribR

    let w = do
            toWidget [julius|
                         document.getElementById(#{fvId posterV}).addEventListener('change',function (e) {
                           if (this.files && this.files[0]) {
                             let fr = new FileReader();
                             fr.onload = function (e) {
                               document.getElementById(#{idImgPoster}).setAttribute('src',e.target.result);
                             };
                             fr.readAsDataURL(this.files[0]);
                           }
                         });
                            |]
            [whamlet|^{extra} ^{md3fileWidget posterV} ^{md3textareaWidget attribV}|]

    return (r, w)


formPosterDelete :: Form ()
formPosterDelete extra = return (pure (), [whamlet|#{extra}|])


postDataEventRegistrationR :: UserId -> EventId -> Handler Html
postDataEventRegistrationR uid eid = do

    ((fr,_),_) <- runFormPost $ formRegistration Nothing Nothing

    case fr of
      FormSuccess (eid',cid') -> do
          now <- liftIO getCurrentTime
          runDB $ insert_ Attendee { attendeeEvent = eid'
                                   , attendeeCard = cid'
                                   , attendeeRegDate = now
                                   }
          addMessageI msgSuccess MsgUserSuccessfullyRegisteredForEvent
          redirect $ DataR $ DataEventR uid eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventRegistrationR uid eid


getDataEventRegistrationR :: UserId -> EventId -> Handler Html
getDataEventRegistrationR uid eid = do

    cid <- toSqlKey <$> runInputGet (ireq intField "cid")

    card <- runDB $ selectOne $ do
        x :& u <- from $ table @Card
            `innerJoin` table @User `on` (\(x :& u) -> x ^. CardUser ==. u ^. UserId)
        where_ $ x ^. CardId ==. val cid
        return (x,u)

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    (fw,et) <- generateFormPost $ formRegistration event (fst <$> card)

    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgRegistration
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/registration/registration")


formRegistration :: Maybe (Entity Event) -> Maybe (Entity Card) -> Form (EventId, CardId)
formRegistration event card extra = do
    (eidR,eidV) <- mreq hiddenField "" (entityKey <$> event)
    (cidR,cidV) <- mreq hiddenField "" (entityKey <$> card)
    let r = (,) <$> eidR <*> cidR
    let w = [whamlet|#{extra} ^{fvInput eidV} ^{fvInput cidV}|]
    return (r,w)


getDataEventScannerR :: UserId -> EventId -> Handler Html
getDataEventScannerR uid eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgScanner
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/scanner/scanner")


postDataEventDeleR :: UserId -> EventId -> Handler Html
postDataEventDeleR uid eid = do
    ((fr,_),_) <- runFormPost formEventDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete eid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ DataEventsR uid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventR uid eid


getDataEventEditR :: UserId -> EventId -> Handler Html
getDataEventEditR uid eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    (fw,et) <- generateFormPost $ formEvent uid event

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/edit")


postDataEventR :: UserId -> EventId -> Handler Html
postDataEventR uid eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    ((fr,fw),et) <- runFormPost $ formEvent uid event

    case fr of
      FormSuccess r -> do
          runDB $ replace eid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ DataEventR uid eid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/catalogue/edit")


getDataEventR :: UserId -> EventId -> Handler Html
getDataEventR uid eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    (fw0,et0) <- generateFormPost formEventDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idActionQrScan <- newIdent
        idButtonQrScan <- newIdent
        $(widgetFile "data/catalogue/event")


formEventDelete :: Form ()
formEventDelete extra = return (pure (), [whamlet|#{extra}|])


getDataEventNewR :: UserId -> Handler Html
getDataEventNewR uid = do

    (fw,et) <- generateFormPost $ formEvent uid Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/new")


formEvent :: UserId -> Maybe (Entity Event) -> Form Event
formEvent manager event extra = do

    tz <- appTimeZone . appSettings <$> getYesod

    (timeR,timeV) <- mreq daytimeLocalField FieldSettings
        { fsLabel = SomeMessage MsgTime
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (utcToLocalTime tz . eventTime . entityVal <$> event)

    (nameR,nameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (eventName . entityVal <$> event)

    (descrR,descrV) <- mreq htmlField  FieldSettings
        { fsLabel = SomeMessage MsgDescription
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = []
        } (eventDescr . entityVal <$> event)

    let r = Event manager <$> (localTimeToUTC tz <$> timeR) <*> nameR <*> descrR
    return (r,$(widgetFile "data/catalogue/form"))


postDataEventsR :: UserId -> Handler Html
postDataEventsR uid = do
    ((fr,fw),et) <- runFormPost $ formEvent uid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ DataEventsR uid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/catalogue/new")


getDataEventsR :: UserId -> Handler Html
getDataEventsR uid = do

    month <- liftIO $ (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay <$>  getCurrentTime

    events <- runDB $ select $ do
        x <- from $ table @Event
        orderBy [desc (x ^. EventTime)]
        return x

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEventsCatalogue
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/catalogue")
