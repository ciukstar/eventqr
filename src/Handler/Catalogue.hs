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
      , MsgUnknownMessageRecipient
      )
    )

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

import Settings (widgetFile, AppSettings (appTimeZone, appVAPIDKeys))
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

import Handler.Tokens (fetchRefreshToken, fetchAccessToken, fetchVapidKeys)


postDataEventCalendarEventAttendeeDeleR :: Month -> Day -> EventId -> AttendeeId -> Handler Html
postDataEventCalendarEventAttendeeDeleR month day eid aid = do
    ((fr,_),_) <- runFormPost formAttendeeRemove
    case fr of
      FormSuccess () -> do
          runDB $ delete aid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ DataEventCalendarEventAttendeesR month day eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventCalendarEventAttendeeR month day eid aid


getDataEventCalendarEventAttendeeR :: Month -> Day -> EventId -> AttendeeId -> Handler Html
getDataEventCalendarEventAttendeeR month day eid aid = do

    attendee <- runDB $ selectOne $ do
        x :& e :& c :& u <- from $ table @Attendee
            `innerJoin` table @Event `on` (\(x :& e) -> x ^. AttendeeEvent ==. e ^. EventId)
            `innerJoin` table @Card `on` (\(x :& _ :& c) -> x ^. AttendeeCard ==. c ^. CardId)
            `innerJoin` table @User `on` (\(_ :& _ :& c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. AttendeeId ==. val aid
        return (x,(e,(c,u)))

    (fw0,et0) <- generateFormPost formAttendeeRemove

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAttendees

        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idButtonShowDialogQrCode <- newIdent
        idDialogQrCode <- newIdent
        idButtonCloseDialogQrCode <- newIdent

        $(widgetFile "data/catalogue/calendar/events/attendees/attendee")


getDataEventCalendarEventAttendeesR :: Month -> Day -> EventId -> Handler Html
getDataEventCalendarEventAttendeesR month day eid = do

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


postDataEventCalendarEventPosterDeleR :: Month -> Day -> EventId -> PosterId -> Handler Html
postDataEventCalendarEventPosterDeleR month day eid pid = do
    ((fr0,_),_) <- runFormPost formPosterDelete
    case fr0 of
      FormSuccess () -> do
          runDB $ delete pid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ DataEventCalendarEventPosterR month day eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventCalendarEventPosterR month day eid


postDataEventCalendarEventPosterR :: Month -> Day -> EventId -> Handler Html
postDataEventCalendarEventPosterR month day eid = do

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
          redirect $ DataR $ DataEventCalendarEventPosterR month day eid

      FormSuccess (Nothing,attrib) -> do
          void $ runDB $ update $ \x -> do
              set x [ PosterAttribution =. val attrib ]
              where_ $ x ^. PosterEvent ==. val eid
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ DataEventCalendarEventPosterR month day eid

      _otherwise -> do
          msgr <- getMessageRender
          addMessageI msgError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPoster
              idOverlay <- newIdent
              idDialogDelete <- newIdent
              $(widgetFile "data/catalogue/calendar/events/poster/poster")


getDataEventCalendarEventPosterR :: Month -> Day -> EventId -> Handler Html
getDataEventCalendarEventPosterR month day eid = do

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


postDataEventCalendarRegistrationR :: Month -> Day -> EventId -> Handler Html
postDataEventCalendarRegistrationR month day eid = do

    ((fr,_),_) <- runFormPost $ formRegistration Nothing Nothing

    case fr of
      FormSuccess (eid',cid') -> do
          now <- liftIO getCurrentTime
          runDB $ insert_ Attendee { attendeeEvent = eid'
                                   , attendeeCard = cid'
                                   , attendeeRegDate = now
                                   }
          addMessageI msgSuccess MsgUserSuccessfullyRegisteredForEvent
          redirect $ DataR $ DataEventCalendarEventR month day eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventCalendarRegistrationR month day eid


getDataEventCalendarRegistrationR :: Month -> Day -> EventId -> Handler Html
getDataEventCalendarRegistrationR month day eid = do

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


getDataEventCalendarScannerR :: Month -> Day -> EventId -> Handler Html
getDataEventCalendarScannerR month day eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgScanner
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/calendar/events/scanner/scanner")


postDataEventCalendarEventDeleR :: Month -> Day -> EventId -> Handler Html
postDataEventCalendarEventDeleR month day eid = do
    ((fr,_),_) <- runFormPost formEventDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete eid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ DataEventCalendarEventsR month day
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventCalendarEventR month day eid


postDataEventCalendarEventR :: Month -> Day -> EventId -> Handler Html
postDataEventCalendarEventR month day eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    ((fr,fw),et) <- runFormPost $ formEventDay day event

    case fr of
      FormSuccess r -> do
          runDB $ replace eid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ DataEventCalendarEventR month day eid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/catalogue/calendar/events/edit")


getDataEventCalendarEventEditR :: Month -> Day -> EventId -> Handler Html
getDataEventCalendarEventEditR month day eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    (fw,et) <- generateFormPost $ formEventDay day event

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/calendar/events/edit")


getDataEventCalendarEventR :: Month -> Day -> EventId -> Handler Html
getDataEventCalendarEventR month day eid = do

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


postDataEventCalendarEventsR :: Month -> Day -> Handler Html
postDataEventCalendarEventsR month day = do
    ((fr,fw),et) <- runFormPost $ formEventDay day Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ DataEventCalendarEventsR month day

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/catalogue/calendar/events/new")


getDataEventCalendarEventNewR :: Month -> Day -> Handler Html
getDataEventCalendarEventNewR month day = do

    (fw,et) <- generateFormPost $ formEventDay day Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/calendar/events/new")


formEventDay :: Day -> Maybe (Entity Event) -> Form Event
formEventDay day event extra = do

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

    let r = Event <$> (localTimeToUTC tz . LocalTime day <$> timeR) <*> nameR <*> descrR
    return (r,$(widgetFile "data/catalogue/calendar/events/form"))


getDataEventCalendarEventsR :: Month -> Day -> Handler Html
getDataEventCalendarEventsR month day = do

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


getDataEventCalendarR :: Month -> Handler Html
getDataEventCalendarR month = do
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


postDataEventAttendeeDeleR :: EventId -> AttendeeId -> Handler Html
postDataEventAttendeeDeleR eid aid = do
    ((fr,_),_) <- runFormPost formAttendeeRemove
    case fr of
      FormSuccess () -> do
          runDB $ delete aid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ DataEventAttendeesR eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventAttendeeR eid aid


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


postDataEventAttendeeNotifyR :: EventId -> AttendeeId -> Handler Html
postDataEventAttendeeNotifyR eid aid = do

    attendee <- runDB $ selectOne $ do
        x :& e :& c :& u <- from $ table @Attendee
            `innerJoin` table @Event `on` (\(x :& e) -> x ^. AttendeeEvent ==. e ^. EventId)
            `innerJoin` table @Card `on` (\(x :& _ :& c) -> x ^. AttendeeCard ==. c ^. CardId)
            `innerJoin` table @User `on` (\(_ :& _ :& c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. AttendeeId ==. val aid
        return (x,(e,(c,u)))

    (fw0,et0) <- generateFormPost formAttendeeRemove
    ((fr1,fw1),et1) <- runFormPost $ formAttendeeNotify ((\(_,(e,(_,u))) -> (e,u)) <$> attendee)

    msgr <- getMessageRender
    
    case fr1 of
      FormSuccess ((subject, message),((True, True),(True,Just email))) -> do
          postMessage attendee subject message
          postEmail email subject message
          redirect $ DataR $ DataEventAttendeeR eid aid
      
      FormSuccess ((subject, message),((True,True), (False,_))) -> do
          postMessage attendee subject message
          redirect $ DataR $ DataEventAttendeeR eid aid
                
      FormSuccess ((subject, message),((False, False),(True,Just email))) -> do
          postEmail email subject message
          redirect $ DataR $ DataEventAttendeeR eid aid

      _otherwise -> do
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
  where

      postMessage :: Maybe (Entity Attendee, (Entity Event, (Entity Card, Entity User)))
                  -> Text -> Html -> Handler ()
      postMessage attendee subject message = do
          vapidKeys <- fetchVapidKeys
          user <- maybeAuth
          case (vapidKeys,user,attendee) of
            (Just vapid,Just publisher@(Entity pid _),Just (_,(_,(_,Entity rid _)))) -> do
                results <- pushNotifications vapid publisher eid subject message
                logNotification pid rid subject message
                addMessageI msgSuccess (MsgNotificationSentToSubscribersN (length $ filter isRight results))
                
            (Nothing,_,_) -> do
                addMessageI msgError MsgVapidNotInitializedProperly
                
            (_,Nothing,_) -> do
                addMessageI msgError MsgUnknownMessagePublisher
                
            (_,_,Nothing) -> do
                addMessageI msgError MsgUnknownMessageRecipient
                
      
      postEmail :: Text -> Text -> Html
                -> Handler ()
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
              


pushNotifications :: VAPIDKeys -> Entity User -> EventId -> Text -> Html -> Handler [Either PushNotificationError ()]
pushNotifications vapid publisher eid subject message = do

    rndr <- getUrlRender

    subscriptions <- runDB $ select $ do
        x <- from $ table @PushSubscription
        return x

    let expath = decodeUtf8 . extractPath . encodeUtf8 . rndr
    manager <- appHttpManager <$> getYesod

    let topic = "EventQrNotification"

    forM subscriptions $ \(Entity _ (PushSubscription _ endpoint' p256dh' auth')) -> do
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


getDataEventAttendeeR :: EventId -> AttendeeId -> Handler Html
getDataEventAttendeeR eid aid = do

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


postDataEventAttendeesR :: EventId -> Handler Html
postDataEventAttendeesR eid = do
    ((fr,fw),et) <- runFormPost $ formAttendee eid Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ DataEventAttendeesR eid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/catalogue/attendees/new")


getDataEventAttendeeNewR :: EventId -> Handler Html
getDataEventAttendeeNewR eid = do

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


getDataEventAttendeesR :: EventId -> Handler Html
getDataEventAttendeesR eid = do

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


postDataEventPosterDeleR :: EventId -> PosterId -> Handler Html
postDataEventPosterDeleR eid pid = do
    ((fr0,_),_) <- runFormPost formPosterDelete
    case fr0 of
      FormSuccess () -> do
          runDB $ delete pid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ DataEventPosterR eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventPosterR eid


postDataEventPosterR :: EventId -> Handler Html
postDataEventPosterR eid = do

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
          redirect $ DataR $ DataEventPosterR eid

      FormSuccess (Nothing,attrib) -> do
          void $ runDB $ update $ \x -> do
              set x [ PosterAttribution =. val attrib ]
              where_ $ x ^. PosterEvent ==. val eid
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ DataEventPosterR eid

      _otherwise -> do
          msgr <- getMessageRender
          addMessageI msgError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPoster
              idOverlay <- newIdent
              idDialogDelete <- newIdent
              $(widgetFile "data/catalogue/poster/poster")


getDataEventPosterR :: EventId -> Handler Html
getDataEventPosterR eid = do

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


postDataEventRegistrationR :: EventId -> Handler Html
postDataEventRegistrationR eid = do

    ((fr,_),_) <- runFormPost $ formRegistration Nothing Nothing

    case fr of
      FormSuccess (eid',cid') -> do
          now <- liftIO getCurrentTime
          runDB $ insert_ Attendee { attendeeEvent = eid'
                                   , attendeeCard = cid'
                                   , attendeeRegDate = now
                                   }
          addMessageI msgSuccess MsgUserSuccessfullyRegisteredForEvent
          redirect $ DataR $ DataEventR eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventRegistrationR eid


getDataEventRegistrationR :: EventId -> Handler Html
getDataEventRegistrationR eid = do

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


getDataEventScannerR :: EventId -> Handler Html
getDataEventScannerR eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgScanner
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/scanner/scanner")


postDataEventDeleR :: EventId -> Handler Html
postDataEventDeleR eid = do
    ((fr,_),_) <- runFormPost formEventDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete eid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR DataEventsR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ DataEventR eid


getDataEventEditR :: EventId -> Handler Html
getDataEventEditR eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    (fw,et) <- generateFormPost $ formEvent event

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/edit")


postDataEventR :: EventId -> Handler Html
postDataEventR eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    ((fr,fw),et) <- runFormPost $ formEvent event

    case fr of
      FormSuccess r -> do
          runDB $ replace eid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ DataEventR eid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/catalogue/edit")


getDataEventR :: EventId -> Handler Html
getDataEventR eid = do

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


getDataEventNewR :: Handler Html
getDataEventNewR = do

    (fw,et) <- generateFormPost $ formEvent Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent
        idOverlay <- newIdent
        $(widgetFile "data/catalogue/new")


formEvent :: Maybe (Entity Event) -> Form Event
formEvent event extra = do

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

    let r = Event <$> (localTimeToUTC tz <$> timeR) <*> nameR <*> descrR
    return (r,$(widgetFile "data/catalogue/form"))


postDataEventsR :: Handler Html
postDataEventsR = do
    ((fr,fw),et) <- runFormPost $ formEvent Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR DataEventsR

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              idOverlay <- newIdent
              $(widgetFile "data/catalogue/new")


getDataEventsR :: Handler Html
getDataEventsR = do

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
