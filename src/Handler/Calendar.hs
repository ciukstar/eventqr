{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Calendar
  ( getCalendarR, getCalendarEventsR, getCalendarEventR
  , getCalendarEventAttendeesR, getCalendarEventAttendeeR
  , getCalendarEventScannerR
  , getCalendarEventRegistrationR, postCalendarEventRegistrationR
  ) where

import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(second))
import Data.Map (Map, fromListWith)
import qualified Data.List as L (length)
import qualified Data.Map as M (lookup, foldr)
import Data.Time.Calendar
    ( toGregorian, weekFirstDay, DayOfWeek (Monday), addDays
    , DayPeriod (periodFirstDay), Day
    )
import Data.Time.Calendar.Month (Month, addMonths, pattern YearMonth)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime
    ( LocalTime (LocalTime, localDay), localTimeToUTC, utcToLocalTime
    , TimeOfDay (TimeOfDay)
    )

import Database.Esqueleto.Experimental
    ( Value (unValue), SqlExpr, select, selectOne, from, table, where_, val
    , (^.), (==.), (:&) ((:&)), (>=.), (<.)
    , innerJoin, on, subSelectCount, asc, orderBy
    )
import Database.Persist (Entity (Entity), entityKey, entityVal, insert_)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( App (appSettings), Handler, Form, widgetTopbar, widgetSnackbar, widgetScanner
    , Route
      ( CalendarEventAttendeesR, CalendarR, CalendarEventsR, CalendarEventR
      , CalendarEventScannerR, CalendarEventRegistrationR, EventPosterR
      , CalendarEventAttendeeR, HomeR, DataR
      )
    , DataR (UserPhotoR, CardQrImageR)
    , AppMessage
      ( MsgCalendar, MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgPrevious, MsgNext, MsgEvents, MsgEvent, MsgName, MsgTime
      , MsgDescription, MsgScanQrCode, MsgScanQrCodeAndLinkToEvent
      , MsgNoEventsForThisDayYet, MsgAttendees, MsgDetails, MsgPhoto
      , MsgClose, MsgNoEventsForThisMonth, MsgTotalEventsForThisMonth
      , MsgQrCode, MsgTotalAttendees, MsgRegistrationDate, MsgCardholder
      , MsgCardNumber, MsgCard, MsgNumberOfAttendees, MsgRegistrationForEvent
      , MsgScanner, MsgRegistration, MsgUserSuccessfullyRegisteredForEvent
      , MsgScanAgain, MsgRegister, MsgCancel, MsgInvalidFormData
      , MsgConfirmUserRegistrationForEventPlease, MsgPoster
      )
    )
    
import Model
    ( EventId, Event(Event, eventTime)
    , Attendee (Attendee, attendeeEvent, attendeeCard, attendeeRegDate)
    , CardId, Card, User (User)
    , EntityField
      ( EventTime, EventId, AttendeeCard, CardId, CardUser, AttendeeEvent
      , UserId, AttendeeId, InfoCard, InfoId
      ), AttendeeId, Info (Info), msgSuccess, msgError
    )

import Settings (widgetFile, AppSettings (appTimeZone))

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, YesodRequest (reqGetParams)
    , getRequest, getMessageRender, getYesod, getMessages, whamlet, addMessageI
    , redirect
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Types (FormResult(FormSuccess), FieldView (fvInput))
import Yesod.Form.Functions (mreq, generateFormPost, runFormPost)
import Yesod.Form.Fields (hiddenField, intField)
import Yesod.Form.Input (runInputGet, ireq)


getCalendarEventAttendeeR :: Month -> Day -> EventId -> AttendeeId -> Handler Html
getCalendarEventAttendeeR month day eid aid = do

    card <- runDB $ selectOne $ do
        x :& c :& u <- from $ table @Attendee
            `innerJoin` table @Card `on` (\(x :& c) -> x ^. AttendeeCard ==. c ^. CardId)
            `innerJoin` table @User `on` (\(_ :& c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. AttendeeId ==. val aid
        return (c,u)

    attrs <- case card of
      Just (Entity cid _, _) -> runDB $ select $ do
          x <- from $ table @Info
          where_ $ x ^. InfoCard ==. val cid
          orderBy [asc (x ^. InfoId)]
          return x
      Nothing -> return []

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAttendees
        idOverlay <- newIdent
        idButtonShowDialogQrCode <- newIdent
        idDialogQrCode <- newIdent
        idButtonCloseDialogQrCode <- newIdent
        $(widgetFile "calendar/events/attendees/card")


getCalendarEventAttendeesR :: Month -> Day -> EventId -> Handler Html
getCalendarEventAttendeesR month day eid = do

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
        
        $(widgetFile "calendar/events/attendees/attendees")


postCalendarEventRegistrationR :: Month -> Day -> EventId -> Handler Html
postCalendarEventRegistrationR month day eid = do

    ((fr,_),_) <- runFormPost $ formRegistration Nothing Nothing

    case fr of
      FormSuccess (eid',cid') -> do
          now <- liftIO getCurrentTime
          runDB $ insert_ Attendee { attendeeEvent = eid'
                                   , attendeeCard = cid'
                                   , attendeeRegDate = now
                                   }
          addMessageI msgSuccess MsgUserSuccessfullyRegisteredForEvent
          redirect $ CalendarEventR month day eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ CalendarEventRegistrationR month day eid


getCalendarEventRegistrationR :: Month -> Day -> EventId -> Handler Html
getCalendarEventRegistrationR month day eid = do

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
        $(widgetFile "calendar/events/registration/registration")


formRegistration :: Maybe (Entity Event) -> Maybe (Entity Card) -> Form (EventId, CardId)
formRegistration event card extra = do
    (eidR,eidV) <- mreq hiddenField "" (entityKey <$> event)
    (cidR,cidV) <- mreq hiddenField "" (entityKey <$> card)
    let r = (,) <$> eidR <*> cidR
    let w = [whamlet|^{extra} ^{fvInput eidV} ^{fvInput cidV}|]
    return (r,w)


getCalendarEventScannerR :: Month -> Day -> EventId -> Handler Html
getCalendarEventScannerR month day eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x
    
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgScanner 
        idOverlay <- newIdent
        $(widgetFile "calendar/events/scanner/scanner")


getCalendarEventR :: Month -> Day -> EventId -> Handler Html
getCalendarEventR month day eid = do

    event <- (second unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId
                
        where_ $ x ^. EventId ==. val eid
        return (x,attendees) )
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent

        idOverlay <- newIdent
        idActionQrScan <- newIdent 
        idButtonQrScan <- newIdent 
        
        $(widgetFile "calendar/events/event")


getCalendarEventsR :: Month -> Day -> Handler Html
getCalendarEventsR month day = do
    
    tz <- appTimeZone . appSettings <$> getYesod

    events <- (second unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId
        
        where_ $ x ^. EventTime >=. val (localTimeToUTC tz (LocalTime day (TimeOfDay 0 0 0)))
        where_ $ x ^. EventTime <.  val (localTimeToUTC tz (LocalTime (addDays 1 day) (TimeOfDay 0 0 0)))
        return (x,attendees) )
    
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgEvents 

        idOverlay <- newIdent
        
        $(widgetFile "calendar/events/events")


getCalendarR :: Month -> Handler Html
getCalendarR month = do
    stati <- reqGetParams <$> getRequest

    let start = weekFirstDay Monday (periodFirstDay month)
    let end = addDays 41 start
    let page = [start .. end]
    let next = addMonths 1 month
    let prev = addMonths (-1) month

    tz <- appTimeZone . appSettings <$> getYesod

    events <- groupByKey (dayAt tz) . (second unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId
                
        where_ $ x ^. EventTime >=. val (localTimeToUTC tz (LocalTime (periodFirstDay month) (TimeOfDay 0 0 0)))
        where_ $ x ^. EventTime <.  val (localTimeToUTC tz (LocalTime (periodFirstDay next) (TimeOfDay 0 0 0)))
        return (x,attendees) )

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCalendar

        idOverlay <- newIdent
        idCalendarPage <- newIdent
        idCalendarLegend <- newIdent
        
        $(widgetFile "calendar/calendar")

  where
      
      total :: Map k [(a,Int)] -> (Int,Int)
      total = M.foldr (\xs (a,b) -> (L.length xs + a, sum (snd <$> xs) + b)) (0,0)
      
      dayAt tz = localDay . utcToLocalTime tz . eventTime . entityVal . fst
      
      groupByKey :: Ord k => (v -> k) -> [v] -> Map k [v]
      groupByKey f = fromListWith (<>) . fmap (\x -> (f x,[x]))
      
