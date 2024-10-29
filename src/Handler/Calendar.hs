{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Calendar
  ( getCalendarR, getEventsR, getEventR
  , getEventAttendeesR, getEventAttendeeR
  ) where

import Data.Bifunctor (Bifunctor(second))
import Data.Map (Map, fromListWith)
import qualified Data.List as L (length)
import qualified Data.Map as M (lookup, foldr)
import Data.Time.Calendar
    ( toGregorian, weekFirstDay, DayOfWeek (Monday), addDays
    , DayPeriod (periodFirstDay, dayPeriod), Day
    )
import Data.Time.Calendar.Month (Month, addMonths, pattern YearMonth)
import Data.Time.LocalTime
    ( LocalTime (LocalTime, localDay), localTimeToUTC, utcToLocalTime
    , TimeOfDay (TimeOfDay)
    )


import Database.Esqueleto.Experimental
    ( SqlExpr, select, selectOne, from, table, where_, val
    , (^.), (==.), (:&) ((:&)), (>=.), (<.)
    , innerJoin, on, subSelectCount, Value (unValue), asc, orderBy
    )
import Database.Persist (Entity (Entity), entityVal)
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( App (appSettings), Handler, widgetTopbar, widgetSnackbar
    , Route
      ( EventAttendeesR, ScannerR, CalendarR, EventsR, EventR, HomeR
      , EventAttendeeR, DataR
      )
    , DataR (UserPhotoR, CardQrImageR)
    , AppMessage
      ( MsgCalendar, MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgPrevious, MsgNext, MsgEvents, MsgEvent, MsgName, MsgTime
      , MsgDescription, MsgScanQrCode, MsgScanQrCodeAndLinkToEvent
      , MsgNoEventsForThisDayYet, MsgAttendees, MsgDetails, MsgPhoto
      , MsgClose, MsgNoEventsForThisMonth, MsgTotalEventsForThisMonth
      , MsgQrCode, MsgTotalAttendees, MsgRegistrationDate, MsgCardholder
      , MsgCardNumber, MsgCard, MsgNumberOfAttendees
      )
    )
    
import Model
    ( EventId, Event(Event, eventTime)
    , Attendee (Attendee)
    , Card, User (User)
    , EntityField
      ( EventTime, EventId, AttendeeCard, CardId, CardUser, AttendeeEvent
      , UserId, AttendeeId, InfoCard, InfoId
      ), AttendeeId, Info (Info)
    )

import Settings (widgetFile, AppSettings (appTimeZone))

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, YesodRequest (reqGetParams)
    , getRequest, getMessageRender, getYesod, getMessages
    )
import Yesod.Persist.Core (YesodPersist(runDB))


getEventAttendeeR :: Day -> EventId -> AttendeeId -> Handler Html
getEventAttendeeR day eid aid = do

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


getEventAttendeesR :: Day -> EventId -> Handler Html
getEventAttendeesR day eid = do

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


getEventR :: Day -> EventId -> Handler Html
getEventR day eid = do

    event <- (second unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId
                
        where_ $ x ^. EventId ==. val eid
        return (x,attendees) )
    
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgEvent

        idOverlay <- newIdent
        idActionQrScan <- newIdent 
        idButtonQrScan <- newIdent 
        
        $(widgetFile "calendar/events/event")


getEventsR :: Day -> Handler Html
getEventsR day = do
    
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
      
