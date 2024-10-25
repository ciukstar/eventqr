{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Calendar
  ( getCalendarR, getEventsR, getEventR
  , getEventAttendeesR
  ) where

import Data.Map (Map, fromListWith)
import qualified Data.List as L (length)
import qualified Data.Map as M (lookup, foldr)
import Data.Time.Calendar
    ( toGregorian, weekFirstDay, DayOfWeek (Monday), addDays
    , DayPeriod (periodFirstDay, periodLastDay, dayPeriod), Day
    )
import Data.Time.Calendar.Month (Month, addMonths, pattern YearMonth)
import Data.Time.Clock (UTCTime(utctDay))
import Data.Time.LocalTime
    ( LocalTime (LocalTime), localTimeToUTC, utc, TimeOfDay (TimeOfDay) )


import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, between, val
    , (^.), (==.), (:&) ((:&))
    , innerJoin, on
    )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, widgetTopbar
    , Route
      ( EventAttendeesR, ScannerR, CalendarR, EventsR, EventR, HomeR
      , DataR
      )
    , DataR (UserPhotoR, CardQrCodeR)
    , AppMessage
      ( MsgCalendar, MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgPrevious, MsgNext, MsgEvents, MsgEvent, MsgName, MsgTime
      , MsgDescription, MsgScanQrCode, MsgScanQrCodeAndLinkToEvent
      , MsgNoEventsForThisDayYet, MsgAttendees, MsgDetails, MsgPhoto
      , MsgClose, MsgNoEventsForThisMonth, MsgTotalEventsForThisMonth
      , MsgQrCode
      )
    )
    
import Model
    ( EventId, Event(Event)
    , Attendee (Attendee)
    , Card, User (User)
    , EntityField
      ( EventTime, EventId, AttendeeCard, CardId, CardUser, AttendeeEvent
      , UserId
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, YesodRequest (reqGetParams)
    , getRequest, getMessageRender
    )
import Yesod.Persist.Core (YesodPersist(runDB))


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

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x
    
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgEvent

        idOverlay <- newIdent
        idActionQrScan <- newIdent 
        idButtonQrScan <- newIdent 
        
        $(widgetFile "calendar/events/event")


getEventsR :: Day -> Handler Html
getEventsR day = do

    events <- runDB $ select $ do
        x <- from $ table @Event
        where_ $ x ^. EventTime `between` ( val $ localTimeToUTC utc (LocalTime day (TimeOfDay 0 0 0))
                                          , val $ localTimeToUTC utc (LocalTime day (TimeOfDay 23 59 0))
                                          )
        return x
    
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

    events <- groupByKey (\(Entity _ (Event time _ _)) -> utctDay time) <$> runDB ( select $ do
        x <- from $ table @Event
        where_ $ x ^. EventTime `between` ( val $ localTimeToUTC utc (LocalTime (periodFirstDay month) (TimeOfDay 0 0 0))
                                          , val $ localTimeToUTC utc (LocalTime (periodLastDay month) (TimeOfDay 0 0 0))
                                          )
        return x )

    let total = M.foldr (\xs a -> L.length xs + a) 0

    msgr <- getMessageRender
    
    defaultLayout $ do
        setTitleI MsgCalendar

        idOverlay <- newIdent
        idCalendarPage <- newIdent
        
        $(widgetFile "calendar/calendar")

  where
      groupByKey :: Ord k => (v -> k) -> [v] -> Map k [v]
      groupByKey f = fromListWith (<>) . fmap (\x -> (f x,[x]))
      
