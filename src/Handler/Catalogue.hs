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
  ) where


import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)

import Data.Map (Map, fromListWith)
import qualified Data.Map as M (lookup, foldr)
import Data.Maybe (fromMaybe)
import qualified Data.List as L (find, length)
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
    , (^.), (==.), (:&) ((:&)), (>=.), (<.)
    , innerJoin, on, asc
    )

import Database.Persist
    ( Entity (Entity, entityKey), delete, entityVal, replace, insert_)
import Database.Persist.Sql (toSqlKey)

import Foundation
    ( App (appSettings), Handler, Form, widgetTopbar, widgetSnackbar, widgetScanner
    , Route (DataR)
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
      , MsgConfirmUserRegistrationForEventPlease
      )
    )

import Material3 (daytimeLocalField, md3textareaWidget, md3widget)

import Model
    ( msgSuccess, msgError
    , EventId, Event(Event, eventName, eventTime, eventDescr)
    , CardId, Card (Card)
    , User (User)
    , AttendeeId, Attendee (Attendee, attendeeRegDate, attendeeCard, attendeeEvent)
    , EntityField
      ( EventTime, EventId, AttendeeCard, CardId, CardUser, AttendeeEvent
      , UserId, UserName, AttendeeId
      )
    )

import Settings (widgetFile, AppSettings (appTimeZone))

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, getMessages
    , whamlet, redirect, addMessageI, SomeMessage (SomeMessage), getYesod
    , MonadHandler (liftHandler), handlerToWidget, getRequest, YesodRequest (reqGetParams)
    )
import Yesod.Form.Fields
    ( textField, textareaField, radioField, optionsPairs
    , Option (optionInternalValue, optionExternalValue)
    , OptionList (olOptions), timeField, hiddenField, intField
    )
import Yesod.Form.Functions (generateFormPost, runFormPost, mreq)
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Types
    ( FormResult(FormSuccess), Field (fieldView), FieldView (fvInput, fvErrors, fvId)
    , FieldSettings (FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


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

    (descrR,descrV) <- mreq textareaField  FieldSettings
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
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAttendees

        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idButtonShowDialogQrCode <- newIdent
        idDialogQrCode <- newIdent
        idButtonCloseDialogQrCode <- newIdent
        
        $(widgetFile "data/catalogue/attendees/attendee")


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

      pairs (Entity cid _, Entity _ (User email _ name _)) = (fromMaybe email name, cid)

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
      $maybe (Entity _ (Card _ _ issued),Entity uid (User email _ uname _)) <- findCard opt cards
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

    (descrR,descrV) <- mreq textareaField  FieldSettings
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

