{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Calendar
  ( getCalendarR, getCalendarEventsR, getCalendarEventR
  , getCalendarEventAttendeesR, getCalendarEventAttendeeR
  , getCalendarEventScannerR
  , getCalendarEventRegistrationR
  , postCalendarEventUserCardRegisterR
  , getCalendarEventUserRegisterR, postCalendarEventUserRegisterR
  , postCalendarEventRegistrationR
  , postCalendarEventUserUnregisterR
  ) where

import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(second))
import qualified Data.List as L (length, find)
import qualified Data.List.Safe as LS (head)
import Data.Map (Map, fromListWith)
import qualified Data.Map as M (lookup, foldr)
import Data.Maybe (isJust)
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
    , innerJoin, on, subSelectCount, asc, orderBy, delete, in_, subSelectList
    )
import Database.Persist (Entity (Entity), entityKey, entityVal, insert_)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

import Foundation
    ( App (appSettings), Handler, Form, widgetTopbar, widgetSnackbar, widgetScanner
    , Route
      ( CalendarEventAttendeesR, CalendarR, CalendarEventsR, CalendarEventR
      , CalendarEventScannerR, CalendarEventRegistrationR, EventPosterR
      , CalendarEventAttendeeR, HomeR, DataR, CalendarEventUserCardRegisterR
      , CalendarEventUserRegisterR, CalendarEventUserUnregisterR
      )
    , DataR (UserPhotoR, CardQrImageR)
    , AppMessage
      ( MsgCalendar, MsgMon, MsgTue, MsgWed, MsgThu, MsgFri, MsgSat, MsgSun
      , MsgPrevious, MsgNext, MsgEvents, MsgEvent, MsgName, MsgTime
      , MsgDescription, MsgScanQrCodeAndLinkToEvent, MsgDuration
      , MsgNoEventsForThisDayYet, MsgAttendees, MsgDetails, MsgPhoto
      , MsgClose, MsgNoEventsForThisMonth, MsgTotalEventsForThisMonth
      , MsgQrCode, MsgTotalAttendees, MsgRegistrationDate, MsgCardholder
      , MsgCardNumber, MsgCard, MsgNumberOfAttendees, MsgRegistrationForEvent
      , MsgScanner, MsgRegistration, MsgUserSuccessfullyRegisteredForEvent
      , MsgScanAgain, MsgRegister, MsgCancel, MsgInvalidFormData, MsgPoster
      , MsgConfirmUserRegistrationForEventPlease, MsgNotYourQrCodeSorry
      , MsgNotManagerOfEventSorry, MsgYouDoNotHaveACardToRegisterYet
      , MsgRegisterForThisEvent, MsgUnsubscribe, MsgYouAreRegisteredForThisEvent
      , MsgRegisterWithQrCode, MsgSubscriptionSuccessful, MsgSelectCardToRegister
      , MsgCards, MsgRegisterForEvent, MsgIssueDate, MsgUnsubscribeAreYouSure
      , MsgConfirmPlease, MsgUnsubscribeSuccessful, MsgNoAttendeesForThisEventYet
      )
    )
    
import Model
    ( msgSuccess, msgError, normalizeNominalDiffTime
    , EventId, Event(Event, eventTime)
    , AttendeeId, Attendee (Attendee, attendeeEvent, attendeeCard, attendeeRegDate)
    , CardId, Card (Card)
    , Info (Info)
    , UserId, User (User)
    , EntityField
      ( EventTime, EventId, AttendeeCard, CardId, CardUser, AttendeeEvent
      , UserId, AttendeeId, InfoCard, InfoId, CardIssued
      )
    )

import Settings (widgetFile, AppSettings (appTimeZone))

import Text.Cassius (cassius)
import Text.Hamlet (Html)
import Text.Julius (juliusFile)

import Yesod.Auth (maybeAuth)
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, YesodRequest (reqGetParams)
    , getRequest, getMessageRender, getYesod, getMessages, whamlet, addMessageI
    , redirect, ToWidget (toWidget), handlerToWidget, MonadHandler (liftHandler)
    )
import Yesod.Form.Types
    ( FormResult(FormSuccess), FieldView (fvInput), Field (fieldView) )
import Yesod.Form.Functions (mreq, generateFormPost, runFormPost)
import Yesod.Form.Fields
    ( hiddenField, intField, radioField, optionsPairs
    , Option (optionExternalValue, optionInternalValue), OptionList (olOptions)
    )
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Persist.Core (YesodPersist(runDB))


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

    user <- maybeAuth

    case (user,card,event) of
      (Just (Entity uid (User _ _ _ False False False _ _ _)),Just (_,Entity uid' _),_) | uid /= uid' -> do
                addMessageI msgError MsgNotYourQrCodeSorry
                redirect $ CalendarEventR month day eid
          
      (Just (Entity uid (User _ _ _ False False True _ _ _)),_,Just (Entity _ (Event mid _ _ _ _))) | uid /= mid -> do
                addMessageI msgError MsgNotManagerOfEventSorry
                redirect $ CalendarEventR month day eid
          
      _otherwise -> return ()

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


postCalendarEventUserUnregisterR :: Month -> Day -> EventId -> UserId -> Handler Html
postCalendarEventUserUnregisterR month day eid uid = do
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
          redirect $ CalendarEventR month day eid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ CalendarEventR month day eid


postCalendarEventUserRegisterR :: Month -> Day -> EventId -> UserId -> Handler Html
postCalendarEventUserRegisterR month day eid uid = do

    ((fr,fw),et) <- runFormPost $ formUserCards uid

    case fr of
      FormSuccess cid -> postCalendarEventUserCardRegisterR month day eid uid cid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCards 
              idOverlay <- newIdent
              $(widgetFile "calendar/events/cards")


formUserCards :: UserId -> Form CardId
formUserCards uid extra = do

    cards <- liftHandler $ runDB $ select $ do
        x :& u <- from $ table @Card
            `innerJoin` table @User `on` (\(x :& u) -> x ^. CardUser ==. u ^. UserId)
        where_ $ x ^. CardUser  ==. val uid
        orderBy [asc (x ^. CardIssued)]
        return (x,u)
    
    (cardR,cardV) <- mreq (md3radioFieldList cards) "" Nothing

    let w = [whamlet|#{extra} ^{fvInput cardV}|]
    return (cardR,w)

  where

      pairs (Entity cid _,Entity _ (User email _ _ _ _ _ _ _ _)) = (email, cid)

      md3radioFieldList :: [(Entity Card,Entity User)] -> Field Handler CardId
      md3radioFieldList cards = (radioField (optionsPairs (pairs <$> cards)))
          { fieldView = \theId name attrs x isReq -> do
                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs <$> cards))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findEvent :: Option CardId -> [(Entity Card,Entity User)] -> Maybe (Entity Card,Entity User)
                    findEvent opt = L.find (\(Entity cid' _,_) -> cid' == optionInternalValue opt)
                unless (null opts) $ toWidget [cassius|
                    div.row
                        .content
                            display: inline-grid
                        .headline
                            display: inline-block
                            white-space: nowrap
                            overflow: hidden
                            text-overflow: ellipsis
                    |]
                [whamlet|
$if null opts
    <figure style="text-align:center">
      <span style="font-size:4rem">&varnothing;
      <figcaption>
        _{MsgYouDoNotHaveACardToRegisterYet}.
$else
  <div *{attrs}>
    $forall (i,opt) <- opts
      $maybe (Entity _ (Card _ _ issued),Entity uid (User email _ uname _ _ _ _ _ _)) <- findEvent opt cards
        <div.max.row.no-margin.padding.wave onclick="document.getElementById('#{theId}-#{i}').click()">

          <img.circle src=@{DataR $ UserPhotoR uid} alt=_{MsgPhoto} loading=lazy>

          <div.content.max>
            <h6.headline.large-text>
              $maybe name <- uname
                #{name}
              $nothing
                #{email}
            <div.supporting-text.small-text>
              _{MsgIssueDate}
            <div.supporting-text.small-text>
              $with dt <- show issued
                <time.day datetime=#{dt}>
                  #{dt}

          <label.radio>
            <input type=radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
              :sel x opt:checked>
            <span>

|]
          }


getCalendarEventUserRegisterR :: Month -> Day -> EventId -> UserId -> Handler Html
getCalendarEventUserRegisterR month day eid uid = do

    (fw,et) <- generateFormPost $ formUserCards uid

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCards 
        idOverlay <- newIdent
        $(widgetFile "calendar/events/cards")


postCalendarEventUserCardRegisterR :: Month -> Day -> EventId -> UserId -> CardId -> Handler Html
postCalendarEventUserCardRegisterR month day eid _uid cid = do
    ((fw,_),_) <- runFormPost formEventUserRegister
    case fw of
      FormSuccess () -> do
          now <- liftIO getCurrentTime
          runDB $ insert_ Attendee { attendeeEvent = eid
                                   , attendeeCard = cid
                                   , attendeeRegDate = now
                                   }
          addMessageI msgSuccess MsgSubscriptionSuccessful
          redirect $ CalendarEventR month day eid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ CalendarEventR month day eid


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

    user <- maybeAuth

    subscribed <- case user of
      Nothing -> return False
      Just (Entity uid _) -> isJust <$> runDB ( selectOne $ do
          x :& c <- from $ table @Attendee
              `innerJoin` table @Card `on` (\(x :& c) -> x ^. AttendeeCard ==. c ^. CardId)
          where_ $ x ^. AttendeeEvent ==. val eid
          where_ $ c ^. CardUser ==. val uid
          return x )

    cards <- case user of
      Nothing -> return []
      Just (Entity uid _) -> runDB $ select $ do
          x <- from $ table @Card
          where_ $ x ^. CardUser ==. val uid
          return x

    (fw,et) <- generateFormPost formEventUserRegister
    (fw0,et0) <- generateFormPost formEventUserUnregister
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEvent 

        idOverlay <- newIdent
        idActions <- newIdent
        idFormEventUserCardRegister <- newIdent
        idButtonQrScan <- newIdent
        idButtonUnsubscribe <- newIdent
        idDialogUnsubscribe <- newIdent
        idButtonCloseDialogUnsubscribe <- newIdent
        
        when (isJust user && subscribed)
            $ toWidget $(juliusFile "templates/calendar/events/unregister.julius")
        $(widgetFile "calendar/events/event")


formEventUserUnregister :: Form ()
formEventUserUnregister extra = return (pure (), [whamlet|^{extra}|])


formEventUserRegister :: Form ()
formEventUserRegister extra = return (pure (), [whamlet|^{extra}|])


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
      
