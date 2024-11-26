{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Handler.Home
  ( getHomeR
  , getEventR, getEventPosterR, getEventScannerR
  , getEventRegistrationR, postEventRegistrationR
  , getEventUserRegisterR, postEventUserRegisterR
  , postEventUserCardRegisterR, postEventUserUnregisterR
  , getEventAttendeesR
  , getEventAttendeeR
  , getScanQrR
  , getAttendeeRegistrationR, postAttendeeRegistrationR
  , getApiEventsR
  , sliceByRole
  ) where

import Control.Monad (when, unless, forM)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(second))
import qualified Data.List as L (find)
import qualified Data.List.Safe as LS (head)
import Data.Maybe (isJust, fromMaybe)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.Month (pattern YearMonth)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))

import Database.Esqueleto.Experimental
    ( SqlQuery, SqlExpr, Value (unValue), select, selectOne, from, table
    , (^.), (>=.), (==.), (:&) ((:&)), (++.), (%), (||.)
    , innerJoin, on, orderBy, asc, subSelectCount, like, limit, lower_
    , delete, subSelectList, in_, where_, val, countRows, Entity (entityVal)
    )
import Database.Persist (Entity (Entity), entityKey, insert_)
import Database.Persist.Sql (toSqlKey, fromSqlKey)

import Foundation
    ( Handler, Form, widgetSnackbar, widgetTopbar, widgetScanner
    , Route
      ( AuthR, CalendarR, HomeR, EventR, EventRegistrationR
      , EventUserRegisterR, EventUserCardRegisterR, EventUserUnregisterR
      , ScanQrR, AttendeeRegistrationR, EventAttendeesR
      , EventAttendeeR, EventScannerR
      , ApiEventsR, DataR, StaticR, EventPosterR
      )
    , DataR
      ( UserPhotoR, CardQrImageR, AccountEventScheduleR, AccountCardNewR
      , CardPhotoR
      )
    , AppMessage
      ( MsgAppName, MsgEventsCalendar, MsgName, MsgCard, MsgSignIn
      , MsgUpcomingEvents, MsgSearch, MsgEvent, MsgAll, MsgDuration
      , MsgScanQrCode, MsgScanQrCodeAndLinkToEvent, MsgDescription
      , MsgScan, MsgWelcomeTo, MsgRegistration, MsgRegister, MsgPhoto
      , MsgConfirmUserRegistrationForEventPlease, MsgCancel, MsgScanAgain
      , MsgDetails, MsgAttendees, MsgUserSuccessfullyRegisteredForEvent
      , MsgInvalidFormData, MsgClose, MsgQrCode, MsgNoUpcomingEventsYet
      , MsgSelectAnEventToRegisterPlease, MsgSearchEvents, MsgRegistrationDate
      , MsgCardholder, MsgCardNumber, MsgNumberOfAttendees, MsgScanner
      , MsgRegistrationForEvent, MsgNoEventsFound, MsgPoster, MsgEventStartTime
      , MsgRegisterForThisEvent, MsgRegisterWithQrCode, MsgUnsubscribe
      , MsgYouAreRegisteredForThisEvent, MsgUnsubscribeAreYouSure, MsgMyEvents
      , MsgUnsubscribeSuccessful, MsgYouDoNotHaveACardToRegisterYet
      , MsgConfirmPlease, MsgSubscriptionSuccessful, MsgSelectCardToRegister
      , MsgCards, MsgRegisterForEvent, MsgIssueDate, MsgYouDoNonHaveCardsYet
      , MsgMyCards, MsgLoginToSeeYourCardsPlease, MsgNotYourQrCodeSorry
      , MsgCardDoesNotContainAdditionalInfo, MsgNotManagerOfEventSorry
      , MsgFillInCard, MsgMyVisitingSchedule, MsgNoAttendeesForThisEventYet
      , MsgUpcoming, MsgOrderNewCard, MsgCardIsRequiredToParticipateInEvents
      , MsgAwaitingModeration, MsgRejected, MsgCardStatusActive, MsgRevoked
      , MsgRequestDate, MsgDateRejected, MsgDateRevoked
      )
    )

import Model
    ( msgSuccess, msgError, normalizeNominalDiffTime
    , EventId, Event (Event, eventDuration)
    , CardId, Card (Card)
    , CardStatus
      ( CardStatusAwaiting, CardStatusApproved, CardStatusRejected
      , CardStatusRevoked
      )
    , UserId, User (User, userManager)
    , AttendeeId, Attendee (Attendee, attendeeEvent, attendeeCard, attendeeRegDate)
    , Info (Info)
    , Poster (Poster)
    , EntityField
      ( EventTime, EventId, CardId, CardUser, UserId, AttendeeCard, AttendeeEvent
      , AttendeeId, InfoCard, InfoId, EventName, EventDescr, PosterEvent
      , CardUpdated, EventManager
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles (img_event_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Blaze.Html (toHtml)
import Text.Cassius (cassius)
import Text.Hamlet (Html)
import Text.Julius (rawJS, juliusFile)

import Yesod.Auth (Route (LoginR), maybeAuth)
import Yesod.Core
    ( TypedContent (TypedContent), Yesod(defaultLayout), getMessages, selectRep
    , provideJson, getMessageRender, newIdent, whamlet, addMessageI, redirect
    , MonadHandler (liftHandler), ToContent (toContent), ToWidget (toWidget)
    , handlerToWidget, provideRep
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq, iopt)
import Yesod.Form.Fields
    ( intField, hiddenField, radioField, textField, boolField, optionsPairs
    , Option (optionInternalValue, optionExternalValue), OptionList (olOptions)
    )
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form.Types
    (FieldView(fvInput), FormResult (FormSuccess), Field (fieldView))
import Yesod.Persist.Core (YesodPersist(runDB))


getApiEventsR :: Handler TypedContent
getApiEventsR = do

    query <- runInputGet $ iopt textField "q"

    mine <- fromMaybe False <$> runInputGet (iopt boolField "mine")

    user <- maybeAuth

    events <- blow <$> runDB ( select $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId

        case (user,mine) of
          (Just (Entity mid (User _ _ _ _ _ True _ _ _)), True) -> where_ $ x ^. EventManager ==. val mid
          _otherwise -> return ()

        case query of
          Just q -> where_ $ ( lower_ (x ^. EventName) `like` ((%) ++. lower_ (val q) ++. (%)) )
              ||. ( lower_ (x ^. EventDescr) `like` ((%) ++. lower_ (val (toHtml q)) ++. (%)) )
          Nothing -> limit 100

        return (x,attendees) )

    selectRep $ provideJson events

  where
      blow = ((\(e,a) -> (e,a,normalizeNominalDiffTime . eventDuration . entityVal $ e)) . second unValue <$>)


postAttendeeRegistrationR :: UserId -> Handler Html
postAttendeeRegistrationR mid = do

    ((fr,_),_) <- runFormPost $ formAttendeeRegistration mid Nothing

    case fr of
      FormSuccess (eid,cid) -> do
          now <- liftIO getCurrentTime
          runDB $ insert_ Attendee { attendeeEvent = eid
                                   , attendeeCard = cid
                                   , attendeeRegDate = now
                                   }
          addMessageI msgSuccess MsgUserSuccessfullyRegisteredForEvent
          redirect HomeR

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect HomeR


getAttendeeRegistrationR :: UserId -> Handler Html
getAttendeeRegistrationR mid = do

    cid <- toSqlKey <$> runInputGet (ireq intField "cid")

    card <- runDB $ selectOne $ do
        x :& u <- from $ table @Card
            `innerJoin` table @User `on` (\(x :& u) -> x ^. CardUser ==. u ^. UserId)
        where_ $ x ^. CardId ==. val cid
        return (x,u)

    (fw,et) <- generateFormPost $ formAttendeeRegistration mid (fst <$> card)

    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgRegistration
        idOverlay <- newIdent
        $(widgetFile "upcoming/attendees/registration/registration")


formAttendeeRegistration :: UserId -> Maybe (Entity Card) -> Form (EventId, CardId)
formAttendeeRegistration mid card extra = do

    (cidR,cidV) <- mreq hiddenField "" (entityKey <$> card)

    now <- liftIO getCurrentTime
    user <- maybeAuth

    events <- liftHandler $ runDB $ select $ do
        x <- from $ table @Event
        where_ $ x ^. EventTime >=. val now
        sliceByRole user (x ^. EventManager ==. val mid)
        orderBy [asc (x ^. EventTime)]
        return x

    (eidR,eidV) <- mreq (md3radioFieldList events) "" Nothing

    let r = (,) <$> eidR <*> cidR
    let w = [whamlet|#{extra} ^{fvInput eidV} ^{fvInput cidV}|]
    return (r,w)

  where

      pairs (Entity eid (Event _ _ name _ _)) = (name, eid)

      md3radioFieldList :: [Entity Event] -> Field Handler EventId
      md3radioFieldList events = (radioField (optionsPairs (pairs <$> events)))
          { fieldView = \theId name attrs x isReq -> do
                opts <- zip [1 :: Int ..] . olOptions <$> handlerToWidget (optionsPairs (pairs <$> events))

                let sel (Left _) _ = False
                    sel (Right y) opt = optionInternalValue opt == y

                let findEvent :: Option EventId -> [Entity Event] -> Maybe (Entity Event)
                    findEvent opt = L.find (\(Entity eid' _) -> eid' == optionInternalValue opt)

                [whamlet|
$if null opts
    <figure style="text-align:center">
      <span style="font-size:4rem">&varnothing;
      <figcaption>
        _{MsgNoUpcomingEventsYet}.
$else
  <div *{attrs} style="height:40svh;overflow-y:auto">
    $forall (i,opt) <- opts
      $maybe Entity _ (Event _ time ename _ _) <- findEvent opt events
        <div.max.row.no-margin.padding.wave onclick="document.getElementById('#{theId}-#{i}').click()">
          <label.radio>
            <input type=radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
              :sel x opt:checked>
            <span>

          <div.max>
            <h6.small>
              #{ename}
            <div.small>
              $with dt <- show time
                <time.full-datetime datetime=#{dt}>
                  #{dt}

|]
          }


getScanQrR :: UserId -> Handler Html
getScanQrR uid = do

    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgScanner
        idOverlay <- newIdent
        $(widgetFile "scanner/scanner")


postEventUserUnregisterR :: EventId -> UserId -> Handler Html
postEventUserUnregisterR eid uid = do
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
          redirect $ EventR eid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ EventR eid


postEventUserCardRegisterR :: EventId -> UserId -> CardId -> Handler Html
postEventUserCardRegisterR eid _uid cid = do
    ((fw,_),_) <- runFormPost formEventUserRegister
    case fw of
      FormSuccess () -> do
          now <- liftIO getCurrentTime
          runDB $ insert_ Attendee { attendeeEvent = eid
                                   , attendeeCard = cid
                                   , attendeeRegDate = now
                                   }
          addMessageI msgSuccess MsgSubscriptionSuccessful
          redirect $ EventR eid

      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ EventR eid


postEventUserRegisterR :: EventId -> UserId -> Handler Html
postEventUserRegisterR eid uid = do

    ((fr,fw),et) <- runFormPost $ formUserCards uid

    case fr of
      FormSuccess cid -> postEventUserCardRegisterR eid uid cid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCards
              idOverlay <- newIdent
              $(widgetFile "upcoming/cards")


getEventUserRegisterR :: EventId -> UserId -> Handler Html
getEventUserRegisterR eid uid = do

    (fw,et) <- generateFormPost $ formUserCards uid

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCards
        idOverlay <- newIdent
        $(widgetFile "upcoming/cards")


formUserCards :: UserId -> Form CardId
formUserCards uid extra = do

    cards <- liftHandler $ runDB $ select $ do
        x :& u <- from $ table @Card
            `innerJoin` table @User `on` (\(x :& u) -> x ^. CardUser ==. u ^. UserId)
        where_ $ x ^. CardUser  ==. val uid
        orderBy [asc (x ^. CardUpdated)]
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
      $maybe (Entity _ (Card _ _ _ status updated _),Entity uid (User email _ uname _ _ _ _ _ _)) <- findEvent opt cards
        <div.max.row.no-margin.padding.wave onclick="document.getElementById('#{theId}-#{i}').click()">

          <img.circle src=@{DataR $ UserPhotoR uid} alt=_{MsgPhoto} loading=lazy>

          <div.content.max>
            <h6.headline.large-text>
              $maybe name <- uname
                #{name}
              $nothing
                #{email}
            <div.supporting-text.small-text>
              $case status
                $of CardStatusAwaiting
                  <span>
                    <i.small>pending
                    _{MsgAwaitingModeration}

                $of CardStatusApproved
                  <span>
                    <i.small>verified
                    _{MsgCardStatusActive}

                $of CardStatusRejected
                  <span>
                    <i.small>block
                    _{MsgRejected}
            <div.supporting-text.small-text>
              $with dt <- show updated
                <time.day datetime=#{dt}>
                  #{dt}

          <label.radio>
            <input type=radio ##{theId}-#{i} name=#{name} :isReq:required=true value=#{optionExternalValue opt}
              :sel x opt:checked>
            <span>

|]
          }


postEventRegistrationR :: EventId -> Handler Html
postEventRegistrationR eid = do

    ((fr,_),_) <- runFormPost $ formRegistration Nothing Nothing

    case fr of
      FormSuccess (eid',cid') -> do
          now <- liftIO getCurrentTime
          runDB $ insert_ Attendee { attendeeEvent = eid'
                                   , attendeeCard = cid'
                                   , attendeeRegDate = now
                                   }
          addMessageI msgSuccess MsgUserSuccessfullyRegisteredForEvent
          redirect $ EventR eid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ EventRegistrationR eid


getEventRegistrationR :: EventId -> Handler Html
getEventRegistrationR eid = do

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
                redirect $ EventR eid

      (Just (Entity uid (User _ _ _ False False True _ _ _)),_,Just (Entity _ (Event mid _ _ _ _))) | uid /= mid -> do
                addMessageI msgError MsgNotManagerOfEventSorry
                redirect $ EventR eid

      _otherwise -> return ()

    (fw,et) <- generateFormPost $ formRegistration event (fst <$> card)

    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgRegistration
        idOverlay <- newIdent
        $(widgetFile "upcoming/registration/registration")


formRegistration :: Maybe (Entity Event) -> Maybe (Entity Card) -> Form (EventId, CardId)
formRegistration event card extra = do
    (eidR,eidV) <- mreq hiddenField "" (entityKey <$> event)
    (cidR,cidV) <- mreq hiddenField "" (entityKey <$> card)
    let r = (,) <$> eidR <*> cidR
    let w = [whamlet|#{extra} ^{fvInput eidV} ^{fvInput cidV}|]
    return (r,w)


getEventAttendeeR :: EventId -> AttendeeId -> Handler Html
getEventAttendeeR eid aid = do

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
        $(widgetFile "upcoming/attendees/card")


getEventAttendeesR :: EventId -> Handler Html
getEventAttendeesR eid = do

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
        $(widgetFile "upcoming/attendees/attendees")


getEventScannerR :: EventId -> Handler Html
getEventScannerR eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x

    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgScanner
        idOverlay <- newIdent
        $(widgetFile "upcoming/scanner/scanner")


getEventR :: EventId -> Handler Html
getEventR eid = do

    user <- maybeAuth

    event <- (second unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId

        where_ $ x ^. EventId ==. val eid
        return (x,attendees) )

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
            $ toWidget $(juliusFile "templates/upcoming/unregister.julius")
        $(widgetFile "upcoming/event")


formEventUserUnregister :: Form ()
formEventUserUnregister extra = return (pure (), [whamlet|^{extra}|])


formEventUserRegister :: Form ()
formEventUserRegister extra = return (pure (), [whamlet|^{extra}|])


getHomeR :: Handler TypedContent
getHomeR = do

    now <- liftIO getCurrentTime
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay $ now

    user <- maybeAuth

    cards <- do
        cards <- case user of
          Nothing -> return []
          Just (Entity uid _) -> runDB $ select $ do
              x :& u <- from $ table @Card
                 `innerJoin` table @User `on` (\(x :& u) -> x ^. CardUser ==. u ^. UserId)
              where_ $ x ^. CardUser  ==. val uid
              orderBy [asc (x ^. CardUpdated)]
              return (x,u)

        forM cards $ \c@(Entity cid _,_) -> (c,) <$> runDB ( select $ do
            x <- from $ table @Info
            where_ $ x ^. InfoCard ==. val cid
            return x )

    mine <- fromMaybe True <$> runInputGet (iopt boolField "mine")

    nMineUpcoming <- case user of
          Just (Entity mid (User _ _ _ _ _ True _ _ _)) -> maybe (0 :: Int) unValue <$> runDB ( selectOne $ do
              x <- from $ table @Event
              where_ $ x ^. EventManager ==. val mid
              where_ $ x ^. EventTime >=. val now
              return countRows )
          _otherwise -> return 0

    nAllUpcoming <- maybe (0 :: Int) unValue <$> runDB ( selectOne $ do
              x <- from $ table @Event
              where_ $ x ^. EventTime >=. val now
              return countRows )

    nMineEvents <- case user of
          Just (Entity mid (User _ _ _ _ _ True _ _ _)) -> maybe (0 :: Int) unValue <$> runDB ( selectOne $ do
              x <- from $ table @Event
              where_ $ x ^. EventManager ==. val mid
              return countRows )
          _otherwise -> return 0

    nAllEvents <- maybe (0 :: Int) unValue <$> runDB ( selectOne $ do
              _ <- from $ table @Event
              return countRows )

    upcoming <- (second unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId

        case (user,mine) of
          (Just (Entity mid (User _ _ _ _ _ True _ _ _)), True) -> where_ $ x ^. EventManager ==. val mid
          _otherwise -> return ()

        where_ $ x ^. EventTime >=. val now
        orderBy [asc (x ^. EventTime)]
        limit 100
        return (x, attendees) )

    events <- (second unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId

        case (user,mine) of
          (Just (Entity mid (User _ _ _ _ _ True _ _ _)), True) -> where_ $ x ^. EventManager ==. val mid
          _otherwise -> return ()

        orderBy [asc (x ^. EventTime)]
        limit 100
        return (x, attendees) )

    selectRep $ do
        provideJson $ blow <$> upcoming

        provideRep $ do
            msgr <- getMessageRender
            msgs <- getMessages
            defaultLayout $ do
                setTitleI MsgAppName

                idOverlay <- newIdent
                idMain <- newIdent
                idDetailsMyCards <- newIdent
                idNavEventList <- newIdent
                idDialogQrCode <- newIdent

                idButtonUpcomingEvents <- newIdent
                idDialogUpcomingEvents <- newIdent
                idButtonCloseDialogUpcomingEvents <- newIdent
                idFormUpcomingFilter <- newIdent

                idButtonSearchEvents <- newIdent
                idDialogSearchEvents <- newIdent
                idButtonCloseDialogSearchEvents <- newIdent
                idFormSearhFilter <- newIdent
                idNavSearchEventsResult <- newIdent
                idInputSearchEvents <- newIdent

                idButtonQrScanner <- newIdent

                when (maybe False (userManager . entityVal) user)
                    $ toWidget $(juliusFile "templates/upcoming/submit.julius")
                $(widgetFile "homepage")

  where
      blow (e,a) = (e,a,normalizeNominalDiffTime . eventDuration . entityVal $ e)


getEventPosterR :: EventId -> Handler TypedContent
getEventPosterR eid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @Poster
        where_ $ x ^. PosterEvent ==. val eid
        return x
    case photo of
      Just (Entity _ (Poster _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_event_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg


sliceByRole :: Maybe (Entity User) -> SqlExpr (Value Bool) -> SqlQuery ()
sliceByRole user expr = case user of
          Just (Entity _ (User _ _ _ True _ _ _ _ _)) -> return ()
          Just (Entity _ (User _ _ _ _ True _ _ _ _)) -> return ()
          Just (Entity _ (User _ _ _ _ _ True _ _ _)) -> where_ expr
          _otherwise -> where_ $ val False
