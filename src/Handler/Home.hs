{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Handler.Home
  ( getHomeR, getFetchR
  , getEventR, getEventPosterR, getEventScannerR
  , getEventRegistrationR, postEventRegistrationR
  , getEventUserRegisterR, postEventUserRegisterR
  , postEventUserCardRegisterR, postEventUserUnregisterR
  , getEventAttendeesR
  , getEventAttendeeR
  , getScanQrR
  , getAttendeeRegistrationR, postAttendeeRegistrationR
  , getApiEventsR
  ) where

import qualified Control.Lens as L ( (^?) )
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (decode)
import qualified Data.Aeson as A (Value)
import Data.Bifunctor (Bifunctor(second))
import qualified Data.List as L (find)
import qualified Data.List.Safe as LS (head)
import Data.Maybe (isJust)
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.Month (pattern YearMonth)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (unValue), select, selectOne, from, table, where_, val
    , (^.), (>=.), (==.), (:&) ((:&)), (++.), (%), (||.)
    , innerJoin, on, orderBy, asc, subSelectCount, like, limit, lower_
    , delete, subSelectList, in_
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
    , DataR (UserPhotoR, CardQrImageR)
    , AppMessage
      ( MsgAppName, MsgEventsCalendar, MsgName, MsgCard, MsgSignIn
      , MsgUpcomingEvents, MsgEvents, MsgSearch, MsgEvent
      , MsgScanQrCode, MsgScanQrCodeAndLinkToEvent, MsgDescription
      , MsgScan, MsgWelcomeTo, MsgRegistration, MsgRegister, MsgPhoto
      , MsgConfirmUserRegistrationForEventPlease, MsgCancel, MsgScanAgain
      , MsgDetails, MsgAttendees, MsgUserSuccessfullyRegisteredForEvent
      , MsgInvalidFormData, MsgClose, MsgQrCode, MsgNoUpcomingEventsYet
      , MsgSelectAnEventToRegisterPlease, MsgSearchEvents, MsgRegistrationDate
      , MsgCardholder, MsgCardNumber, MsgNumberOfAttendees, MsgScanner
      , MsgRegistrationForEvent, MsgNoEventsFound, MsgPoster, MsgEventStartTime
      , MsgRegisterForThisEvent, MsgRegisterWithQrCode, MsgUnsubscribe
      , MsgYouAreRegisteredForThisEvent, MsgUnsubscribeAreYouSure
      , MsgUnsubscribeSuccessful, MsgYouDoNotHaveACardToRegisterYet
      , MsgConfirmPlease, MsgSubscriptionSuccessful, MsgSelectCardToRegister
      , MsgCards, MsgRegisterForEvent, MsgIssueDate, MsgYouDoNonHaveCardsYet
      , MsgMyCards, MsgLoginToSeeYourCardsPlease, MsgFillInCard
      , MsgCardDoesNotContainAdditionalInfo
      )
    )

import Model
    ( msgSuccess, msgError
    , EventId, Event (Event)
    , CardId, Card (Card)
    , UserId, User (User)
    , AttendeeId, Attendee (Attendee, attendeeEvent, attendeeCard, attendeeRegDate)
    , Info (Info)
    , Poster (Poster)
    , EntityField
      ( EventTime, EventId, CardId, CardUser, UserId, AttendeeCard, AttendeeEvent
      , AttendeeId, InfoCard, InfoId, EventName, EventDescr, PosterEvent, CardIssued
      )
    )

import Network.Wreq (get)
import qualified Network.Wreq as WL (responseBody)

import Settings (widgetFile)
import Settings.StaticFiles (img_event_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg)

import Text.Blaze.Html (toHtml)
import Text.Cassius (cassius)
import Text.Hamlet (Html)
import Text.Julius (rawJS, juliusFile)

import Yesod.Auth (YesodAuth(maybeAuthId), Route (LoginR))
import Yesod.Core
    ( TypedContent (TypedContent), Yesod(defaultLayout), getMessages, selectRep
    , provideJson, getMessageRender, newIdent, whamlet, addMessageI, redirect
    , MonadHandler (liftHandler), ToContent (toContent), ToWidget (toWidget)
    , handlerToWidget
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq, iopt)
import Yesod.Form.Fields
    ( urlField, intField, hiddenField, radioField, textField, optionsPairs
    , Option (optionInternalValue, optionExternalValue), OptionList (olOptions)
    )
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form.Types
    (FieldView(fvInput), FormResult (FormSuccess), Field (fieldView))
import Yesod.Persist.Core (YesodPersist(runDB))
import Control.Monad (when, unless, forM)


getApiEventsR :: Handler TypedContent
getApiEventsR = do

    query <- runInputGet $ iopt textField "q"

    events <- (second unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId

        case query of
          Just q -> where_ $ ( lower_ (x ^. EventName) `like` ((%) ++. lower_ (val q) ++. (%)) )
              ||. ( lower_ (x ^. EventDescr) `like` ((%) ++. lower_ (val (toHtml q)) ++. (%)) )
          Nothing -> limit 100

        return (x,attendees) )

    selectRep $ provideJson events


postAttendeeRegistrationR :: Handler Html
postAttendeeRegistrationR = do

    ((fr,_),_) <- runFormPost $ formAttendeeRegistration Nothing

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


getAttendeeRegistrationR :: Handler Html
getAttendeeRegistrationR = do

    cid <- toSqlKey <$> runInputGet (ireq intField "cid")

    card <- runDB $ selectOne $ do
        x :& u <- from $ table @Card
            `innerJoin` table @User `on` (\(x :& u) -> x ^. CardUser ==. u ^. UserId)
        where_ $ x ^. CardId ==. val cid
        return (x,u)

    (fw,et) <- generateFormPost $ formAttendeeRegistration (fst <$> card)

    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgRegistration
        idOverlay <- newIdent
        $(widgetFile "upcoming/attendees/registration/registration")


formAttendeeRegistration :: Maybe (Entity Card) -> Form (EventId, CardId)
formAttendeeRegistration card extra = do

    (cidR,cidV) <- mreq hiddenField "" (entityKey <$> card)

    now <- liftIO getCurrentTime

    events <- liftHandler $ runDB $ select $ do
        x <- from $ table @Event
        where_ $ x ^. EventTime >=. val now
        orderBy [asc (x ^. EventTime)]
        return x

    (eidR,eidV) <- mreq (md3radioFieldList events) "" Nothing

    let r = (,) <$> eidR <*> cidR
    let w = [whamlet|#{extra} ^{fvInput eidV} ^{fvInput cidV}|]
    return (r,w)

  where

      pairs (Entity eid (Event _ _ name _)) = (name, eid)

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
      $maybe Entity _ (Event _ time ename _) <- findEvent opt events
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


getScanQrR :: Handler Html
getScanQrR = do

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
        orderBy [asc (x ^. CardIssued)]
        return (x,u)

    (cardR,cardV) <- mreq (md3radioFieldList cards) "" Nothing

    let w = [whamlet|#{extra} ^{fvInput cardV}|]
    return (cardR,w)

  where

      pairs (Entity cid _,Entity _ (User email _ _ _ _)) = (email, cid)

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
      $maybe (Entity _ (Card _ _ issued),Entity uid (User email _ uname _ _)) <- findEvent opt cards
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

    userId <- maybeAuthId

    event <- (second unValue <$>) <$> runDB ( selectOne $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId

        where_ $ x ^. EventId ==. val eid
        return (x,attendees) )

    subscribed <- case userId of
      Nothing -> return False
      Just uid -> isJust <$> runDB ( selectOne $ do
          x :& c <- from $ table @Attendee
              `innerJoin` table @Card `on` (\(x :& c) -> x ^. AttendeeCard ==. c ^. CardId)
          where_ $ x ^. AttendeeEvent ==. val eid
          where_ $ c ^. CardUser ==. val uid
          return x )

    cards <- case userId of
      Nothing -> return []
      Just uid -> runDB $ select $ do
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
        when (isJust userId && subscribed)
            $ toWidget $(juliusFile "templates/upcoming/unregister.julius")
        $(widgetFile "upcoming/event")


formEventUserUnregister :: Form ()
formEventUserUnregister extra = return (pure (), [whamlet|^{extra}|])


formEventUserRegister :: Form ()
formEventUserRegister extra = return (pure (), [whamlet|^{extra}|])


getHomeR :: Handler Html
getHomeR = do

    now <- liftIO getCurrentTime
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay $ now

    upcoming <- (second unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId

        where_ $ x ^. EventTime >=. val now
        orderBy [asc (x ^. EventTime)]
        return (x, attendees) )

    events <- (second unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Event

        let attendees :: SqlExpr (Value Int)
            attendees = subSelectCount $ do
                a <- from $ table @Attendee
                where_ $ a ^. AttendeeEvent ==. x ^. EventId

        orderBy [asc (x ^. EventTime)]
        limit 100
        return (x, attendees) )

    userId <- maybeAuthId
    cards <- do
        cards <- case userId of
          Nothing -> return []
          Just uid -> runDB $ select $ do
              x :& u <- from $ table @Card
                 `innerJoin` table @User `on` (\(x :& u) -> x ^. CardUser ==. u ^. UserId)
              where_ $ x ^. CardUser  ==. val uid
              orderBy [asc (x ^. CardIssued)]
              return (x,u)

        forM cards $ \c@(Entity cid _,_) -> (c,) <$> runDB ( select $ do
            x <- from $ table @Info
            where_ $ x ^. InfoCard ==. val cid
            return x )


    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppName

        idOverlay <- newIdent
        idMain <- newIdent

        idDialogQrCode <- newIdent

        idButtonUpcomingEvents <- newIdent
        idDialogUpcomingEvents <- newIdent
        idButtonCloseDialogUpcomingEvents <- newIdent

        idButtonSearchEvents <- newIdent
        idDialogSearchEvents <- newIdent
        idButtonCloseDialogSearchEvents <- newIdent
        idListSearchSearchEventsResult <- newIdent
        idInputSearchEvents <- newIdent

        idButtonQrScanner <- newIdent

        $(widgetFile "homepage")


getEventPosterR :: EventId -> Handler TypedContent
getEventPosterR eid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @Poster
        where_ $ x ^. PosterEvent ==. val eid
        return x
    case photo of
      Just (Entity _ (Poster _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_event_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg


getFetchR :: Handler TypedContent
getFetchR = do
    url <- runInputGet $ ireq urlField "url"
    r <- liftIO $ get (unpack url)

    selectRep $ do
        provideJson (decode =<< (r L.^? WL.responseBody) :: Maybe A.Value)
