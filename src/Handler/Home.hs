{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

module Handler.Home
  ( getHomeR, getFetchR
  , getUpcomingEventR
  , getEventRegistrationR
  , postEventRegistrationR
  , getUpcomingEventAttendeesR
  , getUpcomingEventAttendeeR
  , getAttendeeRegistrationR
  , postAttendeeRegistrationR
  ) where

import qualified Control.Lens as L ( (^?) )
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (decode)
import qualified Data.Aeson as A (Value)
import qualified Data.List as L (find)
import Data.Text (unpack)

import Database.Esqueleto.Experimental
    ( select, selectOne, from, table, where_, val, orderBy, asc
    , (^.), (>=.), (==.), (:&) ((:&))
    , innerJoin, on
    )
import Database.Persist (Entity (Entity), entityKey, insert_)
import Database.Persist.Sql (toSqlKey, fromSqlKey)

import Foundation
    ( Handler, Form, widgetSnackbar, widgetMainMenu, widgetTopbar
    , Route
      ( ScanQrR, ScannerR, CalendarR, HomeR, UpcomingEventR, EventRegistrationR
      , AttendeeRegistrationR, UpcomingEventAttendeesR, UpcomingEventAttendeeR
      , DataR
      )
    , DataR (UserPhotoR, CardQrImageR)
    , AppMessage
      ( MsgAppName, MsgEventsCalendar, MsgName, MsgTime
      , MsgUpcomingEvents, MsgEvents, MsgSearch, MsgEvent
      , MsgScanQrCode, MsgScanQrCodeAndLinkToEvent, MsgDescription
      , MsgScan, MsgWelcomeTo, MsgRegistration, MsgRegister, MsgPhoto
      , MsgConfirmUserRegistrationForEventPlease, MsgCancel, MsgScanAgain
      , MsgDetails, MsgAttendees, MsgUserSuccessfullyegisteredForEvent
      , MsgInvalidFormData, MsgClose, MsgQrCode, MsgNoUpcomingEventsYet
      , MsgSelectAnEventToRegisterPlease, MsgSearchEvents, MsgRegistrationDate, MsgCardholder, MsgCardNumber, MsgCard
      )
    )

import Model
    ( msgSuccess, msgError
    , EventId, Event (Event)
    , CardId, Card
    , User (User)
    , Attendee (Attendee, attendeeEvent, attendeeCard, attendeeRegDate)
    , EntityField
      ( EventTime, EventId, CardId, CardUser, UserId, AttendeeCard, AttendeeEvent, AttendeeId, InfoCard, InfoId
      ), AttendeeId, Info (Info)
    )

import Network.Wreq (get)
import qualified Network.Wreq as WL (responseBody)

import Settings (widgetFile)
import Settings.StaticFiles
    (
    )

import Text.Hamlet (Html)

import Yesod.Core
    ( TypedContent, Yesod(defaultLayout), getMessages, selectRep, provideJson
    , getMessageRender, newIdent, whamlet, addMessageI, redirect, handlerToWidget
    , MonadHandler (liftHandler)
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Fields
    ( urlField, intField, hiddenField, radioField, optionsPairs
    , Option (optionInternalValue, optionExternalValue), OptionList (olOptions)
    )
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.Month (pattern YearMonth)
import Yesod.Persist.Core (YesodPersist(runDB))
import Yesod.Form.Functions (generateFormPost, mreq, runFormPost)
import Yesod.Form.Types
    (FieldView(fvInput), FormResult (FormSuccess), Field (fieldView))


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
          addMessageI msgSuccess MsgUserSuccessfullyegisteredForEvent
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

      pairs (Entity eid (Event _ name _)) = (name, eid)

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
      $maybe Entity _ (Event time ename _) <- findEvent opt events
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
          addMessageI msgSuccess MsgUserSuccessfullyegisteredForEvent
          redirect HomeR
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


getUpcomingEventAttendeeR :: EventId -> AttendeeId -> Handler Html
getUpcomingEventAttendeeR eid aid = do

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


getUpcomingEventAttendeesR :: EventId -> Handler Html
getUpcomingEventAttendeesR eid = do

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



getUpcomingEventR :: EventId -> Handler Html
getUpcomingEventR eid = do

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

        $(widgetFile "upcoming/event")


getHomeR :: Handler Html
getHomeR = do

    now <- liftIO getCurrentTime
    let month = (\(y,m,_) -> YearMonth y m) . toGregorian . utctDay $ now

    events <- runDB $ select $ do
        x <- from $ table @Event
        where_ $ x ^. EventTime >=. val now
        orderBy [asc (x ^. EventTime)]
        return x

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppName

        idOverlay <- newIdent
        idMain <- newIdent
        idButtonUpcomingEvents <- newIdent
        idDialogUpcomingEvents <- newIdent
        idButtonCloseDialogUpcomingEvents <- newIdent

        idButtonSearchEvents <- newIdent
        idDialogSearchEvents <- newIdent
        idButtonCloseDialogSearchEvents <- newIdent
        idListSearchSearchEventsResult <- newIdent
        idInputSearchEvents <- newIdent

        idButtonQrScanner <- newIdent
        idDialogMainMenu <- newIdent

        $(widgetFile "homepage")


getFetchR :: Handler TypedContent
getFetchR = do
    url <- runInputGet $ ireq urlField "url"
    r <- liftIO $ get (unpack url)

    selectRep $ do
        provideJson (decode =<< (r L.^? WL.responseBody) :: Maybe A.Value)
