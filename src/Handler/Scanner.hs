{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Scanner (getScanQrR, getScannerR) where

import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    )
import Database.Persist (Entity (Entity))

import Foundation
    ( Handler, widgetTopbar, widgetScanner
    , Route (HomeR, StaticR, EventRegistrationR, AttendeeRegistrationR)
    , AppMessage
      ( MsgScanner, MsgPointYourCameraToScan, MsgRegistrationForEvent
      )
    )
    
import Model (EventId, Event (Event), EntityField (EventId))

import Settings (widgetFile)
import Settings.StaticFiles
    (js_scanner_js)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, getMessageRender
    , newIdent, addScriptAttrs
    )
import Yesod.Persist.Core (YesodPersist(runDB))


getScanQrR :: Handler Html
getScanQrR = do
    
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgScanner
        idOverlay <- newIdent
        $(widgetFile "scanner/qr")


getScannerR :: EventId -> Handler Html
getScannerR eid = do

    event <- runDB $ selectOne $ do
        x <- from $ table @Event
        where_ $ x ^. EventId ==. val eid
        return x
    
    msgr <- getMessageRender
    defaultLayout $ do
        setTitleI MsgScanner
        idOverlay <- newIdent
        $(widgetFile "scanner/scanner")
        

