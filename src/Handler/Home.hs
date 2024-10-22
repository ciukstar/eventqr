{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home
  ( getHomeR
  , getFetchR
  ) where

import qualified Control.Lens as L ( (^?) )
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (decode)
import qualified Data.Aeson as A (Value)
import Data.Text (unpack)

import Foundation
    ( App (appSettings), Handler, widgetSnackbar, widgetMainMenu, widgetTopbar
    , Route
      ( StaticR, FetchR
      )
    , AppMessage
      ( MsgClose, MsgCouldNotGetPosition, MsgAppName, MsgStyleStreets
      , MsgStyleOutdoors, MsgStyleLight, MsgStyleDark, MsgStyleSatellite
      , MsgStyleSatelliteStreets, MsgStyleNavigationDay, MsgStyleNavigationNight
      , MsgRestaurants, MsgShops, MsgNoLocationsWereFound, MsgExploreNearby
      , MsgSearchByNameOrAddress, MsgZoomIn, MsgZoomOut, MsgMyLocation, MsgParks
      , MsgMainMenu, MsgCompass, MsgMapStyleOptions, MsgRestaurantsShopsAndMore
      , MsgAttractions, MsgRadius, MsgInKilometers, MsgNearby, MsgFind
      , MsgPublicInstitutions
      , MsgGeolocationDisabled, MsgAddress, MsgLongitude, MsgLatitude
      , MsgAboutYourLocation, MsgGeolocationStatusDisabledExploreNearby
      , MsgGeolocationStatusUserMessage, MsgGeolocationAlternativesMessagePrefix
      , MsgGeolocationAlternativesMessageSuffix, MsgGeolocationAlternativesMessageAddress
      , MsgGeolocationAlternativesMessageOrThe, MsgGeolocationAlternativesMessageCoordinates
      , MsgGeolocationNotSupportedUserMessage, MsgGeolocationStatusTimeoutUserMessage
      , MsgGeolocationStatusDisabledUserMessage
      )
    )

import Model (keyThemeLight, keyThemeDark)

import Network.Wreq (get)
import qualified Network.Wreq as WL (responseBody)

import Settings (widgetFile)
import Settings.StaticFiles
    ( 
    )

import Text.Hamlet (Html)

import Yesod.Core
    ( TypedContent, Yesod(defaultLayout), getMessages
    , selectRep, provideJson, getMessageRender
    , newIdent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Form.Input (runInputGet, ireq)
import Yesod.Form.Fields (urlField)


getHomeR :: Handler Html
getHomeR = do

    msgr <- getMessageRender

    msgs <- getMessages

    defaultLayout $ do
        setTitleI MsgAppName

        idOverlay <- newIdent
        idMain <- newIdent
        idButtonSwitch <- newIdent

        idButtonSearchTrigger <- newIdent

        idButtonSearchByCategoryTrigger <- newIdent

        idButtonMainMenu <- newIdent
        idDialogMainMenu <- newIdent

        $(widgetFile "homepage")


getFetchR :: Handler TypedContent
getFetchR = do
    url <- runInputGet $ ireq urlField "url"
    r <- liftIO $ get (unpack url)

    selectRep $ do
        provideJson (decode =<< (r L.^? WL.responseBody) :: Maybe A.Value)
