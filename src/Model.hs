{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE QuasiQuotes                #-}

module Model where

import ClassyPrelude.Yesod
    ( Typeable, Text, mkMigrate, mkPersist, persistFileWith
    , share, sqlSettings, String
    )

import Control.Applicative (pure) 

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Data.Aeson.Types (Parser)
import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Eq (Eq)
import Data.Fixed (Fixed (MkFixed))
import Data.Function ((.))
import Data.Maybe (Maybe (Just))
import Data.Ord (Ord)
import Data.Text (pack, unpack) 
import Data.Time.Calendar.Month (Month)
import Data.Time.Clock
    ( UTCTime, NominalDiffTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)

import Database.Esqueleto.Experimental (SqlString)
import Database.Persist.Quasi ( lowerCaseSettings )
import Database.Persist.TH (derivePersistField)

import GHC.Float (Double, int2Double, truncateDouble)
import GHC.Integer (Integer)
import GHC.Num ((*))
import GHC.Real ((/), (^))

import Prelude (truncate, undefined)

import Text.Hamlet (Html)
import Text.Printf (printf)
import Text.Shakespeare.Text (st)
import Text.Show (Show, show)
import Text.Read (Read, readMaybe)

import Yesod.Auth.HashDB (HashDBUser (userPasswordHash, setPasswordHash))
import Yesod.Core.Dispatch (PathPiece, toPathPiece, fromPathPiece)
import Yesod.Form.Fields (Textarea)

import qualified Data.Aeson as A (Value (String, Bool))

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html ( toHtml ) 
import Data.Text.Lazy (toStrict)



data NotificationStatus = NotificationStatusUnread | NotificationStatusRead
    deriving (Show, Read, Eq, Ord)
derivePersistField "NotificationStatus"


data StoreType = StoreTypeDatabase | StoreTypeSession | StoreTypeGoogleSecretManager
    deriving (Show, Read, Eq, Ord)
derivePersistField "StoreType"


instance PathPiece Month where
    toPathPiece :: Month -> Text
    toPathPiece = pack . show

    fromPathPiece :: Text -> Maybe Month
    fromPathPiece = readMaybe . unpack


instance ToJSON Html where
    toJSON :: Html -> A.Value
    toJSON = A.String . toStrict . renderHtml

instance FromJSON Html where
    parseJSON :: A.Value -> Parser Html
    parseJSON (A.String txt) = pure (toHtml txt)
    parseJSON (A.Bool b) = pure (toHtml b)
    parseJSON _ = undefined
    


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")



instance HashDBUser User where
    userPasswordHash :: User -> Maybe Text
    userPasswordHash = userPassword

    setPasswordHash :: Text -> User -> User
    setPasswordHash h u = u { userPassword = Just h }


instance SqlString Textarea



keyApiVapid :: Text
keyApiVapid = "VAPID"

secretRefreshTokenVapid :: Text
secretRefreshTokenVapid = "vapid_refresh_token"

secretVolumeRefreshTokenVapid :: Text
secretVolumeRefreshTokenVapid = [st|/vrt/#{secretRefreshTokenVapid}|]

secretVapid :: Text
secretVapid = "vapid_min_details"

secretVolumeVapid :: Text
secretVolumeVapid = [st|/vmd/#{secretVapid}|]


keyApiGmail :: Text
keyApiGmail = "GMAIL_API"

gmailSendEnpoint :: String -> String
gmailSendEnpoint = printf "https://gmail.googleapis.com/gmail/v1/users/%s/messages/send"

keyAccessTokenGmail :: Text
keyAccessTokenGmail = "gmail_access_token"

keyAccessTokenGmailExpiresIn :: Text
keyAccessTokenGmailExpiresIn = "gmail_access_token_expires_in"

secretRefreshTokenGmail :: Text
secretRefreshTokenGmail = "gmail_refresh_token"

secretVolumeRefreshTokenGmail :: Text
secretVolumeRefreshTokenGmail = [st|/grt/#{secretRefreshTokenGmail}|]


keySendby :: Text
keySendby = "mail_sendby"


mediae :: [(Text,Text)]
mediae = [("s","small"),("m","medium"),("l","large")] :: [(Text,Text)]

langs :: [(Text,Text)]
langs = [("ru","RU"),("en","EN")]

msgSuccess :: Text
msgSuccess = "success"

msgError :: Text
msgError = "error"

keyThemeMode :: Text
keyThemeMode = "eventqr_theme_mode"

paramTaskStatus :: Text
paramTaskStatus = "status"

paramUserId :: Text
paramUserId = "uid"

paramLang :: Text
paramLang = "lang"

paramBacklink :: Text
paramBacklink = "backlink"

keyThemeLight :: Text
keyThemeLight = "light"

keyThemeDark :: Text
keyThemeDark = "dark"

paramTheme :: Text
paramTheme = "theme"

eventChangeTheme :: Text
eventChangeTheme = "changetheme"


nominalDiffTimeToHours :: NominalDiffTime -> Double
nominalDiffTimeToHours =
    (/ 3600.0) . int2Double . truncate . nominalDiffTimeToSeconds


hoursToNominalDiffTime :: Double -> NominalDiffTime
hoursToNominalDiffTime =
    secondsToNominalDiffTime . MkFixed . (* (^) @Integer @Integer 10 12) . truncateDouble @Integer . (* 3600)

