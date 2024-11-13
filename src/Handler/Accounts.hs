{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Accounts
  ( getAccountProfileR, getAccountSettingsR
  ) where

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val
    , (^.), (==.)
    )
import Database.Persist (Entity (Entity))

import Foundation
    ( Handler
    , Route (HomeR, DataR)
    , DataR (AccountSettingsR, AccountProfileR, UserPhotoR)
    , AppMessage
      ( MsgUserAccount, MsgYes, MsgNo, MsgSettings, MsgProfile, MsgName
      , MsgAdministrator, MsgManager, MsgPhoto, MsgTheme
      )
    )

import Model
    ( keyThemeMode
    , UserId, User (User)
    , EntityField (UserId)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout), setTitleI, newIdent)
import Yesod.Persist.Core (YesodPersist(runDB))

getAccountSettingsR :: UserId -> Handler Html
getAccountSettingsR uid = do
        
    defaultLayout $ do
        setTitleI MsgSettings
        idInputThemeMode <- newIdent
        $(widgetFile "accounts/settings")


getAccountProfileR :: UserId -> Handler Html
getAccountProfileR uid = do
    
    user <- runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
        
    defaultLayout $ do
        setTitleI MsgUserAccount 
        $(widgetFile "accounts/profile")
