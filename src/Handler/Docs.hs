{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Docs
  ( getDocsR
  ) where

import Foundation
    ( Handler, widgetSnackbar, widgetTopbar
    , Route (HomeR)
    , AppMessage
      ( MsgAppName, MsgDocumentation, MsgIssueTracking, MsgSourceCode
      , MsgDoc001, MsgDoc002, MsgDoc003, MsgDoc005
      )
    )
    
import Settings (widgetFile)

import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (Html)

import Yesod
    ( getMessageRender, getUrlRender
    )
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages
    )
import Yesod.Core.Widget (setTitleI)


getDocsR :: Handler Html
getDocsR = do
    r <- getUrlRender
    m <- getMessageRender
    msgs <- getMessages
    let t = preEscapedToHtml . m
    defaultLayout $ do
        setTitleI MsgDocumentation
        idOverlay <- newIdent
        $(widgetFile "docs/docs")
