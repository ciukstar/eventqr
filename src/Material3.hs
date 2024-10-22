{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Material3
  ( md3widget
  , md3textareaWidget
  , md3selectWidget
  , daytimeLocalField
  ) where

import Data.Maybe (isJust)
import Data.Text (pack)
import Data.Time (LocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Text.Shakespeare.I18N (RenderMessage)

import Yesod.Core.Handler (HandlerFor)
import Yesod.Core.Widget (whamlet, WidgetFor)
import Yesod.Form.Fields
    ( FormMessage, datetimeLocalField
    )
import Yesod.Form.Types
    ( Field (fieldView)
    , FieldView (fvErrors, fvInput, fvLabel, fvRequired)
    )

    
md3textareaWidget :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3textareaWidget v = [whamlet|
  <div.field.border.round.label.textarea :isJust (fvErrors v):.invalid>
    ^{fvInput v}
    <label>
      #{fvLabel v}
      $if fvRequired v
        <sup>*
    $maybe err <- fvErrors v
      <span.error>#{err}
|]

    
md3selectWidget :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3selectWidget v = [whamlet|
  <div.field.label.suffix.border.round :isJust (fvErrors v):.invalid>
    ^{fvInput v}
    <label>
      #{fvLabel v}
      $if fvRequired v
        <sup>*
    <i>arrow_drop_down
    $maybe err <- fvErrors v
      <span.error>#{err}
|]


md3widget :: RenderMessage m FormMessage => FieldView m -> WidgetFor m ()
md3widget v = [whamlet|
  <div.field.label.border.round :isJust (fvErrors v):.invalid>

    ^{fvInput v}
    <label>
      #{fvLabel v}
      $if fvRequired v
        <sup>*

    $maybe err <- fvErrors v
      <span.error>#{err}
|]


daytimeLocalField :: RenderMessage m FormMessage => Field (HandlerFor m) LocalTime
daytimeLocalField = datetimeLocalField { fieldView = \theId name attrs ex req -> [whamlet|
<input ##{theId} type=datetime-local name=#{name} :req:required value=#{either id showVal ex} *{attrs}>
|] }
  where
      showVal = pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
