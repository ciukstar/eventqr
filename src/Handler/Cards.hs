{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wincomplete-patterns -Werror=incomplete-patterns #-}

module Handler.Cards
  ( getUserCardsR, postUserCardsR
  , getUserCardR, postUserCardR
  , getUserCardNewR
  , postUserCardsNewFieldR
  , getUserCardEditR
  , postUserCardNewFieldR
  , postUserCardDeleR
  , postUserCardInfoDeleR
  , postUserCardApproveR
  , postUserCardRevokeR
  , postUserCardRejectR
  , getCardQrImageR
  , getCardPhotoR
  , getCardsR
  , getCardR
  , postCardApproveR
  , postCardRevokeR
  , postCardRejectR
  ) where


import qualified Codec.QRCode as QR 
import Codec.QRCode.Data.QRCodeOptions (defaultQRCodeOptions)
import Codec.QRCode.Data.ErrorLevel as ErrorLevel (ErrorLevel (M))
import Codec.QRCode.JuicyPixels (toImage)
import Codec.Picture (encodeBitmap)
import Codec.QRCode (TextEncoding(Iso8859_1OrUtf8WithoutECI))

import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (Bifunctor(first, second))
import Data.Maybe (isJust)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)

import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, where_, val, update, set
    , (^.), (?.), (==.), (:&)((:&)), (=.)
    , orderBy, asc, innerJoin, on, delete, just, leftJoin
    )
    
import Database.Persist
    ( Entity (Entity), entityVal, insert, insertMany_, replace, upsert)
import qualified Database.Persist as P (delete, (=.))
import Database.Persist.Sql (fromSqlKey)

import Foundation
    ( Handler, Form, App, widgetTopbar, widgetSnackbar
    , Route (DataR, StaticR, HomeR)
    , DataR
      ( UserPhotoR, UsersR, UserR, UserCardsR, UserCardR, CardQrImageR
      , UserCardNewR, UserCardsNewFieldR, UserCardEditR, UserCardNewFieldR
      , UserCardDeleR, CardsR, CardR, CardApproveR, CardRevokeR, CardRejectR
      , UserCardRejectR, UserCardRevokeR, UserCardApproveR, CardPhotoR
      , UserCardInfoDeleR
      )
    , AppMessage
      ( MsgPhoto, MsgUser, MsgName, MsgAwaiting, MsgTakePhoto
      , MsgDeleteAreYouSure, MsgDele, MsgConfirmPlease, MsgCancel
      , MsgSave, MsgCardDoesNotContainAdditionalInfo, MsgQrCode
      , MsgRecordAdded, MsgInvalidFormData, MsgRecordDeleted, MsgNewField
      , MsgDetails, MsgCards, MsgCard, MsgAdd, MsgClose, MsgCardNumber
      , MsgCardholder, MsgValue, MsgNewFieldNameRequired, MsgRecordEdited
      , MsgUserHasNoCardsYet, MsgRejected, MsgFullName
      , MsgAwaitingModeration, MsgCardStatusActive, MsgModeration
      , MsgNoCardsForModerationYet, MsgApproved, MsgAll, MsgApprove
      , MsgReject, MsgRevoked, MsgIssueDate, MsgRequestDate, MsgStatus
      , MsgRequestApproved, MsgRevoke, MsgCardRevoked, MsgDateRevoked
      , MsgDateRejected, MsgCardRejected, MsgModerator, MsgUploadPhoto
      )
    )

import Material3 (md3widget, md3textareaWidget)
    
import Model
    ( msgSuccess, msgError
    , UserId, User(User, userName)
    , CardId, Card (Card)
    , CardStatus
      ( CardStatusAwaiting, CardStatusApproved, CardStatusRejected
      , CardStatusRevoked
      )
    , Info (Info), Photo (Photo)
    , EntityField
      ( UserId, CardUpdated, CardUser, CardId, InfoId, InfoCard, PhotoCard
      , CardStatus, CardModerator, PhotoMime, PhotoPhoto, PhotoAttribution, InfoName
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
    , img_camera_24dp_0000F5_FILL0_wght400_GRAD0_opsz24_svg
    )

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (Html)
import Text.Read (readMaybe)

import Yesod.Auth (YesodAuth(maybeAuthId))
import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessageRender, getMessages
    , TypedContent (TypedContent), ToContent (toContent), redirect, whamlet
    , SomeMessage (SomeMessage), notFound
    , addMessageI, toHtml, fileSourceByteString, getPostParams
    , YesodRequest (reqGetParams), getRequest, FileInfo (fileContentType)
    , MonadHandler (liftHandler)
    )
import Yesod.Form.Input (runInputGet, iopt)
import Yesod.Form.Fields
    ( textField, htmlField, fileField
    )
import Yesod.Form.Functions (generateFormPost, mreq, mopt, runFormPost)
import Yesod.Form.Types
    ( FormResult (FormSuccess)
    , FieldSettings(FieldSettings, fsLabel, fsTooltip, fsId, fsName, fsAttrs)
    , FieldView (fvInput, fvId, fvErrors, fvRequired, fvLabel)
    )
import Yesod.Persist.Core (YesodPersist(runDB))
import Data.Text.Lazy (toStrict)


postCardRejectR :: UserId -> CardId -> Handler Html
postCardRejectR uid cid = do

    stati <- reqGetParams <$> getRequest

    card <- runDB $ selectOne $ do
        x <- from $ table @Card
        where_ $ x ^. CardId ==. val cid
        where_ $ x ^. CardStatus ==. val CardStatusAwaiting
        return x

    ((fr,_),_) <- runFormPost formCardReject
    case (card, fr) of
      (Just _,FormSuccess ()) -> do
          now <- liftIO getCurrentTime
          runDB $ update $ \x -> do
              set x [ CardStatus =. val CardStatusRejected
                    , CardUpdated =. just (val now)
                    , CardModerator =. just (val uid)
                    ]
              where_ $ x ^. CardId ==. val cid
          addMessageI msgSuccess MsgCardRejected
          redirect (DataR $ CardR uid cid,stati)
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect (DataR $ CardR uid cid,stati)


postCardRevokeR :: UserId -> CardId -> Handler Html
postCardRevokeR uid cid = do

    stati <- reqGetParams <$> getRequest

    card <- runDB $ selectOne $ do
        x <- from $ table @Card
        where_ $ x ^. CardId ==. val cid
        where_ $ x ^. CardStatus ==. val CardStatusApproved
        return x

    ((fr,_),_) <- runFormPost formCardRevoke
    case (card, fr) of
      (Just _,FormSuccess ()) -> do
          now <- liftIO getCurrentTime
          runDB $ update $ \x -> do
              set x [ CardStatus =. val CardStatusRevoked
                    , CardUpdated =. just (val now)
                    , CardModerator =. just (val uid)
                    ]
              where_ $ x ^. CardId ==. val cid
          addMessageI msgSuccess MsgCardRevoked
          redirect (DataR $ CardR uid cid,stati)
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect (DataR $ CardR uid cid,stati)


postCardApproveR :: UserId -> CardId -> Handler Html
postCardApproveR uid cid = do

    stati <- reqGetParams <$> getRequest

    card <- runDB $ selectOne $ do
        x <- from $ table @Card
        where_ $ x ^. CardId ==. val cid
        where_ $ x ^. CardStatus ==. val CardStatusAwaiting
        return x

    ((fr,_),_) <- runFormPost formCardApprove
    case (card, fr) of
      (Just _,FormSuccess ()) -> do
          now <- liftIO getCurrentTime
          runDB $ update $ \x -> do
              set x [ CardStatus =. val CardStatusApproved
                    , CardUpdated =. just (val now)
                    , CardModerator =. just (val uid)
                    ]
              where_ $ x ^. CardId ==. val cid
          addMessageI msgSuccess MsgRequestApproved
          redirect (DataR $ CardR uid cid,stati)
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect (DataR $ CardR uid cid,stati)


getCardR :: UserId -> CardId -> Handler Html
getCardR uid cid = do

    stati <- reqGetParams <$> getRequest

    card <- runDB $ selectOne $ do
        x :& u :& m <- from $ table @Card
            `innerJoin` table @User `on` (\(c :& u) -> c ^. CardUser ==. u ^. UserId)
            `leftJoin` table @User `on` (\(c :& _ :& m) -> c ^. CardModerator ==. m ?. UserId)
        where_ $ x ^. CardId ==. val cid
        return ((x,u),m)

    attrs <- runDB $ select $ do
        x <- from $ table @Info
        where_ $ x ^. InfoCard ==. val cid
        orderBy [asc (x ^. InfoId)]
        return x

    (fwApprove,etApprove) <- generateFormPost formCardApprove
    (fwRevoke,etRevoke) <- generateFormPost formCardRevoke
    (fwReject,etReject) <- generateFormPost formCardReject    
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCard
        idOverlay <- newIdent
        idButtonShowDialogQrCode <- newIdent
        idDialogQrCode <- newIdent
        idButtonCloseDialogQrCode <- newIdent
        $(widgetFile "data/moderation/card")


formCardReject :: Form ()
formCardReject extra = return (pure (), [whamlet|^{extra}|])


formCardRevoke :: Form ()
formCardRevoke extra = return (pure (), [whamlet|^{extra}|])


formCardApprove :: Form ()
formCardApprove extra = return (pure (), [whamlet|^{extra}|])


postUserCardRejectR :: UserId -> CardId -> Handler Html
postUserCardRejectR uid cid = do

    stati <- reqGetParams <$> getRequest
    mid <- maybeAuthId

    card <- runDB $ selectOne $ do
        x <- from $ table @Card
        where_ $ x ^. CardId ==. val cid
        where_ $ x ^. CardStatus ==. val CardStatusAwaiting
        return x

    ((fr,_),_) <- runFormPost formCardReject
    case (card, fr) of
      (Just _,FormSuccess ()) -> do
          now <- liftIO getCurrentTime
          runDB $ update $ \x -> do
              set x [ CardStatus =. val CardStatusRejected
                    , CardUpdated =. just (val now)
                    , CardModerator =. val mid
                    ]
              where_ $ x ^. CardId ==. val cid
          addMessageI msgSuccess MsgCardRejected
          redirect (DataR $ UserCardR uid cid,stati)
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect (DataR $ UserCardR uid cid,stati)


postUserCardRevokeR :: UserId -> CardId -> Handler Html
postUserCardRevokeR uid cid = do

    stati <- reqGetParams <$> getRequest
    mid <- maybeAuthId

    card <- runDB $ selectOne $ do
        x <- from $ table @Card
        where_ $ x ^. CardId ==. val cid
        where_ $ x ^. CardStatus ==. val CardStatusApproved
        return x

    ((fr,_),_) <- runFormPost formCardRevoke
    case (card, fr) of
      (Just _,FormSuccess ()) -> do
          now <- liftIO getCurrentTime
          runDB $ update $ \x -> do
              set x [ CardStatus =. val CardStatusRevoked
                    , CardUpdated =. just (val now)
                    , CardModerator =. val mid
                    ]
              where_ $ x ^. CardId ==. val cid
          addMessageI msgSuccess MsgCardRevoked
          redirect (DataR $ UserCardR uid cid,stati)
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect (DataR $ UserCardR uid cid,stati)


postUserCardApproveR :: UserId -> CardId -> Handler Html
postUserCardApproveR uid cid = do

    stati <- reqGetParams <$> getRequest

    mid <- maybeAuthId

    card <- runDB $ selectOne $ do
        x <- from $ table @Card
        where_ $ x ^. CardId ==. val cid
        where_ $ x ^. CardStatus ==. val CardStatusAwaiting
        return x

    ((fr,_),_) <- runFormPost formCardApprove
    case (card, fr) of
      (Just _,FormSuccess ()) -> do
          now <- liftIO getCurrentTime
          runDB $ update $ \x -> do
              set x [ CardStatus =. val CardStatusApproved
                    , CardUpdated =. just (val now)
                    , CardModerator =. val mid
                    ]
              where_ $ x ^. CardId ==. val cid
          addMessageI msgSuccess MsgRequestApproved
          redirect (DataR $ UserCardR uid cid,stati)
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect (DataR $ UserCardR uid cid,stati)


getCardsR :: UserId -> Handler Html
getCardsR uid = do

    status <- ((readMaybe . unpack) =<<) <$> runInputGet ( iopt textField "status" )

    cards <- runDB $ select $ do
        x :& u <- from $ table @Card
            `innerJoin` table @User `on` (\(c :& u) -> c ^. CardUser ==. u ^. UserId)
            
        case status of
          Just s -> where_ $ x ^. CardStatus ==. val s
          Nothing -> return ()
          
        orderBy [asc (x ^. CardUpdated)]
        return (x,u)
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgModeration
        idOverlay <- newIdent
        idFormFilter <- newIdent
        $(widgetFile "data/moderation/cards")


postUserCardInfoDeleR :: UserId -> CardId -> Text -> Handler Html
postUserCardInfoDeleR uid cid field = do
    ((fr,_),_) <- runFormPost formCardInfoDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete $ do
              x <- from $ table @Info
              where_ $ x ^. InfoCard ==. val cid
              where_ $ x ^. InfoName ==. val field
              
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ UserCardEditR uid cid
          
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ UserCardEditR uid cid


postUserCardDeleR :: UserId -> CardId -> Handler Html
postUserCardDeleR  uid cid = do
    ((fr,_),_) <- runFormPost formCardDelete
    case fr of
      FormSuccess () -> do
          runDB $ P.delete cid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ UserCardsR uid
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ UserCardR uid cid


postUserCardNewFieldR :: UserId -> CardId -> Handler Html
postUserCardNewFieldR uid cid = do

    fields <- (second toHtml <$>) . filter paramsOut <$> getPostParams

    card <- runDB $ selectOne $ do
        x <- from $ table @Card
        where_ $ x ^. CardId ==. val cid
        return x
        
    idFormInfoDelete <- newIdent
    (fw00,et00) <- liftHandler $ generateFormPost formCardInfoDelete

    let route = DataR $ UserCardNewFieldR uid cid
    ((fr,fw),et) <- runFormPost $ formCard idFormInfoDelete route uid card fields
    
    case fr of
      FormSuccess ((_,_, attrs),(Just name,Just value)) -> do
          (fw,et) <- generateFormPost $ formCard idFormInfoDelete route uid Nothing (attrs <> [(name,value)])

          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/edit")
              
      FormSuccess ((_,_, attrs),(Just name,Nothing)) -> do
          (fw,et) <- generateFormPost $ formCard idFormInfoDelete route uid Nothing (attrs <> [(name,"")])

          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/edit")
              
      FormSuccess ((_,_, attrs),(Nothing,_)) -> do
          (fw,et) <- generateFormPost $ formCard idFormInfoDelete route uid Nothing attrs

          msgr <- getMessageRender
          addMessageI msgError MsgNewFieldNameRequired
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/edit")

      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/edit")


postUserCardR :: UserId -> CardId -> Handler Html
postUserCardR uid cid = do

    fields <- (second toHtml <$>) . filter paramsOut <$> getPostParams

    card <- runDB $ selectOne $ do
        x <- from $ table @Card
        where_ $ x ^. CardId ==. val cid
        return x

    idFormInfoDelete <- newIdent
    ((fr,fw),et) <- runFormPost $ formCard idFormInfoDelete (DataR $ UserCardNewFieldR uid cid) uid card fields

    case fr of
      FormSuccess ((c,Just fi,attrs),_) -> do
          runDB $ replace cid c
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (Photo cid (fileContentType fi) bs Nothing)
                    [ PhotoMime P.=. fileContentType fi
                    , PhotoPhoto P.=. bs
                    , PhotoAttribution P.=. Nothing
                    ]
          runDB $ delete $ do
              x <- from $ table @Info
              where_ $ x ^. InfoCard ==. val cid
          runDB $ insertMany_ $ uncurry (Info cid) <$> attrs
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ UserCardR uid cid
          
      FormSuccess ((c,Nothing,attrs),_) -> do
          runDB $ replace cid c
          runDB $ delete $ do
              x <- from $ table @Info
              where_ $ x ^. InfoCard ==. val cid
          runDB $ insertMany_ $ uncurry (Info cid) <$> attrs
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ UserCardR uid cid
          
      _otherwise -> do
          idFormInfoDelete <- newIdent
          (fw00,et00) <- liftHandler $ generateFormPost formCardInfoDelete
          msgr <- getMessageRender
          addMessageI msgError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/edit")


getUserCardEditR :: UserId -> CardId -> Handler Html
getUserCardEditR uid cid = do

    card <- runDB $ selectOne $ do
        x <- from $ table @Card
        where_ $ x ^. CardId ==. val cid
        return x

    attrs <- ((\(Entity _ (Info _ name value)) -> (name, value)) <$>) <$> runDB (select $ do
        x <- from $ table @Info
        where_ $ x ^. InfoCard ==. val cid
        orderBy [asc (x ^. InfoId)]
        return x )
        
    idFormInfoDelete <- newIdent
    (fw00,et00) <- liftHandler $ generateFormPost formCardInfoDelete
        
    (fw,et) <- generateFormPost $ formCard idFormInfoDelete (DataR $ UserCardNewFieldR uid cid) uid card attrs

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCard 
        idOverlay <- newIdent
        $(widgetFile "data/users/cards/edit")


postUserCardsNewFieldR :: UserId -> Handler Html
postUserCardsNewFieldR uid = do

    fields <- (second toHtml <$>) . filter paramsOut <$> getPostParams

    idFormInfoDelete <- newIdent
    (fw00,et00) <- liftHandler $ generateFormPost formCardInfoDelete
    
    let route = DataR $ UserCardsNewFieldR uid
    ((fr,fw),et) <- runFormPost $ formCard idFormInfoDelete route uid Nothing fields
    
    case fr of
      FormSuccess ((_,_, attrs),(Just name,Just value)) -> do
          (fw,et) <- generateFormPost $ formCard idFormInfoDelete route uid Nothing (attrs <> [(name,value)])

          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/new")
              
      FormSuccess ((_,_, attrs),(Just name,Nothing)) -> do
          (fw,et) <- generateFormPost $ formCard idFormInfoDelete route uid Nothing (attrs <> [(name,"")])

          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/new")
              
      FormSuccess ((_,_, attrs),(Nothing,_)) -> do
          (fw,et) <- generateFormPost $ formCard idFormInfoDelete route uid Nothing attrs

          msgr <- getMessageRender
          addMessageI msgError MsgNewFieldNameRequired
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/new")

      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/new")


postUserCardsR :: UserId -> Handler Html
postUserCardsR uid = do

    fields <- (second toHtml <$>) . filter paramsOut <$> getPostParams

    idFormInfoDelete <- newIdent
    ((fr,fw),et) <- runFormPost $ formCard idFormInfoDelete (DataR $ UserCardsNewFieldR uid) uid Nothing fields

    case fr of
      FormSuccess ((c,Just fi,attrs),_) -> do
          cid <- runDB $ insert c
          bs <- fileSourceByteString fi
          void $ runDB $ upsert (Photo cid (fileContentType fi) bs Nothing)
                    [ PhotoMime P.=. fileContentType fi
                    , PhotoPhoto P.=. bs
                    , PhotoAttribution P.=. Nothing
                    ]
          runDB $ delete $ do
              x <- from $ table @Info
              where_ $ x ^. InfoCard ==. val cid
          runDB $ insertMany_ $ uncurry (Info cid) <$> attrs
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ UserCardsR uid
          
      FormSuccess ((c,Nothing,attrs),_) -> do
          cid <- runDB $ insert c
          runDB $ delete $ do
              x <- from $ table @Info
              where_ $ x ^. InfoCard ==. val cid
          runDB $ insertMany_ $ uncurry (Info cid) <$> attrs
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ UserCardsR uid
          
      _otherwise -> do
          idFormInfoDelete <- newIdent
          (fw00,et00) <- liftHandler $ generateFormPost formCardInfoDelete
          
          msgr <- getMessageRender
          addMessageI msgError MsgInvalidFormData
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgCard 
              idOverlay <- newIdent
              $(widgetFile "data/users/cards/new")


getUserCardNewR :: UserId -> Handler Html
getUserCardNewR uid = do
    idFormInfoDelete <- newIdent
    (fw00,et00) <- liftHandler $ generateFormPost formCardInfoDelete
    
    (fw,et) <- generateFormPost $ formCard idFormInfoDelete (DataR $ UserCardsNewFieldR uid) uid Nothing []

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCard 
        idOverlay <- newIdent
        $(widgetFile "data/users/cards/new")


formCard :: Text -> Route App -> UserId -> Maybe (Entity Card) -> [(Text, Html)]
         -> Form ((Card,Maybe FileInfo,[(Text,Html)]),(Maybe Text,Maybe Html))
formCard idFormInfoDelete route uid card fields extra = do

    user <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @User
        where_ $ x ^. UserId ==. val uid
        return x
        
    now <- liftIO getCurrentTime

    attrs <- forM fields $ \(name, value) -> mreq htmlField FieldSettings
        { fsLabel = SomeMessage name
        , fsName = Just name, fsId = Nothing, fsTooltip = Nothing
        , fsAttrs = [("autocomplete","off")]
        } (Just value) >>= \x -> return (first ((name,) <$>) x)

    samples <- if null fields then do
        msgr <- getMessageRender
        name <- mreq textField FieldSettings
            { fsLabel = SomeMessage MsgFullName
            , fsName = Just (msgr MsgFullName), fsTooltip = Nothing, fsId = Nothing
            , fsAttrs = [("autocomplete","off")]
            } (userName . entityVal =<< user) >>= \x -> return ( first ((\y -> (msgr MsgFullName, toHtml y)) <$>) x )
        return [name] 
              
        else return []
    
    (fieldR,fieldV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsName = Just paramFieldNewName
        , fsId = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing
        
    (valR,valV) <- mopt htmlField FieldSettings
        { fsLabel = SomeMessage MsgValue
        , fsName = Just paramFieldNewValue
        , fsId = Nothing, fsTooltip = Nothing, fsAttrs = []
        } Nothing

    (photoR,photoV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none"),("accept","image/*")]
        } Nothing

    let qr = ""
    
    let r = (,) <$> ( (,,) ( maybe (Card uid qr now CardStatusAwaiting Nothing Nothing) entityVal card )
                      <$> photoR <*> traverse fst attrs
                    )
                <*> ( (,) <$> fieldR <*> valR )

    idDialogDelete <- newIdent
    idDetailsNewField <- newIdent
    idLabelPhoto <- newIdent
    idImgPhoto <- newIdent
    idButtonUploadPhoto <- newIdent
    idButtonTakePhoto <- newIdent
    idOverlay <- newIdent
    idDialogSnapshot <- newIdent
    idButtonCloseDialogSnapshot <- newIdent
    idVideo <- newIdent
    idButtonCapture <- newIdent 
    
    return (r, $(widgetFile "data/users/cards/form"))


formCardInfoDelete :: Form ()
formCardInfoDelete extra = return (pure (),[whamlet|^{extra}|])


paramFieldNewName :: Text
paramFieldNewName = "newName"

paramFieldNewValue :: Text
paramFieldNewValue = "newValue"

paramFieldToken :: Text
paramFieldToken = "_token"

paramsOut :: (Text, Text) -> Bool
paramsOut (p,_) = (p /= paramFieldToken) && (p /= paramFieldNewName) && (p /= paramFieldNewValue)


getUserCardR :: UserId -> CardId -> Handler Html
getUserCardR uid cid = do

    card <- runDB $ selectOne $ do
        x :& u :& m <- from $ table @Card
            `innerJoin` table @User `on` (\(c :& u) -> c ^. CardUser ==. u ^. UserId)
            `leftJoin` table @User `on` (\(c :& _ :& m) -> c ^. CardModerator ==. m ?. UserId)
        where_ $ x ^. CardId ==. val cid
        return ((x, u), m)

    attrs <- runDB $ select $ do
        x <- from $ table @Info
        where_ $ x ^. InfoCard ==. val cid
        orderBy [asc (x ^. InfoId)]
        return x

    (fwApprove,etApprove) <- generateFormPost formCardApprove
    (fwRevoke,etRevoke) <- generateFormPost formCardRevoke
    (fwReject,etReject) <- generateFormPost formCardReject    

    (fw0,et0) <- generateFormPost formCardDelete
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCard
        idOverlay <- newIdent
        idButtonShowDialogQrCode <- newIdent
        idDialogQrCode <- newIdent
        idButtonCloseDialogQrCode <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/users/cards/card")


formCardDelete :: Form ()
formCardDelete extra = return (pure (),[whamlet|^{extra}|])


getUserCardsR :: UserId -> Handler Html
getUserCardsR uid = do

    cards <- runDB $ select $ do
        x :& u <- from $ table @Card
            `innerJoin` table @User `on` (\(c :& u) -> c ^. CardUser ==. u ^. UserId)
        where_ $ x ^. CardUser ==. val uid
        orderBy [asc (x ^. CardUpdated)]
        return (x,u)
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUser
        idOverlay <- newIdent
        $(widgetFile "data/users/cards/cards")


getCardQrImageR :: CardId -> Handler TypedContent
getCardQrImageR cid = do
    
    let input = show (fromSqlKey cid)

    case QR.encode (defaultQRCodeOptions ErrorLevel.M) Iso8859_1OrUtf8WithoutECI input of
      Just qrcode -> return $ TypedContent "image/bmp" $ toContent $ encodeBitmap $ toImage 4 7 qrcode
      Nothing -> notFound


getCardPhotoR :: CardId -> Handler TypedContent
getCardPhotoR cid = do
    photo <- runDB $ selectOne $ do
        x <- from $ table @Photo
        where_ $ x ^. PhotoCard ==. val cid
        return x
    case photo of
      Just (Entity _ (Photo _ mime bs _)) -> return $ TypedContent (encodeUtf8 mime) $ toContent bs
      Nothing -> redirect $ StaticR img_account_circle_24dp_013048_FILL0_wght400_GRAD0_opsz24_svg
