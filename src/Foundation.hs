{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation where

import Control.Monad.Logger (LogSource)
import Control.Lens ((^?), (?~), to, folded, filtered, _2)
import qualified Control.Lens as L ((^.))

import Import.NoFoundation

import Data.Aeson.Lens (key, AsValue(_String))
import qualified Data.ByteString.Base64.Lazy as B64L (encode)
import qualified Data.CaseInsensitive as CI
import Data.Function ((&))
import Data.Kind (Type)
import qualified Data.List.Safe as LS (head)
import qualified Data.Text as T (intercalate)
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar.Month (Month)
import qualified Data.Text.Lazy.Encoding as TLE (encodeUtf8)

import Database.Esqueleto.Experimental
    ( SqlExpr, selectOne, from, table, where_, val, select, unionAll_, not_
    , (^.)
    , countRows, unValue, asc, orderBy, valList, in_
    )
import qualified Database.Esqueleto.Experimental as E ((==.), Value)
import Database.Persist.Sql (ConnectionPool, runSqlPool)


import Network.Mail.Mime
    ( Part(Part, partType, partEncoding, partDisposition, partContent, partHeaders)
    , Mail
      ( mailTo, mailHeaders, mailParts), emptyMail
    , Address (Address), Encoding (None), Disposition (DefaultDisposition)
    , PartContent (PartContent), renderMail'
    )
import Network.Wreq (post, postWith, FormParam ((:=)), defaults, auth, oauth2Bearer)
import qualified Network.Wreq as W (get, responseHeader, responseBody)
import qualified Network.Wreq.Lens as WL (responseBody, responseStatus, statusCode)

import System.Directory (doesFileExist)
import System.IO (readFile')

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Email.Validate (emailAddress, localPart)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Text.Julius (juliusFile)
import Text.Shakespeare.Text (stext)

import qualified Yesod.Auth.Email as AE
import Yesod.Auth.Email
    ( authEmail, registerR, loginR, forgotPasswordR, setpassR
    , emailCredsId, emailCredsAuthId, emailCredsStatus, emailCredsVerkey, emailCredsEmail
    , EmailCreds (EmailCreds)
    , Email, Identifier, EmailCreds, SaltedPass, VerKey, VerUrl
    , YesodAuthEmail
      ( AuthEmailId, getEmail, getEmailCreds, setPassword, getPassword
      , verifyAccount, needOldPassword, setVerifyKey, getVerifyKey, sendVerifyEmail
      )
    )
-- import Yesod.Auth.HashDB (authHashDBWithForm)
import Yesod.Auth.OAuth2.Google (oauth2GoogleScopedWidget)
import Yesod.Auth.Message
    ( defaultMessage, englishMessage, russianMessage
    , AuthMessage
      ( InvalidLogin, LoginTitle, RegisterLong, ConfirmationEmailSentTitle, SetPassTitle
      , NewPass, CurrentPassword, ConfirmPass, PasswordResetTitle, Register, EnterEmail
      , SetPass, PasswordResetPrompt, SendPasswordResetEmail
      ) )
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.I18n.English (englishFormMessage)
import Yesod.Form.I18n.Russian (russianFormMessage)
import Material3 (md3widget)
import qualified Data.ByteString.Lazy as BSL


-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }


mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a


widgetScanner :: Route App -> Widget
widgetScanner callback = do
    msgr <- getMessageRender
    idFigureScanner <- newIdent
    idFigcaptionLabelSanner <- newIdent
    let idScannerVideo = "scannervideo" :: Text    
    addScriptAttrs (StaticR js_scanner_js) [("type","module")]
    $(widgetFile "widgets/scanner")


widgetTopbar :: Maybe (Route App,[(Text,Text)]) -- ^ Back button
             -> Text                            -- ^ Title 
             -> Text                            -- ^ Overlay id
             -> Maybe Text                      -- ^ Id of delete dialog
             -> Maybe (Route App)               -- ^ Edit button
             -> Widget
widgetTopbar backlink title idOverlay idDialogDelete editRoute = do
    stati <- reqGetParams <$> getRequest
    rndr <- getUrlRenderParams
    idDialogMainMenu <- newIdent
    $(widgetFile "widgets/topbar")


widgetAccount :: Widget
widgetAccount = do
    user <- maybeAuth
    unread <- case user of
      Nothing -> return 0
      Just (Entity uid _) -> handlerToWidget $ maybe 0 unValue <$> runDB ( selectOne $ do
        x <- from $ table @Notification
        where_ $ x ^. NotificationRecipient E.==. val uid
        where_ $ x ^. NotificationStatus E.==. val NotificationStatusUnread
        return (countRows :: SqlExpr (E.Value Int)))
        
    $(widgetFile "widgets/account")


widgetSnackbar :: [(Text,Html)] -> Widget
widgetSnackbar msgs = $(widgetFile "widgets/snackbar")


widgetMainMenuTrigger :: Text -> Text -> Widget
widgetMainMenuTrigger idOverlay idDialogMainMenu = $(widgetFile "widgets/trigger")
    

widgetMainMenu :: Text -> Text -> Widget
widgetMainMenu idOverlay idDialogMainMenu = do
    userId <- maybeAuthId
    curr <- getCurrentRoute
    idButtonMainMenuClose <- newIdent
    $(widgetFile "widgets/menu")



widgetTheme :: Widget
widgetTheme = $(widgetFile "widgets/theme")


widgetLang :: Route App -> Text -> Widget
widgetLang action backlink  = do
    
    language <- fromMaybe "en" . LS.head <$> languages
    
    idMenuLang <- newIdent
    idHiddenSelect <- newIdent
    idFormLang <- newIdent
    idInputBacklink <- newIdent
    
    $(widgetFile "widgets/lang")
  where
      resolveLang :: Lang -> AppMessage
      resolveLang "ru" = MsgLangRu
      resolveLang _ = MsgLangEn


postLangR :: Handler ()
postLangR = do
    lang <- runInputPost $ ireq textField paramLang
    back <- runInputPost $ ireq urlField paramBacklink
    setLanguage lang
    redirect back


-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        lang <- fromMaybe "en" . LS.head <$> languages

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            -- addStylesheet $ StaticR css_bootstrap_css
                                    -- ^ generated from @Settings/StaticFiles.hs@
            $(widgetFile "default-layout")
          
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute :: App -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized ServiceWorkerR _ = return Authorized

    isAuthorized HomeR _ = setUltDestCurrent >> return Authorized
    
    isAuthorized (EventR _) _ = return Authorized
    isAuthorized (EventPosterR _) _ = return Authorized
    isAuthorized (EventScannerR _) _ = return Authorized
    isAuthorized (EventAttendeesR _) _ = return Authorized
    isAuthorized (EventAttendeeR _ _) _ = return Authorized
    isAuthorized (EventRegistrationR _) _ = return Authorized
    isAuthorized (EventUserRegisterR _ uid) _ = isAuthenticatedSelf uid
    isAuthorized (EventUserCardRegisterR _ uid _) _ = isAuthenticatedSelf uid
    isAuthorized (EventUserUnregisterR _ uid) _ = isAuthenticatedSelf uid
    
    isAuthorized (ScanQrR uid) _ = isManagerSelfOrAdmin uid
    isAuthorized (AttendeeRegistrationR uid) _ = isManagerSelfOrAdmin uid
    
    isAuthorized ApiEventsR _ = return Authorized        
    
    isAuthorized (CalendarR _) _ = return Authorized
    isAuthorized (CalendarEventsR _ _) _ = return Authorized
    isAuthorized (CalendarEventR {}) _ = return Authorized
    isAuthorized (CalendarEventScannerR {}) _ = return Authorized
    isAuthorized (CalendarEventRegistrationR {}) _ = return Authorized
    isAuthorized (CalendarEventUserRegisterR _ _ _ uid) _ = isAuthenticatedSelf uid    
    isAuthorized (CalendarEventUserCardRegisterR _ _ _ uid _) _ = isAuthenticatedSelf uid
    isAuthorized (CalendarEventUserUnregisterR _ _ _ uid) _ = isAuthenticatedSelf uid
    
    isAuthorized (CalendarEventAttendeesR {}) _ = return Authorized
    isAuthorized (CalendarEventAttendeeR {}) _ = return Authorized
    
    isAuthorized DocsR _ = setUltDestCurrent >> return Authorized
    
    isAuthorized LangR _ = return Authorized
    
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized SitemapR _ = return Authorized
    isAuthorized WebAppManifestR _ = return Authorized
    
    isAuthorized (StaticR _) _ = return Authorized    
    
    isAuthorized r@(DataR TokensGmailR) _ = setUltDest r >> isAdmin
    isAuthorized (DataR TokensGmailClearR) _ = isAdmin
    isAuthorized (DataR TokensGmailHookR) _ = isAdmin
    
    isAuthorized (DataR TokensVapidR) _ = isAdmin
    isAuthorized (DataR TokensVapidClearR) _ = isAdmin
    isAuthorized (DataR TokensVapidHookR) _ = isAdmin

    isAuthorized (DataR (AccountProfileR uid)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountSettingsR uid)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountEventScheduleR uid)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountEventR uid _)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountEventUnregisterR uid _)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountEventAttendeesR uid _)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountEventScheduleCalendarR uid _)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountEventScheduleCalendarEventsR uid _ _)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountEventScheduleCalendarEventR uid _ _ _)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountEventScheduleCalendarUnregisterR uid _ _ _)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountEventScheduleCalendarEventAttendeesR uid _ _ _)) _ = isAuthenticatedSelf uid
                            
    isAuthorized (DataR (AccountCardNewR uid)) _ = isAuthenticatedSelf uid
        
    isAuthorized (DataR (UserDeleR _)) _ = isAdmin
    isAuthorized (DataR (UserEditR _)) _ = isAdmin
    isAuthorized (DataR UserNewR) _ = isAdmin
    isAuthorized (DataR (UserR _)) _ = isAdmin
    isAuthorized (DataR UsersR) _ = setUltDestCurrent >> isAdmin
    isAuthorized (DataR (UserPhotoR _)) _ = return Authorized
    
    isAuthorized (DataR (AccountNotificationsR uid)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountNotificationR uid _)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountNotificationDeleR uid _)) _ = isAuthenticatedSelf uid
        
    isAuthorized (DataR (AccountPushSettingsR uid)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (UserSubscriptionsR uid)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (UserUnsubscribeR uid _)) _ = isAuthenticatedSelf uid
    
    isAuthorized (DataR (UserCardsR _)) _ = isAdmin
    isAuthorized (DataR (UserCardR _ _)) _ = isAdmin
    isAuthorized (DataR (UserCardsNewFieldR _)) _ = isAdmin
    isAuthorized (DataR (UserCardNewR _)) _ = isAdmin
    isAuthorized (DataR (UserCardEditR _ _)) _ = isAdmin
    isAuthorized (DataR (UserCardNewFieldR _ _)) _ = isAdmin
    isAuthorized (DataR (UserCardDeleR _ _)) _ = isAdmin
        
    isAuthorized (DataR (CardQrImageR _)) _ = return Authorized

    isAuthorized (DataR (DataEventsR uid)) _ = setUltDestCurrent >> isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventNewR uid)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventR uid _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventEditR uid _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventDeleR uid _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventScannerR uid _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventRegistrationR uid _)) _ = isManagerSelfOrAdmin uid
    
    isAuthorized (DataR (DataEventPosterDeleR uid _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventPosterR uid _)) _ = isManagerSelfOrAdmin uid
        
    isAuthorized (DataR (DataEventAttendeesR uid _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventAttendeeR uid _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventAttendeeNewR uid _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventAttendeeDeleR uid _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventAttendeeNotifyR uid _ _)) _ = isManagerSelfOrAdmin uid

    isAuthorized (DataR (DataEventCalendarR uid _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventCalendarEventsR uid _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventCalendarEventR uid _ _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventCalendarEventNewR uid _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventCalendarEventEditR uid _ _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventCalendarEventDeleR uid _ _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventCalendarScannerR uid _ _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventCalendarRegistrationR uid _ _ _)) _ = isManagerSelfOrAdmin uid
    
    isAuthorized (DataR (DataEventCalendarEventPosterDeleR uid _ _ _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventCalendarEventPosterR uid _ _ _)) _ = isManagerSelfOrAdmin uid
        
    isAuthorized (DataR (DataEventCalendarEventAttendeesR uid _ _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventCalendarEventAttendeeR uid _ _ _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventCalendarEventAttendeeDeleR uid _ _ _ _)) _ = isManagerSelfOrAdmin uid
    isAuthorized (DataR (DataEventCalendarEventAttendeeNotifyR uid _ _ _ _)) _ = isManagerSelfOrAdmin uid
    
    
    

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

    errorHandler :: ErrorResponse -> HandlerFor App TypedContent
    errorHandler NotFound = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgPageNotFound
            idHeader <- newIdent
            idHeaderStart <- newIdent
            $(widgetFile "error/not-found")
        provideRep $ return $ object ["message" .= ("Page not found." :: Text)]
        provideRep $ return ("Page not found." :: Text)

    errorHandler (PermissionDenied msg) = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgPermissionDenied
            msgr <- getMessageRender
            msgs <- getMessages
            idOverlay <- newIdent
            $(widgetFile "error/permission-denied")
        provideRep $ do
            msgr <- getMessageRender
            return $ object ["message" .= (msgr MsgPermissionDenied <> "Permission Denied. " <> msg)]
        provideRep $ return $ "Permission Denied. " <> msg

    errorHandler (InvalidArgs msgs) = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgInvalidArguments
            $(widgetFile "error/invalid-args")
        provideRep $ return $ object ["message" .= msgs]
        provideRep $ return $ T.intercalate ", " msgs

    errorHandler x = defaultErrorHandler x


getServiceWorkerR :: Handler TypedContent
getServiceWorkerR = do
    rndr <- getUrlRenderParams
    msgr <- getMessageRender
    actionDismissNotifcation <- newIdent
    actionReadNotifcation <- newIdent
    return $ TypedContent typeJavascript $ toContent $ $(juliusFile "static/js/sw.julius") rndr


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authLayout :: (MonadHandler m, HandlerSite m ~ App) => WidgetFor App () -> m Html
    authLayout w = liftHandler $ do
        defaultLayout $ do
            setTitleI MsgSignIn
            $(widgetFile "auth/layout")

    loginHandler :: AuthHandler App Html
    loginHandler = do
        app <- getYesod
        tp <- getRouteToParent
        rndr <- getUrlRender
        backlink <- fromMaybe (rndr HomeR) <$> lookupSession keyUtlDest
        let indexes = [1..] 
        authLayout $ do
            setTitleI LoginTitle
            idButtonBack <- newIdent
            $(widgetFile "auth/login")

    authenticate :: (MonadHandler m, HandlerSite m ~ App) => Creds App -> m (AuthenticationResult App)
    authenticate (Creds plugin ident extra) =  liftHandler $ case plugin of
      "google" -> do
          let name :: Maybe Text
              name = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "name" . _String
              
          let picture :: Maybe Text
              picture = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "picture" . _String
              
          let email :: Maybe Text
              email = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "email" . _String

          case email of
              Just em -> do
                  Entity uid _ <- runDB $ upsert User { userEmail = em
                                                      , userPassword = Nothing
                                                      , userName = name
                                                      , userSuper = False
                                                      , userAdmin = False
                                                      , userManager = False
                                                      , userAuthType = UserAuthTypeGoogle
                                                      , userVerkey = Nothing
                                                      , userVerified = True
                                                      }
                                  [ UserName =. name
                                  , UserPassword =. Nothing
                                  , UserAuthType =. UserAuthTypeGoogle
                                  , UserVerkey =. Nothing
                                  , UserVerified =. True
                                  ]
                                  
                  _ <- return $ UserError InvalidLogin

                  case picture of
                    Just src -> do
                        r <- liftIO $ W.get (unpack src)
                        case (r ^? W.responseHeader "Content-Type" . to decodeUtf8, BSL.toStrict <$> r ^? W.responseBody) of
                            (Just mime, Just bs) -> void $ runDB $ upsert
                                UserPhoto { userPhotoUser = uid
                                          , userPhotoMime = mime
                                          , userPhotoPhoto = bs
                                          , userPhotoAttribution = Nothing
                                          }
                                [UserPhotoMime =. mime, UserPhotoPhoto =. bs]
                                
                            _otherwise -> return ()
                            
                    Nothing -> return ()
                  return $ Authenticated uid
                  
              _otherwise -> return $ UserError InvalidLogin

      _ -> do
          user <- runDB $ selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserEmail E.==. val ident
              return x
          case user of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> return $ UserError InvalidLogin

    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = [ oauth2GoogleScopedWidget $(widgetFile "auth/google") ["email","openid","profile"]
                        (googleApiConfClientId . appGoogleApiConf . appSettings $ app)
                        (googleApiConfClientSecret . appGoogleApiConf . appSettings $ app)
                      , authEmail
                      -- , authHashDBWithForm formLogin (Just . UniqueUser)
                      ]

    renderAuthMessage :: App -> [Text] -> AuthMessage -> Text
    renderAuthMessage _ [] = defaultMessage
    renderAuthMessage _ ("en":_) = englishMessage
    renderAuthMessage _ ("ru":_) = russianMessage
    renderAuthMessage app (_:xs) = renderAuthMessage app xs


instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    forgotPasswordHandler :: AuthHandler App Html
    forgotPasswordHandler = do
        (fw,et) <- liftHandler $ generateFormPost formForgotPassword
        parent <- getRouteToParent
        msgs <- getMessages
        authLayout $ do
            setTitleI PasswordResetTitle
            idFormForgotPassword <- newIdent
            $(widgetFile "auth/forgot") 
      where
          formForgotPassword :: Form Text
          formForgotPassword extra = do
              (r,v) <- mreq emailField FieldSettings
                  { fsLabel = SomeMessage MsgEmailAddress
                  , fsId = Just "forgotPassword", fsName = Just "email", fsTooltip = Nothing
                  , fsAttrs = [("autocomplete","email")]
                  } Nothing
              return (r,[whamlet|#{extra}^{md3widget v}|])

    setPasswordHandler :: Bool -> AuthHandler App TypedContent
    setPasswordHandler old = do
        parent <- getRouteToParent
        (fw,et) <- liftHandler $ generateFormPost formSetPassword    
        msgs <- getMessages
        selectRep $ provideRep $ authLayout $ do
            setTitleI SetPassTitle 
            $(widgetFile "auth/password")
      where
          formSetPassword :: Form (Text,Text,Text)
          formSetPassword extra = do
              (currR,currV) <- mreq passwordField FieldSettings
                  { fsLabel = SomeMessage CurrentPassword
                  , fsTooltip = Nothing
                  , fsId = Just "currentPassword"
                  , fsName = Just "current"
                  , fsAttrs = [("autocomplete","current-password")]
                  } Nothing
              (newR,newV) <- mreq passwordField FieldSettings
                  { fsLabel = SomeMessage NewPass
                  , fsTooltip = Nothing
                  , fsId = Just "newPassword"
                  , fsName = Just "new"
                  , fsAttrs = [("autocomplete","new-password")]
                  } Nothing
              (confR,confV) <- mreq passwordField FieldSettings
                  { fsLabel = SomeMessage ConfirmPass
                  , fsTooltip = Nothing
                  , fsId = Just "confirmPassword"
                  , fsName = Just "confirm"
                  , fsAttrs = [("autocomplete","off")]
                  } Nothing

              let r = (,,) <$> currR <*> newR <*> confR
              let w = do
                      toWidget [cassius|
                                       ##{fvId currV}, ##{fvId newV}, ##{fvId confV}
                                         align-self: stretch
                                       |]
                      [whamlet|
                              #{extra}
                              $if old
                                ^{md3widget currV}
                              ^{md3widget newV}
                              ^{md3widget confV}
                              |]
              return (r,w)


    confirmationEmailSentResponse :: AE.Email -> AuthHandler App TypedContent
    confirmationEmailSentResponse email = do
        parent <- getRouteToParent
        msgs <- getMessages
        selectRep $ provideRep $ authLayout $ do
            setTitleI ConfirmationEmailSentTitle
            $(widgetFile "auth/confirmation")

    registerHandler :: AuthHandler App Html
    registerHandler = do
        (fw,et) <- liftHandler $ generateFormPost formRegEmailForm
        parent <- getRouteToParent
        msgs <- getMessages
        authLayout $ do
            setTitleI RegisterLong
            $(widgetFile "auth/register")
      where
          formRegEmailForm :: Form Text
          formRegEmailForm extra = do
              (emailR,emailV) <- mreq emailField FieldSettings
                  { fsLabel = SomeMessage MsgEmailAddress
                  , fsId = Just "email", fsName = Just "email", fsTooltip = Nothing
                  , fsAttrs = [("autocomplete","email")]
                  } Nothing
              let w = [whamlet|
                          #{extra}
                          ^{md3widget emailV}
                      |]
              return (emailR,w)


    emailLoginHandler :: (Route Auth -> Route App) -> Widget
    emailLoginHandler parent = do

        (fw,et) <- liftHandler $ generateFormPost formEmailLogin
        msgs <- getMessages

        idFormEmailLoginWarpper <- newIdent
        idFormEmailLogin <- newIdent

        $(widgetFile "auth/email") 

      where
          formEmailLogin :: Form (Text,Text)
          formEmailLogin extra = do
              (emailR,emailV) <- mreq emailField FieldSettings
                  { fsLabel = SomeMessage MsgEmailAddress
                  , fsTooltip = Nothing, fsId = Just "email", fsName = Just "email"
                  , fsAttrs = [("autocomplete","email")]
                  } Nothing
              (passR,passV) <- mreq passwordField FieldSettings
                  { fsLabel = SomeMessage MsgPassword
                  , fsTooltip = Nothing, fsId = Just "password", fsName = Just "password"
                  , fsAttrs = [("autocomplete","current-password")]
                  } Nothing
              let r = (,) <$> emailR <*> passR
                  w = do

                      users <- liftHandler $ runDB ( select $ do
                          x <- from $ table @User
                          where_ $ x ^. UserAuthType `in_` valList [UserAuthTypeEmail,UserAuthTypePassword]
                          where_ $ not_ $ x ^. UserSuper
                          orderBy [asc (x ^. UserId)]
                          return x )

                      supers <- liftHandler $ runDB ( select $ do
                          x <- from $ table @User
                          where_ $ x ^. UserAuthType `in_` valList [UserAuthTypeEmail,UserAuthTypePassword]
                          where_ $ x ^. UserSuper
                          orderBy [asc (x ^. UserId)]
                          return x )

                      let accounts = users <> supers
                      toWidget [julius|
                          Array.from(
                            document.getElementById('idButtonAccountsMenu').querySelectorAll('a.row')
                          ).forEach(x => {
                            x.addEventListener('click',e => {
                              e.preventDefault();
                              e.stopPropagation();
                              document.getElementById(#{fvId emailV}).value = x.dataset.email;
                              document.getElementById(#{fvId passV}).value = x.dataset.pass;
                              document.getElementById('idButtonAccountsMenu').click();
                            });
                          });
                      |]
                      [whamlet|
                           <nav.right-align>
                             <button.border.transparent type=button data-ui=#demoAccountsMenu #idButtonAccountsMenu>
                               <i.no-round>demography
                               <span>_{MsgDemoUserAccounts}
                               <i>arrow_drop_down
                               <menu #demoAccountsMenu>
                                 $forall Entity uid (User email _ uname super admin manager _ _ _) <- accounts
                                   $with pass <- maybe "" (TE.decodeUtf8 . localPart) (emailAddress $ TE.encodeUtf8 email)
                                     <a.row href=# data-email=#{email} data-pass=#{pass}>
                                       <img.circle.small src=@{DataR $ UserPhotoR uid} loading=lazy alt=_{MsgPhoto}>
                                       <div.max>
                                         $maybe name <- uname
                                           <h6.small>#{name}
                                         $nothing
                                           <h6.small>#{email}
                                         <div.small-text>
                                           $if super
                                             <p.upper>_{MsgSuperuser}
                                           $elseif admin
                                             <p.upper>_{MsgAdministrator}
                                           $elseif manager
                                             <p.upper>_{MsgManager}
                                           $else
                                             <p.lower>_{MsgUser}

                           #{extra}

                           ^{md3widget emailV}
                           ^{md3widget passV}
                      |]
              return (r,w)


    afterPasswordRoute :: App -> Route App
    afterPasswordRoute _ = HomeR


    addUnverified :: AE.Email -> VerKey -> AuthHandler App (AuthEmailId App)
    addUnverified email vk = liftHandler $ runDB $ insert
        (User email Nothing Nothing  False False False UserAuthTypeEmail (Just vk) False)


    sendVerifyEmail :: AE.Email -> VerKey -> VerUrl -> AuthHandler App ()
    sendVerifyEmail email _ verurl = do

        renderMsg <- getMessageRender

        tokenInfo <- liftHandler $ runDB $ selectOne $ do
            x <- from $ table @Token
            where_ $ x ^. TokenApi E.==. val keyApiGmail
            return x

        secretExists <- liftIO $ doesFileExist $ unpack secretVolumeRefreshTokenGmail

        (rtoken,sender) <- case (tokenInfo,secretExists) of
          (Just (Entity tid (Token _ StoreTypeDatabase)),_) -> do
              refresh <- liftHandler $ (unValue <$>) <$> runDB ( selectOne $ do
                  x <- from $ table @Store
                  where_ $ x ^. StoreToken E.==. val tid
                  where_ $ x ^. StoreKey E.==. val secretRefreshTokenGmail
                  return $ x ^. StoreVal )
              sender <- liftHandler $ (unValue <$>) <$> runDB ( selectOne $ do
                  x <- from $ table @Store
                  where_ $ x ^. StoreToken E.==. val tid
                  where_ $ x ^. StoreKey E.==. val keySendby
                  return $ x ^. StoreVal )
              return (refresh,sender)

          (Just (Entity _ (Token _ StoreTypeSession)),_) -> do
                refresh <- lookupSession secretRefreshTokenGmail
                sender <- lookupSession keySendby
                return (refresh,sender)

          (Just (Entity tid (Token _ StoreTypeGoogleSecretManager)),True) -> do

              refresh <- liftIO $ readFile' $ unpack secretVolumeRefreshTokenGmail

              sender <- liftHandler $ (unValue <$>) <$> runDB ( selectOne $ do
                  x <- from $ table @Store
                  where_ $ x ^. StoreToken E.==. val tid
                  where_ $ x ^. StoreKey E.==. val keySendby
                  return $ x ^. StoreVal )

              return (Just (pack refresh),sender)

          (_,True) -> do
              refresh <- liftIO $ readFile' $ unpack secretVolumeRefreshTokenGmail
              return (Just (pack refresh),Just "me")

          _otherwise -> return (Nothing,Nothing)

        atoken <- case rtoken of
          Just refresh -> do
              settings <- appSettings <$> getYesod

              r <- liftIO $ post "https://oauth2.googleapis.com/token"
                  [ "refresh_token" := refresh
                  , "client_id" := (googleApiConfClientId . appGoogleApiConf $ settings)
                  , "client_secret" := (googleApiConfClientSecret . appGoogleApiConf $ settings)
                  , "grant_type" := ("refresh_token" :: Text)
                  ]

              return $ r ^? WL.responseBody . key "access_token" . _String
          Nothing -> return Nothing

        case (atoken,sender) of
          (Just at,Just sendby) -> do

              let mail = (emptyMail $ Address Nothing "noreply")
                      { mailTo = [Address Nothing email]
                      , mailHeaders = [("Subject", renderMsg MsgVerifyYourEmailAddress)]
                      , mailParts = [[textPart, htmlPart]]
                      }
                    where
                      textPart = Part
                          { partType = "text/plain; charset=utf-8"
                          , partEncoding = None
                          , partDisposition = DefaultDisposition
                          , partContent = PartContent $ TLE.encodeUtf8 [stext|
                              _{MsgAppName}

                              _{MsgConfirmEmailPlease}.

                              #{verurl}

                              _{MsgThankYou}.
                              |]
                          , partHeaders = []
                          }
                      htmlPart = Part
                          { partType = "text/html; charset=utf-8"
                          , partEncoding = None
                          , partDisposition = DefaultDisposition
                          , partContent = PartContent $ renderHtml [shamlet|
                              <h1>
                                #{renderMsg MsgAppName}
                              <p>
                                #{renderMsg MsgConfirmEmailPlease}.
                              <p>
                                <a href=#{verurl}>#{verurl}
                              <p>
                                #{renderMsg MsgThankYou}.
                              |]
                          , partHeaders = []
                          }

              raw <- liftIO $ TE.decodeUtf8 . toStrict . B64L.encode <$> renderMail' mail

              let opts = defaults & auth ?~ oauth2Bearer (TE.encodeUtf8 at)
              response <- liftIO $ tryAny $ postWith
                  opts (gmailSendEnpoint $ unpack sendby) (object ["raw" .= raw])

              case response of
                Left e@(SomeException _) -> case fromException e of
                  Just (HttpExceptionRequest _ (StatusCodeException r' _bs)) -> do
                      case r' L.^. WL.responseStatus . WL.statusCode of
                        401 -> do
                            liftIO $ print response
                        403 -> do
                            liftIO $ print response
                        _   -> do
                            liftIO $ print response
                  _other -> do
                      liftIO $ print response
                Right _ok -> return ()
          _otherwise -> do
              curr <- getCurrentRoute
              addMessageI msgError MsgGmailAccountNotSet
              redirect $ fromMaybe HomeR curr


    getVerifyKey :: AuthEmailId App -> AuthHandler App (Maybe VerKey)
    getVerifyKey = liftHandler . runDB . fmap (userVerkey =<<) . get

    setVerifyKey :: AuthEmailId App -> VerKey -> AuthHandler App ()
    setVerifyKey uid k = liftHandler $ runDB $ update uid [UserVerkey =. Just k]

    needOldPassword :: AuthId App -> AuthHandler App Bool
    needOldPassword _ = return False

    verifyAccount :: AuthEmailId App -> AuthHandler App (Maybe (AuthId App))
    verifyAccount uid = liftHandler $ runDB $ do
        mu <- get uid
        case mu of
          Nothing -> return Nothing
          Just _ -> do
              update uid [UserVerified =. True, UserVerkey =. Nothing]
              return $ Just uid

    getPassword :: AuthId App -> AuthHandler App (Maybe SaltedPass)
    getPassword = liftHandler . runDB . fmap (userPassword =<<) . get

    setPassword :: AuthId App -> SaltedPass -> AuthHandler App ()
    setPassword uid pass = liftHandler $ runDB $ update uid [UserPassword =. Just pass]

    getEmailCreds :: Identifier -> AuthHandler App (Maybe (EmailCreds App))
    getEmailCreds email = liftHandler $ runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
          Nothing -> return Nothing
          Just (Entity uid u) -> return $ Just EmailCreds
              { emailCredsId = uid
              , emailCredsAuthId = Just uid
              , emailCredsStatus = isJust $ userPassword u
              , emailCredsVerkey = userVerkey u
              , emailCredsEmail = email
              }

    getEmail :: AuthEmailId App -> AuthHandler App (Maybe Yesod.Auth.Email.Email)
    getEmail = liftHandler . runDB . fmap (fmap userEmail) . get
    

formLogin :: Route App -> Widget
formLogin route = do

    users <- liftHandler $ runDB $ select $ from $
        ( do
              x <- from $ table @User
              where_ $ not_ $ x ^. UserSuper
              return x
        )
        `unionAll_`
        ( do
              x <- from $ table @User
              where_ $ x ^. UserSuper
              return x
        )
    
    msgs <- getMessages
    idInputUsername <- newIdent
    idInputPassword <- newIdent
    $(widgetFile "auth/form")


isAuthenticatedSelf :: UserId -> Handler AuthResult
isAuthenticatedSelf uid = do
    muid <- maybeAuthId
    case muid of
        Just uid' | uid == uid' -> return Authorized
                  | otherwise -> unauthorizedI MsgAnotherAccountAccessProhibited
        Nothing -> unauthorizedI MsgLoginPlease


isAdmin :: Handler AuthResult
isAdmin = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ True _ _ _ _ _)) -> return Authorized
        Just (Entity _ (User _ _ _ _ True _ _ _ _)) -> return Authorized
        Just (Entity _ (User _ _ _ _ False _ _ _ _)) -> unauthorizedI MsgAccessDeniedAdminsOnly
        Nothing -> unauthorizedI MsgSignInToAccessPlease


isManagerSelfOrAdmin :: UserId -> Handler AuthResult
isManagerSelfOrAdmin uid = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ True _ _ _ _ _)) -> return Authorized
        Just (Entity _ (User _ _ _ _ True _ _ _ _)) -> return Authorized
        
        Just (Entity uid' (User _ _ _ _ _ True _ _ _))
            | uid == uid' -> return Authorized
            | otherwise -> unauthorizedI MsgAnotherAccountAccessProhibited
            
        Just (Entity _ (User _ _ _ False False False _ _ _)) -> unauthorizedI MsgAccessDeniedManagerOrAdminOnly
            
        Nothing -> unauthorizedI MsgSignInToAccessPlease


isAdministrator :: Handler Bool
isAdministrator = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ _ True _ _ _ _)) -> return True
        Just (Entity _ (User _ _ _ _ False _ _ _ _)) -> return False
        Nothing -> return False
    

isEventManager :: Handler Bool
isEventManager = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ _ _ True _ _ _)) -> return True
        Just (Entity _ (User _ _ _ _ _ False _ _ _)) -> return False
        Nothing -> return False


-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    case muid of
        Nothing -> unauthorizedI MsgLoginToAccessPlease
        Just _ -> return Authorized



instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ [] = defaultFormMessage
    renderMessage _ ("en":_) = englishFormMessage
    renderMessage _ ("ru":_) = russianFormMessage
    renderMessage app (_:xs) = renderMessage app xs

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
