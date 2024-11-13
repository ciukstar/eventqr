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

module Foundation where

import Control.Monad.Logger (LogSource)

import Import.NoFoundation

import qualified Data.CaseInsensitive as CI
import Data.Kind (Type)
import qualified Data.List.Safe as LS (head)
import qualified Data.Text as T (intercalate)
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar.Month (Month)

import Database.Esqueleto.Experimental
    ( SqlExpr, selectOne, from, table, where_, val, select, unionAll_, not_
    , (^.)
    , countRows, unValue
    )
import qualified Database.Esqueleto.Experimental as E ((==.), Value)
import Database.Persist.Sql (ConnectionPool, runSqlPool)

import Text.Email.Validate (emailAddress, localPart)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)

import Yesod.Auth.HashDB (authHashDBWithForm)
import Yesod.Auth.Message
    ( AuthMessage(InvalidLogin), defaultMessage, englishMessage, russianMessage )
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.I18n.English (englishFormMessage)
import Yesod.Form.I18n.Russian (russianFormMessage)
import Text.Julius (juliusFile)


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
    isAuthorized (CalendarEventAttendeesR {}) _ = return Authorized
    isAuthorized (CalendarEventAttendeeR {}) _ = return Authorized
    
    isAuthorized DocsR _ = setUltDestCurrent >> return Authorized
    
    isAuthorized PwdResetR _ = return Authorized
    isAuthorized LangR _ = return Authorized
    
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized

    
    isAuthorized FetchR _ = setUltDestCurrent >> return Authorized
    
    isAuthorized r@(DataR TokensGmailR) _ = setUltDest r >> isAdmin
    isAuthorized (DataR TokensGmailClearR) _ = isAdmin
    isAuthorized (DataR TokensGmailHookR) _ = isAdmin
    
    isAuthorized (DataR TokensVapidR) _ = isAdmin
    isAuthorized (DataR TokensVapidClearR) _ = isAdmin
    isAuthorized (DataR TokensVapidHookR) _ = isAdmin
        
    
    isAuthorized (DataR (UserDeleR _)) _ = isAdmin
    isAuthorized (DataR (UserEditR _)) _ = isAdmin
    isAuthorized (DataR UserNewR) _ = isAdmin
    isAuthorized (DataR (UserR _)) _ = isAdmin
    isAuthorized (DataR UsersR) _ = setUltDestCurrent >> isAdmin
    isAuthorized (DataR (UserPhotoR _)) _ = return Authorized
    isAuthorized (DataR (UserNotificationsR uid)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (UserNotificationR uid _)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (UserNotificationDeleR uid _)) _ = isAuthenticatedSelf uid
        
    isAuthorized (DataR (UserSettingsR uid)) _ = isAuthenticatedSelf uid
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


getPwdResetR :: Handler Html
getPwdResetR = do
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRestoreLogin
        $(widgetFile "auth/restore")


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
    redirectToReferer _ = False

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate (Creds _plugin ident _extra) = liftHandler $ do
        user <- runDB $ selectOne $ do
            x <- from $ table @User
            where_ $ x ^. UserEmail E.==. val ident
            return x
        return $ case user of
                Just (Entity uid _) -> Authenticated uid
                Nothing -> UserError InvalidLogin

    authPlugins :: App -> [AuthPlugin App]
    authPlugins _app = [authHashDBWithForm formLogin (Just . UniqueUser)]

    renderAuthMessage :: App -> [Text] -> AuthMessage -> Text
    renderAuthMessage _ [] = defaultMessage
    renderAuthMessage _ ("en":_) = englishMessage
    renderAuthMessage _ ("ru":_) = russianMessage
    renderAuthMessage app (_:xs) = renderAuthMessage app xs
    

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
    
    msgr <- getMessageRender
    msgs <- getMessages
    idOverlay <- newIdent
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
        Just (Entity _ (User _ _ _ True _ _)) -> return Authorized
        Just (Entity _ (User _ _ _ _ True _)) -> return Authorized
        Just (Entity _ (User _ _ _ _ False _)) -> unauthorizedI MsgAccessDeniedAdminsOnly
        Nothing -> unauthorizedI MsgSignInToAccessPlease


isManagerSelfOrAdmin :: UserId -> Handler AuthResult
isManagerSelfOrAdmin uid = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ True _ _)) -> return Authorized
        Just (Entity _ (User _ _ _ _ True _)) -> return Authorized
        
        Just (Entity uid' (User _ _ _ _ _ True))
            | uid == uid' -> return Authorized
            | otherwise -> unauthorizedI MsgAnotherAccountAccessProhibited
            
        Just (Entity _ (User _ _ _ False False False)) -> unauthorizedI MsgAccessDeniedManagerOrAdminOnly
            
        Nothing -> unauthorizedI MsgSignInToAccessPlease


isAdministrator :: Handler Bool
isAdministrator = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ _ True _)) -> return True
        Just (Entity _ (User _ _ _ _ False _)) -> return False
        Nothing -> return False
    

isEventManager :: Handler Bool
isEventManager = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ _ _ True)) -> return True
        Just (Entity _ (User _ _ _ _ _ False)) -> return False
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
