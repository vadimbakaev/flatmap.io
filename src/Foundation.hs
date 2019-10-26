{-
    flatmap.io IT job search based on geo and technology.
    Copyright (C) 2019 Vadim Bakaev

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Foundation where

import Control.Monad.Logger (LogSource)
import Database.Persist.MongoDB hiding (master)
import Import.NoFoundation
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)

import qualified Data.CaseInsensitive as CI
import qualified Data.List as L (concatMap, nub, sort)
import qualified Data.Text.Encoding as TE
import Yesod.Auth.OAuth2 (getUserResponseJSON)
import Yesod.Auth.OAuth2.GitHub
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

-- Every handler will have access to the data present here.
data App =
  App
    { appSettings :: AppSettings
    , appStatic :: Static -- ^ Settings for static file serving.
    , appConnPool :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger :: Logger
    , appMapboxAccessToken :: Text
    , appMapQuestKey :: Text
    , appOAuth2ClientId :: Text
    , appOAuth2ClientSecret :: Text
    }

data MenuItem =
  MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    , menuItemButton :: Bool
    , menuItemIcon :: Maybe Text
    }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

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
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a
   = forall (m :: * -> *). (MonadIO m) =>
                             ReaderT MongoContext m a

getCompanies :: DB [Entity Company]
getCompanies = selectList [] []

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
                                                                      where
  approot :: Approot App
  approot =
    ApprootRequest $ \app req ->
      case appRoot $ appSettings app of
        Nothing -> getApprootText guessApproot app req
        Just root -> root
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    Just <$>
    defaultClientSessionBackend
      120 -- timeout in minutes
      "config/client_session_key.aes"
    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware handler = do
    addHeader
      "Content-Security-Policy"
      "default-src * data: blob: ; script-src 'self' 'unsafe-eval' 'unsafe-inline' www.google-analytics.com stackpath.bootstrapcdn.com cdnjs.cloudflare.com code.jquery.com api.tiles.mapbox.com kit.fontawesome.com data: blob: ; style-src 'self' 'unsafe-inline' fonts.googleapis.com stackpath.bootstrapcdn.com cdnjs.cloudflare.com kit-free.fontawesome.com api.tiles.mapbox.com; font-src 'self' data: fonts.gstatic.com kit-free.fontawesome.com; img-src 'self' data: https://s2.googleusercontent.com blob: https://2.googleusercontent.com www.google-analytics.com; connect-src 'self' https://*.tiles.mapbox.com https://api.mapbox.com https://events.mapbox.com https://www.google-analytics.com"
    addHeader "X-Frame-Options" "deny"
    addHeader "X-Content-Type-Options" "nosniff"
    addHeader "Referrer-Policy" "origin"
    addHeader "Feature-Policy" "geolocation 'self'"
    addHeader "Strict-Transport-Security" "max-age=31536000; includeSubDomains"
    defaultYesodMiddleware handler
  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    muser <- maybeAuth
    mlang <- lookupGetParam "lang"
    mindustry <- lookupGetParam "industry"
    mremote <- lookupGetParam "remote"
    mcurrentRoute <- getCurrentRoute
    companies <- runDB getCompanies
    let languagesSelector =
          L.sort $ L.nub $ L.concatMap (companyStack . entityVal) companies
    let industriesSelector =
          L.sort $ L.nub $ companyIndustry . entityVal <$> companies
        -- Define the menu items of the header.
    let menuItems =
          [ NavbarRight $
            MenuItem
              { menuItemLabel = "Pendings"
              , menuItemRoute = PendingR
              , menuItemAccessCallback =
                  Just True == (userIsAdmin . entityVal <$> muser)
              , menuItemButton = False
              , menuItemIcon = Just "fas fa-user-check"
              }
          , NavbarRight $
            MenuItem
              { menuItemLabel = "Saved"
              , menuItemRoute = BookmarksR
              , menuItemAccessCallback = isJust muser
              , menuItemButton = False
              , menuItemIcon = Just "fa fa-heart"
              }
          , NavbarRight $
            MenuItem
              { menuItemLabel = "Add Company"
              , menuItemRoute = CompanyR
              , menuItemAccessCallback = True
              , menuItemButton = False
              , menuItemIcon = Just "fa fa-plus-square"
              }
          , NavbarRight $
            MenuItem
              { menuItemLabel = "Contacts"
              , menuItemRoute = ContactsR
              , menuItemAccessCallback = True
              , menuItemButton = False
              , menuItemIcon = Just "fa fa-users"
              }
          , NavbarRight $
            MenuItem
              { menuItemLabel = "Login"
              , menuItemRoute = AuthR $ PluginR "github" ["forward"]
              , menuItemAccessCallback = isNothing muser
              , menuItemButton = True
              , menuItemIcon = Just "fab fa-github"
              }
          , NavbarRight $
            MenuItem
              { menuItemLabel = "Logout"
              , menuItemRoute = AuthR LogoutR
              , menuItemAccessCallback = isJust muser
              , menuItemButton = False
              , menuItemIcon = Just "fas fa-sign-out-alt"
              }
          ]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
    let navbarRightFilteredMenuItems =
          [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
    pc <-
      widgetToPageContent $ do
        addStylesheet $ StaticR css_bootstrap_css
        $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
  addStaticContent ::
       Text -- ^ The file extension
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
        -- Generate a unique filename based on the content itself
    where
      genFileName lbs = "autogen-" ++ base64md5 lbs
    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return $
    appShouldLogAll (appSettings app) ||
    level == LevelWarn || level == LevelError
  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

instance YesodBreadcrumbs App
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
                         where
  breadcrumb ::
       Route App -- ^ The route the user is visiting currently.
    -> Handler (Text, Maybe (Route App))
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = MongoContext
  runDB :: ReaderT MongoContext Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runMongoDBPool
      (mgAccessMode $ appDatabaseConf $ appSettings master)
      action
      (appConnPool master)

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
  authenticate ::
       (MonadHandler m, HandlerSite m ~ App)
    => Creds App
    -> m (AuthenticationResult App)
  authenticate creds =
    liftHandler $
    runDB $ do
      muserEntity <- getBy $ UniqueUser $ credsIdent creds
      createdAt <- liftIO getCurrentTime
      case muserEntity of
        Just (Entity uid _) -> do
          n <- count [GitHubUserId ==. uid]
          case n of
            0 ->
              case getUserResponseJSON creds of
                Left errorMsg -> print errorMsg
                Right gitHubUser -> insert_ $ GitHub uid gitHubUser
            _ -> pure ()
          return $ Authenticated uid
        Nothing -> do
          userId <-
            insert
              User
                { userIdent = credsIdent creds
                , userIsAdmin = False
                , userCreatedAt = createdAt
                }
          _ <-
            case getUserResponseJSON creds of
              Left errorMsg -> print errorMsg
              Right gitHubUser -> insert_ $ GitHub userId gitHubUser
          return $ Authenticated userId
    -- You can add other plugins like Google Email, email or OAuth here
  authPlugins :: App -> [AuthPlugin App]
  authPlugins App {..} =
    [oauth2GitHubScoped ["user"] appOAuth2ClientId appOAuth2ClientSecret]

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

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
