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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Handler.Home where

import Control.Monad (join, mfilter)
import Data.Aeson
import Data.Bifunctor (bimap)
import Database (getAllCompanies)
import Import
import Util.Company (recentlyAdded)
import Util.Geo (toGeo)

-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = getSearchR

getSearchR :: Handler Html
getSearchR = do
  mlang <- mfilter (/= "All languages") <$> lookupGetParam "lang"
  mremote <- fmap (== "on") <$> lookupGetParam "remote"
  mindustry <- mfilter (/= "All industries") <$> lookupGetParam "industry"
  now <- liftIO getCurrentTime
  (companies, newCompanies) <-
    join bimap toGeo . partition (recentlyAdded now) <$>
    runDB (getAllCompanies mlang mremote mindustry)
  defaultLayout $ do
    muser <- maybeAuth
    let isLogged = isJust muser
    addScriptRemote "https://code.jquery.com/jquery-3.4.1.min.js"
    App {..} <- getYesod
    aDomId <- newIdent
    setTitle $
      toHtml $ mconcat ["Discover ", fromMaybe "new" mlang, " opportunity"]
    $(widgetFile "homepage")

searchForR :: Text -> Handler Html
searchForR lang = redirect (SearchR, [("lang", lang)])
