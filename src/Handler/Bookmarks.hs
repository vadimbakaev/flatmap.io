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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Bookmarks where

import Data.Bifunctor (bimap)
import Import
import Util.Company (recentlyAdded)
import Util.Geo (toGeo)

getBookmarksR :: Handler TypedContent
getBookmarksR = do
  muser <- maybeAuth
  case muser of
    Nothing -> sendResponseStatus status407 ()
    Just (Entity userId _) -> do
      mbookmarks <- runDB $ selectFirst [BookmarksUserId ==. userId] []
      let savedIds = maybe [] (bookmarksItems . entityVal) mbookmarks
      savedCompanies <- runDB $ selectList [CompanyId <-. savedIds] []
      selectRep $ do
        provideRep $
          defaultLayout $ do
            now <- liftIO getCurrentTime
            let (companies, newCompanies) =
                  bimap toGeo toGeo $
                  partition (recentlyAdded now) savedCompanies
            addScriptRemote "https://code.jquery.com/jquery-3.4.1.min.js"
            App {..} <- getYesod
            aDomId <- newIdent
            setTitle "Saved companies"
            $(widgetFile "bookmarks")
        provideJson $ WishlistResponse savedIds

postBookmarksR :: Handler Value
postBookmarksR = do
  WishlistRequest itemToAdd <- requireInsecureJsonBody
  muser <- maybeAuth
  case muser of
    Nothing -> sendResponseStatus status407 ()
    Just (Entity userId _) ->
      runDB $ do
        mbookmarks <- selectFirst [BookmarksUserId ==. userId] []
        case mbookmarks of
          Nothing -> do
            insert_ $ Bookmarks userId newItems
            returnJson $ WishlistResponse newItems
            where newItems = [itemToAdd]
          Just (Entity bookmarksId (Bookmarks _ savedItems)) -> do
            update bookmarksId [BookmarksItems =. newItems]
            returnJson $ WishlistResponse newItems
            where newItems = removeOrAdd itemToAdd savedItems

removeOrAdd :: Eq a => a -> [a] -> [a]
removeOrAdd x = go
  where
    go [] = [x]
    go (y:ys)
      | x == y = ys
      | otherwise = y : go ys
