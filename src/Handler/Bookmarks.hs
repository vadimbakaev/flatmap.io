{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Bookmarks where

import Handler.Home (toGeo)
import Import

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
            let companies = toGeo savedCompanies
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
