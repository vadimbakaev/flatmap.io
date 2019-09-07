{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Handler.Bookmarks where

import Import

postBookmarksR :: Handler Value
postBookmarksR = do
  WishlistRequest itemToAdd <- requireInsecureJsonBody
  muser <- maybeAuth
  case muser of
    Nothing -> sendResponseStatus status407 ()
    Just (Entity userId _) -> do
      mbookmarks <- runDB $ selectFirst [BookmarksUserId ==. userId] []
      case mbookmarks of
        Nothing -> do
          runDB $ insert_ $ Bookmarks userId newItems
          returnJson $ WishlistResponse newItems
          where newItems = [itemToAdd]
        Just (Entity bookmarksId (Bookmarks _ savedItems)) -> do
          runDB $ update bookmarksId [BookmarksItems =. newItems]
          returnJson $ WishlistResponse newItems
          where newItems = removeOrAdd itemToAdd savedItems

removeOrAdd :: Eq a => a -> [a] -> [a]
removeOrAdd x = go
  where
    go [] = [x]
    go (y:ys)
      | x == y = ys
      | otherwise = y : go ys
