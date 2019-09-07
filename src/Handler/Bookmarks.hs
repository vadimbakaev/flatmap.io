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
      mbookmarks <- runDB $ retrieveBookmarks userId
      case mbookmarks of
        Nothing -> do
          runDB $ insert_ $ Bookmarks userId newItems
          returnJson $ WishlistResponse newItems
          where newItems = [itemToAdd]
        Just (Entity bookmarksId (Bookmarks _ savedItems)) -> do
          runDB $ update bookmarksId [BookmarksItems =. newItems]
          returnJson $ WishlistResponse newItems
          where newItems = removeOrAdd itemToAdd savedItems

retrieveBookmarks :: UserId -> DB (Maybe (Entity Bookmarks))
retrieveBookmarks userId = selectFirst [BookmarksUserId ==. userId] []

removeOrAdd :: Eq a => a -> [a] -> [a]
removeOrAdd x xs
  | x `elem` xs = [x' | x' <- xs, x' /= x]
  | otherwise = x : xs
