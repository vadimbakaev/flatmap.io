{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Wishlist where

import Import

postWishlistR :: Handler Value
postWishlistR = do
  muser <- maybeAuth
  case muser of
    Nothing -> sendResponseStatus status407 ()
    Just (Entity id user) -> do
      WishlistRequest itemToAdd <- requireInsecureJsonBody
      _ <- print itemToAdd
      returnJson $ WishlistResponse [itemToAdd]
