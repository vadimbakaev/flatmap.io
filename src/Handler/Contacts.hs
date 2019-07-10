{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Contacts where

import Import

getContactsR :: Handler Html
getContactsR =
  defaultLayout $ do
    setTitle "Get in touch"
    $(widgetFile "contacts")
