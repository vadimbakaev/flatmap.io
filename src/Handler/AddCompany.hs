{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.AddCompany where

import Import

getAddCompanyR :: Handler Html
getAddCompanyR =
  defaultLayout $ do
    setTitle "Add Company"
    $(widgetFile "add-company")
