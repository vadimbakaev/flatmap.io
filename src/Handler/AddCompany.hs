{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.AddCompany where

import Import

getAddCompanyR :: Handler Html
getAddCompanyR =
  defaultLayout $ do
    setTitle "Add Company"
    $(widgetFile "add-company-form")

postAddCompanyR :: Handler Html
postAddCompanyR = do
  newCompany <-runInputPost $ NewCompany
    <$> ireq textField "companyName"
    <*> ireq textField "website"
    <*> ireq textField "linkedin"
    <*> ireq textField "github"
    <*> ireq textField "address"
    <*> ireq checkBoxField "startup"
    <*> ireq checkBoxField "remote"
  defaultLayout $ do
    setTitle $ toHtml $ mconcat ["Thank you for added ", name newCompany]
    $(widgetFile "add-company-thank-you")

data NewCompany =
  NewCompany
    { name :: Text
    , webSite :: Text
    , linkedIn :: Text
    , gitHub :: Text
    , address :: Text
    , isStartup :: Bool
    , isRemote :: Bool
    }
  deriving (Show)
