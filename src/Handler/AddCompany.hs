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
getAddCompanyR = do
  (widget, enctype) <- generateFormPost newCompanyForm
  defaultLayout $ do
    setTitle "Add Company"
    $(widgetFile "add-company-form")

postAddCompanyR :: Handler Html
postAddCompanyR = do
  ((result, widget), enctype) <- runFormPost newCompanyForm
  case result of
    FormSuccess newCompany ->
      defaultLayout $ do
        setTitle $ toHtml $ mconcat ["Thank you for added ", name newCompany]
        $(widgetFile "add-company-thank-you")
    _ ->
      defaultLayout
        [whamlet|
            <div .container>
              <p>Invalid input, try again, please.
              <form method=post action=@{AddCompanyR} enctype=#{enctype}>
                 ^{widget}
                 <button>Submit
         |]

newCompanyForm :: Html -> MForm Handler (FormResult NewCompany, Widget)
newCompanyForm =
  renderTable $
  NewCompany <$> areq textField "Company name" Nothing <*>
  areq textField "Website" Nothing <*>
  areq textField "LinkedIn" Nothing <*>
  areq textField "GitHub" Nothing <*>
  areq textField "Operative office address" Nothing <*>
  areq boolField "Is it Startup?" (Just False) <*>
  areq boolField "Is it Remote fiendly?" (Just False)


  --TODO add languages, save newCompany on DB
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
