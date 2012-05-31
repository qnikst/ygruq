{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Time
import Handler.Quote

-- | form widget
quoteCreate formWidget enctype = $(widgetFile "quote-create")

-- | version list widget
versionCreate versions = $(widgetFile "quote-version")


getHomeR :: Handler RepHtml
getHomeR = do
    -- get database info
    (notApproved,versions) <- runDB $ do
        a <- count [QuoteApproved ==. False]
        b <- selectList [] [Desc TarballTimestamp]
        return (a,b)
    -- create form widet
    time <- liftIO $ zonedTimeToUTC <$> getZonedTime
    (formWidget,enctype) <- generateFormPost $ renderTable $ quoteAForm time Nothing
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

