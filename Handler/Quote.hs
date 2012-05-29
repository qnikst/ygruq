module Handler.Quote
    where

import Import

postQuoteCreateR :: Handler RepHtml
postQuoteCreateR = undefined
getQuoteCreateR  :: Handler RepHtml
getQuoteCreateR  = undefined

getQuoteListR    :: Handler RepHtml
getQuoteListR    = do
    quotes <- runDB $ selectList [QuoteApproved ==. True] []
    defaultLayout $ do
        $(widgetFile "quote-list")


getQuoteListPageR:: Int -> Handler RepHtml
getQuoteListPageR page = undefined
getQuoteAbyssListR:: Handler RepHtml
getQuoteAbyssListR     = undefined
getQuoteAbyssListPageR::Int-> Handler RepHtml
getQuoteAbyssListPageR  page  = undefined
