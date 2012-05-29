module Handler.Quote
    where

import Import
import Data.Time

quoteAForm :: Maybe Quote -> IO (AForm App App Quote)
quoteAForm mquote = liftIO getCurrentTime >>= \time ->
    return $ 
        Quote 
        <$> aopt textField "Отправитель" (quoteSender <$> mquote)
        <*> aopt textField "Автор"       (quoteAuthor <$> mquote)
        <*> areq (selectFieldList sources) "Место" (quoteSource <$> mquote)
        <*> areq textField "Текст"       (quoteText   <$> mquote)
        <*> areq textField "Ссылка"      (quoteProoflink <$> mquote)
        <*> pure time 
        <*> pure False
        where sources = [("Gentoo.Ru",Gru)
                        ,("gentoo@conference.gentoo.ru",GruConf)
                        ,("gentoo@conference.jabber.ru",JRuConf)
                        ,("gentoo-user-ru@lists.gentoo.org",RuMail)
                        ,("ru.gentoo-wiki.com",RuWiki)
                        ,("Другое",OtherSource)]





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

getQuoteAbyssListR :: Handler RepHtml
getQuoteAbyssListR = do
    quotes <- runDB $ selectList [] []
    defaultLayout $ do
        $(widgetFile "quote-list")

getQuoteAbyssListPageR::Int-> Handler RepHtml
getQuoteAbyssListPageR  page  = undefined


-- getQuotesList :: Maybe Int -> Bool -> Entity
--listQuotes mpage True  = runDB $ selectList [QuoteApproved ==.True] []
--listQuotes mpage False = runDB $ selectList [] []
