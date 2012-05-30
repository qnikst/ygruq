{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Quote
    where

import Prelude (head)
import Import
import Data.Time
import Data.Text (pack, unpack, append)
import Control.Applicative
import Control.Arrow
import Text.Blaze
import Text.Hamlet (shamlet)
import Yesod.Feed
import Yesod.Default.Config (appExtra)


data QuotePage = Approved | Abyss | Create

instance ToMarkup LinkSource where
    toMarkup (Gru) = "Gentoo.ru"
    toMarkup GruConf = "getnoo@conference.gentoo.ru"
    toMarkup JRuConf = "gentoo@conference.jabber.ru"
    toMarkup RuMail  = "gentoo-user-ru@lists.gentoo.org"
    toMarkup RuWiki  = "ru.gentoo-wiki.com"
    toMarkup OtherSource = "Другое"


quoteAForm :: UTCTime -> Maybe Quote -> AForm App App Quote
quoteAForm time mquote = Quote 
            <$> aopt textField "Отправитель" (quoteSender <$> mquote)
            <*> aopt textField "Автор"       (quoteAuthor <$> mquote)
            <*> areq (selectFieldList sources) "Место" (quoteSource <$> mquote)
            <*> areq urlField "Ссылка"      (quoteProoflink <$> mquote)
            <*> areq textareaField "Текст"   (quoteText   <$> mquote)
            <*> pure time 
            <*> pure False
        where 
            sources :: [(Text,LinkSource)]
            sources = [("gentoo.ru",Gru)
                      ,("gentoo@conference.gentoo.ru",GruConf)
                      ,("gentoo@conference.jabber.ru",JRuConf)
                      ,("gentoo-user-ru@lists.gentoo.org",RuMail)
                      ,("ru.gentoo-wiki.com",RuWiki)
                      ,("Другое",OtherSource)]



postQuoteCreateR :: Handler RepHtml
postQuoteCreateR = do
    time <- liftIO getCurrentTime
    ((result,formWidget),enctype) <- runFormPost $ renderTable $ quoteAForm time Nothing
    case result of
        FormSuccess quote -> do
            quoteId <- runDB $ insert quote
            defaultLayout $ do
                $(widgetFile "quote-show")
        _other -> do 
            defaultLayout $ do
                $(widgetFile "quote-create")




getQuoteCreateR  :: Handler RepHtml
getQuoteCreateR  = do
    let pageType = Create
    time <- liftIO $ getCurrentTime
    (formWidget,enctype) <- generateFormPost $ renderTable $ quoteAForm time Nothing
    defaultLayout $ do
        $(widgetFile "quote-list-wrapper")
        $(widgetFile "quote-create")


getQuoteListR    :: Handler RepHtml
getQuoteListR    = do
    let maid = Nothing
        pageType = Approved
    quotes <- runDB $ selectList [QuoteApproved ==. True] []
    defaultLayout $ do
        $(widgetFile "quote-list-wrapper")
        $(widgetFile "quote-list")


getQuoteListPageR:: Int -> Handler RepHtml
getQuoteListPageR page = undefined

getQuoteShowR :: QuoteId -> Handler RepHtml
getQuoteShowR = undefined

getQuoteAbyssListR :: Handler RepHtml
getQuoteAbyssListR = do
    maid <- maybeAuth
    let pageType = Abyss
    quotes <- runDB $ selectList [QuoteApproved==.False] [Asc QuoteTimestamp]
    defaultLayout $ do
        $(widgetFile "quote-list-wrapper")
        $(widgetFile "quote-list")

postQuoteAbyssProcessR :: Handler RepHtml
postQuoteAbyssProcessR = do
    toDelete  <- lookupPostParam "delete"
    toApprove <- lookupPostParam "approve"
    qLst      <- lookupPostParams "abyss"
    let qlst' = map (Key . read . unpack) qLst 
    case (toDelete,toApprove) of
        (Just _, Nothing) -> delete' qlst'
        (Nothing, Just _) -> approve' qlst'
        (_,_)             -> do
            setMessage [shamlet|Invalid command|]
    toMaster <- getRouteToMaster
    redirect $ toMaster QuoteAbyssListR
    where
        delete' list = do
            runDB $ mapM delete list
            setMessage [shamlet|Цитаты были удалены|]
        approve' list = do  
            runDB $ mapM (flip update [QuoteApproved =. True]) list
            setMessage [shamlet|Цитаты были опубликованы|]

getQuoteFeedR :: Handler RepAtomRss
getQuoteFeedR = do 
    quotes <- runDB $ selectList [ QuoteApproved==.True]
                                 [ Desc QuoteTimestamp
                                 , LimitTo 100
                                 ]
    newsFeed Feed 
        { feedTitle = "Gentoo.ru Quotes"
        , feedLinkSelf = QuoteFeedR
        , feedLinkHome = HomeR 
        , feedDescription = "Description"
        , feedLanguage = "ru"
        , feedUpdated  = (quoteTimestamp $ entityVal $ head $ quotes)
        , feedEntries  =  (map toFeed quotes)
        }
    where
        toFeed (Entity i q) = 
            FeedEntry 
                { feedEntryLink    = QuoteShowR i
                , feedEntryUpdated = quoteTimestamp q
                , feedEntryTitle   = ("Цитата №" `append` (toPathPiece i))
                , feedEntryContent = (toHtml $ quoteText q)
                }
