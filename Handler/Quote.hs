{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Handler.Quote
    where

import Prelude (head)
import Import
import Data.Time
import Data.Text (append)
import Data.Maybe
import Database.Persist.GenericSql.Raw
import Control.Monad
import Text.Blaze
import Text.Hamlet (shamlet)
import Yesod.Feed
import Yesod.Pagination
import Model.Tarball

-- | Pagination
quoteApprovedPager :: PaginationData App App
quoteApprovedPager = PaginationData 
    { paginationPerPage = 10
    , paginationLink    = linkPage ApprovedListR
    , paginationRender  = defaultRender 3 4
    }

quoteAbyssPager :: PaginationData App App
quoteAbyssPager = PaginationData 
    { paginationPerPage = 10
    , paginationLink    = linkPage AbyssListR
    , paginationRender  = defaultRender 3 4
    }

quotePager :: QuotePage -> PaginationData App App
quotePager quotepage = PaginationData 
    { paginationPerPage = 10
    , paginationLink    = linkPage $ case quotepage of {Approved -> ApprovedListR; Abyss -> AbyssListR}
    , paginationRender  = defaultRender 3 4
    }

instance ToMarkup LinkSource where
    toMarkup (Gru) = "Gentoo.ru"
    toMarkup GruConf = "gentoo@conference.gentoo.ru"
    toMarkup JRuConf = "gentoo@conference.jabber.ru"
    toMarkup RuMail  = "gentoo-user-ru@lists.gentoo.org"
    toMarkup RuWiki  = "ru.gentoo-wiki.com"
    toMarkup Awesome = "awesome@c.g.r"
    toMarkup GruTalks = "talks@c.g.r"
    toMarkup GruRion  = "rion@c.g.r"
    toMarkup LOR      = "linux.org.ru"
    toMarkup OtherSource = "Другое"

isApproved :: QuotePage -> Bool
isApproved Approved = True
isApproved _ = False


--showQuote :: QuoteGeneric SqlPersist -> GHandler App App ()
showQuote q quote = 
  let quoteId = Just q 
  in $(whamletFile "templates/quote-show.hamlet")

-- | create quote form
quoteAForm :: GHandler s0 m0 (AForm App App Quote)
quoteAForm = do
    time <- liftIO $ zonedTimeToUTC <$> getZonedTime
    sender' <- Just <$> lookupSession "sendername"
    return $
        Quote 
            <$> aopt textField "Автор"       Nothing 
            <*> aopt textField "Отправитель" sender'
            <*> areq (selectFieldList sources) "Место" Nothing 
            <*> areq urlField "Ссылка"     Nothing 
            <*> areq textareaField "Текст"   Nothing 
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
    form <- quoteAForm
    ((result,formWidget),enctype) <- runFormPost $ renderDivs form 
    showOnly <- lookupPostParam "show"
    case result of
        FormSuccess quote -> do
            when (isJust (quoteSender quote)) $ 
              setSession "sendername" $ fromJust $ quoteSender quote
            case showOnly of
                   Just _ -> do
                       setMessage [shamlet|Просмотр цитаты <strong>цитата не добавлена</strong>|] 
                   Nothing -> do 
                       quoteId  <- runDB (insert quote)
                       setMessage [shamlet|Цитата была успешно добавлена|]
                       createFile
                       toMaster <- getRouteToMaster
                       redirect $ toMaster $ QuoteShowR quoteId
            defaultLayout $ do
                menuWidget Create
                let quoteId = Nothing
                $(widgetFile "quote-show")
                when (isJust showOnly) $ $(widgetFile "quote-create")
        _other -> do 
            defaultLayout $ do
                menuWidget Create
                $(widgetFile "quote-create")

getQuoteCreateR  :: Handler RepHtml
getQuoteCreateR  = do
    sender <- Just <$> lookupSession "sendername"
    form <- quoteAForm
    (formWidget,enctype) <- generateFormPost $ renderTable form 
    defaultLayout $ do
        menuWidget Create
        $(widgetFile "quote-create")

pagerHandler :: QuotePage -> [Entity Quote] -> Maybe (GWidget App App ()) -> GHandler App App RepHtml
pagerHandler quotepage quotes pager = do
    abyssAuth <- maybeAuth
    let maid = case quotepage of
         Approved -> Nothing
         Abyss    -> abyssAuth
    defaultLayout $ do
        menuWidget quotepage
        $(widgetFile "quote-list")

showPage :: QuotePage -> Int -> Handler RepHtml
showPage quotepage n = do
    withPagination (quotePager quotepage) (return n) [QuoteApproved ==. isApproved quotepage] [] $ handler
    where handler = pagerHandler quotepage

showAll :: QuotePage -> Handler RepHtml
showAll quotepage = do
    quotes <- runDB $ selectList [QuoteApproved ==. isApproved quotepage] []
    pagerHandler quotepage quotes Nothing

quoteList    :: QuotePage -> Handler RepHtml
quoteList quotepage   = do
    page <- runInputGet $ iopt intField "page"
    maybe (showAll quotepage) (showPage quotepage) page

getApprovedListR :: Handler RepHtml
getApprovedListR = quoteList Approved

getAbyssListR :: Handler RepHtml
getAbyssListR = quoteList Abyss

getQuoteShowR :: QuoteId -> Handler RepHtml
getQuoteShowR q = do
    quote <- runDB $ get404 q
    defaultLayout $ do
        menuWidget $ if quoteApproved quote then Approved else Abyss
        let quoteId = Just q
        $(widgetFile "quote-show")

postQuoteAbyssProcessR :: Handler RepHtml
postQuoteAbyssProcessR = do
    _ <- requireAuth
    toDelete  <- lookupPostParam "delete"
    toApprove <- lookupPostParam "approve"
    qLst      <- lookupPostParams "abyss"
    let qlst' = catMaybes $ map (fromPathPiece) qLst 
    case (toDelete,toApprove) of
        (Just _, Nothing) -> delete' qlst'
        (Nothing, Just _) -> approve' qlst'
        (_,_)             -> do
            setMessage [shamlet|Invalid command|]
    toMaster <- getRouteToMaster
    redirect $ toMaster AbyssListR
    where
        delete' list = do
            _ <- runDB $ mapM delete list
            setMessage [shamlet|Цитаты были удалены|]
        approve' list = do  
            _ <- runDB $ mapM (flip update [QuoteApproved =. True]) list
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
