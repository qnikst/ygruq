-- |
-- author: Alexander V Vershilov <alexander.vershilov@gmail.com>
--
-- Module provides simple and extensible paginator widget that doesn't make any 
-- assumptions on Route structure or type of page handling and doesn't load entities
-- that are not needed.
--
-- Basic usage
--
-- @
--
-- import Yesod.Pagination (PaginationData(..))
-- import qualified Yesod.Pagination as P
--
-- mkYesod "App" [parseRoutes|
--  \/route\/#Int MyRouteR GET
--  |]
--
-- myData = PaginationData
--              { paginationPerPage = 10
--              , paginationLink    = MyRouteR 
--              , paginationRender  = defaultMenuRender 2 3
--              }
-- 
-- getMyRouteR page = 
--      (myEntities, pager) <- P.generate myData page [ EntityVisible==.True ] 
--                                                    [ Desc  EntityDate]
--      defaultLayout $ do
--          toWidget [whamlet| 
--                   ^{pager} 
--                   $forall entity <- myEntities
--                     ^{showEntity entity}
--                   ^{pager}
--                   |]
-- @
--
module Yesod.Pagination
    ( PaginationData(..)
    , MenuRender
    , generate
    , withPagination
    , defaultRender
    )
    where

import Prelude
import Yesod
import Data.List
import Control.Monad

-- | Configuration datatype used in pagination
--
data PaginationData sub master = PaginationData 
        { paginationPerPage  :: Int                    -- ^ number of element per page
        , paginationLink     :: Int -> Route master    -- ^ link to another page
        , paginationRender   :: MenuRender sub master  -- ^ render function
        }

type MenuRender sub master = Int -> Int -> (Int -> Route master) -> GWidget sub master ()

-- | Generate menu and output data
-- N.B. this function doesn't check if Limit and Offset options exist in select
--      options
generate :: ((PersistQuery (PersistEntityBackend val) (GHandler sub master))
            , PersistEntity val, YesodPersist master
            , YesodPersistBackend master ~ PersistEntityBackend val) =>
            PaginationData sub master                -- ^ configuration
            -> [Filter val]                          -- ^ filter for where 
            -> [SelectOpt val]                       -- ^ options for select
            -> Int                                   -- ^ current page
            -> GHandler sub master ([Entity val], GWidget sub master ())
generate (PaginationData perpage link render) p o page = do
    c <- runDB $ count p
    unless (page >= 0) $ notFound
    if (page * perpage) > c 
        then notFound 
        else do
            let ls = if (c `mod` perpage)==0 then 0 else 1
                totalPages = c `div` perpage + ls
                s1 = page*perpage 
            entities <- runDB $ selectList p  ( OffsetBy ((page-1)*perpage)
                                              : LimitTo perpage
                                              : o
                                              )
            return (entities, render page totalPages link)

-- | Top level wrapper over generate function, that allowes to easily extend 
-- handler with paginator
-- 
-- @
-- handleRoute = withPagination def 
--                              (maybe 0 id <$> lookupGetParam "page") 
--                              []
--                              []
--                              $ \entities pager -> do
--                   ....
-- @
--
withPagination :: ((PersistQuery (PersistEntityBackend val) (GHandler sub master))
                  , PersistEntity val, YesodPersist master
                  , YesodPersistBackend master ~ PersistEntityBackend val) =>
                  (PaginationData sub master) 
                  -> (GHandler sub master Int)
                  -> [Filter val]
                  -> [SelectOpt val]
                  -> ([Entity val] -> GWidget sub master () -> GHandler sub master a)
                  -> GHandler sub master a
withPagination opt getPage pred sopt next = 
    getPage >>= generate  opt pred sopt >>= \(e,w) -> next e w
    

-- | helper type for menu generation
data Page = Skip | This | Page Int

-- | Default menu renderer is uses it creates an list of links
--   with e links at edges and r links around current pages, adds "..." 
--   where it's needed.
defaultRender :: (Yesod master) => 
                 Int                      -- ^ number of pages at sides
                 -> Int                   -- ^ number of pages around current page
                 -> MenuRender sub master -- ^ output widget
defaultRender e r p n l = do
    let pre = p - 1
        nex = p + 1
        pages  = setSpace $ [1..e]++[(p-r)..(p+r)]++[(n-e)..n]
    toWidget [cassius|
      ul.pagination
        margin: 5px 0px;
        padding: 0px;
        text-align: center;
      .pagination li
        display: inline;
        list-style-type: none;
        margin: 0px;
        padding: 0px 3px;
        text-align: center;
      .pagination li.this_page
        padding: 0px 5px;
      .pagination li.this_page a
        background-color: #C5C5C5;
    |]
    [whamlet|
        <ul .pagination>
          $if (>) p 1
           <li .previous_page_more>
             <a href=@{l pre}>&lt;
          $forall v <- pages
            $case v
              $of This
                <li .this_page><a>#{p}</a>
              $of Page k
                <li>
                  <a href=@{l k}>#{k}
              $of Skip
                <li><a>...</a>
          $if (<) nex n
           <li .next_page>
             <a href=@{l nex}>&gt;
    |]
    where 
        setSpace []                        = []
        setSpace (x1:[])    | x1 == p      = [This]
                            | otherwise    = [Page x1]
        setSpace (x1:x2:xs) | x1 >  n      = []
                            | x1 >= x2     = setSpace (x1:xs)
                            | x1 == p      = This:setSpace (x2:xs)
                            | (x1+1) /= x2 = (Page x1):Skip:setSpace (x2:xs)
                            | otherwise    = (Page x1):setSpace (x2:xs)

