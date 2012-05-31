{--|
 - Module provides simple and extensible paginator widget
 - This module loads data for concrete page and creates a paginator
 - widget that can be used in template
 -
 - code ..
 -    (data
 --}
module Yesod.Pagination
    where

import Prelude
import Yesod
import Data.List


data PaginationData master m a b c = PaginationData 
        { paginationPerPage  :: Int                 -- ^ number of element per page
        , paginationLink     :: Int -> Route master -- ^ link to another page
        , paginationRender   :: b                   -- ^ render function
        }


{- 
generate :: PaginationData master ->   -- ^ options
            Int ->                     -- ^ currentpage
-}
generate (PaginationData perpage link render) page p o = do
    c <- runDB $ count p
    if (page * perpage) > c 
        then error "404" 
        else do
            let totalPages = c `div` perpage
                s1 = page*perpage 
            entities <- runDB $ selectList p  ( OffsetBy (page*perpage)
                                              : LimitTo perpage
                                              : o
                                              )
            return (entities, render page totalPages link)


data Page = Skip | This | Page Int

defaultRender p n l =do
    addCassius [cassius|
      div.pager
        text-align: center;
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
    let e  = 3
        r  = 4
        pre = p - 1
        nex = p + 1
        pages  = setSpace $ nub $ filter ((>)n) $ filter ((<)0) ([1..e]++[(p-r)..(p+r)]++[(n-e)..n])
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
        setSpace (x1:[])    | x1 == p      = [This]
                            | otherwise    = [Page x1]
        setSpace (x1:x2:xs) | x1 == p      = This:setSpace (x2:xs)
                            | (x1+1) /= x2 = (Page x1):Skip:setSpace (x2:xs)
                            | otherwise    = (Page x1):setSpace (x2:xs)

