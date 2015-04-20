{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import Control.Lens hiding (children)
import Data.ByteString.Lazy (ByteString)
import Data.List (group, nub, sort, sortBy, union)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Lucid
import Network.Wreq hiding (head_)
import Text.Taggy
import Text.Taggy.Lens

data POWResults = POWResults {
    name :: T.Text
  , currentTotal :: Maybe Int
  , previousTotal :: Maybe Int
  } deriving (Eq, Ord, Show)

table :: [Node] -> Maybe T.Text
table row =  row ^? ix 1 . contents

makeTableIsh :: Response ByteString -> [Maybe T.Text]
makeTableIsh = toListOf $ responseBody . to (decodeUtf8With lenientDecode)
               . html . allNamed (only "tr") . children . to table

processList :: Response ByteString -> Response ByteString -> [POWResults]
processList r1 r2 = nub res
  where
    sr1 = sanitize $ r1
    sr2 = sanitize $ r2
    csr1 = countSanitized sr1
    csr2 = countSanitized sr2

    -- There has GOT to be a better way to logic this. But, here goes.
    -- First, let's get a list of all possible names.
    allNames = sr1 `union` sr2

    -- Now, for each name, get a count of current and previous POWs
    res = fmap (\n -> POWResults n (lookup n csr1) (lookup n csr2)) allNames

    sanitize r =
      let
        l = [ x | Just x <- makeTableIsh r, x /= "\160" ]
        l' = concatMap (T.splitOn "," . T.replace "." "" . T.replace "\160" "") l
        trim' = T.unwords . T.words
      in fmap trim' l'
    countSanitized x =
      reverse . sort . map (head &&& length) . group . sort $ x

maybeAdd :: Maybe Int -> Maybe Int -> Int
maybeAdd (Just a) Nothing  = a
maybeAdd (Just a) (Just b) = a + b
maybeAdd Nothing (Just b)  = b
maybeAdd Nothing Nothing   = 0

htmlLeaderboard :: [POWResults] -> Html ()
htmlLeaderboard xs = do
  table_ $ do
    tr_ $ do
      th_ "Rank"
      th_ "Name"
      th_ $
        a_
        [href_ "http://www.as.ysu.edu/~curmath/pow/pow_web_2014-15/index_winners_2014_fall.html"]
        "Last semester"
      th_ $
        a_
        [href_ "http://www.as.ysu.edu/~curmath/pow/pow_web_2014-15/index_winners_2015_spring.html"]
        "This semester"
      th_ "Year Total"
      mapM_ (\(powres, i) -> p i powres) (zip sortedXs ([1..] :: [Integer]))
  where
    sortedXs = sortBy (\(POWResults _ b c) (POWResults _ e f) -> (maybeAdd e f) `compare` (maybeAdd b c)) xs

    p :: Integer -> POWResults -> Html ()
    p n (POWResults person cur prev) =
      tr_ $ do
        td_ (toHtml . show $ n)
        td_ (toHtml person)
        td_ $ maybeToField prev
        td_ $ maybeToField cur
        td_ $ toHtml $ show ma <> " " <> pluralize ma
          where ma = maybeAdd cur prev

    pluralize 1 = "problem"
    pluralize _ = "problems"

    maybeToField :: Maybe Int -> Html ()
    maybeToField (Just n) = toHtml $ show n <> " " <> pluralize n
    maybeToField Nothing = toHtml ("" :: String)

boilerplate :: Html () -> Html ()
boilerplate t =
  doctypehtml_ $ do
    head_ $ do
      title_ "YSU Math Problem of the Week Leaderboard"
      style_ css
    body_ $ do
      div_ [id_ "main"] $ do
        h1_ "YSU Problem of the Week Leaderboard"
        t
        p_ $
          small_ $
            a_ [href_ "https://github.com/relrod/pow-leaderboard"] "code source"
        p_ $
          small_ $
            a_ [href_ "https://elrod.me"] "ⓒ 2015 Ricky Elrod"
  where
    css = "* { font-family: sans-serif; }\
          \html, body { margin: 0; padding: 0; }\
          \#main { width: 700px; margin: 0 auto; }\
          \#main table { width: 100%; }\
          \#main table th { text-align: left; }"

main :: IO ()
main = do
  current <- get "http://www.as.ysu.edu/~curmath/pow/pow_web_2014-15/index_winners_2015_spring.html"
  previous <- get "http://www.as.ysu.edu/~curmath/pow/pow_web_2014-15/index_winners_2014_fall.html"
  TLIO.putStrLn . renderText . boilerplate . htmlLeaderboard . processList current $ previous
