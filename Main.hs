{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import Control.Lens hiding (children)
import Data.ByteString.Lazy (ByteString)
import Data.List (group, sort)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Lucid
import Network.Wreq hiding (head_)
import Text.Taggy
import Text.Taggy.Lens

table :: [Node] -> Maybe T.Text
table row =  row ^? ix 1 . contents

makeTableIsh :: Response ByteString -> [Maybe T.Text]
makeTableIsh = toListOf $ responseBody . to (decodeUtf8With lenientDecode)
               . html . allNamed (only "tr") . children . to table

processList :: Response ByteString -> [(Int, T.Text)]
processList r =
  let l = [ x | Just x <- makeTableIsh r, x /= "\160" ]
      l' = concatMap (T.splitOn "," . T.replace "." "" . T.replace "\160" "") l
      splitted = fmap T.strip l'
  in reverse . sort . map (length &&& head) . group . sort $ splitted

htmlLeaderboard :: [(Int, T.Text)] -> Html ()
htmlLeaderboard l = do
  table_ $ do
    tr_ $ do
      th_ "Rank"
      th_ "Name"
      th_ "Problems solved"
    mapM_ (\((entries, person), n) -> p entries person n) (zip l ([1..] :: [Integer]))
  where
    p :: Int -> T.Text -> Integer -> Html ()
    p entries person n =
      tr_ $ do
        td_ (toHtml . show $ n)
        td_ (toHtml person)
        td_ (toHtml $ show entries <> " " <> pluralize entries)
    pluralize 1 = "problem"
    pluralize _ = "problems"

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
            a_ [href_ "http://www.as.ysu.edu/~curmath/pow/pow_web_2014-15/index_winners_2015_spring.html"] "information source"
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
  get "http://www.as.ysu.edu/~curmath/pow/pow_web_2014-15/index_winners_2015_spring.html"
  >>= TLIO.putStrLn . renderText . boilerplate . htmlLeaderboard . processList
