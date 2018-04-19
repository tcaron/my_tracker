{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Clay as C
import Control.Monad.Trans (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.FromRow (FromRow, fromRow, field)
import GHC.Generics (Generic)
import Lucid
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty (middleware, scotty, get, json, html, param, ActionM)

dbName :: String
dbName = "mytracker.db"

data Tracker = Tracker 
  { site :: T.Text
  , page :: T.Text 
  , hits :: Int 
  } deriving (Show, Generic)


instance ToJSON Tracker 

getAllTracker :: IO [Tracker] 
getAllTracker = do  
  conn <- SQL.open dbName
  let query = "SELECT site, page, hits FROM tracker" 
  results <- SQL.query_ conn query :: IO [Tracker]
  return results 

getHitsText :: Tracker -> T.Text
getHitsText = T.pack . show . hits

instance FromRow Tracker where
  fromRow = Tracker <$> field <*> field <*> field

mkpage :: Lucid.Html () -> Lucid.Html () -> L.Text
mkpage titleStr page = renderText $ do
  doctype_
  html_ $ do
    header_ $ do
      title_ titleStr
    body_ page

divCss = C.div C.# C.byClass "divCss" C.? do
  C.backgroundColor  C.beige
  C.border           C.solid (C.px 1) C.black

homeRoute :: [Tracker] -> Lucid.Html ()
homeRoute trackers = do
  h1_ "Trackers"
  a_ [ href_ "/export" ] "Export"
  table_[class_"divCss"] $ mapM_ (td_ . toHtml . format ) trackers
  where format trackers  = T.concat [site trackers,",",site trackers,",",getHitsText trackers]


{-renderTracker :: T.Text -> ActionM ()
renderTracker t = print t -}

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  middleware simpleCors
  
  get "/" $ do
    trackers <- liftIO getAllTracker
    html $ mkpage " Home" $ homeRoute trackers
  
  get "/export" $ do
    trackers <- liftIO getAllTracker
    json trackers
  
  {- get "/tracker" $ do renderTracker "test" -}

