{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Servant.API
import Servant.Client

data Category
  = Programming
  | Misc
  | Dark
  | Pun
  | Spooky
  | Christmas
  deriving (Eq,Show)

type Categories = CommaSep Category

data Lang = CS | DE | EN | ES | FR | PT
  deriving (Eq,Show)

data Flag
  = Nsfw
  | Religious
  | Political
  | Racist
  | Sexist
  | Explicit
  deriving (Eq,Show)

type Flags = CommaSep Flag

newtype CommaSep a = CommaSep [a]
  deriving (Eq,Show)

instance FromHttpApiData a => FromHttpApiData (CommaSep a) where
  parseUrlPiece t = fmap CommaSep $
                    mapM parseUrlPiece $
                    T.splitOn "," t
  parseQueryParam = fmap CommaSep .
                    mapM parseUrlPiece .
                    T.splitOn ","
instance ToHttpApiData a => ToHttpApiData (CommaSep a) where
  toUrlPiece (CommaSep ts) = T.intercalate "," $ map toUrlPiece ts
  toQueryParam (CommaSep ts) = T.intercalate "," $ map toQueryParam ts

newtype EmptyAny a = EmptyAny (CommaSep a)
  deriving (Eq,Show)

instance FromHttpApiData a => FromHttpApiData (EmptyAny a) where
  parseUrlPiece t = case t of
    "Any" -> pure $ EmptyAny (CommaSep [])
    _ -> EmptyAny <$> parseUrlPiece t
instance ToHttpApiData a => ToHttpApiData (EmptyAny a) where
  toUrlPiece (EmptyAny (CommaSep [])) = "Any"
  toUrlPiece (EmptyAny t) = toUrlPiece t

data JokeType = Single | Twopart
  deriving (Eq,Show)

data Range = Range
  { from :: Int
  , to :: Int
  }
  deriving (Eq,Show)

instance FromHttpApiData Range where
  parseQueryParam t = do
    let ts = T.splitOn "-" t
    ints <- mapM parseQueryParam ts
    case ints of
      [f,t] -> pure $ Range f t
      _ -> Left "Wrong number of components in range"

type JokeApi
  = "joke"
  :> Capture "category" Categories
  :> QueryParam "lang" Lang
  :> QueryParam "blacklistFlags" Flags
  :> QueryParam "type" JokeType
  :> QueryParam "contains" Text
  :> QueryParam "idRange" Range
  :> Get '[JSON] Value

api :: Proxy JokeApi
api = Proxy

-- joke :: Categories
--      -> Maybe Lang
--      -> Maybe Flags
--      -> Maybe JokeType
--      -> Maybe Text
--      -> Maybe Range
--      -> ClientM Value
-- joke = client api

main :: IO ()
main = putStrLn "Hello, Haskell!"
