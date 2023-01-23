module Bluebook.ManPage.Name
    ( Name
    , mkName
    , getName
    ) where

import Bluebook.Prelude

import Data.Char (isSpace)
import qualified Data.Text as T

newtype Name = Name
    { unName :: Text
    }
    deriving newtype (Eq, Ord, Show)

mkName :: Text -> Either String Name
mkName n
    | T.null n = Left "Name cannot be empty"
    | T.any isSpace n = Left "Name cannot contain whitespace"
    | otherwise = Right $ Name n

getName :: Name -> Text
getName = unName
