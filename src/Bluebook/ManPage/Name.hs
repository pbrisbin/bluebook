module Bluebook.ManPage.Name
    ( Name
    , mkName
    , mkNameUnsafe
    , getName
    ) where

import Bluebook.Prelude

import Data.Char (isSpace)
import qualified Data.Text as T
import Web.HttpApiData (FromHttpApiData(..))

newtype Name = Name
    { unName :: Text
    }
    deriving newtype (Eq, Ord, Show)

instance FromHttpApiData Name where
    parseUrlPiece = first pack . mkName

mkName :: Text -> Either String Name
mkName n
    | T.null n = Left "Name cannot be empty"
    | T.any isSpace n = Left "Name cannot contain whitespace"
    | otherwise = Right $ Name n

mkNameUnsafe :: Text -> Name
mkNameUnsafe = Name

getName :: Name -> Text
getName = unName
