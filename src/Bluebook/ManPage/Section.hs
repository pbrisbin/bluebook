module Bluebook.ManPage.Section
    ( Section
    , sectionFromRef
    , sectionFromSuffix
    , sectionFromPath
    , sectionNumber
    , sectionRef
    , sectionSuffix
    , sectionPath
    ) where

import Bluebook.Prelude

import Web.HttpApiData (FromHttpApiData(..))

data Section
    = Section1
    | Section2
    | Section3
    | Section4
    | Section5
    | Section6
    | Section7
    | Section8
    deriving stock (Eq, Ord, Show, Bounded, Enum)

instance FromHttpApiData Section where
    parseUrlPiece = first pack . sectionFromPath . unpack

sectionFromRef :: Text -> Either String Section
sectionFromRef = \case
    "(1)" -> Right Section1
    "(2)" -> Right Section2
    "(3)" -> Right Section3
    "(4)" -> Right Section4
    "(5)" -> Right Section5
    "(6)" -> Right Section6
    "(7)" -> Right Section7
    "(8)" -> Right Section8
    x -> Left $ "Invalid section reference: " <> show x

sectionFromSuffix :: String -> Either String Section
sectionFromSuffix = \case
    ".1" -> Right Section1
    ".2" -> Right Section2
    ".3" -> Right Section3
    ".4" -> Right Section4
    ".5" -> Right Section5
    ".6" -> Right Section6
    ".7" -> Right Section7
    ".8" -> Right Section8
    x -> Left $ "Invalid section suffix: " <> show x

sectionFromPath :: FilePath -> Either String Section
sectionFromPath = \case
    "man1" -> Right Section1
    "man2" -> Right Section2
    "man3" -> Right Section3
    "man4" -> Right Section4
    "man5" -> Right Section5
    "man6" -> Right Section6
    "man7" -> Right Section7
    "man8" -> Right Section8
    x -> Left $ "Invalid section suffix: " <> show x

sectionNumber :: Section -> Int
sectionNumber = (+ 1) . fromEnum

sectionRef :: Section -> Text
sectionRef = ("(" <>) . (<> ")") . pack . show . sectionNumber

sectionSuffix :: Section -> String
sectionSuffix = ("." <>) . show . sectionNumber

sectionPath :: Section -> FilePath
sectionPath = ("man" <>) . show . sectionNumber
