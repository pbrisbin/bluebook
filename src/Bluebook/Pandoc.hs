module Bluebook.Pandoc
    ( addHeaderLinks
    , reduceHeaderLevels
    , convertManPageRefs
    , linkBareUrls
    , module X
    ) where

import Bluebook.Prelude

import Bluebook.ManPage
import qualified Data.Text as T
import Text.Pandoc.Class as X
import Text.Pandoc.Definition as X
import Text.Pandoc.Error as X
import Text.Pandoc.Logging as X
import Text.Pandoc.Options as X
import Text.Pandoc.Readers.Man as X
import Text.Pandoc.Readers.Markdown as X
import Text.Pandoc.Walk as X
import Text.Pandoc.Writers.HTML as X
import Text.Pandoc.Writers.Man as X

addHeaderLinks :: Block -> Block
addHeaderLinks = linkIdentifiers . addIdentifiers

linkIdentifiers :: Block -> Block
linkIdentifiers = \case
    (Header n attr inner) | Just identifier <- getIdentifier attr ->
        Header n attr [link inner $ "#" <> identifier]
    x -> x

addIdentifiers :: Block -> Block
addIdentifiers = \case
    (Header n attr inner) | Just attr' <- addIdentifier inner attr ->
        Header n attr' inner
    x -> x

getIdentifier :: Attr -> Maybe Text
getIdentifier = \case
    ("", _, _) -> Nothing
    (x, _, _) -> Just x

addIdentifier :: [Inline] -> Attr -> Maybe Attr
addIdentifier inner = \case
    ("", classes, kvs) -> Just (toIdentifier inner, classes, kvs)
    _ -> Nothing

toIdentifier :: [Inline] -> Text
toIdentifier =
    T.filter (`elem` (['-'] <> ['a' .. 'z'] <> ['0' .. '9']))
        . T.toLower
        . T.replace " " "-"
        . T.unwords
        . concatMap inlineText

inlineText :: Inline -> [Text]
inlineText = \case
    Str t -> [t]
    Code _ t -> [t]
    _ -> []

reduceHeaderLevels :: Block -> Block
reduceHeaderLevels = \case
    Header n attr inner -> Header (min 6 (n + 1)) attr inner
    x -> x

convertManPageRefs :: [Inline] -> [Inline]
convertManPageRefs = \case
    (Emph [Str x] : Str y : rest)
        | Right inner <- parse (Emph . pure) $ x <> y -> inner
        <> convertManPageRefs rest

    (Strong [Str x] : Str y : rest)
        | Right inner <- parse (Strong . pure) $ x <> y -> inner
        <> convertManPageRefs rest

    (Str x : Emph [Str y] : rest)
        | Right inner <- parse (Emph . pure) $ x <> y -> inner
        <> convertManPageRefs rest

    (Str x : Strong [Str y] : rest)
        | Right inner <- parse (Strong . pure) $ x <> y -> inner
        <> convertManPageRefs rest

    (Str x : rest) | Right inner <- parse id x ->
        inner <> convertManPageRefs rest

    (a : rest) -> a : convertManPageRefs rest

    [] -> []
    where parse f = withPunctuation $ fmap (f . linkManPage) . manPageFromRef

linkManPage :: ManPage -> Inline
linkManPage page = link [Str $ manPageToRef page] $ "/" <> manPageUrlPath page

linkBareUrls :: [Inline] -> [Inline]
linkBareUrls = concatMap $ \case
    Str x -> concatMap renderIfLink $ T.words x
    x -> [x]
  where
    renderIfLink x = fromMaybe [Str x] $ hush $ withPunctuation tryLink x
    tryLink t = link [Str t] t
        <$ guard ("http://" `T.isInfixOf` t || "https://" `T.isInfixOf` t)

withPunctuation
    :: (Text -> Either String Inline)
    -- ^ Function to apply to de-punctuated text
    -> Text
    -- ^ Text that may have puncuation
    -> Either String [Inline]
    -- ^ Result with trailing punctuation re-added if necessary
withPunctuation f x = do
    (t, c) <- note "No last character" $ T.unsnoc x
    if c `elem` puncuation then (: [Str $ pack [c]]) <$> f t else pure <$> f x
  where
    puncuation :: String
    puncuation = ".,:;"

link :: [Inline] -> Text -> Inline
link inner url = Link nullAttr inner (url, "")
