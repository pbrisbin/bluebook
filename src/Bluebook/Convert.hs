module Bluebook.Convert
    ( manPage2Html

    -- * Exported for testing
    , addHeaderLinks
    , reduceHeaderLevels
    , convertManPageRefs
    , linkBareUrls
    ) where

import Bluebook.Prelude

import Bluebook.Error
import Bluebook.Manifest (Manifest)
import qualified Bluebook.Manifest as Manifest
import Bluebook.ManPage (ManPage)
import qualified Bluebook.ManPage as ManPage
import qualified Data.Text as T
import Text.Blaze.Html (Html)
import Text.Pandoc.Definition as Pandoc
import Text.Pandoc.Options as Pandoc
import Text.Pandoc.Readers.Man as Pandoc
import Text.Pandoc.Walk as Pandoc
import Text.Pandoc.Writers.HTML as Pandoc

manPage2Html :: MonadIO m => Manifest -> Text -> m Html
manPage2Html m body = runPandoc $ do
    doc <- Pandoc.readMan Pandoc.def body
    Pandoc.writeHtml5 Pandoc.def
        $ Pandoc.walk addHeaderLinks
        $ Pandoc.walk reduceHeaderLevels
        $ Pandoc.walk (convertManPageRefs m)
        $ Pandoc.walk linkBareUrls doc

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

convertManPageRefs :: Manifest -> [Inline] -> [Inline]
convertManPageRefs m = \case
    (Emph [Str x] : Str y : rest)
        | Right inner <- parse (Emph . pure) $ x <> y
        -> inner <> convertManPageRefs m rest

    (Strong [Str x] : Str y : rest)
        | Right inner <- parse (Strong . pure) $ x <> y
        -> inner <> convertManPageRefs m rest

    (Str x : Emph [Str y] : rest)
        | Right inner <- parse (Emph . pure) $ x <> y
        -> inner <> convertManPageRefs m rest

    (Str x : Strong [Str y] : rest)
        | Right inner <- parse (Strong . pure) $ x <> y
        -> inner <> convertManPageRefs m rest

    (Str x : rest) | Right inner <- parse id x ->
        inner <> convertManPageRefs m rest

    (a : rest) -> a : convertManPageRefs m rest

    [] -> []
    where parse f = withPunctuation $ fmap (f . linkManPage) . manPageFromRef m

manPageFromRef :: Manifest -> Text -> Either String ManPage
manPageFromRef m =
    note "Referenced man-page does not exist" . (`Manifest.lookupRef` m)

linkManPage :: ManPage -> Inline
linkManPage page = link [Str $ ManPage.ref page] $ ManPage.url page

linkBareUrls :: [Inline] -> [Inline]
linkBareUrls = concatMap $ \case
    Str x -> concatMap renderIfLink $ T.words x
    x -> [x]
  where
    renderIfLink x = fromMaybe [Str x] $ hush $ withPunctuation tryLink x

    tryLink :: Text -> Either String Inline
    tryLink t
        | Just t' <- T.stripPrefix "<" =<< T.stripSuffix ">" t = tryLink t'
        | Just t' <- T.stripPrefix "(" =<< T.stripSuffix ")" t = tryLink t'
        | Just t' <- T.stripPrefix "[" =<< T.stripSuffix "]" t = tryLink t'
        | "http://" `T.isInfixOf` t = Right $ link [Str t] t
        | "https://" `T.isInfixOf` t = Right $ link [Str t] t
        | otherwise = Left "Not a link"

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
