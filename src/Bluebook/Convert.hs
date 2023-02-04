module Bluebook.Convert
    ( PageDetails(..)
    , pageDetails
    , pageDetailsFromPath
    , man2Html

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
import System.FilePath (splitExtension, takeBaseName)
import Text.Blaze.Html (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Html (class_)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Readers.Man
import Text.Pandoc.Walk
import Text.Pandoc.Writers.HTML

data PageDetails = PageDetails
    { name :: Text
    , date :: Text
    , section :: Int
    , sectionName :: Text
    , title :: Text
    }

pageDetails :: Int -> Text -> Text -> PageDetails
pageDetails section name date = PageDetails { .. }
  where
    sectionName = case section of
        1 -> "User Commands"
        2 -> "System Calls"
        3 -> "Library Calls"
        4 -> "Devices"
        5 -> "Files"
        6 -> "Games"
        7 -> "Overviews, Conventions, and Miscellaneous"
        8 -> "System Management Commands"
        _ -> "User Commands"
    title = name <> "(" <> show section <> ")"

-- | Infer details from @manN/foo.N.html@, used for errors
pageDetailsFromPath :: FilePath -> PageDetails
pageDetailsFromPath path = pageDetails section (T.toUpper $ pack name) "-"
  where
    (name, n) = splitExtension $ takeBaseName path
    section = fromMaybe 1 $ readMaybe n

man2Html :: MonadIO m => Maybe Manifest -> Text -> m (Html, PageDetails)
man2Html mManifest body = runPandoc $ do
    doc <- readMan def body
    content <-
        writeHtml5 def
        $ convertRefs
        $ walk addHeaderLinks
        $ walk reduceHeaderLevels
        $ walk linkBareUrls doc

    let details = lookupDetails doc
        html = Html.section ! Html.class_ "man-page" $ do
            Html.header $ Html.ul $ do
                Html.li $ Html.toHtml $ title details
                Html.li $ Html.toHtml $ title details

            content

            Html.footer $ Html.ul $ do
                Html.li $ toHtml $ sectionName details
                Html.li $ toHtml $ date details
                Html.li $ toHtml $ title details

    pure (html, details)
  where
    convertRefs = case mManifest of
        Nothing -> id
        Just m -> walk (convertManPageRefs m)

lookupDetails :: Pandoc -> PageDetails
lookupDetails (Pandoc meta _) = pageDetails
    (fromMaybe 1 $ readMaybe . unpack . metaText =<< lookupMeta "section" meta)
    (inlineTexts $ docTitle meta)
    (inlineTexts $ docDate meta)

metaText :: MetaValue -> Text
metaText = \case
    MetaInlines is -> inlineTexts is
    _ -> ""

inlineTexts :: [Inline] -> Text
inlineTexts = mconcat . concatMap inlineText

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
    Space -> [" "]
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
