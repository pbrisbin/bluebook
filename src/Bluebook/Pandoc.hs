module Bluebook.Pandoc
    ( addHeaderLinks
    , reduceHeaderLevels
    , convertManPageRefs
    , linkBareUrls
    , module X
    ) where

import           Bluebook.Prelude

import           Bluebook.ManPage
import qualified Data.Text                as T
import           Text.Pandoc.Class        as X
import           Text.Pandoc.Definition   as X
import           Text.Pandoc.Error        as X
import           Text.Pandoc.Options      as X
import           Text.Pandoc.Readers.Man  as X
import           Text.Pandoc.Walk         as X
import           Text.Pandoc.Writers.HTML as X

addHeaderLinks :: Block -> Block
addHeaderLinks = linkIdentifiers . addIdentifiers

linkIdentifiers :: Block -> Block
linkIdentifiers = \case
    (Header n attr is) | Just identifier <- getIdentifier attr -> Header
        n
        attr
        [Link nullAttr is ("#" <> identifier, "Link to this section")]
    x -> x

addIdentifiers :: Block -> Block
addIdentifiers = \case
    (Header n attr is) | Just attr' <- addIdentifier is attr ->
        Header n attr' is
    x -> x

getIdentifier :: Attr -> Maybe Text
getIdentifier = \case
    ("", _, _) -> Nothing
    (x, _, _)  -> Just x

addIdentifier :: [Inline] -> Attr -> Maybe Attr
addIdentifier is = \case
    ("", classes, kvs) -> Just (toIdentifier is, classes, kvs)
    _                  -> Nothing

toIdentifier :: [Inline] -> Text
toIdentifier =
    T.filter (`elem` (['-'] <> ['a' .. 'z'] <> ['0' .. '9']))
        . T.toLower
        . T.replace " " "-"
        . T.unwords
        . concatMap inlineText

inlineText :: Inline -> [Text]
inlineText = \case
    Str t          -> [t]
    Emph is        -> concatMap inlineText is
    Underline is   -> concatMap inlineText is
    Strong is      -> concatMap inlineText is
    Strikeout is   -> concatMap inlineText is
    Superscript is -> concatMap inlineText is
    Subscript is   -> concatMap inlineText is
    SmallCaps is   -> concatMap inlineText is
    Quoted _ is    -> concatMap inlineText is
    Cite _ is      -> concatMap inlineText is
    Code _ t       -> [t]
    Link _ is _    -> concatMap inlineText is
    Image _ is _   -> concatMap inlineText is
    Span _ is      -> concatMap inlineText is
    _              -> []

reduceHeaderLevels :: Block -> Block
reduceHeaderLevels = \case
    x@(Header 6 _ _) -> x
    Header n attr is -> Header (n + 1) attr is
    x                -> x

-- |
--
-- @
-- ..., Str "foo(1)", ...
-- ..., Emph/Strong (Str "foo(1)"), ...
-- ..., Emph/Strong "foo", Str "(1)", ...
-- ..., Emph/Strong "foo", Emph/Strong "(1)", ...
-- @
--
convertManPageRefs :: Block -> Block
convertManPageRefs = walkInlines go
  where
    go = \case
        -- emph "foo", "(1)", ...
        (Emph [Str x] : Str y : rest)
            | (Right ref, mP) <- refAndPuncuation $ x <> y
            -> [linkManPage ref] <> maybe [] (\p -> [Str p]) mP <> go rest

        -- strong "foo", "(1)", ...
        (Strong [Str x] : Str y : rest)
            | (Right ref, mP) <- refAndPuncuation $ x <> y
            -> [linkManPage ref] <> maybe [] (\p -> [Str p]) mP <> go rest

        -- "foo", emph "(1)", ...
        (Str x : Emph [Str y] : rest)
            | (Right ref, mP) <- refAndPuncuation $ x <> y
            -> [linkManPage ref] <> maybe [] (\p -> [Str p]) mP <> go rest

        -- "foo", strong "(1)", ...
        (Str x : Strong [Str y] : rest)
            | (Right ref, mP) <- refAndPuncuation $ x <> y
            -> [linkManPage ref] <> maybe [] (\p -> [Str p]) mP <> go rest

        -- strong "foo(1)", ..., ...
        (Strong [Str x] : rest) | (Right ref, mP) <- refAndPuncuation x ->
            [linkManPage ref] <> maybe [] (\p -> [Str p]) mP <> go rest

        -- emph "foo(1)", ..., ...
        (Emph [Str x] : rest) | (Right ref, mP) <- refAndPuncuation x ->
            [linkManPage ref] <> maybe [] (\p -> [Str p]) mP <> go rest

        -- "foo(1)", ..., ...
        (Str x : rest) | (Right ref, mP) <- refAndPuncuation x ->
            [linkManPage ref] <> maybe [] (\p -> [Str p]) mP <> go rest

        -- Skip
        (a : rest) -> [a] <> go rest

        -- Done
        [] -> []

    refAndPuncuation t
        | "." `T.isSuffixOf` t = (manPageFromRef $ T.dropEnd 1 t, Just ".")
        | "," `T.isSuffixOf` t = (manPageFromRef $ T.dropEnd 1 t, Just ",")
        | ":" `T.isSuffixOf` t = (manPageFromRef $ T.dropEnd 1 t, Just ":")
        | ";" `T.isSuffixOf` t = (manPageFromRef $ T.dropEnd 1 t, Just ";")
        | otherwise = (manPageFromRef t, Nothing)

linkManPage :: ManPage -> Inline
linkManPage page = Link
    nullAttr
    [Strong [Str $ manPageToRef page]]
    (manPageUrlPath page, manPageToRef page)

linkBareUrls :: [Inline] -> [Inline]
linkBareUrls = concatMap go
  where
    go :: Inline -> [Inline]
    go = \case
        Str x -> concatMap tryUrl $ T.words x
        x     -> [x]

    tryUrl :: Text -> [Inline]
    tryUrl t
        | "http://" `T.isInfixOf` t = renderLink t
        | "https://" `T.isInfixOf` t = renderLink t
        | otherwise = [Str t]

    renderLink :: Text -> [Inline]
    renderLink t
        | "." `T.isSuffixOf` t = renderLinkAndSuffix "." $ T.dropEnd 1 t
        | "," `T.isSuffixOf` t = renderLinkAndSuffix "." $ T.dropEnd 1 t
        | ":" `T.isSuffixOf` t = renderLinkAndSuffix "." $ T.dropEnd 1 t
        | ";" `T.isSuffixOf` t = renderLinkAndSuffix "." $ T.dropEnd 1 t
        | otherwise = [Link nullAttr [Str t] (t, t)]

    renderLinkAndSuffix :: Text -> Text -> [Inline]
    renderLinkAndSuffix suffix url =
        [Link nullAttr [Str url] (url, url), Str suffix]

walkInlines :: ([Inline] -> [Inline]) -> Block -> Block
walkInlines f = \case
    Plain is -> Plain $ f is
    Para is -> Para $ f is
    LineBlock iss -> LineBlock $ map f iss
    x@CodeBlock{} -> x
    x@RawBlock{} -> x
    BlockQuote bs -> BlockQuote $ map (walkInlines f) bs
    OrderedList x bss -> OrderedList x $ map (map (walkInlines f)) bss
    BulletList bss -> BulletList $ map (map (walkInlines f)) bss
    DefinitionList ibs ->
        DefinitionList $ map (bimap f (map (map (walkInlines f)))) ibs
    Header x y is -> Header x y $ f is
    x@HorizontalRule{} -> x
    x@Table{} -> x
    Div x bs -> Div x $ map (walkInlines f) bs
    x@Null{} -> x
