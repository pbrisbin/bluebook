module Bluebook.Pandoc
    ( addHeaderLinks
    , reduceHeaderLevels
    , convertManPageRefs
    , module X
    ) where

import Bluebook.Prelude

import Bluebook.ManPage
import qualified Data.Text as T
import Text.Pandoc.Class as X
import Text.Pandoc.Definition as X
import Text.Pandoc.Error as X
import Text.Pandoc.Options as X
import Text.Pandoc.Readers.Man as X
import Text.Pandoc.Walk as X
import Text.Pandoc.Writers.HTML as X

addHeaderLinks :: Block -> Block
addHeaderLinks = linkIdentifiers . addIdentifiers

linkIdentifiers :: Block -> Block
linkIdentifiers = id -- TODO

addIdentifiers :: Block -> Block
addIdentifiers = \case
    (Header n attr is) | Just attr' <- addIdentifier is attr ->
        Header n attr' is
    x -> x

addIdentifier :: [Inline] -> Attr -> Maybe Attr
addIdentifier is = \case
    ("", classes, kvs) -> Just (toIdentifier is, classes, kvs)
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
    Emph is -> concatMap inlineText is
    Underline is -> concatMap inlineText is
    Strong is -> concatMap inlineText is
    Strikeout is -> concatMap inlineText is
    Superscript is -> concatMap inlineText is
    Subscript is -> concatMap inlineText is
    SmallCaps is -> concatMap inlineText is
    Quoted _ is -> concatMap inlineText is
    Cite _ is -> concatMap inlineText is
    Code _ t -> [t]
    Link _ is _ -> concatMap inlineText is
    Image _ is _ -> concatMap inlineText is
    Span _ is -> concatMap inlineText is
    _ -> []

reduceHeaderLevels :: Block -> Block
reduceHeaderLevels = \case
    x@(Header 6 _ _) -> x
    Header n attr is -> Header (n + 1) attr is
    x -> x

-- |
--
-- @
-- ..., Str "foo(1)", ...
-- ..., Emph/Strong (Str "foo(1)"), ...
-- ..., Emph/Strong "foo", Str "(1)", ...
-- ..., Emph/String "foo", Emph/Strong "(1)", ...
-- @
--
convertManPageRefs :: Block -> Block
convertManPageRefs = \case
    Plain is -> Plain $ convertInlines is
    Para is -> Para $ convertInlines is
    LineBlock iss -> LineBlock $ map convertInlines iss
    x@CodeBlock{} -> x
    x@RawBlock{} -> x
    BlockQuote bs -> BlockQuote $ map convertManPageRefs bs
    OrderedList x bss -> OrderedList x $ map (map convertManPageRefs) bss
    BulletList bss -> BulletList $ map (map convertManPageRefs) bss
    DefinitionList ibs -> DefinitionList
        $ map (bimap convertInlines (map (map convertManPageRefs))) ibs
    Header x y is -> Header x y $ convertInlines is
    x@HorizontalRule{} -> x
    x@Table{} -> x
    Div x bs -> Div x $ map convertManPageRefs bs
    x@Null{} -> x

convertInlines :: [Inline] -> [Inline]
convertInlines = \case
    -- emph "foo", "(1)", ...
    (Emph [Str x] : Str y : rest)
        | (Right ref, mP) <- refAndPuncuation $ x <> y
        -> [linkManPage ref]
            <> maybe [] (\p -> [Str p]) mP
            <> convertInlines rest

    -- strong "foo", "(1)", ...
    (Strong [Str x] : Str y : rest)
        | (Right ref, mP) <- refAndPuncuation $ x <> y
        -> [linkManPage ref]
            <> maybe [] (\p -> [Str p]) mP
            <> convertInlines rest

    -- "foo", emph "(1)", ...
    (Str x : Emph [Str y] : rest)
        | (Right ref, mP) <- refAndPuncuation $ x <> y
        -> [linkManPage ref]
            <> maybe [] (\p -> [Str p]) mP
            <> convertInlines rest

    -- "foo", strong "(1)", ...
    (Str x : Strong [Str y] : rest)
        | (Right ref, mP) <- refAndPuncuation $ x <> y
        -> [linkManPage ref]
            <> maybe [] (\p -> [Str p]) mP
            <> convertInlines rest

    -- "foo(1)", ..., ...
    (Str x : rest) | (Right ref, mP) <- refAndPuncuation x ->
        [linkManPage ref] <> maybe [] (\p -> [Str p]) mP <> convertInlines rest

    -- Skip
    (a : rest) -> [a] <> convertInlines rest

    -- Done
    [] -> []
  where
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
