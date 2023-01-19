{-# LANGUAGE QuasiQuotes #-}

module Bluebook.CSS
    ( styles
    ) where

import Bluebook.Prelude

import Text.Shakespeare.Text (st)

styles :: Text
styles = [st|
body {
    font-family: sans-serif;
    margin-left: auto;
    margin-right: auto;
    max-width: 960px;
}

header {
    border-bottom: 1px solid #A00000;
}

h1, h2, h3, h4, h5, h6, strong {
    color: #A00000;
}

em {
    color: #006000;
    font-style: normal;
    font-weight: bold;
}
|]
