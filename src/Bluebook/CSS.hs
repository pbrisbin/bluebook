{-# LANGUAGE QuasiQuotes #-}

module Bluebook.CSS
    ( styles
    ) where

import           Bluebook.Prelude

import           Text.Shakespeare.Text (st)

styles :: Text
styles = [st|
body {
    color: #505050;
    font-family: sans-serif;
    margin-left: auto;
    margin-right: auto;
    max-width: 960px;
}

header {
    border-bottom: 1px solid;
}

nav ul {
    margin-bottom: 2em;
    margin-left: 0;
    padding-left: 0;
}

nav li {
    display: inline-block;
    padding-right: 1em;
}

h1, h2, h3, h4, h5, h6, strong {
    color: #A00000;
}

h1 a,
h2 a,
h3 a,
h4 a,
h5 a,
h6 a {
    color: #A00000;
}

h1 a:link,
h2 a:link,
h3 a:link,
h4 a:link,
h5 a:link,
h6 a:link {
    text-decoration: none;
}

h1 a:hover,
h2 a:hover,
h3 a:hover,
h4 a:hover,
h5 a:hover,
h6 a:hover {
    text-decoration: underline;
}

em {
    color: #006000;
    font-style: normal;
    font-weight: 700;
}

/* work around bug in some of our docs */
em strong {
    font-weight: 700;
}

footer {
    font-size: 90%;
    padding-bottom: 1em;
    padding-top: 1em;
    text-align: center;
}
|]
