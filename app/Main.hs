{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main
    ( main
    ) where

import Bluebook.Prelude

import Bluebook.Handler.Html
import Bluebook.Handler.ManPage.Section
import Bluebook.Handler.Ping
import Bluebook.Handler.Root
import Bluebook.ManPage.Section
import Bluebook.Settings
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import Servant.HTML.Blaze

-- brittany-disable-next-binding

type API
       = QueryParam "q" Text :> Get '[HTML] Html
    :<|> Capture "section" Section :> SectionAPI
    :<|> "ping" :> Get '[HTML] Html

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    settings@Settings {..} <- loadSettings
    putStrLn $ "Listening on port " <> show settingsPort
    run settingsPort $ middleware $ app settings

app :: Settings -> Application
app = serve api . server

api :: Proxy API
api = Proxy

server :: Settings -> Server API
server settings =
    handleGetRoot settings :<|> handleSection settings :<|> handleGetPing

middleware :: Application -> Application
middleware = logStdout
