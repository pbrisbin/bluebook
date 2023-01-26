{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Bluebook.API
    ( run
    ) where

import Bluebook.Prelude

import Bluebook.App
import Bluebook.Handler.Html
import Bluebook.Handler.ManPage.Section
import Bluebook.Handler.Ping
import Bluebook.Handler.Root
import Bluebook.ManPage.Section
import Bluebook.Settings
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Logging
import Servant
import Servant.HTML.Blaze

-- brittany-disable-next-binding

type API
       = QueryParam "q" Text :> Get '[HTML] Html
    :<|> Capture "section" Section :> SectionAPI
    :<|> "ping" :> Get '[HTML] Html

run :: IO ()
run = do
    settings@Settings {..} <- loadSettings
    app <- loadApp settings

    runLoggerLoggingT app
        $ logInfo
        $ "Starting up"
        :# ["port" .= settingsPort, "MANPATH" .= settingsManPath]

    Warp.run settingsPort $ middleware app $ waiApp app

waiApp :: (HasLogger env, HasAppRoot env, HasManPath env) => env -> Application
waiApp = serve api . server

api :: Proxy API
api = Proxy

server :: (HasLogger env, HasAppRoot env, HasManPath env) => env -> Server API
server env = hoistServer api (`runAppT` env) server'

server'
    :: ( MonadIO m
       , MonadLogger m
       , MonadError ServerError m
       , MonadReader env m
       , HasAppRoot env
       , HasManPath env
       )
    => ServerT API m
server' = handleGetRoot :<|> handleSection :<|> handleGetPing

middleware :: HasLogger env => env -> Middleware
middleware = requestLogger
