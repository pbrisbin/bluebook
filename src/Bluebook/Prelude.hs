module Bluebook.Prelude
    ( module X
    ) where

import Control.Error.Util as X (hush, note)
import Control.Monad.Error.Class as X (MonadError)
import Control.Monad.IO.Unlift as X (MonadUnliftIO(..))
import Data.List.Extra as X (dropPrefix, dropSuffix, stripPrefix, stripSuffix)
import Data.Text as X (pack, unpack)
import Data.Traversable as X (for)
import Lens.Micro.Platform as X (Lens', lens, view, (^.))
import Relude as X hiding (readFile')
