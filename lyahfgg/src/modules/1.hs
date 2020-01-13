module Module(exportFunctionFromModule) where

import Data.List
import Data.List(filter)
import Data.List hiding (map)
import qualified Data.List as L

exportFunctionFromModule :: String -> String
exportFunctionFromModule x = x