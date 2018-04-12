module Module(sumIt) where -- exporting only sumIt

import Data.Char hiding (toLower) -- importing only one function

import Prelude

import Data.List
import qualified Data.Set as Set

sumIt x y = x + y

const42 = 42

-- compilation process
-- синтаксический разбор
-- проверка типов
-- de-sugar
-- code gen
--  - Core -> STG machine
--  - STG machine -> C--
--  - C-- -> Generate code for rarget platform or LLVM