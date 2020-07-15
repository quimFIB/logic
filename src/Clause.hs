-- |

module Clause where

import LogicSymbols
import Data.Set

newtype Clause = Set (Literal)
