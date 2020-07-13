-- |

module Clause where

import Form
import Data.Set

newtype Clause = Set (Literal Atom)
