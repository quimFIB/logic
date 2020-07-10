-- |

module UniSet where

import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import qualified PropSymbols as PS

data UniSet = UniSet {vSet :: MultiSet.MultiSet PS.Var, eqSet :: Set.Set PS.Equal}
