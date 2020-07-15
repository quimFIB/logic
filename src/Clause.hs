-- |

module Clause where

import LogicSymbols
import Data.Set
import QfreeForm

type Clause = Set Literal

-- form2clauses :: FormF Form -> Set Clause
-- form2clauses (Ltr l) = singleton (Ltr l)
-- form2clauses (And s0 s1) =
