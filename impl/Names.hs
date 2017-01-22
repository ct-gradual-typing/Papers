module Names where

import Unbound.LocallyNameless hiding (comp)
import Unbound.LocallyNameless.Alpha     

n2s :: Name a -> String
n2s = name2String
