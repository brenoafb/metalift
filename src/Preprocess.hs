module Preprocess where

import Language.Haskell.TH
import Data.Generics

type FunctionData = [Name]

getLiftedFunctionNames :: [Dec] -> FunctionData
getLiftedFunctionNames decs = map getName sigs
  where sigs = getSigs decs
        getName (SigD n _) = n

getSigs :: [Dec] -> [Dec]
getSigs = everything (++) ([] `mkQ` sig)
  where sig d@(SigD _ _) = [d]
        sig _ = []

-- getArity :: Type -> Int
-- getArity (AppT t1 t2) = 1 + getArity t1
-- getArity _ = 0
