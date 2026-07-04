module PythonHS.VM.VMScopeContext (VMScopeContext (..)) where

import qualified Data.Set as Set

data VMScopeContext = VMScopeContext
  { vmScopeContextIsTopLevel :: Bool,
    vmScopeContextGlobalDecls :: Set.Set String
  }
