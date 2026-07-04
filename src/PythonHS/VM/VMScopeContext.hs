module PythonHS.VM.VMScopeContext (VMScopeContext (..)) where

import Data.Set qualified as Set

data VMScopeContext = VMScopeContext
  { vmScopeContextIsTopLevel :: Bool,
    vmScopeContextGlobalDecls :: Set.Set String
  }
