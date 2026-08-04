module PythonHS.Evaluator.ModuleAttr (ModuleAttr (..)) where

data ModuleAttr value = ModuleAttr
  { moduleAttrName :: String,
    moduleAttrValue :: value
  }
  deriving (Eq, Show)
