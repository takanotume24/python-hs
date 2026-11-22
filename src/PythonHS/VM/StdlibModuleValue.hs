module PythonHS.VM.StdlibModuleValue (stdlibModuleValue) where

import PythonHS.Evaluator.ModuleAttr (ModuleAttr (..))
import PythonHS.Evaluator.Value (Value (..))

-- | Build the module value bound by @import <name>@. Only @sys@ needs
-- pre-populated marker attributes; other modules resolve their members through
-- builtin dispatch.
stdlibModuleValue :: String -> Value
stdlibModuleValue moduleName =
  ModuleValue
    { moduleValueName = moduleName,
      moduleValueAttrs = moduleAttrs
    }
  where
    moduleAttrs =
      case moduleName of
        "sys" ->
          [ ModuleAttr {moduleAttrName = "argv", moduleAttrValue = ListValue {listValueItems = []}},
            ModuleAttr {moduleAttrName = "path", moduleAttrValue = ListValue {listValueItems = [StringValue {stringValue = "."}]}},
            ModuleAttr {moduleAttrName = "version", moduleAttrValue = StringValue {stringValue = "python-hs 0.1.0"}},
            ModuleAttr {moduleAttrName = "platform", moduleAttrValue = StringValue {stringValue = "python-hs"}}
          ]
        _ -> []
