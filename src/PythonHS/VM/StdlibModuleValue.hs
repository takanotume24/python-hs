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
            ModuleAttr {moduleAttrName = "version", moduleAttrValue = StringValue {stringValue = "3.12.16 (python-hs)"}},
            ModuleAttr {moduleAttrName = "version_info", moduleAttrValue = TupleValue {tupleValueItems = [IntValue {intValue = 3}, IntValue {intValue = 12}, IntValue {intValue = 16}, StringValue {stringValue = "final"}, IntValue {intValue = 0}]}},
            ModuleAttr {moduleAttrName = "platform", moduleAttrValue = StringValue {stringValue = "linux"}},
            ModuleAttr {moduleAttrName = "maxsize", moduleAttrValue = IntValue {intValue = 9223372036854775807}},
            ModuleAttr {moduleAttrName = "maxunicode", moduleAttrValue = IntValue {intValue = 1114111}},
            ModuleAttr {moduleAttrName = "byteorder", moduleAttrValue = StringValue {stringValue = "little"}},
            ModuleAttr {moduleAttrName = "hexversion", moduleAttrValue = IntValue {intValue = 50791152}},
            ModuleAttr {moduleAttrName = "executable", moduleAttrValue = StringValue {stringValue = "python-hs"}},
            ModuleAttr {moduleAttrName = "prefix", moduleAttrValue = StringValue {stringValue = "/usr/local"}},
            ModuleAttr {moduleAttrName = "copyright", moduleAttrValue = StringValue {stringValue = "python-hs"}},
            ModuleAttr {moduleAttrName = "dont_write_bytecode", moduleAttrValue = IntValue {intValue = 1}}
          ]
        _ -> []
