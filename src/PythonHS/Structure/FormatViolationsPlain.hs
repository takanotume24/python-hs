module PythonHS.Structure.FormatViolationsPlain (formatViolationsPlain) where

import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation (..))
import PythonHS.Structure.ViolationCategory (ViolationCategory (..))

formatViolationsPlain :: [PositionalArgViolation] -> String
formatViolationsPlain violations = unlines (map formatOne violations)
  where
    formatOne v =
      filePath v
        ++ ":"
        ++ show (line v)
        ++ ":"
        ++ show (column v)
        ++ " ["
        ++ categoryName (category v)
        ++ "] "
        ++ snippet v

    categoryName DataConCategory = "data_constructor"
    categoryName FunDeclCategory = "function_declaration"
    categoryName TupleCategory = "tuple"
