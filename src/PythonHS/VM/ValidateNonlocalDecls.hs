module PythonHS.VM.ValidateNonlocalDecls (validateNonlocalDecls) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.VM.ValidateNonlocalDeclsConfig (ValidateNonlocalDeclsConfig (..))

-- | Validate @nonlocal@ declarations against the function parameters and the
-- names captured from enclosing scopes.
validateNonlocalDecls :: ValidateNonlocalDeclsConfig -> Either String ()
validateNonlocalDecls config =
  case filter isInvalid (Map.toList nonlocalNames) of
    [] -> Right ()
    (name, pos) : _ -> Left (errorMessage name pos)
  where
    params = validateNonlocalDeclsParams config
    nonlocalNames = validateNonlocalDeclsNames config
    globalNames = validateNonlocalDeclsGlobalNames config
    locals = validateNonlocalDeclsLocals config

    isInvalid (name, _) =
      name `elem` params
        || Set.member name globalNames
        || Map.notMember name locals

    errorMessage name pos
      | name `elem` params = "Syntax error: name '" ++ name ++ "' is parameter and nonlocal at " ++ showPos pos
      | Set.member name globalNames = "Syntax error: name '" ++ name ++ "' is nonlocal and global at " ++ showPos pos
      | otherwise = "Syntax error: no binding for nonlocal '" ++ name ++ "' found at " ++ showPos pos
