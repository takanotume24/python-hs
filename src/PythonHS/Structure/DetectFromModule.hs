module PythonHS.Structure.DetectFromModule (detectFromModule) where

import Data.Generics (everything, mkQ)
import Data.List (nub)
import Language.Haskell.Exts
  ( ConDecl (..),
    Decl (..),
    Exp (..),
    Match (..),
    Module (..),
    Pat (..),
    QualConDecl (..),
    SrcSpan (..),
    SrcSpanInfo (..),
    prettyPrint,
  )
import PythonHS.Structure.DetectFromModuleConfig (DetectFromModuleConfig (..))
import PythonHS.Structure.DetectModuleConfig (DetectModuleConfig (..))
import PythonHS.Structure.ExtractConAppResult (ExtractConAppResult (..))
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation (..))
import PythonHS.Structure.ViolationCategory (ViolationCategory (..))

detectFromModule :: DetectFromModuleConfig -> [PositionalArgViolation]
detectFromModule fromConfig =
  let recordConNames = detectFromModuleRecordConNames fromConfig
      config = detectFromModuleModuleConfig fromConfig
      m = moduleAst config
      path = moduleFilePath config

      goDecl (DataDecl _ _ _ _ qs _) = concatMap goQ qs
      goDecl (FunBind _ ms) = concatMap goM ms
      goDecl _ = []

      goQ (QualConDecl _ _ _ c) = goC c

      goC (ConDecl l n types)
        | null types = []
        | isSrcSpanInfo l = [mkViolation path l DataConCategory (prettyPrint n)]
        | otherwise = []
      goC (InfixConDecl l n1 _ n2)
        | isSrcSpanInfo l = [mkViolation path l DataConCategory (prettyPrint n1 ++ " " ++ prettyPrint n2)]
        | otherwise = []
      goC (RecDecl _ _ _) = []

      goM (Match l _ ps _ _)
        | length ps >= 2 && isSrcSpanInfo l = [mkViolation path l FunDeclCategory (unwords (map prettyPrint (take 3 ps)))]
        | otherwise = []
      goM (InfixMatch l p _ ps _ _)
        | length allPats >= 2 && isSrcSpanInfo l = [mkViolation path l FunDeclCategory (unwords (map prettyPrint (take 3 allPats)))]
        | otherwise = []
        where
          allPats = p : ps

      goTupleExp (Tuple l _ es)
        | length es >= 2 && isSrcSpanInfo l = [mkViolation path l TupleCategory ("(" ++ unwords (map prettyPrint (take 3 es)) ++ ")")]
        | otherwise = []
      goTupleExp _ = []

      goTuplePat (PTuple l _ ps)
        | length ps >= 2 && isSrcSpanInfo l = [mkViolation path l TupleCategory ("(" ++ unwords (map prettyPrint (take 3 ps)) ++ ")")]
        | otherwise = []
      goTuplePat _ = []

      goPositionalRecordConApp e =
        case extractConApp e of
          Just result
            | extractConAppResultCount result >= 1,
              prettyPrint (extractConAppResultName result) `elem` recordConNames,
              isSrcSpanInfo (extractConAppResultSpan result) ->
                [ mkViolation path (extractConAppResultSpan result) PositionalRecordConCategory (prettyPrint (extractConAppResultName result) ++ " ...")
                ]
          _ -> []
        where
          extractConApp (Con l conName) = Just (ExtractConAppResult {extractConAppResultName = conName, extractConAppResultCount = 0, extractConAppResultSpan = l})
          extractConApp (App _ func _) =
            case extractConApp func of
              Just result -> Just (ExtractConAppResult {extractConAppResultName = extractConAppResultName result, extractConAppResultCount = extractConAppResultCount result + 1, extractConAppResultSpan = extractConAppResultSpan result})
              Nothing -> Nothing
          extractConApp _ = Nothing

      isSrcSpanInfo (SrcSpanInfo (SrcSpan _ line _ _ _) _) = line > 0

      mkViolation p (SrcSpanInfo (SrcSpan _ line col _ _) _) cat snip =
        PositionalArgViolation
          { filePath = p,
            line = line,
            column = col,
            category = cat,
            snippet = snip
          }
   in case m of
        Module _ _ _ _ decls ->
          nub $
            concatMap goDecl decls
              ++ everything (++) (mkQ [] goTupleExp) m
              ++ everything (++) (mkQ [] goTuplePat) m
              ++ everything (++) (mkQ [] goPositionalRecordConApp) m
        _ -> []
