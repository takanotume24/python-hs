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
import PythonHS.Structure.DetectModuleConfig (DetectModuleConfig (..))
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation (..))
import PythonHS.Structure.ViolationCategory (ViolationCategory (..))

detectFromModule :: [String] -> DetectModuleConfig -> [PositionalArgViolation]
detectFromModule recordConNames config =
  let m = moduleAst config
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
        where allPats = p : ps

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
          Just (conName, argCount, l)
            | argCount >= 1
            , prettyPrint conName `elem` recordConNames
            , isSrcSpanInfo l ->
                [ mkViolation path l PositionalRecordConCategory (prettyPrint conName ++ " ...")
                ]
          _ -> []
        where
          extractConApp (Con l conName) = Just (conName, 0 :: Int, l)
          extractConApp (App _ func _) =
            case extractConApp func of
              Just (cn, n, l) -> Just (cn, n + 1, l)
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

