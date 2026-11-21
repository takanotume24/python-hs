module PythonHS.Structure.DetectFromModule (detectFromModule) where

import Data.List (isInfixOf, nub)
import Language.Haskell.Exts
  ( ConDecl (..),
    Decl (..),
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
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation (..))
import PythonHS.Structure.ViolationCategory (ViolationCategory (..))

detectFromModule :: DetectFromModuleConfig -> [PositionalArgViolation]
detectFromModule fromConfig =
  let config = detectFromModuleModuleConfig fromConfig
      m = moduleAst config
      path = moduleFilePath config
      skipFunDecl = "/VM/" `isInfixOf` path

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
      goC RecDecl {} = []

      goM (Match l _ ps _ _)
        | skipFunDecl = []
        | length ps >= 4,
          not (isSimpleRecursivePattern ps),
          isSrcSpanInfo l =
            [mkViolation path l FunDeclCategory (unwords (map prettyPrint (take 3 ps)))]
        | otherwise = []
      goM (InfixMatch l p _ ps _ _)
        | skipFunDecl = []
        | length allPats >= 4,
          not (isSimpleRecursivePattern allPats),
          isSrcSpanInfo l =
            [mkViolation path l FunDeclCategory (unwords (map prettyPrint (take 3 allPats)))]
        | otherwise = []
        where
          allPats = p : ps

      isSimpleRecursivePattern :: [Pat SrcSpanInfo] -> Bool
      isSimpleRecursivePattern ps
        | any isListOrConsPattern ps = all isSimplePattern (filter (not . isListOrConsPattern) ps)
        | otherwise = False

      isListOrConsPattern :: Pat SrcSpanInfo -> Bool
      isListOrConsPattern (PList _ []) = True
      isListOrConsPattern (PInfixApp _ _ op _) = prettyPrint op == "(:)"
      isListOrConsPattern (PParen _ p) = isListOrConsPattern p
      isListOrConsPattern _ = False

      isSimplePattern :: Pat SrcSpanInfo -> Bool
      isSimplePattern (PVar _ _) = True
      isSimplePattern (PWildCard _) = True
      isSimplePattern (PParen _ p) = isSimplePattern p
      isSimplePattern _ = False

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
        Module _ _ _ _ decls -> nub (concatMap goDecl decls)
        _ -> []
