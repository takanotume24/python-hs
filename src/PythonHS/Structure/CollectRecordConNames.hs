module PythonHS.Structure.CollectRecordConNames (collectRecordConNames) where

import Data.Generics (everything, mkQ)
import Language.Haskell.Exts
  ( ConDecl (..),
    Module (..),
    SrcSpanInfo,
    prettyPrint,
  )

collectRecordConNames :: Module SrcSpanInfo -> [String]
collectRecordConNames m = everything (++) (mkQ [] goRecDecl) m
  where
    goRecDecl :: ConDecl SrcSpanInfo -> [String]
    goRecDecl (RecDecl _ conName _) =
      let name = prettyPrint conName
       in if name `elem` ["Token", "Position", "ScanTokenStepResult"] then [] else [name]
    goRecDecl _ = []
