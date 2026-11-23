module HsLint.Ghc2024LikeExtensions (ghc2024LikeExtensions) where

import Language.Haskell.Exts (KnownExtension (..))

ghc2024LikeExtensions :: [KnownExtension]
ghc2024LikeExtensions =
  [ ImportQualifiedPost,
    NamedFieldPuns,
    OverloadedStrings,
    RecordWildCards,
    LambdaCase,
    TupleSections,
    BinaryLiterals,
    MultiWayIf,
    GADTs,
    RankNTypes,
    ScopedTypeVariables,
    TypeApplications,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    TypeOperators,
    ExistentialQuantification,
    DataKinds,
    PolyKinds,
    KindSignatures,
    DefaultSignatures,
    DeriveFunctor,
    DeriveGeneric,
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    InstanceSigs,
    ConstraintKinds,
    TypeFamilies,
    BangPatterns,
    PostfixOperators,
    PatternGuards,
    TypeSynonymInstances,
    MonoLocalBinds
  ]
