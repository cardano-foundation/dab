let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/pending/haskell-ci.dhall

in    haskellCi.generalCi
        haskellCi.matrixSteps
        ( Some
            { ghc =
              [ haskellCi.GHC.GHC8105
              , haskellCi.GHC.GHC901
              ]
            , cabal = [ haskellCi.Cabal.Cabal34 ]
            }
        )
    : haskellCi.CI.Type
