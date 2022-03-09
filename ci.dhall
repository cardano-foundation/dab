let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/pending/haskell-ci.dhall

in    haskellCi.generalCi
        haskellCi.matrixSteps
        ( Some
            { ghc =
              [ haskellCi.GHC.GHC8107 ]
            , cabal = [ haskellCi.Cabal.Cabal34 ]
            }
        )
    : haskellCi.CI.Type
