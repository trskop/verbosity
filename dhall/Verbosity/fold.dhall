  let
    Verbosity = ../Verbosity.dhall

in let
    foldVerbosity
      : ∀(r : Type)
      → ∀ (handler
            : { Silent : {} → r
              , Normal : {} → r
              , Verbose : {} → r
              , Annoying : {} → r
              }
          )
      → ∀(verbosity : Verbosity)
      → r

      = λ(r : Type)
      → λ(handler
            : { Silent : {} → r
              , Normal : {} → r
              , Verbose : {} → r
              , Annoying : {} → r
              }
          )
      → λ(verbosity : Verbosity)
      → merge handler verbosity

in
    foldVerbosity
