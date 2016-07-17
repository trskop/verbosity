# ChangeLog / ReleaseNotes


## Version 0.2.2.0

* Relaxed `data-default-class` dependency that allows bilding with version
  0.1.\*.
* Introducing functions:
    * `increment :: Verbosity -> Maybe Verbosity`
    * `increment' :: Verbosity -> Verbosity`

## Version 0.2.1.0

* Introducing optional instance for cereal's `Serialize` type class. Dependency
  on `cereal` package can be enabled using `-fcereal` build flag. (new)
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/verbosity-0.2.1.0>


## Version 0.2.0.0

* Introducing module `Data.Verbosity.Class` which contains definition of
  `HasVerbosity` type class. (new)
* Introducing function `fromInt :: Int -> Maybe Verbosity`. (new)
* Introducing function
  `parse :: (Eq string, IsString string) => string -> Maybe Verbosity`. (new)
* NFData instance, if compiled with `-fdeepseq`, which is the default case.
  (new)
* Depends on [transformers][] package in case [base][] <4.8. (new)
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/verbosity-0.2.0.0>


## Version 0.1.0.0

* First public release.
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/verbosity-0.1.0.0>


[base]:
  http://hackage.haskell.org/package/base
  "base package on Hackage"
[Hackage]:
  http://hackage.haskell.org/
  "HackageDB (or just Hackage) is a collection of releases of Haskell packages."
[transformers]:
  http://hackage.haskell.org/package/transformers
  "transformers package on Hackage"
