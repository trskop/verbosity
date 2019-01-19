# ChangeLog / ReleaseNotes


## Version 0.3.0.0

* Optional instances for `JoinSemiLattice`, `MeetSemiLattice`,
  `BoundedJoinSemiLattice`, `BoundedMeetSemiLattice`, `Lattice`, and
  `BoundedLattice`. Dependency on `lattices` package can be enabled using
  `-flattices` build flag. (**new**)
* Optional instances for `Dhall.Interpret` `Dhall.Inject` type classes.
  Dependency on `dhall` package can be enabled using `-fdhall` build flag.
  (**new**)
* Updated documentation to include example that uses `generic-lens` to define
  `HasVerbosity` instances. (**minor change**)
* Dropped support for GHC \< 8.  As a consequence `Data`, `Generic`, and
  `Typeable` are always derived.  The last one is implied by the fact that GHC
  now always deririves it. (**breaking change**)


## Version 0.2.3.0

* Introducing function
  `modifyVerbosity :: HasVerbosity s => (Verbosity -> Verbosity) -> s -> s`
  (**new**)
* Introducing optional instance for safecopy's `SafeCopy` type class. Dependency
  on `safecopy` package can be enabled using `-fsafecopy` build flag. (**new**)
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/verbosity-0.2.3.0>


## Version 0.2.2.0

* Relaxed `data-default-class` dependency that allows bilding with version
  0.1.\*. (**change**)
* Introducing functions (**new**):
    * `increment :: Verbosity -> Maybe Verbosity`
    * `increment' :: Verbosity -> Verbosity`
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/verbosity-0.2.2.0>


## Version 0.2.1.0

* Introducing optional instance for cereal's `Serialize` type class. Dependency
  on `cereal` package can be enabled using `-fcereal` build flag. (**new**)
* Uploaded to [Hackage][]: <http://hackage.haskell.org/package/verbosity-0.2.1.0>


## Version 0.2.0.0

* Introducing module `Data.Verbosity.Class` which contains definition of
  `HasVerbosity` type class. (**new**)
* Introducing function `fromInt :: Int -> Maybe Verbosity`. (**new**)
* Introducing function
  `parse :: (Eq string, IsString string) => string -> Maybe Verbosity`. (**new**)
* NFData instance, if compiled with `-fdeepseq`, which is the default case.
  (**new**)
* Depends on [transformers][] package in case [base][] <4.8. (**new**)
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
