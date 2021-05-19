Changelog for (Code) Conjure
============================


upcoming
--------

* only automatically include an `if` for the return type of the given function


v0.2.8
------

* export the `A`, `B`, `C`, `D`, `E` and `F` helper types


v0.2.6
------

* require Express v0.1.10 due to `hasHole` being now exported there
* require Eq result on `conjure1`, `conjure2` and `conjure3`
* code cleanup and more tests


v0.2.4
------

* allow conjuring from specifications in addition to partial definitions
  (`conjure1`, `conjure2`, `conjure3` and related functions)
* improve examples
* improve criteria for automatic primitive inclusion:
	- only include `if :: ... -> Bool` if there are `Bool` primitives
	- include `False` and `True` automatically only on Speculate's background
* add code-optional candidate nubbing and debug functions


v0.2.2
------

* by default, search for 60 argument combinations
  among 100000 enumerated combinations


v0.2.0
------

* search until 100% match is found and exit
* other misc changes


v0.1.2
------

For the changelog of earlier versions, check the git commit history.
