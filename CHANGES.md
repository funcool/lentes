# Changelog #

## Version 1.4.0-SNAPSHOT

Date: 2024-09-03

- Improved docs (thank you @blak3mill3r, @ComedyTomedy, @luposlip, and @rgkirch).

Date: 2020-01-07

- Fix internal cache race conditions.
- Add Atom2 impl (thanks to @jellelicht).


## Version 1.3.3 ##

Date: 2019-10-22

- Fix jvm impl (bug introduced in previous commits).


## Version 1.3.2 ##

Date: 2019-10-22

Same as 1.3.1 (just docs updates).


## Version 1.3.1 ##

Date: 2019-10-22

Same as 1.3.0 (just docs updates).


## Version 1.3.0 ##

Date: 2019-10-22

- Improve equality checking on derived atoms.
- Minor code/performance improvements.


## Version 1.2.0 ##

Date: 2016-09-30

- Change license to BSD 2-Clause
- Remove the deprecated `getter` function.
- Fix `select-keys` lense in cljs impl.
- Improve derived atom perfromance.
  Adding cache and proxying the watchers in order to reduce
  N watchers to derived atom to 1 watcher to the source atom.


## Version 1.1.0 ##

Date: 2016-06-22

- Improve documentation.
- Add missing IAtom marker for derived atom (cljs only)
- Add the ability to create read only derived refs.
- Add `derive` function as substitute to `focus-atom` (backward compatible).
- Add arity 1 for `lens` function that allows create read only lenses.
- Add the ability to disable equality check on derived atoms.
- Deprecate `getter` in favor of `lens`.


## Version 1.0.1 ##

Date: 2016-03-20

- Minor code style fixes.
- Add getter lense shortcut.


## Version 1.0.0 ##

Date: 2016-02-26

Initial version (split from cats 1.2.1).
