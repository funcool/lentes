# Changelog #

## Version 1.2.0-SNAPSHOT ##

Date: unreleased

- Change license to BSD 2-Clause
- Remove the deprecated `getter` function.
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
