# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres
to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased]

### Added

* user-defined thresholds for tests assessment
* possibility to add plot of revisions in report
* New `View()`, `summary()` and `print()` method to visualize the revision analysis


### Changed

* pivot from {formattable} dependency to {flextable} to build the tables
* `simulate_long()` is now an exported function to construct datasets example

### Fixed

* solve a display glitch in functions print.rjd3rev_vintages() and print.rjd3rev_revisions() 


## [1.3.2] - 2024-07-10

### Added

* new functions to visualise the vintages with method `View()` and `plot()`

### Changed

* add linting in the code
* declare all the dependencies outside of base
* remove wrong JARS

### Fixed

* resolved issues [#32](https://github.com/rjdverse/rjd3revisions/issues/32) with removing useless code 


## [1.3.1] - 2024-04-22

### Fixed

* solve bug with .Rd file of function revision_analysis


## [1.3.0] - 2024-04-22

### Changed

* refactor function revision_analysis()

### Fixed

* solve bug create_vintages with xlsx with vertical and horizontal view

### Added

* generic functions associated to create_vintages() and get_revisions() for better input visualisation


## [1.2.1] - 2024-04-12

### Changed

* refactor vignette

### Fixed

* solve bug create_vintages with xlsx
* fixed render report

### Added

* new date format accepted


## [1.2.0] - 2024-04-11

### Added

* new input accepted (vertical, horizontal)
* new functions to check the input format (check_XXX())
* new functions to convert an input format into another (from_XXX_to_XXX())
* generate report in pdf format
* new function to simulate long format input data

### Changed

* function to create vintage accept different format input (vertical, horizontal and long)


## [1.1.0] - 2023-08-28

### Fixed

* some bugs


## [1.0.0] - 2023-06-16

### Added

* Release based on JD+_main : v3.0.2

[Unreleased]: https://github.com/rjdverse/rjd3revisions/compare/v1.3.2...HEAD
[1.3.2]: https://github.com/rjdverse/rjd3revisions/releases/tag/v1.3.1...v1.3.2
[1.3.1]: https://github.com/rjdverse/rjd3revisions/releases/tag/v1.3.0...v1.3.1
[1.3.0]: https://github.com/rjdverse/rjd3revisions/releases/tag/v1.2.1...v1.3.0
[1.2.1]: https://github.com/rjdverse/rjd3revisions/releases/tag/v1.2.0...v1.2.1
[1.2.0]: https://github.com/rjdverse/rjd3revisions/releases/tag/v1.1.0...v1.2.0
[1.1.0]: https://github.com/rjdverse/rjd3revisions/releases/tag/v1.0.0...v1.1.0
[1.0.0]: https://github.com/rjdverse/rjd3revisions/releases/tag/v1.0.0
