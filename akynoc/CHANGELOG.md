# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- next-header -->

## [Unreleased] - ReleaseDate

## [0.1.0] - 2026-02-06
## [0.1.0] - 2026-02-06

### Added
- Full parser implementation
- Command line interface with `clap`

### Changes
- Another language rename to `akyno`
- Reworked parser using memoization to allow left recursion

### Fixes
- Fix lexing of long comments

### Internal
- Setup dist CI
- Reconfigure into workspace named `nocky`

## 0.1.0-alpha.1 - 2026-01-24
### Added
- Lexer implemented via logos
- Parser implementation started via chumsky
- Project renamed to `nockyc`
- Basic README

<!-- next-url -->
[Unreleased]: https://github.com/anoaky/akyno/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/anoaky/akyno/compare/v0.1.0...v0.1.0
[0.1.0]: https://github.com/anoaky/akyno/compare/v0.1.0-alpha.1...v0.1.0
