# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 05-08-22

### Added

- Implement PartialOrd and Ord for Term and RawTerm
- Implement Hash for Term and RawTerm

### Changed

- Converted Term::Map from `HashMap<String, Term>` to `HashMap<Term, Term>`
- Term::Float and RawTerm::Float is now `OrderedFloat<f64>` instead of `f64`

### Removed

- Deleted Term::MapArbitrary because normal Term::Map does the same

## [0.2.4] - 04-04-22

### Fixed

- Term::from RawTerm crashed on invalid utf8 bytes

## [0.2.3] - 04-04-22
