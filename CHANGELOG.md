# 0.4.0.0

- Revised the `Show` instance of `ID` to produce a valid Haskell expression
  when applied to nested values of `ID`.
- Revised the `ID.fromSymbol` function so that users are presented with a
  helpful error if they forget to apply the function to a concrete type.
- Simplified the error handling for `ID.fromText` by merging the `TextTooShort`
  and `TextTooLong` errors into a unified `InvalidLength` error.
- Renamed `ID.checksumDigit` to `ID.checksum`.
- Made `ID` an instance of the `Finitary` type class.
- Made `ID` an instance of the `Generic` type class.

# 0.3.0.0

- Added support for ARC (Alien Residence Certificate) numbers.
- Added support for type-checked ID literals.
- Refined and reorganised the public API.

# 0.2.0.6

- Revised version bounds of dependencies.

# 0.2.0.5

- Revised version bounds of dependencies.

# 0.2.0.4

- Added support for GHC 9.12.

# 0.2.0.3

- Added support for GHC 9.10.

# 0.2.0.2

- Revised upper version bounds of dependencies.

# 0.2.0.1

- Added support for GHC 9.8.

# 0.2.0.0

- Refreshed package documentation.
- Refreshed all package dependencies.
- Removed dependency on `generic-arbitrary` package.

# 0.1.0.0

- Initial release.
