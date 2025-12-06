# `roc-id`
<a href="https://jonathanknowles.github.io/roc-id/"><img src="https://img.shields.io/badge/API-Documentation-227755" /></a>

This package provides a [Haskell](https://www.haskell.org/) implementation of
the ROC (Taiwan) Uniform Identification Number (中華民國統一證號) format.

This format is used by both National Identification Cards (國民身分證) and
Alien Resident Certificates (居留證). Each identification number consists of a
single uppercase letter followed by nine decimal digits, with the final digit
serving as a checksum, calculated according to a standard algorithm.

Example: `A123456789`

This package offers functions for validating, decoding, and encoding these
numbers.

For more details of the Uniform Identification Number format, see:

* https://zh.wikipedia.org/wiki/中華民國國民身分證
* https://en.wikipedia.org/wiki/National_identification_card_(Taiwan)
* https://en.wikipedia.org/wiki/Resident_certificate
