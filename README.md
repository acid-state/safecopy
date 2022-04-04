SafeCopy
========

[![Build Status](https://github.com/acid-state/safecopy/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/acid-state/safecopy/actions/workflows/haskell-ci.yml)
[![Hackage Status](https://img.shields.io/hackage/v/safecopy.svg?color=informational)][hackage]
[![safecopy on Stackage Nightly](https://stackage.org/package/safecopy/badge/nightly)](https://stackage.org/nightly/package/safecopy)
[![Stackage LTS version](https://www.stackage.org/package/safecopy/badge/lts?label=Stackage)](https://www.stackage.org/package/safecopy)
[![Public Domain](http://b.repl.ca/v1/license-public-blue.png)](https://en.wikipedia.org/wiki/Public_domain_software)
[![Haskell](http://b.repl.ca/v1/language-haskell-4e6272.png)](Http://www.haskell.org)

[hackage]: https://hackage.haskell.org/package/safecopy

SafeCopy extends the parsing and serialization capabilities of
[`Data.Serialize`](https://github.com/GaloisInc/cereal) to include nested
version control.  Nested version control means that you can change the
definition and binary format of a type nested deep within other types without
problems.
