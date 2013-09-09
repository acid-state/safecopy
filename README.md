SafeCopy
========

[![Build Status](https://travis-ci.org/acid-state/safecopy.png?branch=master)](https://travis-ci.org/acid-state/safecopy)
[![Public Domain](http://b.repl.ca/v1/license-public-blue.png)](https://en.wikipedia.org/wiki/Public_domain_software)
[![Haskell](http://b.repl.ca/v1/language-haskell-4e6272.png)](http://www.haskell.org)

SafeCopy extends the parsing and serialization capabilities of
[`Data.Serialize`](https://github.com/GaloisInc/cereal) to include nested
version control.  Nested version control means that you can change the
definition and binary format of a type nested deep within other types without
problems.
