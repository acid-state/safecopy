0.10.4
======

Add a Typeable a superclass to SafeCopy.  The previous version in effect
had the Typeable constraint anyway, this means less need to specify it.
The SafeCopy' type alias is now identical to SafeCopy.  This should not
break any code except perhaps some GADT types that use "deriving Typeable".
These may need a standalone deriving instance.

0.10.0
======

This version replaces the default implementation of getCopy and putCopy
with a full implementation using GHC.Generics.  Before these functions
simply serialized and deserialized their argument.  Now they function
identically to the instances generated by deriveSafeCopy.  This means
that embedded values with SafeCopy instances will be migrated properly,
and that you can replace template haskell with standalone deriving
instances such as "deriving instance SafeCopy Foo where kind = extension;
version = 3".

The one caveat is that the new default implementation of getCopy and
putCopy adds the constraint "Typeable a", so that it can build a set of
the subtypes that appear in a.  This will only affect code that already
used the default instance, not code that used deriveSafeCopy or custom
SafeCopy instances.  If you do run into this you can add a custom SafeCopy
instance with the old implementations mentioned above.

0.9.4
=====
  - Support ghc-8.4.1
  - Travis config for ghc-8.2.1
  - SafeCopy instance for Data.List.NonEmpty.NonEmpty

0.9.1
=====

 - fixed tests to work with QuickCheck-2.8.2
 - add SafeCopy instance for Word
 - updates for template-haskell 2.11
 - export some internal TH derivation helpers

0.9.0
=====

This version changes the way `Float` and `Double` are serialized to a
more compact format. Old data should be migrated automatically. As a
result, however, the `Float` and `Double` data serialized by this version can not be read
by older versions of `safecopy`.

This change originated as a modification to the way `cereal` 0.5 serializes `Float` and `Double`.

[https://github.com/GaloisInc/cereal/commit/47d839609413e3e9d1147b99c34ae421ae36bced](https://github.com/GaloisInc/cereal/commit/47d839609413e3e9d1147b99c34ae421ae36bced)

[https://github.com/GaloisInc/cereal/issues/35](https://github.com/GaloisInc/cereal/issues/35)


