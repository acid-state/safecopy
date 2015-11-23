0.9.0
=====

This version changes the way `Float` and `Double` are serialized to a
more compact format. Old data should be migrated automatically. As a
result, however, the `Float` and `Double` data serialized by this version can not be read
by older versions of `safecopy`.

This change originated as a modification to the way `cereal` 0.5 serializes `Float` and `Double`.

[https://github.com/GaloisInc/cereal/commit/47d839609413e3e9d1147b99c34ae421ae36bced](https://github.com/GaloisInc/cereal/commit/47d839609413e3e9d1147b99c34ae421ae36bced)

[https://github.com/GaloisInc/cereal/issues/35](https://github.com/GaloisInc/cereal/issues/35)


