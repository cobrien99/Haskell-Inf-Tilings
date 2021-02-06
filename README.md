# FYP
A repository for storing my final year project at TCD. An investigation of using Haskell to generate infinite quasicrystalline patterns

# Bugfixes
To get around the "can't load framework: <FrameworkName> (not found)" bug on MacOS Big Sur run
```
DYLD_INSERT_LIBRARIES=`pwd`/macos11ghcwa.dylib stack build
```
For more info, and to get `macos11ghcwa.dylib` see https://github.com/yairchu/macos11-haskell-workaround
(Note: this error is fixed on newer versions of GHC, I just had to downgrade to use reflex-dom)

Might have to make a /images folder on startup if there isn't one already
without one, diagams outputing to `images/output.svg` will fail