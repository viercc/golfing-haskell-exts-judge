# Autojudge "Golfing language extensions"

A code golf challenge decribed in [this post by Taylor Fausak](https://dev.to/tfausak/golfing-language-extensions-2obl)
drowned some people into madness.

Manually judging answers for the challenge was so tiresome and error-prone.
This program automates it.

## Requirement

* GHC 8.10.* is installed and can invoke its compiler by command `ghc`.
* cabal-install is set up.

## How to compile

`cabal build judge`

## How to run

1. Prepare
   * Haskell program to be tested (`a.hs`)
   * A file listing name of GHC language extensions (`extensions.txt`) each separated by whitespace characters
2. `cabal run -- judge a.hs --exts extensions.txt`

## How to read outputs

```
$ cabal run -- judge original.hs --exts extensions.txt
Up to date
PASSED
  Score(chars/exts):3.5238095238095237
=================================
SourceStat {srcLenCodepoints = 74, srcLenUtf8Bytes = 84}
exts=21=21+0
```

* `PASSED` -- Input is valid. The program compiles with given list of extensions, and
  removing any one of extension make it fail to compile. If the input was invalid, `FAILED` is printed out.
* `Score(chars/exts):` -- How good the input is. Measured by how many characters (in Unicode codepoints)
  are in the program, divided by how many extensions required.
* `SourceStat{..}` -- Exists for the time when you want to measure by bytes
* `exts=N=E+I` -- Some extensions imply other extensions.
  
  If an extension `E` implies another extension `I`, enabling `E` also enables `I`.
  This can be overridden by specifying `NoI`. It was not clear how to score:
  
  * One requires `E` but don't require `I` to be enabled
  * One requires both `E` and `I` to be enabled
  
  This judge program don't allow former to advertize "require 2 extensions", but allow for latter.
  When seeing both `E` and `I` in the extensions list file, the judge counts them as 2 extensions,
  and rejects the former program.
  
  The output `exts=N=E+I` means the judge recognizes `N` extensions total, including `I` extensions
  implied by other extensions listed and `E` extensions no other extensions imply it.

## Sample inputs

* `original.hs`, `extensions.txt`: One presented in the blog post
* `isovector.hs`, `extensions_isovector.txt`: Current best, [From comments on reddit](https://www.reddit.com/r/haskell/comments/hzz8g5/golfing_language_extensions/fzos0eu/)

## Caveat

This program does not test the case
2 or more extensions are omitted simultaneously.

