# Autojudge "Golfing language extensions"

A code gold challenge decribed in [this post by Taylor Fausak](https://dev.to/tfausak/golfing-language-extensions-2obl)
drowned some people into madness of this strange code golf.

Manually judging validness was so tiresome and error-prone.
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

## Sample inputs

* `original.hs`, `extensions.txt`: One presented in the blog post
* `isovector.hs`, `extensions_isovector.txt`: Current best, [From comments on reddit](https://www.reddit.com/r/haskell/comments/hzz8g5/golfing_language_extensions/fzos0eu/)

## Caveat

I think humans can spot easily, but this does not test the case
2 or more extensions are omitted simultaneously.

