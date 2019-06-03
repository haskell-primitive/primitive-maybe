# primitive-maybe

[![Hackage](https://img.shields.io/hackage/v/primitive-maybe.svg)](https://hackage.haskell.org/package/primitive-maybe)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.com/haskell-primitive/primitive-maybe.svg?branch=master)](https://travis-ci.com/haskell-primitive/primitive-maybe)

`primitive-maybe` provides types `SmallMaybeArray a`  and `MaybeArray a` which
are equivalent to `SmallArray (Maybe a)` and `Array (Maybe a)`, respectively,
but shaves off an indirection during indexing, making it more efficient for 
these purposes. 
