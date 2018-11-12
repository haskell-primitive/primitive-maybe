# primitive-maybe

`primitive-maybe` provides types `SmallMaybeArray a`  and `MaybeArray a` which
are equivalent to `SmallArray (Maybe a)` and `Array (Maybe a)`, respectively,
but shaves off an indirection during indexing, making it more efficient for 
these purposes. 
