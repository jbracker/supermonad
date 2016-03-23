// An expression used to define a constant or initizlize a variable must
// not make use a function referring to a constant or variable that has not
// been (or had a chance to be) initialized yet. Thus, initializing
// expressions making use of a function defined in the same let block
// are not considered "well-initialized" and should be rejected.
// (This is overly strict, but a simple rule.)
//
// The following program is thus erroneous, even though some some
// of the uses of functions is safe.

let
    fun f(x : Integer) : Integer = x * x;
    const m : Integer = f(3);	// This is actually OK; f is "safe"
    fun g(y : Integer) : Integer = p * y;
    const n : Integer = g(4);	// But using g here is definitely problematic.
    const p : Integer = 10
in
    putint(m + n + p)
