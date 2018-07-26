
This is an example of using Citron to generate a parser for a grammar.

The goal is to parse a Swift function header (the part that comes before
the function body), like "func add(a: Int, b: Int) -> Int" and convert
that to an in-memory data structure. We use that to print out the type
definition for that function, which for this example would be
`(Int, Int) -> Int`.

