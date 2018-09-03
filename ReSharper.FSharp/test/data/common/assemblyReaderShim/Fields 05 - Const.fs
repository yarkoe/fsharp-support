module Module

let [<Literal>] stringLiteral = Foo.String
let [<Literal>] intLiteral = Foo.Int
let [<Literal>] boolLiteral = Foo.Bool
let [<Literal>] doubleLiteral = Foo.Double

let [<Literal>] errorLiteral1 = Foo.NoConst
let [<Literal>] errorLiteral2 = Foo.StaticNoConst