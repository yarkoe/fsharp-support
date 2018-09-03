module Module

type Bar() =
    inherit Foo()

    override x.Method(param) = ()
    override x.Method() = ()
