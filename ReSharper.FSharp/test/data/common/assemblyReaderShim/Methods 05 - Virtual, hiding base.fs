module Module

type BarVirtual() =
    inherit FooVirtual()

    override x.GetType() = Unchecked.defaultof<_>

type Bar() =
    inherit Foo()

    override x.GetType() = Unchecked.defaultof<_>
