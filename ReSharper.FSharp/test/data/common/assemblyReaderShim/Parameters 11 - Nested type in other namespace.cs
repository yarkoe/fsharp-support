namespace Ns1
{
    public class Bar
    {
        public class Nested
        {
        }
    }
}

namespace Ns2
{
    public class Foo
    {
        public Foo(Ns1.Bar.Nested ns)
        {
        }
    }
}
