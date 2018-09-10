public class Foo<T0>
{
  public class Bar<T1>
  {
    public Bar(T0 param1, T1 param2)
    {
    }

    public T2 Method<T2>(T0 param1, T2 param2) => param2;
  }
}
