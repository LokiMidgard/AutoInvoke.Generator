using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace AutoInvoke.Generator.IntegrationTests; 
[TestClass]
public class HelloWorldGeneratorIntegrationTests {
    [TestMethod]
    public void Generated_HelloWorld() {
        Greeter.Run();

        CollectionAssert.AreEquivalent(new[] { "Bar" }, Greeter.calledFor);
    }
}

internal static partial class Greeter {

    public static void Run() => GetHelloWorld();

    public static List<string> calledFor = new List<string>();
    [FindAndInvoke]
    public static void GetHelloWorld<T>() where T : Foo {
        calledFor.Add(typeof(T).Name);
    }
}

public abstract class Foo {

}
public class Bar : Foo { }
