
# AutoInvoke

This Generator let you anotate an Parameterless Generic Method with exactly one TypeArgument.

It will then generate a method with the same name and no type arguments that calls your anotated
method for every (non static) Type decleared in your project, that satisfies the type constraints.



## Sample

Assume you have the following Interface:

```c#
internal interface IFileLoder {
    public abstract static IFileLoder Init(string path);
    public abstract static string FileExtension { get; }
}
```

This describes a File loader for different types in our project.

And following implementation: 

```c#
internal class AudioLoader : IFileLoder {
    public static string FileExtension => ".mp3";

    public static IFileLoder Init(string Path) {
        return new AudioLoader(path);
    }
    // the rest of the code...
}
```

Which defines how we want to load mp3 files.

We now want to automaticly get a list of all `IFileLoader` so we know what files we can handle,
and we do not want to manualy handel such a list. 

An Implementation could look like this:

```c#
internal delegate IFileLoder LoadFile(string path);
internal partial class FileHandler {
    private readonly Dictionary<string, LoadFile> loaders = new();

    public FileHandler() {
        LoadLoaders();
    }

    public void LoadFile(string file) {
        if (loaders.TryGetValue(Path.GetExtension(file), out var loaderFactory)) {
            var loader = loaderFactory(file);
            // use loader to do things
        }
    }


    [AutoInvoke.Generator.FindAndInvoke]
    public void LoadLoaders<T>() where T : IFileLoder {
        this.loaders.Add(T.FileExtension, T.Init);
    }
}
```

The field loaders will have all extensions our code can handle, and has to every extension
the corresponding `Init`-Method.

The Generated code will look like this:

```c#
partial class FileHandler {
    private void LoadLoaders() {
        LoadLoaders<AutoInvoke.Generator.Example.AudioLoader>();
    }
}
```


## Featurs and limitations

- You can control wich type of types shold get called. E.g. by
  defaurd no calls are generated for abstract classes. But you can overide this setting
- The anotated method can be static
- The return type can be void or Task. Tasks will be awaited.

### Limitations
- Currently Selfreferencing generic types are not supported. E.g. you can't get all Types `T` that implement `ICompalable<T>`.
  You can however get all types that implement `ICompalable<Foo>`
- You can't call static classes. Generics do not allow this.


### Possible Featurs 
- Support selfreferencing Types
- Allow other return types and return an array of the results.