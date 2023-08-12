
namespace AutoInvoke.Generator.Example;

internal delegate IFileLoder<string> LoadFile(string path);
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

    [AutoInvoke.FindAndInvoke]
    public void LoadLoaders<T>() where T : IFileLoder<string> {
        this.loaders.Add(T.FileExtension, T.Init);
    }
}

/// <summary>
/// Implement This to 
/// </summary>

internal interface IFileLoder<T> {
    public abstract static IFileLoder<T> Init(string path);
    public abstract static string FileExtension { get; }
}

internal class AudioLoader : IFileLoder<string> {
    public static string FileExtension => ".mp3";

    public static IFileLoder<string> Init(string Path) {
        return new AudioLoader();
    }
}
