namespace AutoInvoke.Generator;

internal class ArrayComparer {
    public static ArrayComparer<T> Default<T>() => ArrayComparer<T>.Default;

}
internal class ArrayComparer<T> : IEqualityComparer<T[]> {
    private ArrayComparer() {

    }

    public static ArrayComparer<T> Default { get; } = new();
    public bool Equals(T[] x, T[] y) {
        return x.SequenceEqual(y);
    }

    public int GetHashCode(T[] obj) {
        int hashCode = -2128827177;
        foreach (var item in obj) {
            hashCode = hashCode * -1521134295 + item?.GetHashCode() ?? 0;
        }
        return hashCode;
    }
}