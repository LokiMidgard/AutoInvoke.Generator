// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.NetworkInformation;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using System.Text.RegularExpressions;
using static System.Net.Mime.MediaTypeNames;

namespace AutoInvoke.Generator.SourceCodeWriter;

internal interface IWriter : IDisposable {
    public void Write(string text);
    public void Write(ReadOnlySpan<char> text);
}

internal static class SourceCode {
    public static ISourceCodeWriter New(LanguageVersion languageVersion) => FromStringBuilder(new StringBuilder(), languageVersion);
    public static ISourceCodeWriter FromStringBuilder(StringBuilder builder, LanguageVersion languageVersion) => new SourceCodeStringBuilderWriter(builder, languageVersion);
    public static ISourceCodeWriter WithIndention(this ISourceCodeWriter writer) => new SourceCodeIndentWriter(writer);
    public static ISourceCodeWriter WithReplace(this ISourceCodeWriter writer, string toReplace, string replacement) => new SourceCodeReplaceWriter(writer, toReplace, replacement);
    public static ISourceCodeWriter WithPrePostFix(this ISourceCodeWriter writer, string preFix = "", string postFix = "", string linePreFix = "", string linePostFix = "") => new SourceCodePrePostWriter(writer, preFix, postFix, linePreFix, linePostFix);

}

internal interface ISourceCodeWriter : IDisposable, IWriter {
    LanguageVersion LanguageVersion { get; }
    void WriteLine();
    void WriteLine(Action<IWriter> line);
    void WriteLine(ReadOnlySpan<char> text);
    void WriteLine(string text);
    string ToString();
}
internal interface ISourceCodeIndentWriter : ISourceCodeWriter {
    IDisposable Indent();
}

[System.Diagnostics.CodeAnalysis.SuppressMessage("IDisposableAnalyzers.Correctness", "IDISP025:Class with no virtual dispose method should be sealed", Justification = "<Pending>")]
file abstract class SourceCodeWriter : ISourceCodeWriter {
    private bool disposedValue;
    private bool isInUse;
    public LanguageVersion LanguageVersion { get; }

    protected SourceCodeWriter(LanguageVersion languageVersion) {
        LanguageVersion = languageVersion;
    }

    void ISourceCodeWriter.WriteLine() {
        ThrowIfInUSe();
        this.WriteLine();
    }

    void ISourceCodeWriter.WriteLine(Action<IWriter> line) {
        ThrowIfInUSe();
        this.WriteLine(line);
    }

    void ISourceCodeWriter.WriteLine(ReadOnlySpan<char> text) {
        ThrowIfInUSe();
        this.WriteLine(text);
    }

    void ISourceCodeWriter.WriteLine(string text) {
        ThrowIfInUSe();
        this.WriteLine(text);
    }

    void IWriter.Write(string text) {
        ThrowIfInUSe();
        this.Write(text);
    }

    void IWriter.Write(ReadOnlySpan<char> text) {
        ThrowIfInUSe();
        this.Write(text);
    }

    private void ThrowIfInUSe() {
        if (isInUse) {
            throw new InvalidOperationException("This CodeWriter is in use by another one");
        }
    }

    internal IDisposable Use() {
        ThrowIfInUSe();
        isInUse = true;
        return new OnDispose(() => isInUse = false);
    }

    public void Write(string text) => Write(text.AsSpan());
    public abstract void Write(ReadOnlySpan<char> text);
    public void WriteLine() => WriteLine("".AsSpan());
    public void WriteLine(string text) => WriteLine(text.AsSpan());
    public void WriteLine(Action<IWriter> line) {
        line(this);
        this.WriteLine();
    }
    public abstract void WriteLine(ReadOnlySpan<char> text);
    public abstract bool IsAtStartOfLine();

    public override string ToString() {
        return ToStringfile();
    }

    protected abstract string ToStringfile();

    protected virtual void OnEnd() { }

    public void Dispose() {
        if (!disposedValue) {
            this.OnEnd();
            disposedValue = true;
        }
        GC.SuppressFinalize(this);
    }
}

file class SourceCodeReplaceWriter : SourceCodeWriter {
    private readonly SourceCodeWriter writer;
    private readonly string toReplace;
    private readonly string replacement;

    public SourceCodeReplaceWriter(ISourceCodeWriter writer, string toReplace, string replacement) : base(writer.LanguageVersion) {
        this.writer = writer as SourceCodeWriter ?? throw new ArgumentException($"You can't use other {nameof(ISourceCodeWriter)} implementations then provided to you by this lib.", nameof(writer));
        this.toReplace = toReplace;
        this.replacement = replacement;
    }

    public override bool IsAtStartOfLine() {
        return writer.IsAtStartOfLine();
    }

    public override void Write(ReadOnlySpan<char> text) {
        var enumerator = text.Split(this.toReplace.AsSpan());
        var firstEntriy = true;
        foreach (var item in enumerator) {
            if (firstEntriy) {
                firstEntriy = false;
            } else {
                writer.Write(replacement);
            }
            writer.Write(item);
        }
    }

    public override void WriteLine(ReadOnlySpan<char> text) {
        writer.WriteLine(text);
    }

    protected override string ToStringfile() {
        return writer.ToString();
    }
}

internal static class SourceCodeWriterExtensions {

    public static void WriteString(this ISourceCodeWriter writer, string text) {
        if (writer.LanguageVersion >= (LanguageVersion)1100) { // Raw stringliteral support
            var matches = Regex.Matches(text, "\"\"(\")+").OfType<Match>();
            var numberOfQuotes = matches.Select(x => x.Length).Concat(new int[] { 3 }).Max();
            var quotes = new string('"', numberOfQuotes);
            writer.WriteLine(quotes);
            if (writer is not SourceCodeIndentWriter indentWriter) {
#pragma warning disable IDISP001 // Dispose created
                indentWriter = new(writer);
#pragma warning restore IDISP001 // Dispose created
            }

            using (indentWriter.Indent()) {
                indentWriter.WriteLine(text);
                indentWriter.WriteLine(quotes);
            }
            if (writer is not SourceCodeIndentWriter) {
                indentWriter.Dispose();
            }
        } else {
            using var prefixWriter = new SourceCodePrePostWriter(writer, preFix: "\"", postFix: "\"", linePreFix: "+ \"", linePostFix: "\\n\"");
            using var substitutionWriter = new SourceCodeReplaceWriter(new SourceCodeReplaceWriter(prefixWriter, "\"", "\\\""), "\t", "\\t");
            substitutionWriter.Write(text);
        }
    }

    public static void WriteBlock(this ISourceCodeWriter writer, Action<ISourceCodeWriter> blockContent) {
        writer.WriteLine("{");
        if (writer is not SourceCodeIndentWriter indentWriter) {
#pragma warning disable IDISP001 // Dispose created
            indentWriter = new SourceCodeIndentWriter(writer);
#pragma warning restore IDISP001 // Dispose created
        }
        using (indentWriter.Indent()) {
            blockContent(indentWriter);
        }
        if (writer is not SourceCodeIndentWriter) {
            indentWriter.Dispose();
        }
        writer.WriteLine("}");
    }

    public static void WriteSingleLineNamespace(this ISourceCodeWriter w, string @namespace) {
        w.WriteLine($"namespace {@namespace};");
    }

    public static void WriteNamespaceBlock(this ISourceCodeWriter w, string @namespace, Action<ISourceCodeWriter> block) {
        w.WriteLine($"namespace {@namespace};");
        w.WriteBlock(block);
    }
    public enum Visibility {
        Private,
        file,
        Protected,
        Public,
    }

    public static string AsString(this Visibility visibility) {
        return visibility switch {
            Visibility.Private => "private",
            Visibility.file => "file",
            Visibility.Protected => "protected",
            Visibility.Public => "public",
            _ => throw new NotSupportedException()
        };
    }

    public static void WriteMethodBlock(this ISourceCodeWriter w, Visibility visibility, string methodName, bool isStatic, bool isAsync, IEnumerable<(string type, string name)> parameters, string returnType, Action<ISourceCodeWriter> block) {

        w.Write(visibility.AsString());
        w.Write(" ");
        if (isStatic) {
            w.Write("static ");
        }
        if (isAsync) {
            w.Write("async ");
        }
        w.Write(returnType);
        w.Write(" ");
        w.Write(methodName);
        w.Write("(");
        bool firstParameter = true;
        foreach (var parameter in parameters) {
            if (firstParameter) {
                firstParameter = false;
            } else {
                w.Write(", ");
            }
            w.Write(parameter.type)
           ; w.Write(" ")
              ; w.Write(parameter.name);
        }
        w.Write(") ");

        w.WriteBlock(block.Invoke);
    }

    public static void WriteParent(this ISourceCodeWriter w, TypeDeclarationSyntax parentClass, Action<ISourceCodeWriter> block) {
        var parentList = ParentClass.GetParentClasses(parentClass);
        ParentClass.WriteParent(w, parentList, block);
    }

}

file class SourceCodeStringBuilderWriter : SourceCodeWriter {

    private readonly StringBuilder builder;

    private bool isAtStartOfLine;

    public SourceCodeStringBuilderWriter(StringBuilder parent, LanguageVersion languageVersion) : base(languageVersion) {
        this.builder = parent;
        isAtStartOfLine = true;
    }

    public override void Write(ReadOnlySpan<char> text) {
        if (text.IndexOf('\n') == -1) {
            builder.Append(text.ToString());
            isAtStartOfLine = false;
        } else {
            var enumerator = text.Split("\n".AsSpan());
            bool isFirst = true;
            foreach (var line in enumerator) {
                if (isFirst) {
                    isFirst = false;
                    Write(line.Trim('\r'));
                } else {
                    WriteLine(line.Trim('\r'));

                }
            }
        }
    }

    public override void WriteLine(ReadOnlySpan<char> text) {
        if (text.Length > 0) {
            Write(text);
        }
        builder.Append(Environment.NewLine);
        isAtStartOfLine = true;
    }

    public override bool IsAtStartOfLine() => this.isAtStartOfLine;
    protected override string ToStringfile() {
        return builder.ToString();
    }
}

file class SourceCodeIndentWriter : SourceCodeWriter, ISourceCodeIndentWriter {
    private readonly SourceCodeWriter writer;
    private readonly List<string> indentations = new();
    private readonly IDisposable revokeUse;
    private int indent;
    private bool isAtStartOfLine;

    public SourceCodeIndentWriter(ISourceCodeWriter writer) : base(writer.LanguageVersion) {
        this.writer = writer as SourceCodeWriter ?? throw new ArgumentException($"You can't use other {nameof(ISourceCodeWriter)} implementations then provided to you by this lib.", nameof(writer));

        this.revokeUse = this.writer.Use();

        indentations.Add(string.Empty);
        isAtStartOfLine = this.writer.IsAtStartOfLine();
    }

    public override void Write(ReadOnlySpan<char> text) {
        if (isAtStartOfLine) {
            writer.Write(indentations[indent]);
        }
        if (text.IndexOf('\n') == -1) {
            writer.Write(text);
            isAtStartOfLine = false;
        } else {
            var enumerator = text.Split("\n".AsSpan());
            foreach (var line in enumerator) {
                WriteLine(line.Trim('\r'));
            }
        }
    }

    public override void WriteLine(ReadOnlySpan<char> text) {
        if (text.Length > 0) {
            Write(text);
        }
        writer.Write(Environment.NewLine);
        isAtStartOfLine = true;
    }
    protected override void OnEnd() {
        revokeUse.Dispose();
        base.OnEnd();
    }

    public IDisposable Indent() {
        indent++;
        if (indentations.Count <= indent) {
            indentations.Add(new string(' ', 4 * indent));
        }
        return new OnDispose(() => { indent--; });
    }
    protected override string ToStringfile() {
        return writer.ToString();
    }
    public override bool IsAtStartOfLine() => this.isAtStartOfLine;
}

file class OnDispose : IDisposable {
    private bool disposedValue;
    private readonly Action onDispose;

    public OnDispose(Action value) {
        this.onDispose = value;
    }

    protected virtual void Dispose(bool disposing) {
        if (!disposedValue) {
            if (disposing) {
                onDispose();
            }
            disposedValue = true;
        }
    }

    void IDisposable.Dispose() {
        // Do not change this code. Put cleanup code in 'Dispose(bool disposing)' method
        Dispose(disposing: true);
        GC.SuppressFinalize(this);
    }
}

file class SourceCodePrePostWriter : SourceCodeWriter {

    private readonly SourceCodeWriter writer;
    private readonly IDisposable revokeUse;
    private readonly string linePreFix;
    private readonly string preFix;
    private readonly string linePostFix;
    private readonly string postFix;
    private bool isAtStartOfLine;

    public SourceCodePrePostWriter(ISourceCodeWriter writer, string preFix = "", string postFix = "", string linePreFix = "", string linePostFix = "") : base(writer.LanguageVersion) {
        this.writer = writer as SourceCodeWriter ?? throw new ArgumentException($"You can't use other {nameof(ISourceCodeWriter)} implementations then provided to you by this lib.", nameof(writer));

        this.revokeUse = this.writer.Use();

        this.linePreFix = linePreFix;
        this.linePostFix = linePostFix;
        this.preFix = preFix;
        this.postFix = postFix;
        isAtStartOfLine = true;
        if (preFix.Length > 0) {
            writer.Write(preFix);
        }
    }

    protected override void OnEnd() {
        writer.Write(linePostFix);
        revokeUse.Dispose();
        base.OnEnd();
    }

    public override void Write(ReadOnlySpan<char> text) {
        if (isAtStartOfLine) {
            writer.Write(linePreFix);
        }
        if (text.IndexOf('\n') == -1) {
            writer.Write(text.ToString());
            isAtStartOfLine = false;
        } else {
            var enumerator = text.Split("\n".AsSpan());
            foreach (var line in enumerator) {
                WriteLine(line.Trim('\r'));
            }
        }
    }

    public override void WriteLine(ReadOnlySpan<char> text) {
        if (text.Length > 0) {
            Write(text);
        }
        writer.Write(linePostFix);
        writer.WriteLine();
        isAtStartOfLine = true;
    }
    protected override string ToStringfile() {
        return writer.ToString();
    }
    public override bool IsAtStartOfLine() => this.isAtStartOfLine;

}

file static partial class MemoryExtensions {

    public static SpanSplitEnumerator<char> Split(this ReadOnlySpan<char> span, ReadOnlySpan<char> separator)
        => new(span, separator);
}

file ref struct SpanSplitEnumerator<T> where T : IEquatable<T> {
    private readonly ReadOnlySpan<T> toSplit;
    private readonly ReadOnlySpan<T> separator;
    private int offset;
    private int index;

    public readonly SpanSplitEnumerator<T> GetEnumerator() => this;

    public SpanSplitEnumerator(ReadOnlySpan<T> span, ReadOnlySpan<T> separator) {
        toSplit = span;
        this.separator = separator;
        index = 0;
        offset = 0;
    }

    public readonly ReadOnlySpan<T> Current => toSplit.Slice(offset, index - 1);

    public bool MoveNext() {
        if (toSplit.Length - offset < index) { return false; }
        var slice = toSplit.Slice(offset += index);

        var nextIndex = slice.IndexOf(separator);
        index = (nextIndex != -1 ? nextIndex + separator.Length : slice.Length + 1);
        return true;
    }
}

file class ParentClass {
    public ParentClass(string keyword, string name, string constraints, ParentClass? child) {
        Keyword = keyword;
        Name = name;
        Constraints = constraints;
        Child = child;
    }

    public ParentClass? Child { get; }
    public string Keyword { get; }
    public string Name { get; }
    public string Constraints { get; }

    public static ParentClass GetParentClasses(TypeDeclarationSyntax typeSyntax) {
        // Try and get the parent syntax. If it isn't a type like class/struct, this will be null
        TypeDeclarationSyntax? parentSyntax = typeSyntax;
        ParentClass? parentClassInfo = null;

        // Keep looping while we're in a supported nested type
        while (parentSyntax != null && IsAllowedKind(parentSyntax.Kind())) {
            // Record the parent type keyword (class/struct etc), name, and constraints
            parentClassInfo = new ParentClass(
                keyword: parentSyntax.Keyword.ValueText,
                name: parentSyntax.Identifier.ToString() + parentSyntax.TypeParameterList,
                constraints: parentSyntax.ConstraintClauses.ToString(),
                child: parentClassInfo); // set the child link (null initially)

            // Move to the next outer type
            parentSyntax = (parentSyntax.Parent as TypeDeclarationSyntax);
        }

        // return a link to the outermost parent type
        return parentClassInfo ?? throw new NotSupportedException();

    }

    public static void WriteParent(ISourceCodeWriter w, ParentClass parentClass, Action<ISourceCodeWriter> block) {

        w.Write("partial ");
        w.Write(parentClass.Keyword);
        w.Write(" ");
        w.Write(parentClass.Name);
        w.Write(" ");
        w.Write(parentClass.Constraints);

        w.WriteBlock(w => {
            if (parentClass.Child is not null) {
                WriteParent(w, parentClass.Child, block);
            } else {
                block(w);
            }
        });
    }

    public override string ToString() {
        return this.Name + "." + (Child?.ToString() ?? "");
    }

    // We can only be nested in class/struct/record
    private static bool IsAllowedKind(SyntaxKind kind) =>
        kind is SyntaxKind.ClassDeclaration or
        SyntaxKind.InterfaceDeclaration or
        SyntaxKind.StructDeclaration or
        SyntaxKind.RecordDeclaration;

}
