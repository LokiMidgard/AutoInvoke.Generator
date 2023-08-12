using System;
using System.Collections.Generic;
using System.Text;

using Microsoft.CodeAnalysis;

using Microsoft.CodeAnalysis.CSharp;

using MethodDeclarationSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.MethodDeclarationSyntax;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Immutable;
using AutoInvoke.Generator.Configuration;
using AutoInvoke.Generator;
using System.Reflection;
using System.Xml.Linq;
using System.Net.Mime;
using AnyOfTypes;
using AutoInvoke.Generator.SourceCodeWriter;

namespace AutoInvoke.Generator;

[Generator(LanguageNames.CSharp)]
public class InvokeGenerator : IIncrementalGenerator {
    private static readonly DiagnosticDescriptor IG0001 = new DiagnosticDescriptor("IG0001", "Invalid Return Type", "The Return Type must be void or Task", "Design", DiagnosticSeverity.Error, true);
    private static readonly DiagnosticDescriptor IG0002 = new DiagnosticDescriptor("IG0002", "No Parameter Allowd", "The parameter list must be empty", "Design", DiagnosticSeverity.Error, true);
    private static readonly DiagnosticDescriptor IG0003 = new DiagnosticDescriptor("IG0003", "Wrong number of Type Parameters", "The method needs exactly one Type Parameter", "Design", DiagnosticSeverity.Error, true);
    public void Initialize(IncrementalGeneratorInitializationContext context) {
        var fullQualifiedAttribute = typeof(FindAndInvokeAttribute).FullName!;

        context.RegisterPostInitializationOutput(context => context.AddSource("attribute.g.cs", SourceGenerator.Helper.CopyCode.Copy.AutoInvokeFindAndInvokeAttribute));

        var methodsAndDiagnostics = context.SyntaxProvider.ForAttributeWithMetadataName(fullQualifiedAttribute,
            (node, cancel) =>
                node.IsKind(SyntaxKind.MethodDeclaration)
                    && node is MethodDeclarationSyntax methodDeclatation
                    && methodDeclatation.Parent is TypeDeclarationSyntax,
            (context, cancel) => Transform(context));

        var diagnostics = methodsAndDiagnostics.Where(x => x.IsSecond).SelectMany((x, cancel) => x.Second);

        context.RegisterSourceOutput(diagnostics, (context, data) => context.ReportDiagnostic(data));

        var methodsToHandle = methodsAndDiagnostics.Where(x => x.IsFirst).SelectMany((x, canel) => x.First);

        var typeFilters = methodsToHandle.Select((x, cancel) => (x.ImplementedMethodName, x.Configurations)).Collect();

        var typeInfo = context.SyntaxProvider.CreateSyntaxProvider((node, cancel) => node is BaseTypeDeclarationSyntax, (context, cancel) => {
            var syntax = (BaseTypeDeclarationSyntax)context.Node;

            if (context.SemanticModel.GetDeclaredSymbol(context.Node, cancellationToken: cancel) is not INamedTypeSymbol symbol) {
                return default;
            }
            var baseTypeBuilder = ImmutableArray.CreateBuilder<string>();
            {
                var current = symbol.BaseType;
                while (current is not null) {
                    baseTypeBuilder.Add(GetName(current));
                    current = current.BaseType;
                }
            }
            var name = GetName(symbol);

            var baseTypes = baseTypeBuilder.ToImmutableHashSet();
            var interfaces = symbol.AllInterfaces.Select(GetName).ToImmutableHashSet();

            var isStatic = symbol.IsStatic;
            var isAbstract = symbol.IsAbstract;
            var isStruct = symbol.IsValueType;
            var isClass = symbol.IsReferenceType;
            var isRecord = symbol.IsRecord;
            var hasParameterlessConstructor = isStruct || ((isClass || isRecord) && symbol.Constructors.Length == 0 || symbol.Constructors.Any(x => x.Parameters.Length == 0));
            var isInterface = syntax is InterfaceDeclarationSyntax;
            return (name, isStatic, isAbstract, isStruct, isClass, isRecord, isInterface, hasParameterlessConstructor, baseTypes, interfaces);
        }).Where(x => x.name is not null && !x.isStatic);

        var typesToHandle = typeInfo.Combine(typeFilters).Select((input, cancel) => {
            var ((name, isStatic, isAbstract, isStruct, isClass, isRecord, isInterface, hasParameterlessConstructor, baseTypes, interfaces), filters) = input;
            var matchingTransformers = filters.Where(filter =>
                filter.Configurations.Any(configuration => {
                    return ((configuration.CallForInterfaces
                             && isInterface)
                            || (configuration.CallForStructs && isStruct)
                            || ((configuration.CallForAbstractClasses == true || isAbstract == false)
                                && (
                                    (configuration.CallForClasses && isClass)
                                    || (configuration.CallForRecords && isRecord)
                                )))
                                && configuration.TypesToHandle.All(condition => {
                                    return condition switch {
                                        ConstructorConstraint => hasParameterlessConstructor,
                                        DefaultConstraint => isClass,
                                        TypeConstraint typeConstraint => name == typeConstraint.Type || interfaces.Contains(typeConstraint.Type) || baseTypes.Contains(typeConstraint.Type),
                                        ClassConstraint => isClass,
                                        StructConstraint => isStruct,
                                        RegexConstraint regex => Regex.IsMatch(name, regex.Pattern),
                                        _ => throw new NotImplementedException($"{condition} is not implemented")
                                    };
                                });
                })
            ).ToImmutableArray();
            return (matchingTransformers, typeName: name);
        }).Where(x => x.matchingTransformers.Length > 0).Collect();

        var data = methodsToHandle.Combine(typesToHandle)
            .Select((input, cancel) => {
                var (method, types) = input;
                var correctTypes = types.Where(x => x.matchingTransformers.Any(x => x.ImplementedMethodName == method.ImplementedMethodName));
                return (method, types: correctTypes.Select(x => x.typeName).ToImmutableArray());
            });

        var langVersion = context.ParseOptionsProvider.Select((x, cancel) => ((CSharpParseOptions)x).LanguageVersion);

        context.RegisterSourceOutput(data.Combine(langVersion), (context, data) => {
            var ((method, types), langVersion) = data;
            using var sb = SourceCode.New(langVersion);
            sb.WriteLine("// <auto-generated/>");
            sb.WriteLine("#nullable enable");

            sb.WriteSingleLineNamespace(method.Namespace);
            sb.WriteParent(method.DefinedIn, w => {

                w.WriteMethodBlock(SourceCodeWriterExtensions.Visibility.Private, method.ImplementedMethodName, method.IsStactic, method.IsAsync, Enumerable.Empty<(string, string)>(), method.IsAsync ? "System.Threading.Tasks.Task" : "void", w => {
                    if (method.IsAsync) {
                        w.WriteLine(w => {
                            w.Write("await System.Threading.Tasks.Task.WhenAll(");
                            w.Write(string.Join(", ", types.Select(type => $"{method.MethodToCall}<{type}>()")));
                            w.Write(");");
                        });
                    } else {
                        foreach (var type in types) {
                            w.WriteLine($"{method.MethodToCall}<{type}>();");
                        }
                    }
                });
            });

            context.AddSource($"{method.Namespace}.{method.DefinedIn.Keyword}.{method.ImplementedMethodName}.g", sb.ToString());
        });
    }

    private static AnyOf<ImmutableArray<MethodConfiguration>, ImmutableArray<Diagnostic>> Transform(GeneratorAttributeSyntaxContext context) {
        var method = (MethodDeclarationSyntax)context.TargetNode;

        var stringTypeInfo = context.SemanticModel.Compilation.GetTypeByMetadataName("System.String");
        var int32TypeInfo = context.SemanticModel.Compilation.GetTypeByMetadataName("System.Int32");
        var typeTypeInfo = context.SemanticModel.Compilation.GetTypeByMetadataName("System.Type");
        var taskTypeInfo = context.SemanticModel.Compilation.GetTypeByMetadataName("System.Threading.Tasks.Task");

        var methodName = method.Identifier.Text;
        var isAsync = !((method.ReturnType as PredefinedTypeSyntax)?.Keyword.Text == "void");

        var errors = ImmutableArray.CreateBuilder<Diagnostic>();
        if (isAsync) {
            //check if it is actually a Task
            if (context.SemanticModel.GetTypeInfo(method.ReturnType).Type is not INamedTypeSymbol propablyTaskSymbol
            || !SymbolEqualityComparer.Default.Equals(taskTypeInfo, propablyTaskSymbol)) {
                errors.Add(Diagnostic.Create(IG0001, Location.Create(method.SyntaxTree, method.ReturnType.Span)));
            }
        }
        if (method.ParameterList.Parameters.Count > 0) {
            errors.Add(Diagnostic.Create(IG0002, Location.Create(method.SyntaxTree, method.ParameterList.Span)));
        }
        if (method.TypeParameterList?.Parameters.Count != 1) {
            errors.Add(Diagnostic.Create(IG0003, Location.Create(method.SyntaxTree, (method.TypeParameterList?.Span ?? new Microsoft.CodeAnalysis.Text.TextSpan(method.Identifier.Span.Start, method.ParameterList.Span.End - method.Identifier.Span.Start)))));
        }
        if (errors.Count > 0) {
            return errors.ToImmutable();
        }
        var typeParameter = method.ConstraintClauses.SelectMany(constraint => constraint.Constraints.Where(y => {
            var kind = y.Kind();
            return kind is SyntaxKind.ClassConstraint
            or SyntaxKind.ConstructorConstraint
            or SyntaxKind.DefaultConstraint
            or SyntaxKind.StructConstraint
            or SyntaxKind.TypeConstraint;
        })
        .Select(y => {
            return y switch {
                ConstructorConstraintSyntax constructorConstraint => new ConstructorConstraint() as Constraints,
                DefaultConstraintSyntax constructorConstraint => new DefaultConstraint(),
                ClassOrStructConstraintSyntax classOrStructConstraintSyntax when classOrStructConstraintSyntax.IsKind(SyntaxKind.StructConstraint) => new StructConstraint(),
                ClassOrStructConstraintSyntax classOrStructConstraintSyntax when classOrStructConstraintSyntax.IsKind(SyntaxKind.ClassConstraint) => new ClassConstraint(),
                TypeConstraintSyntax typeConstraint => new TypeConstraint() { Type = GetName(context.SemanticModel.GetTypeInfo(typeConstraint.Type).Type as INamedTypeSymbol ?? throw new NotSupportedException($"Was not able to get type of {typeConstraint}")) },
                _ => throw new NotImplementedException()
            };
        })).ToImmutableList();
        var typeDeclaration = (TypeDeclarationSyntax)method.Parent!;
        var parent = typeDeclaration;

        string @namespace;
        {
            var currentNameSpace = ((IMethodSymbol)context.TargetSymbol).ContainingNamespace;
            var builder = new StringBuilder();
            if (!currentNameSpace.IsGlobalNamespace) {
                while (currentNameSpace is not null) {
                    var ns = currentNameSpace.IsGlobalNamespace ? "" : currentNameSpace.Name;
                    if (builder.Length != 0 && !currentNameSpace.IsGlobalNamespace) {
                        builder.Insert(0, '.');
                    }
                    builder.Insert(0, ns);
                    currentNameSpace = currentNameSpace.ContainingNamespace;
                }
            }
            @namespace = builder.ToString();
        }
        var configurations = context.Attributes.Select(x => {
            RunConfiguration configuration;
            if (x.ConstructorArguments.Length == 0) {
                configuration = new RunConfiguration() { TypesToHandle = typeParameter };
            } else if (SymbolEqualityComparer.Default.Equals(x.ConstructorArguments[0].Type, typeTypeInfo)) {
                configuration = new RunConfiguration() { TypesToHandle = typeParameter.Add(new TypeConstraint() { Type = GetName(x.ConstructorArguments[0].Value as INamedTypeSymbol ?? throw new NotSupportedException("Pattern must not be null")) }) };
            } else if (SymbolEqualityComparer.Default.Equals(x.ConstructorArguments[0].Type, stringTypeInfo)) {
                configuration = new RunConfiguration() { TypesToHandle = typeParameter.Add(new RegexConstraint() { Pattern = x.ConstructorArguments[0].Value as string ?? throw new NotSupportedException("Pattern must not be null") }) };
            } else {
                throw new NotImplementedException();
            }

            if (x.NamedArguments.FirstOrDefault(named => named.Key == nameof(FindAndInvokeAttribute.CallForStructs)).Value is TypedConstant srtuctConst && srtuctConst.Value is bool callForStructs) {
                configuration.CallForStructs = callForStructs;
            }
            if (x.NamedArguments.FirstOrDefault(named => named.Key == nameof(FindAndInvokeAttribute.CallForRecords)).Value is TypedConstant recordConst && recordConst.Value is bool callForRecords) {
                configuration.CallForRecords = callForRecords;
            }
            if (x.NamedArguments.FirstOrDefault(named => named.Key == nameof(FindAndInvokeAttribute.CallForInterfaces)).Value is TypedConstant interfaceConst && interfaceConst.Value is bool callForInterfaces) {
                configuration.CallForInterfaces = callForInterfaces;
            }
            if (x.NamedArguments.FirstOrDefault(named => named.Key == nameof(FindAndInvokeAttribute.CallForAbstractClasses)).Value is TypedConstant abstractConst && abstractConst.Value is bool callForAbstractClasses) {
                configuration.CallForAbstractClasses = callForAbstractClasses;
            }
            if (x.NamedArguments.FirstOrDefault(named => named.Key == nameof(FindAndInvokeAttribute.CallForClasses)).Value is TypedConstant classConst && classConst.Value is bool callForClasses) {
                configuration.CallForClasses = callForClasses;
            }

            var methodName = x.NamedArguments.FirstOrDefault(named => named.Key == nameof(FindAndInvokeAttribute.MethodName)).Value is TypedConstant methodNameConst && methodNameConst.Value is string methodName2
                ? methodName2
                : null;
            return (configuration: configuration, methodName: methodName);
        }).GroupBy(x => x.methodName).Select(x => {
            var list = x.Select(x => x.configuration).ToImmutableList();
            return new MethodConfiguration() {
                Configurations = list,
                DefinedIn = parent,
                Namespace = @namespace,
                ImplementedMethodName = x.Key ?? methodName, // This works since the other method is Generic?
                IsStactic = method.Modifiers.Any(t => t.IsKind(SyntaxKind.StaticKeyword)),
                MethodToCall = methodName,
                IsAsync = isAsync
            };
        }).ToImmutableArray();
        return configurations;
    }

    private static string GetName(INamedTypeSymbol symbol) {
        StringBuilder sb = new();
        sb.Append(symbol.ToDisplayString());
        return sb.ToString();
    }
}
