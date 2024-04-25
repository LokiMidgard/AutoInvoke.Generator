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
using System.Security.Cryptography.X509Certificates;
using System.Runtime.CompilerServices;
using System.Linq;

namespace AutoInvoke.Generator;

[Generator(LanguageNames.CSharp)]
public class InvokeGenerator : IIncrementalGenerator {
    private static readonly DiagnosticDescriptor IG0001 = new DiagnosticDescriptor("IG0001", "A Single Type can't satisfy all constrints", "One Type Parameter must contain all others, otherwise a singele Type will not Satisfy all Constrains", "Design", DiagnosticSeverity.Error, true);
    public void Initialize(IncrementalGeneratorInitializationContext context) {
        var fullQualifiedAttribute = typeof(FindAndInvokeAttribute).FullName!;



        context.RegisterPostInitializationOutput(context => context.AddSource("attribute.g.cs", SourceGenerator.Helper.CopyCode.Copy.AutoInvokeFindAndInvokeAttribute));

        var methodsAndDiagnostics = context.SyntaxProvider.ForAttributeWithMetadataName(fullQualifiedAttribute,
            (node, cancel) =>
                node.IsKind(SyntaxKind.MethodDeclaration)
                    && node is MethodDeclarationSyntax methodDeclatation
                    && methodDeclatation.Parent is TypeDeclarationSyntax,
            Transform);

        var diagnostics = methodsAndDiagnostics.Where(x => x.IsSecond).SelectMany((x, cancel) => x.Second);

        context.RegisterSourceOutput(diagnostics, (context, data) => context.ReportDiagnostic(data));

        var scanExternalAssemblys = methodsAndDiagnostics.Select((x, cancel) => x.IsFirst && x.First.Any(x => x.Configurations.Any(x => x.ScanExternalAssemblys))).Collect().Select((x, cancel) => x.Any(x => x));

        var externalSymbol = context.MetadataReferencesProvider
            .Combine(context.CompilationProvider)
            .SelectMany((input, cancel) => {
                var (metadataReference, compilation) = input;
                var assamblyOrModule = compilation.GetAssemblyOrModuleSymbol(metadataReference);
                if (assamblyOrModule is IAssemblySymbol assembly) {
                } else if (assamblyOrModule is IModuleSymbol module) {
                    assembly = module.ContainingAssembly;
                } else {
                    return Enumerable.Empty<INamedTypeSymbol>();
                }

                IEnumerable<INamedTypeSymbol> GetTypes(INamespaceSymbol namespaceSymbol) {
                    foreach (var type in namespaceSymbol.GetTypeMembers()) {
                        yield return type;
                    }
                    foreach (var ns in namespaceSymbol.GetNamespaceMembers()) {
                        foreach (var type in GetTypes(ns)) {
                            yield return type;
                        }
                    }
                }

                return GetTypes(assembly.GlobalNamespace).Where(x => x.DeclaredAccessibility == Accessibility.Public && x.CanBeReferencedByName);
            })
            .Combine(scanExternalAssemblys)
            .Select((input, cancel) => input.Right ? input.Left : null!)
            .Where(x => x is not null);
        ;








        var methodsToHandle = methodsAndDiagnostics.Where(x => x.IsFirst).SelectMany((x, canel) => x.First);

        var typeFilters = methodsToHandle.Select((x, cancel) => (x.ImplementedMethodName, x.MethodToCall, x.Configurations)).Collect();

        IncrementalValuesProvider<INamedTypeSymbol> internalSymbols = context.SyntaxProvider.CreateSyntaxProvider((node, cancel) => node is BaseTypeDeclarationSyntax, (context, cancel) => {
            var syntax = (BaseTypeDeclarationSyntax)context.Node;

            if (context.SemanticModel.GetDeclaredSymbol(context.Node, cancellationToken: cancel) is not INamedTypeSymbol symbol) {
                return default!;// we check for nall later…
            }
            return symbol;
        });
        var typesToHandleIntern = RegisterSymbolHandling(context, methodsToHandle, typeFilters, internalSymbols);
        var typesToHandleExtern = RegisterSymbolHandling(context, methodsToHandle, typeFilters, externalSymbol);



        var data = methodsToHandle.Combine(typesToHandleIntern)
            .Select((input, cancel) => {
                var (method, types) = input;
                var correctTypes = types.Select(x => (type: x, transformer: x.matchingTransformers.Where(x => x.ImplementedMethodName == method.ImplementedMethodName).ToArray()))
                .SelectMany(x => x.transformer.Select(y => y.TypeParameters.Select(x => x.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)).ToArray())).ToImmutableArray();
                return (method, types: correctTypes);
            }).Combine(typesToHandleExtern)
            .Select((input, cancel) => {
                var ((method, previousTypes), types) = input;
                var correctTypes = types
                .Select(x => (type: x, transformer: x.matchingTransformers.Where(x => x.ImplementedMethodName == method.ImplementedMethodName).ToArray()))
                .SelectMany(x => x.transformer.Select(y => y.TypeParameters.Select(x => x.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)).ToArray()))
                .Concat(previousTypes)
                .ToImmutableArray()
                ;
                return (method, types: correctTypes);
            })

            ;



        var langVersion = context.ParseOptionsProvider.Select((x, cancel) => ((CSharpParseOptions)x).LanguageVersion);

        context.RegisterSourceOutput(data.Combine(langVersion), (context, data) => {
            var ((method, types), langVersion) = data;
            using var sb = SourceCode.New(langVersion);
            sb.WriteLine("// <auto-generated/>");
            sb.WriteLine("#nullable enable");

            sb.WriteSingleLineNamespace(method.Namespace);
            sb.WriteParent(method.DefinedIn, w => {

                string returnType = method.ReturnType is null ? "void" : $"{GetName(method.ReturnType)}[]";

                w.WriteMethodBlock(SourceCodeWriterExtensions.Visibility.Private, method.ImplementedMethodName, method.IsStactic, false, method.Parameters.Select(x => (GetName(x.Type), x.Name)), returnType, w => {
                    if (method.ReturnType is null) {
                        foreach (var type in types.Distinct(ArrayComparer.Default<string>())) {
                            w.WriteLine($"{method.MethodToCall.Name}<{string.Join(", ", type)}>({string.Join(", ", method.Parameters.Select(x => x.Name))});");
                        }
                    } else {


                        w.WriteLine("return new [] {");
                        using (var indent = w.WithIndention())
                        using (indent.Indent()) {
                            indent.WriteLine(string.Join(",\n", types.Distinct(ArrayComparer.Default<string>()).Select(type => $"{method.MethodToCall.Name}<{string.Join(", ", type)}>({string.Join(", ", method.Parameters.Select(x => x.Name))})")));
                        }
                        w.WriteLine("};");

                    }
                });
            });

            context.AddSource($"{method.Namespace}.{method.DefinedIn.Keyword}.{method.ImplementedMethodName}.g", sb.ToString());
        });

    }

    private static IncrementalValueProvider<ImmutableArray<(ImmutableArray<(IMethodSymbol MethodToCall, string ImplementedMethodName, ImmutableList<RunConfiguration> Configurations, ImmutableArray<ITypeSymbol> TypeParameters)> matchingTransformers, string typeName)>> RegisterSymbolHandling(IncrementalGeneratorInitializationContext context, IncrementalValuesProvider<MethodConfiguration> methodsToHandle, IncrementalValueProvider<ImmutableArray<(string ImplementedMethodName, IMethodSymbol MethodToCall, ImmutableList<RunConfiguration> Configurations)>> typeFilters, IncrementalValuesProvider<INamedTypeSymbol> symbols) {
        var typeInfo = symbols

            .Where(x => x is not null && !x.IsStatic);

        var typesToHandle = typeInfo.Combine(typeFilters).Select((input, cancel) => {
            var (symbol, filters) = input;
            var matchingTransformers = filters
            .SelectMany(filter =>
                Satisys(filter.MethodToCall, symbol).Select(x =>
                (filter.MethodToCall, filter.ImplementedMethodName, filter.Configurations, TypeParameters: x)
                )
            )

            .Where(filter =>
                 filter.Configurations.Any(configuration => {

                     return ((configuration.CallForInterfaces
                              && symbol.TypeKind == TypeKind.Interface)
                             || (configuration.CallForStructs && symbol.TypeKind == TypeKind.Struct)
                             || (configuration.CallForEnums && symbol.TypeKind == TypeKind.Enum)
                             || ((configuration.CallForAbstractClasses == true || symbol.IsAbstract == false)
                                 && (
                                     (configuration.CallForClasses && symbol.TypeKind == TypeKind.Class)
                                     || (configuration.CallForRecords && symbol.IsRecord)
                                 )))
                                 && configuration.TypesToHandle.All(condition => {
                                     return Regex.IsMatch(symbol.Name, condition.Pattern);
                                 });
                 })
            ).ToImmutableArray();
            return (matchingTransformers, typeName: symbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        }).Where(x => x.matchingTransformers.Length > 0).Collect();
        return typesToHandle;


    }

    private static AnyOf<ImmutableArray<MethodConfiguration>, ImmutableArray<Diagnostic>> Transform(GeneratorAttributeSyntaxContext context, CancellationToken cancellation) {
        var method = (MethodDeclarationSyntax)context.TargetNode;

        var stringTypeInfo = context.SemanticModel.Compilation.GetTypeByMetadataName("System.String");
        var int32TypeInfo = context.SemanticModel.Compilation.GetTypeByMetadataName("System.Int32");
        var typeTypeInfo = context.SemanticModel.Compilation.GetTypeByMetadataName("System.Type");
        var taskTypeInfo = context.SemanticModel.Compilation.GetTypeByMetadataName("System.Threading.Tasks.Task");

        var methodSymbol = context.TargetSymbol as IMethodSymbol ?? throw new NotSupportedException();

        var methodName = method.Identifier.Text;
        var returnType = !((method.ReturnType as PredefinedTypeSyntax)?.Keyword.Text == "void") ? methodSymbol.ReturnType : null;


        var parameters = methodSymbol.Parameters;


        {
            ITypeParameterSymbol? typeParameterToImplement;
            if (methodSymbol.TypeParameters.Length == 1) {
                // if it is only one parameter, it can be a concrete type or a self referencing type  there are no other TypeParameter that are independent
                typeParameterToImplement = methodSymbol.TypeParameters[0];
            } else {
                // We need to find one type parameter, that is dependend on all other TypeParameters.
                // We may not have two Type parameters that are independent of each other
                // like `where T1 : string where T2 : int`.
                // What is allowed is `where T1 : Foo<T2> where T2 : INumber`
                typeParameterToImplement = methodSymbol.TypeParameters
                    .Where(x => GetAllReferencedTypeParameter(x).Distinct(SymbolEqualityComparer.Default).Count() == methodSymbol.TypeParameters.Length).FirstOrDefault();

            }

            if (typeParameterToImplement is null) {

                return methodSymbol.DeclaringSyntaxReferences
                    .Cast<SyntaxReference>()
                    .Select(x => x.GetSyntax(cancellation))
                    .Cast<MethodDeclarationSyntax>()
                    .Select(x => Diagnostic.Create(IG0001, Location.Create(x.SyntaxTree, x.TypeParameterList?.Span ?? x.Span)))
                    .ToImmutableArray();
            }
        }

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
                configuration = new RunConfiguration() { TypesToHandle = ImmutableList.Create<RegexConstraint>() };
            } else if (SymbolEqualityComparer.Default.Equals(x.ConstructorArguments[0].Type, stringTypeInfo)) {
                configuration = new RunConfiguration() { TypesToHandle = ImmutableList.Create(new RegexConstraint() { Pattern = x.ConstructorArguments[0].Value as string ?? throw new NotSupportedException("Pattern must not be null") }) };
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
            if (x.NamedArguments.FirstOrDefault(named => named.Key == nameof(FindAndInvokeAttribute.ScanExternalAssamblies)).Value is TypedConstant scanConst && scanConst.Value is bool scanConstValue) {
                configuration.ScanExternalAssemblys = scanConstValue;
            }

            var methodName = x.NamedArguments.FirstOrDefault(named => named.Key == nameof(FindAndInvokeAttribute.MethodName)).Value is TypedConstant methodNameConst && methodNameConst.Value is string methodName2
                ? methodName2
                : null;
            return (configuration, methodName);
        }).GroupBy(x => x.methodName).Select(x => {
            var list = x.Select(x => x.configuration).ToImmutableList();
            return new MethodConfiguration() {
                Configurations = list,
                DefinedIn = parent,
                Namespace = @namespace,
                Parameters = parameters,
                ImplementedMethodName = x.Key ?? methodName, // This works since the other method is Generic?
                IsStactic = method.Modifiers.Any(t => t.IsKind(SyntaxKind.StaticKeyword)),
                MethodToCall = methodSymbol,
                ReturnType = returnType
            };
        }).ToImmutableArray();
        return configurations;
    }

    private static string GetName(ITypeSymbol symbol) {
        return symbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
    }

    private static INamedTypeSymbol SubstituteTypeParameters(INamedTypeSymbol toConstrainTo, ITypeSymbol toSubstitute) {
        if (toConstrainTo.IsGenericType) {
            var parameters = toConstrainTo.TypeArguments.ToBuilder();
            for (int i = 0; i < parameters.Count; i++) {
                if (SymbolEqualityComparer.Default.Equals(parameters[i], toSubstitute)) {
                    parameters[i] = toConstrainTo.ConstructedFrom.TypeArguments[i];
                } else if (parameters[i] is INamedTypeSymbol namedType) {
                    parameters[i] = SubstituteTypeParameters(namedType, toSubstitute);
                }
            }
            toConstrainTo = toConstrainTo.ConstructedFrom.Construct(parameters.ToImmutable(), toConstrainTo.TypeArguments.Select(x => x.NullableAnnotation).ToImmutableArray());
        }
        return toConstrainTo;
    }


    private static ImmutableArray<ImmutableArray<ITypeSymbol>> Satisys(IMethodSymbol methodSymbol, INamedTypeSymbol type) {

        if (type.IsGenericType) {
            return ImmutableArray<ImmutableArray<ITypeSymbol>>.Empty; // We only support concrete implementations
        }

        ITypeParameterSymbol? typeParameterToImplement;
        if (methodSymbol.TypeParameters.Length == 1) {
            // if it is only one parameter, it can be a concrete type or a self referencing type  there are no other TypeParameter that are independent
            typeParameterToImplement = methodSymbol.TypeParameters[0];
        } else {
            // We need to find one type parameter, that is dependend on all other TypeParameters.
            // We may not have two Type parameters that are independent of each other
            // like `where T1 : string where T2 : int`.
            // What is allowed is `where T1 : Foo<T2> where T2 : INumber`
            typeParameterToImplement = methodSymbol.TypeParameters
                .Where(x => GetAllReferencedTypeParameter(x).Distinct(SymbolEqualityComparer.Default).Count() == methodSymbol.TypeParameters.Length).FirstOrDefault();

        }

        if (typeParameterToImplement is null) {
            return ImmutableArray<ImmutableArray<ITypeSymbol>>.Empty;
        }

        // We do not need to check other Type Parameters explicitly, since Type parameter already contains every other
        var allValidParameters = CheckTypeConsraint(typeParameterToImplement, type, TypeConstraintResultTree.CreateRoot().ToList(), true);

        // check if all TypeParameters were assingend
        ImmutableArray<ImmutableArray<ITypeSymbol>> immutableArray = allValidParameters.Select(x =>

                    methodSymbol.TypeParameters
                    .Select(typeParameter => x.TypeMapping.GetValueOrDefault(typeParameter)).OfType<ITypeSymbol>().ToImmutableArray())
                    .Where(x => x.Length == methodSymbol.TypeParameters.Length)
                    .ToImmutableArray();
        return immutableArray;

        ImmutableArray<TypeConstraintResultTree> CheckTypeConsraint(ITypeSymbol constraint, ITypeSymbol checkAgainst, ImmutableArray<TypeConstraintResultTree> tree, bool firstCall = false, ITypeParameterSymbol? currentTypeConstraint = null) {

            if (!tree.Any()) {
                return tree.AddTrue();
            }

            if (checkAgainst is INamedTypeSymbol namedCheckAgainst && constraint is INamedTypeSymbol namedConstraint) {
                // check if the types are equal ignoring type parameters
                var exactCheck = CheckAgainstExactType(tree, namedCheckAgainst, namedConstraint).AddChild((currentTypeConstraint, checkAgainst));
                if (exactCheck.Length != 0) {
                    return exactCheck;
                }

                if (namedConstraint.TypeKind != TypeKind.Interface) {

                    // the type dose not match, we check the base types, is one matches, we are good
                    var currentBaseType = namedCheckAgainst;
                    while ((currentBaseType = currentBaseType?.BaseType) is not null) {
                        // we may not call CheckTypeConstraint, since then we would check interfactes multiple Times
                        var result = CheckAgainstExactType(tree, currentBaseType, namedConstraint);
                        if (result.Any()) {
                            // we check againtst a concrete typeConstraint, if TypeConstraint is class it can't be an interface so we do not need check interfaces.
                            return result.AddChild((currentTypeConstraint, namedCheckAgainst));
                        }
                    }
                    return tree.AddFalse();
                } else if (checkAgainst.TypeKind != TypeKind.Interface) {

                    // if that did not work check if there is an interface implementation
                    return namedCheckAgainst.AllInterfaces.Select(@interface => CheckTypeConsraint(constraint, @interface, tree))
                        .SelectMany(x => x)
                        .AddChild((currentTypeConstraint, checkAgainst)); // If none matched the list will be empty so none is set true
                } else {
                    return ImmutableArray<TypeConstraintResultTree>.Empty;
                }

                ImmutableArray<TypeConstraintResultTree> CheckAgainstExactType(ImmutableArray<TypeConstraintResultTree> tree, INamedTypeSymbol namedCheckAgainst, INamedTypeSymbol namedConstraint) {
                    if (!SymbolEqualityComparer.Default.Equals(GeneralizeGeneric(namedConstraint), GeneralizeGeneric(namedCheckAgainst))) {
                        return tree.AddFalse();
                    }
                    // if it is geneirc also check type parameters
                    if (namedConstraint.IsGenericType) {
                        // I think this is not nessesarry since two "identical" types with different number of arguments have differend Unbound typest
                        // TODO: Check this.
                        if (namedConstraint.TypeArguments.Length != namedCheckAgainst.TypeArguments.Length) {
                            return tree.AddFalse();
                        }

                        //check all generic parameters
                        // every TypeParameter must be valid, so chaining the different nodes in a long linek
                        var subTree = tree;
                        for (global::System.Int32 i = 0; i < namedConstraint.TypeArguments.Length; i++) {
                            subTree = CheckTypeConsraint(namedConstraint.TypeArguments[i], namedCheckAgainst.TypeArguments[i], subTree);
                            if (!subTree.Any()) {
                                break; // we can stop if subTree has no valid 
                            }
                        }
                        return subTree;
                    }
                    return tree.AddTrue();
                }

            } else if (constraint is ITypeParameterSymbol typeParameterConstraint) {

                // to support recursive TypeParameter, we need to replace every occurance of
                // typeParameterToImplement with type (expcept for the initial)


                var differentMappings = tree.GroupBy(x => x.TypeMapping.ContainsKey(typeParameterConstraint));


                tree = differentMappings.SelectMany(mapping => {
                    if (mapping.Key) {

                        return mapping.GroupBy(s => s.TypeMapping[typeParameterConstraint], SymbolEqualityComparer.Default)
                        .SelectMany(x => CheckTypeConsraint((ITypeSymbol)x.Key!, checkAgainst, x.AddTrue())).Where(x => x.Succsess);

                        //CheckTypeConsraint(s.TypeMapping[typeParameterConstraint], checkAgainst, mapping.AddTrue());
                        //return mapping.Select(s => SymbolEqualityComparer.Default.Equals(s.TypeMapping[typeParameterConstraint], checkAgainst) ? s.AddTrue() : s.AddFalse()).Where(x => x.Succsess);
                    } else {
                        var tree = mapping.AddChild((typeParameterConstraint, checkAgainst));


                        //if (!firstCall && SymbolEqualityComparer.Default.Equals(typeParameterConstraint, typeParameterToImplement)) {
                        //    return CheckTypeConsraint(type, checkAgainst, tree).AddTrue();
                        //}

                        if (typeParameterConstraint.HasConstructorConstraint && (
                            checkAgainst is not INamedTypeSymbol namedCheckAgainst2
                            || namedCheckAgainst2.IsAbstract
                            || !namedCheckAgainst2.Constructors.Where(x => !x.IsStatic && x.Parameters.Length == 0).Any()
                            )) {
                            return tree.AddFalse(); // dose not satisfy constructor constraint
                        }
                        if (typeParameterConstraint.HasNotNullConstraint && checkAgainst.NullableAnnotation == NullableAnnotation.Annotated) {
                            return tree.AddFalse();
                        }
                        if (typeParameterConstraint.HasReferenceTypeConstraint && !checkAgainst.IsReferenceType) {
                            return tree.AddFalse();
                        }
                        if (typeParameterConstraint.HasUnmanagedTypeConstraint && !checkAgainst.IsUnmanagedType) {
                            return tree.AddFalse();
                        }
                        if (typeParameterConstraint.HasValueTypeConstraint && !checkAgainst.IsValueType) {
                            return tree.AddFalse();
                        }
                        if (typeParameterConstraint.ConstraintTypes.Length == 0) {
                            return tree.AddChild((typeParameterConstraint, checkAgainst));
                        } else {

                            var subTree = tree;
                            foreach (var typeConstraint in typeParameterConstraint.ConstraintTypes) {
                                subTree = CheckTypeConsraint(typeConstraint, checkAgainst, subTree, currentTypeConstraint: typeParameterConstraint);
                                if (subTree.Length == 0) {
                                    break;
                                }
                            }
                            return subTree;
                        }

                    }
                }).AddTrue();

                return tree;


            } else if (constraint is IArrayTypeSymbol arrayConstraint && checkAgainst is IArrayTypeSymbol arrayCheckedAgainst) {
                return CheckTypeConsraint(arrayConstraint.ElementType, arrayCheckedAgainst.ElementType, tree).AddChild((currentTypeConstraint, checkAgainst));
            } else if (constraint is IDynamicTypeSymbol dynamicConstraint && checkAgainst is IDynamicTypeSymbol dynamicCheckedAgainst) {
                return tree.AddChild((currentTypeConstraint, checkAgainst));
            } else if (constraint is IFunctionPointerTypeSymbol functionPointerConstraint && checkAgainst is IFunctionPointerTypeSymbol functionPointerCheckedAgainst) {
                return SymbolEqualityComparer.Default.Equals(functionPointerConstraint.Signature, functionPointerCheckedAgainst.Signature) ? tree.AddChild((currentTypeConstraint, checkAgainst)) : tree.AddFalse();
            } else if (constraint is IPointerTypeSymbol pointerConstraint && checkAgainst is IPointerTypeSymbol pointerCheckedAgainst) {
                return CheckTypeConsraint(pointerConstraint.PointedAtType, pointerCheckedAgainst.PointedAtType, tree).AddChild((currentTypeConstraint, checkAgainst));
            } else {
                return tree.AddFalse();
            }

        }



        T GeneralizeGeneric<T>(T symbol) where T : ITypeSymbol {
            if (symbol is INamedTypeSymbol namedSymbol && namedSymbol.IsGenericType) {
                return (T)namedSymbol.ConstructUnboundGenericType(); // If symbol is INamedTypeSymbol then INamedTypeSymbol can be assingend to T

            }
            return symbol;
        }

    }
    private static IEnumerable<ITypeParameterSymbol> GetAllReferencedTypeParameter(ITypeSymbol symbol) {
        if (symbol is INamedTypeSymbol namedTypeSymbol) {
            return namedTypeSymbol.TypeArguments.OfType<ITypeParameterSymbol>();
        } else if (symbol is IArrayTypeSymbol arrayTypeSymbol) {
            return GetAllReferencedTypeParameter(arrayTypeSymbol.ElementType);
        } else if (symbol is ITypeParameterSymbol typeParameterSymbol) {
            return typeParameterSymbol.ConstraintTypes.SelectMany(GetAllReferencedTypeParameter).Concat(Enumerable.Repeat(typeParameterSymbol, 1));
            //} else if (symbol is IPointerTypeSymbol pointerTypeSymbol) {
            //} else if (symbol is IDynamicTypeSymbol dynamicTypeSymbol) {
            //} else if (symbol is IErrorTypeSymbol errorTypeSymbol) {
            //} else if (symbol is IFunctionPointerTypeSymbol functionPointerTypeSymbol) {
        } else {
            return Enumerable.Empty<ITypeParameterSymbol>();
        }
    }
}
