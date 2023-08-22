using Microsoft.CodeAnalysis;

using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

namespace AutoInvoke.Generator;

internal class TypeConstraintResultTree {

    public TypeConstraintResultTree? Parent { get; }
    public IReadOnlyList<TypeConstraintResultTree> Childrean { get; }
    private List<TypeConstraintResultTree> childrean;
    public bool Succsess { get; private set; }
    public IImmutableDictionary<ITypeParameterSymbol, ITypeSymbol> TypeMapping { get; }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="parent"></param>
    /// <param name="typeMapping"></param>
    /// <param name="initalSuccsess">if <code>false</code>, <see cref="TypeConstraintResultTree.Succsess"/> is <code>false</code>. Otherwise Parent Type mappings are checked</param>
    private TypeConstraintResultTree(TypeConstraintResultTree? parent, IImmutableDictionary<ITypeParameterSymbol, ITypeSymbol> typeMapping, bool initalSuccsess) {
        childrean = new List<TypeConstraintResultTree>();
        Childrean = childrean.AsReadOnly();
        Parent = parent;

        // succsess is true unless we find a conflicting TypeMapping
        Succsess = initalSuccsess && (Parent?.Succsess ?? true);
        if (Parent is not null && Succsess) {
            foreach (var key in typeMapping.Keys) {
                if (this.Parent.TypeMapping.TryGetValue(key, out var other)) {
                    if (!SymbolEqualityComparer.Default.Equals(other, typeMapping[key])) {
                        Succsess = false; break;
                    }
                }
            }
        }
        if (Succsess) {
            if (parent is not null) {
                TypeMapping = typeMapping.AddRange(parent.TypeMapping);
            } else {
                TypeMapping = typeMapping;
            }
        } else {
            TypeMapping = ImmutableDictionary<ITypeParameterSymbol, ITypeSymbol>.Empty;
        }
    }

    public TypeConstraintResultTree AddTrue() {
        return this; // we can ignore nodes that are just `true`
    }
    public TypeConstraintResultTree AddFalse() {
        return new TypeConstraintResultTree(this, ImmutableDictionary.Create<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default), false);
    }
    public static TypeConstraintResultTree CreateRoot() {
        return new TypeConstraintResultTree(null, ImmutableDictionary.Create<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default), true);
    }

    public TypeConstraintResultTree AddChild(IImmutableDictionary<ITypeParameterSymbol, ITypeSymbol> typeMapping) {
        return new TypeConstraintResultTree(this, typeMapping, true);
    }
    public TypeConstraintResultTree AddChild(IEnumerable<(ITypeParameterSymbol typeParameter, ITypeSymbol type)> typeMapping) {
        return new TypeConstraintResultTree(this, typeMapping.ToImmutableDictionary<(ITypeParameterSymbol typeParameter, ITypeSymbol type), ITypeParameterSymbol, ITypeSymbol>(x => x.typeParameter, x => x.type, SymbolEqualityComparer.Default), true);
    }

    public ImmutableArray<TypeConstraintResultTree> ToList() {
        return ImmutableArray.Create(this);
    }
}

internal static class TypeConstraintResultTreeExtension {
    public static ImmutableArray<TypeConstraintResultTree> AddTrue(this IEnumerable<TypeConstraintResultTree> list) {
        return list is ImmutableArray<TypeConstraintResultTree> immutableArray ? immutableArray : list.ToImmutableArray();
    }
    public static ImmutableArray<TypeConstraintResultTree> AddFalse(this IEnumerable<TypeConstraintResultTree> list) {
        foreach (var item in list) {
            item.AddFalse();
        }
        return ImmutableArray<TypeConstraintResultTree>.Empty;
    }

    public static ImmutableArray<TypeConstraintResultTree> AddChild(this IEnumerable<TypeConstraintResultTree> list, IImmutableDictionary<ITypeParameterSymbol?, ITypeSymbol> typeMapping) {
        if (typeMapping is null) {
            return list.AddTrue();
        }
        return list.Select(x => x.AddChild(typeMapping!)).Where(x => x.Succsess).ToImmutableArray();
    }
    public static ImmutableArray<TypeConstraintResultTree> AddChild(this IEnumerable<TypeConstraintResultTree> list, IEnumerable<(ITypeParameterSymbol? typeParameter, ITypeSymbol type)> typeMapping) {
        if (typeMapping is null) {
            return list.AddTrue();
        }
        return list.Select(x => x.AddChild(typeMapping.Where(x => x.typeParameter is not null)!)).Where(x => x.Succsess).ToImmutableArray();
    }
    public static ImmutableArray<TypeConstraintResultTree> AddChild(this IEnumerable<TypeConstraintResultTree> list, params (ITypeParameterSymbol? typeParameter, ITypeSymbol type)[] typeMapping) {
        if (typeMapping is null) {
            return list.AddTrue();
        }
        return list.Select(x => x.AddChild(typeMapping.Where(x => x.typeParameter is not null)!)).Where(x => x.Succsess).ToImmutableArray();
    }
}