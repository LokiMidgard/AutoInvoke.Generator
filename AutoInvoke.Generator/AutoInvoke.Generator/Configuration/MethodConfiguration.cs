// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace AutoInvoke.Generator.Configuration;

internal record MethodConfiguration {
    public required ImmutableList<RunConfiguration> Configurations { get; set; }
    public required TypeDeclarationSyntax DefinedIn { get; set; }
    public required IMethodSymbol MethodToCall { get; set; }
    public required string ImplementedMethodName { get; set; }
    public required string Namespace { get; set; }
    public required bool IsStactic { get; set; }
    public TypeSyntax? ReturnType { get; set; }
    public ImmutableArray<IParameterSymbol> Parameters { get; internal set; }
}

