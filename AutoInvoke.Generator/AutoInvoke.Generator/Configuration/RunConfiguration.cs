// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;

namespace AutoInvoke.Generator.Configuration;

internal record RunConfiguration {
    public bool ScanExternalAssemblys { get; set; } = false;
    public required ImmutableList<TypeConfiguration> TypeConfigurations { get; set; }
}


internal record TypeConfiguration {
    public required CallFor CallFor { get; set; }
    public required string? TypeNamePattern { get; set; }
}