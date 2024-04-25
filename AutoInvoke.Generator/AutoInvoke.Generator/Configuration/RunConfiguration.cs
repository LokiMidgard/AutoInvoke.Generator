// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;

namespace AutoInvoke.Generator.Configuration;

internal record RunConfiguration {
    public bool ScanExternalAssemblys { get; set; } = false;
    public required ImmutableList<RegexConstraint> TypesToHandle { get; set; }
    public bool CallForAbstractClasses { get; set; } = false;
    public bool CallForInterfaces { get; set; } = false;
    public bool CallForStructs { get; set; } = true;
    public bool CallForEnums { get; set; } = true;
    public bool CallForRecords { get; set; } = true;
    public bool CallForClasses { get; set; } = true;
}
