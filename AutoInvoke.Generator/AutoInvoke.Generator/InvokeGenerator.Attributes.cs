// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace AutoInvoke;

[SourceGenerator.Helper.CopyCode.Copy]
[System.AttributeUsage(System.AttributeTargets.Method, Inherited = false, AllowMultiple = true)]
[System.Diagnostics.Conditional("AutoNotifyGenerator_DEBUG")]
internal sealed class FindAndInvokeAttribute : System.Attribute {
#pragma warning disable CS0169 // Remove unused parameter
#pragma warning disable IDE0060 // Remove unused parameter
#pragma warning disable CS8618 // Non-nullable field must contain a non-null value when exiting constructor. Consider declaring as nullable.
    public FindAndInvokeAttribute() {

    }

    public FindAndInvokeAttribute(string pattern) {

    }

    public bool ScanExternalAssamblies { get; set; }
    public string MethodName { get; set; }
    public bool CallForAbstractClasses { get; set; }
    public bool CallForInterfaces { get; set; }
    public bool CallForStructs { get; set; }
    public bool CallForClasses { get; set; }
    public bool CallForRecords { get; set; }

#pragma warning restore CS0169 // Remove unused parameter
#pragma warning restore IDE0060 // Remove unused parameter
#pragma warning restore CS8618 // Non-nullable field must contain a non-null value when exiting constructor. Consider declaring as nullable.
}
