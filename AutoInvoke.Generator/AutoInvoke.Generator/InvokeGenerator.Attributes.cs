// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;
using System.ComponentModel;

namespace AutoInvoke;

[SourceGenerator.Helper.CopyCode.Copy]
[System.AttributeUsage(System.AttributeTargets.Method, Inherited = false, AllowMultiple = false)]
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

    /// <summary>
    /// The name of the generated method.
    /// </summary>
    public string MethodName { get; set; }



#pragma warning restore CS0169 // Remove unused parameter
#pragma warning restore IDE0060 // Remove unused parameter
#pragma warning restore CS8618 // Non-nullable field must contain a non-null value when exiting constructor. Consider declaring as nullable.
}

[SourceGenerator.Helper.CopyCode.Copy]
[System.AttributeUsage(System.AttributeTargets.GenericParameter, Inherited = false, AllowMultiple = false)]
internal sealed class CallForAttribute : System.Attribute {

    // This is a positional argument
    public CallForAttribute(CallFor callFor) {
        this.CallFor= callFor;

    }

    /// <summary>
    /// Specifies what kind of types this attribute applies to.
    /// </summary>
    public CallFor CallFor{ get; }

    // This is a named argument
    /// <summary>
    /// The pattern to match the type name against.
    /// </summary>
    public string[]? Pattern { get; set; }
}

[SourceGenerator.Helper.CopyCode.Copy]
[System.Flags]
internal enum CallFor {
    None= 0,
    AbstractClass = 1 << 0,
    Interface = 1 << 1,
    Struct = 1 << 2,
    Class = 1 << 3,
    RecordClass = 1 << 4,
    Enum = 1 << 5,
    RecordStruct = 1 << 6,
    AbstractTypes = AbstractClass | Interface,
    All = AbstractClass | Interface | Struct | Class | RecordClass | RecordStruct | Enum,
    NonAbstractTypes = Struct | Class | Enum | RecordStruct | RecordClass,
}
