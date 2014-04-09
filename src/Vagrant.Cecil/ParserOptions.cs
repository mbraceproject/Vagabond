using System;
using System.Reflection;

namespace Nessos.Vagrant.Cecil
{
    interface IAssemblyParseOptions
    {
        bool MakePublic { get; }
        bool IgnoreType(Type type);
        MemberInfo RemapReference(MemberInfo member);
    }

    class DefaultAssemblyParseOptions : IAssemblyParseOptions
    {
        public DefaultAssemblyParseOptions() { }

        public bool MakePublic { get { return true; } }
        public bool IgnoreType(Type type) { return false; }
        public MemberInfo RemapReference(MemberInfo member) { return member; }
    }
}
