using System;
using System.Reflection;

namespace Nessos.Vagrant.Cecil
{
    public interface IAssemblyParserConfig
    {
        bool MakePublicDefinition(MemberInfo member);
        bool IgnoreDefinition(MemberInfo member);
        MemberInfo RemapReference(MemberInfo member);
    }

    class DefaultAssemblyParserConfig : IAssemblyParserConfig
    {
        public DefaultAssemblyParserConfig() { }

        public bool MakePublicDefinition(MemberInfo member) { return false; }
        public bool IgnoreDefinition(MemberInfo member) { return false; }
        public MemberInfo RemapReference(MemberInfo member) { return member; }
    }
}
