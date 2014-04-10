using System;
using System.Reflection;

namespace Nessos.Vagrant.Cecil
{
    public enum TypeParseAction { Ignore, ParseNested, ParseAll };

    public interface IAssemblyParserConfig
    {
        bool MakePublic(MemberInfo member);
        bool EraseMember(MemberInfo member);

        TypeParseAction GetTypeParseAction(Type type);
        MemberInfo RemapReference(MemberInfo member);
    }

    class DefaultAssemblyParserConfig : IAssemblyParserConfig
    {
        public DefaultAssemblyParserConfig() { }

        public bool MakePublic(MemberInfo member) { return false; }
        public bool EraseMember(MemberInfo member) { return false; }

        public TypeParseAction GetTypeParseAction(Type type) { return TypeParseAction.ParseAll; }
        public MemberInfo RemapReference(MemberInfo member) { return member; }
    }
}
