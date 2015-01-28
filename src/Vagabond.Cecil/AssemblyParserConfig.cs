using System;
using System.Reflection;

using Mono.Cecil;

namespace Nessos.Vagabond.Cecil
{
    public enum TypeParseAction { Ignore, ParseNested, ParseAll };

    public interface IAssemblyParserConfig
    {
        bool MakePublic(MemberInfo member);
        bool EraseMember(MemberInfo member);

        TypeParseAction GetTypeParseAction(Type type);
        
        bool RemapReference(Type type, out Type outType);
    }

    class DefaultAssemblyParserConfig : IAssemblyParserConfig
    {
        public DefaultAssemblyParserConfig() { }

        public bool MakePublic(MemberInfo member) { return false; }
        public bool EraseMember(MemberInfo member) { return false; }

        public TypeParseAction GetTypeParseAction(Type type) { return TypeParseAction.ParseAll; }

        public bool RemapReference(Type type, out Type outType) { outType = null; return false; }
    }
}
