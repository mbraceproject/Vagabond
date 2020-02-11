using System;
using System.Reflection;

using Mono.Cecil;

namespace MBrace.Vagabond.AssemblyParser
{
    /// <summary>
    /// Dynamic type parse action enumeration.
    /// </summary>
    public enum TypeParseAction 
    {
        /// <summary>
        /// Ignores specified type.
        /// </summary>
        Ignore, 
        /// <summary>
        /// Only parse nested types. Nested types will simply keep the same qualified name.
        /// </summary>
        ParseNested, 
        /// <summary>
        /// Parse this and all nested types.
        /// </summary>
        ParseAll 
    }

    /// <summary>
    ///     Assembly parser configuration object
    /// </summary>
    public interface IAssemblyParserConfig
    {
        /// <summary>
        /// Specifies if provided MemberInfo should be made public.
        /// </summary>
        /// <param name="member">Member under test.</param>
        /// <returns></returns>
        bool MakePublic(MemberInfo member);

        /// <summary>
        /// Specifies if provided MemberInfo should be erased.
        /// </summary>
        /// <param name="member">Member under test.</param>
        /// <returns></returns>
        bool EraseMember(MemberInfo member);

        /// <summary>
        /// Gets parse action for supplied System.Type
        /// </summary>
        /// <param name="type">Type under test.</param>
        /// <returns></returns>
        TypeParseAction GetTypeParseAction(Type type);
        
        /// <summary>
        /// Specifies if supplied type reference should be remapped.
        /// </summary>
        /// <param name="type">Type reference to be checked.</param>
        /// <param name="outType">Byref type to be assigned for remapping.</param>
        /// <returns>True if remapping is required.</returns>
        bool RemapReference(Type type, out Type outType);
    }

    /// <summary>
    /// Default Assembly parsing configuration
    /// </summary>
    public class DefaultAssemblyParserConfig : IAssemblyParserConfig
    {
        bool IAssemblyParserConfig.MakePublic(MemberInfo member) { return false; }
        bool IAssemblyParserConfig.EraseMember(MemberInfo member) { return false; }

        TypeParseAction IAssemblyParserConfig.GetTypeParseAction(Type type) { return TypeParseAction.ParseAll; }

        bool IAssemblyParserConfig.RemapReference(Type type, out Type outType) { outType = null; return false; }
    }
}
