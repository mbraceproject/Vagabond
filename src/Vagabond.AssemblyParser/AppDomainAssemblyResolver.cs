using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using System.Threading.Tasks;
using Mono.Cecil;

namespace MBrace.Vagabond.AssemblyParser
{
    class AppDomainAssemblyResolver : DefaultAssemblyResolver
    {
        private readonly Dictionary<string, AssemblyDefinition> _cache = new Dictionary<string, AssemblyDefinition>();
        private static readonly Func<IEnumerable<Assembly>> s_loadContextReader = GetLoadedAssemblyReader();

        public override AssemblyDefinition Resolve(AssemblyNameReference name)
        {
            AssemblyDefinition assembly;

            if (_cache.TryGetValue(name.FullName, out assembly))
                return assembly;

            var appDomainAssembly = s_loadContextReader()
                .Where(a => a.FullName == name.FullName)
                .FirstOrDefault();

            if (appDomainAssembly?.Location != null)
            {
                assembly = AssemblyDefinition.ReadAssembly(appDomainAssembly.Location);
                _cache[assembly.FullName] = assembly;
                return assembly;
            }

            return base.Resolve(name);
        }

        private static Func<IEnumerable<Assembly>> GetLoadedAssemblyReader()
        {
            // AssemblyLoadContext is not available in netstandard2.0
            // Use reflection to access its APIs
            var loadContextTy = Type.GetType("System.Runtime.Loader.AssemblyLoadContext");
            if (loadContextTy is null)
            {
                return (() => System.AppDomain.CurrentDomain.GetAssemblies());
            }

            var asm = Assembly.GetExecutingAssembly();
            var loadContextM = loadContextTy.GetMethod("GetLoadContext", BindingFlags.Static | BindingFlags.Public);
            var currentLoadContext = loadContextM.Invoke(null, new[] { asm });
            var assembliesProperty = loadContextTy.GetProperty("Assemblies");
            return (() => (IEnumerable<Assembly>) assembliesProperty.GetValue(currentLoadContext));
        }
    }
}
