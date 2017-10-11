using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Mono.Cecil;

namespace MBrace.Vagabond.AssemblyParser
{
    class AppDomainAssemblyResolver : DefaultAssemblyResolver
    {
        Dictionary<string, AssemblyDefinition> cache = new Dictionary<string, AssemblyDefinition>();

        public override AssemblyDefinition Resolve(AssemblyNameReference name)
        {
            AssemblyDefinition assembly;

            if (cache.TryGetValue(name.FullName, out assembly))
                return assembly;

            var appDomainAssembly =
                System.AppDomain.CurrentDomain.GetAssemblies()
                    .Where(a => a.FullName == name.FullName)
                    .FirstOrDefault();

            if (appDomainAssembly?.Location != null)
            {
                assembly = AssemblyDefinition.ReadAssembly(appDomainAssembly.Location);
                cache[assembly.FullName] = assembly;
                return assembly;
            }

            return base.Resolve(name);
        }
    }
}
