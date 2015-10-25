#r "../../bin/FsPickler.dll"
#r "../../bin/Vagabond.AssemblyParser.dll"
#r "../../bin/Vagabond.dll"
#r "../../bin/ThunkServer.exe"

// before running sample, don't forget to set binding redirects to FSharp.Core in InteractiveHost.exe

using System.Linq;
using ThunkServer;

ThunkClient.Executable = "../../bin/ThunkServer.exe";
var client = ThunkClient.InitLocal();

client.EvaluateDelegate(() => Console.WriteLine("C# Interactive, meet Vagabond!"));

client.EvaluateDelegate(() => System.Diagnostics.Process.GetCurrentProcess().Id);

client.EvaluateDelegate(() => Enumerable.Range(0, 10).Select(x => x + 1).Where(x => x % 2 == 0).Sum());