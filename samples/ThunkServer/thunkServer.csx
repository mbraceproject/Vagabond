#r "bin/Debug/netcoreapp3.1/FsPickler.dll"
#r "bin/Debug/netcoreapp3.1/Vagabond.AssemblyParser.dll"
#r "bin/Debug/netcoreapp3.1/Vagabond.dll"
#r "bin/Debug/netcoreapp3.1/ThunkServer.exe"

// before running sample, don't forget to set binding redirects to FSharp.Core in InteractiveHost.exe

using System.Linq;
using ThunkServer;

ThunkClient.Executable = "bin/Debug/net45/ThunkServer.exe";
var client = ThunkClient.InitLocal();

client.EvaluateDelegate(() => Console.WriteLine("C# Interactive, meet Vagabond!"));

client.EvaluateDelegate(() => System.Diagnostics.Process.GetCurrentProcess().Id);

client.EvaluateDelegate(() => Enumerable.Range(0, 10).Select(x => x + 1).Where(x => x % 2 == 0).Sum());

int x = 1;
for (int i = 0; i < 10; i++)
    x = client.EvaluateDelegate(() => x + x);

x;