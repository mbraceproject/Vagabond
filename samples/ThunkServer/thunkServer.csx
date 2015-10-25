#r "../../bin/FsPickler.dll"
#r "../../bin/Vagabond.AssemblyParser.dll"
#r "../../bin/Vagabond.dll"
#r "../../bin/ThunkServer.exe"

using ThunkServer;

ThunkClient.Executable = "../../bin/ThunkServer.exe";
var client = ThunkClient.InitLocal();

client.EvaluateDelegate(() => Console.WriteLine("C# Interactive, meet Vagabond!"));

client.EvaluateDelegate(() => System.Diagnostics.Process.GetCurrentProcess().Id);