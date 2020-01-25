#!/bin/bash

set -eu
cd `dirname $0`

dotnet tool restore
dotnet paket restore

MONO=""
[ "X$OS" = "XWindows_NT" ] || MONO=mono

$MONO packages/build/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
