#!/bin/bash

set -eu
cd `dirname $0`

dotnet tool restore
dotnet fake build "$@"
