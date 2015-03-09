#!/bin/bash
if [ "X$OS" = "XWindows_NT" ] ; then
  # use .Net

  .vaket/vaket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  .vaket/vaket.exe restore -v
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  vackages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
else

  # use mono
  mono .vaket/vaket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  mono .vaket/vaket.exe restore -v
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi
  mono vackages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
fi