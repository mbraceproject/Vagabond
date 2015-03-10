#!/bin/bash
if [ "X$OS" = "XWindows_NT" ] ; then
  # use .Net

  .Paket/Paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  .Paket/Paket.exe restore -v
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
else

  # use mono
  mono .Paket/Paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  mono .Paket/Paket.exe restore -v
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi
  mono packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
fi