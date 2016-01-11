#!/bin/sh
N=$1
shift
./soln "$@" +RTS -N -qa -RTS
