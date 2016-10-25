#!/bin/sh

export PATH=$PATH:$HOME/.cache/rebar3/bin

DIR=`dirname $0`

# start tests
cd $DIR
rebar3 do compile, eunit, cover -v
