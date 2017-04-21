#!/bin/sh

DIR=$HOME/.cache/rebar3/bin

mkdir -p $DIR
wget https://s3.amazonaws.com/rebar3/rebar3 -O $DIR/rebar3
chmod +x $DIR/rebar3
