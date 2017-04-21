#!/bin/sh

export PATH=$PATH:$HOME/.cache/rebar3/bin

rebar3 compile
tail -f /var/log/*
