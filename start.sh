#!/bin/sh
cd `dirname $0`
exec erl -sname cloudpane -pa $PWD/ebin $PWD/deps/*/ebin -s reloader -s cloudpane $1 
