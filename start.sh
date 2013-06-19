#!/bin/sh
cd `dirname $0`
exec erl -sname cloudpane -pa $PWD/deps/*/ebin $PWD/apps/cloudpane/ebin -s reloader -s cloudpane $1 
