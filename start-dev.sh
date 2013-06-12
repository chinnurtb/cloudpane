#!/bin/sh
cd `dirname $0`
exec erl -sname cloudpane -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s cloudpane -mnesia dir '"db/production"' $1 
