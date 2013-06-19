#!/bin/sh
cd `dirname $0`
exec erl -sname cloudpane -pa $PWD/apps/cloudpane/ebin $PWD/deps/*/ebin $1 
