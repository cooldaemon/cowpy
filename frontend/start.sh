#!/bin/sh
erl -sname cowpy -pa ebin -pa deps/*/ebin -s cowpy \
	-eval "io:format(\"~n~nThe following examples are available:~n\")." \
	-eval "io:format(\"* WSGI Handler: http://localhost:8080/~n\")."
