#!/bin/bash
erl -noshell -eval "io:fwrite(\"~p~n\",['puzzle-8-queens':queens()]),halt()"
