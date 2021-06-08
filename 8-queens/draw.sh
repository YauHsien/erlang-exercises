#!/bin/bash
erl -noshell -eval "wx:new(),io:fwrite(\"~p~n\",[image_list:generate_image('puzzle-8-queens':queens(), 3, 0.5, \"result.png\")]),wx:destroy(),halt()"
