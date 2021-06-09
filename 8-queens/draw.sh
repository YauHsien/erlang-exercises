#!/bin/bash
mkdir -p solutions
rm -f solutions/*
erl -noshell -eval "wx:new(),image_list:generate_image('puzzle-8-queens':queens(), 1, \"solutions/result-~p.png\"),wx:destroy(),halt()"
