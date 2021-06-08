#!/bin/bash
erl -noshell -eval "wx:new(),image_list:generate_image('puzzle-8-queens':queens(), 3, 0.5, \"result.png\"),wx:destroy(),halt()"
