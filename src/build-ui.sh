#!/bin/bash

elm-make App.elm --output App.js
cp index.html ~/Documents/Programming/Clojure/rdb/resources/public
cp main.css ~/Documents/Programming/Clojure/rdb/resources/public
cp App.js ~/Documents/Programming/Clojure/rdb/resources/public
