#!/bin/sh

set -ex

elm-make src/Main.elm --output script.js
elm-css src/Stylesheets.elm
