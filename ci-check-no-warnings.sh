#!/usr/bin/env bash


rm -rf elm-stuff/build-artifacts/0.18.0/user && elm-make src/Main.elm --warn --output /dev/null 2>&1 | while read -r line ; do
    if [[ "$line" = *"WARNINGS"* ]]; then
        exit 1
    fi
done
