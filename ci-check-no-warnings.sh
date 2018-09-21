#!/usr/bin/env bash


rm -rf elm-stuff/build-artifacts/0.19.0/user && elm-make src/MusicTheory.elm --warn --output /dev/null 2>&1 | while read -r line ; do
    if [[ "$line" = *"WARNINGS"* ]]; then
        exit 1
    fi
done
