#!/bin/sh

exec erl -pa apps/epatom/ebin deps/*/ebin -boot start_sasl 