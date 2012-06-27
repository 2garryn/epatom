#!/bin/sh

exec erl -pa ./ebin deps/*/ebin -boot start_sasl 