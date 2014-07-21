#!/usr/bin/env bash

rebar compile
cd rel; rebar generate
cd ..

./rel/thunderl/bin/thunderl console
