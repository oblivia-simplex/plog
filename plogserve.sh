#! /usr/bin/env bash

# get absolute path of $1

blog_path=$(realpath "$1")
bind="$2"
port="$3"

if [ -z "$blog_path" ]; then
    echo "Usage: $0 <blog_path> [bind] [port]"
    echo "or     $0 <blog_path> [port] # to bind to localhost"
    echo "or     $0 <blog_path>        # to use default port 9697"
    exit 1
fi

if [ -z "$port" ]; then
    port="$bind"
    bind="127.0.0.1"
fi

if [ -z "$port" ]; then
    port="9697"
fi

name="$(basename "$blog_path" | tr -dc 'A-Za-z0-9_')_$(printf "%04x" ${RANDOM})_plog"

echo "=== Serving $blog_path on $bind:$port in container $name ===" >&2

docker run --rm -tid \
  -v "$blog_path":/www/content \
  -p $bind:$port:9697 \
  --name $name \
  pseudosue/plog
