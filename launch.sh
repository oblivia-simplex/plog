#! /usr/bin/env bash

PORT="$1"

if [ -z "$PORT" ] ; then
  if (( $(id -u) == 0 )) ; then
    PORT=80
  else
    PORT=8080
  fi
fi

screen swipl -l plog.prolog -g "server($PORT)"

