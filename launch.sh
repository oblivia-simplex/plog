#! /usr/bin/env bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
##
# Set the bind port with the environment variable, PLOG_PORT
##

[ -f "${DIR}/content/env.rc" ] && source ${DIR}/content/env.rc

[ -n "$PLOG_PORT" ] || PLOG_PORT=80

[ -n "$PLOG_NAME" ] || PLOG_NAME=plog_`cat /dev/urandom | tr -dc 0-9 | head -c 5`

docker build -t plog ${DIR}

CMD="docker container run -dt \
  -it \
  -p ${PLOG_PORT}:80 \
  --mount=type=bind,source=${DIR}/content/,target=/www/content/ \
  plog"

$CMD

