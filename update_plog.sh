#! /usr/bin/env bash

# Check to see if there have been any updates to the plog docker image
read -r digest status < <(docker pull pseudosue/plog:latest |  awk -F': ' '/Digest/,/Status/ { printf "%s ",$2 }')
echo "digest = $digest"
echo "status = $status"

# Get running plog containers
mapfile -t CONTAINERS < <(docker container ls -a | awk '$2=="plog"{ print $1 }')

for c in ${CONTAINERS[@]}; do
	image=$(docker container inspect $c | jq -r .[].Image)
	blogdir=$(docker container inspect $c | jq -r '.[].HostConfig.Binds[]|select(endswith("content"))' | cut -d: -f1)
	port=$(docker container inspect $c | jq -r '.[].NetworkSettings.Ports | keys[0]' | cut -d/ -f1)

	echo "Image: $image"
	echo "Blog:  $blogdir"
	echo "Port:  $port"

	if [[ "$image" == "$digest" ]]; then
		echo "[+] Up to date!"
		continue
	fi

	echo "[-] Needs updating."
	echo "[-] Stopping container $c"
	docker container stop $c
	./plog/plogserve.sh "$blogdir" "127.0.0.1" "$port"
done

