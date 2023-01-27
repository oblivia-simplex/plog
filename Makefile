IMAGE=plog

all: $(IMAGE)

$(IMAGE): Dockerfile
	docker build -t $(IMAGE) .

