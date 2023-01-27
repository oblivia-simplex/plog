IMAGE=pseudosue/plog

all: $(IMAGE)

$(IMAGE): Dockerfile
	docker build -t $(IMAGE) .

publish: $(IMAGE)
	docker push $(IMAGE)
