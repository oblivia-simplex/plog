FROM swipl:latest

RUN apt-get update && \
  apt-get install -y git screen

RUN mkdir /www

COPY . /www

WORKDIR /www

EXPOSE 80

CMD screen swipl -l plog.prolog -g 'server(80)'
