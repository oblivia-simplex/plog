FROM swipl:latest

RUN apt-get update && \
  apt-get install -y git screen && \
  rm -rf /var/lib/apt/lists

RUN mkdir /www

COPY . /www

WORKDIR /www

EXPOSE 9697

CMD screen swipl -l plog.pl -g start
