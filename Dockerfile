FROM swipl:latest

RUN apt-get update && \
  apt-get install -y git tmux && \
  rm -rf /var/lib/apt/lists

RUN mkdir /www

COPY . /www

WORKDIR /www

EXPOSE 9697

CMD tmux new-session swipl -l plog.pl -g start
