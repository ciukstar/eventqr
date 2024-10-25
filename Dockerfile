FROM ubuntu:22.04
RUN mkdir -p /opt/eventqr \
        && apt-get update \
        && apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
        && apt-get install -y ca-certificates && update-ca-certificates \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

ARG YESOD_DEMO_LANG=EN
ARG YESOD_TIME_ZONE

WORKDIR       /opt/eventqr
COPY eventqr  /opt/eventqr
COPY static   /opt/eventqr/static
COPY config   /opt/eventqr/config
COPY demo     /opt/eventqr/demo

ENV YESOD_PORT=8080
ENV YESOD_DEMO_LANG=${YESOD_DEMO_LANG}
ENV YESOD_TIME_ZONE=${YESOD_TIME_ZONE}

EXPOSE 8080
CMD ["./eventqr"]
