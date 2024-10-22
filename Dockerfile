FROM ubuntu:22.04
RUN mkdir -p /opt/almamap \
        && apt-get update \
        && apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
        && apt-get install -y ca-certificates && update-ca-certificates \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

ARG YESOD_DEMO_LANG=EN
ARG YESOD_MAPBOX_PK

WORKDIR       /opt/almamap
COPY almamap /opt/almamap
COPY static   /opt/almamap/static
COPY config   /opt/almamap/config
COPY demo     /opt/almamap/demo

ENV YESOD_PORT=8080
ENV YESOD_DEMO_LANG=${YESOD_DEMO_LANG}
ENV YESOD_MAPBOX_PK=${YESOD_MAPBOX_PK}

EXPOSE 8080
CMD ["./almamap"]
