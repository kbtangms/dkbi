FROM rocker/r-ver:3.5.1

LABEL author="Thomas Jc"

# Set up necessary dependencies
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libxml2-dev \
  libcairo2-dev \
  libsqlite-dev \
  libmariadbd-dev \
  libmariadb-client-lgpl-dev \
  libpq-dev \
  libssh2-1-dev \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  libjpeg62-turbo-dev \
  libsasl2-dev \
  curl

# Install common R packages (note: dev-cran)
RUN install2.r --error \
    --repos "http://bizops.danke.life:4242/dev-cran/latest" \
    --deps TRUE \
    tidyverse \
    dplyr \
    lubridate \
    devtools \
    formatR \
    remotes \
    selectr \
    DBI \
    RMySQL \
    mongolite \
    openxlsx \
    attempt \
    httr \
    jsonlite \
    base64enc \
    cli \
    plumber

# Internal package (note: dev-internal)
RUN install2.r --error \
    --repos "http://bizops.danke.life:4242/dev-internal/latest" \
    --deps TRUE \
    dkbi

# Working directory
WORKDIR /app

# Set timezone to China/Shanghai
ENV TZ=Asia/Shanghai
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && \
    echo $TZ > /etc/timezone

# On boot-up
CMD ["R"]
