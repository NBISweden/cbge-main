# syntax=docker/dockerfile:1.4
FROM --platform=linux/amd64 rocker/shiny:latest

# General updates and system dependencies
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y \
    git \
    libxml2-dev \
    libmagick++-dev \
    libssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libglpk-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install renv
RUN Rscript -e 'install.packages("renv")'

# Copy renv.lock file
COPY renv.lock /srv/shiny-server/renv.lock

# Restore R environment
RUN Rscript -e 'setwd("/srv/shiny-server/");renv::restore();'

# Copy app files
COPY app/ /srv/shiny-server/

# Ensure shiny user exists
RUN if id shiny &>/dev/null && [ "$(id -u shiny)" -ne 999 ]; then \
        userdel -r shiny; \
        id -u 999 &>/dev/null && userdel -r "$(id -un 999)"; \
    fi; \
    useradd -u 999 -m -s /bin/bash shiny; \
    chown -R shiny:shiny /srv/shiny-server/ /var/lib/shiny-server/ /var/log/shiny-server/

# Switch to shiny user
USER shiny

# Expose port
EXPOSE 3838

# Run Shiny server
CMD ["/usr/bin/shiny-server"]