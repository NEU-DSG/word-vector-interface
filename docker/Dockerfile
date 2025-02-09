# Start from a Shiny Server container from the Rocker Project
FROM --platform=linux/amd64 rocker/shiny:4.2

# Install necessary Ubuntu software packages
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    git-core \
    libxml2-dev \
    nano

# Install necessary R packages
# NOTE: By using the "repos" parameter option, the R packages are version-locked to 
#   those available on a certain date.
RUN R -e 'install.packages(c(\
        "shiny", \
        "shinydashboard", \
        "shinyjs", \
        "magrittr", \
        "DT", \
        "rjson", \
        "tidyverse", \
        "wordcloud", \
        "ggrepel", \
        "devtools" \
      ), \
      repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2022-09-14"\
    ); \
    library("devtools"); \
    devtools::install_github("bmschmidt/wordVectors")'

# Copy application into Docker image
COPY --chown=shiny ./ /srv/shiny-server/wvi/

# Use a modified Shiny Server configuration
COPY ./docker/shiny-server.conf /etc/shiny-server/

# Log Shiny app errors to standard error
ENV SHINY_LOG_STDERR=1

USER shiny

# Run app
CMD ["/usr/bin/shiny-server"]
