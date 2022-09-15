# Start from a Shiny Server container from the Rocker Project
# https://rocker-project.org/
FROM rocker/shiny:4.2.0

# Install necessary Ubuntu software packages
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    git-core \
    libxml2-dev \
    nano
# NOTE: nano is there for looking at logs

# Install necessary R packages
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
COPY ./ /srv/shiny-server/wvi/

# Use a modified Shiny Server configuration
COPY ./docker/shiny-server.conf /etc/shiny-server/

USER shiny

# Run app
CMD ["/usr/bin/shiny-server"]
