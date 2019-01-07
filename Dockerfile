# Dockerfile template taken from 
# https://www.bjoern-hartmann.de/post/learn-how-to-dockerize-a-shinyapp-in-7-steps/ 

# Install R version 3.5
FROM rocker/r-ver:devel

# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    wget

# Download and install shiny server 
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    # TODO check if further package if you needed
    R -e "install.packages(c('shiny', 'shinydashboard', 'grDevices', 'zoo', 'lattice', 'ggplot2', 'reshape2', 'lubridate', 'scales', 'readxl', 'tidyverse', 'rmarkdown'), repos='$MRAN')" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ 

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /app /srv/shiny-server/

# Make the ShinyApp available at port 3838
EXPOSE 3838

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
