FROM rocker/shiny:latest

# Update and install system libraries
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    build-essential \
    libjpeg-dev \
    libpng-dev \
    gfortran

# Install R packages
RUN R -e "install.packages('vctrs', dependencies=TRUE, repos='http://cran.rstudio.com/')" && \
    R -e "install.packages(c('shiny', 'shinydashboard', 'shinycssloaders', 'readxl', 'DT', 'ggplot2', 'plotly', 'e1071', 'class', 'rpart', 'cluster', 'kernlab', 'Rtsne', 'factoextra', 'dplyr', 'gridExtra', 'caret'), dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Copy application files
COPY ./app.R /srv/shiny-server/app.R
COPY ./R /srv/shiny-server/R

# Set ownership
RUN chown -R shiny:shiny /srv/shiny-server

# Expose the application port
EXPOSE 3838

# Run the application
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app.R')"]
