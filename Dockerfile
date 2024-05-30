# Use the rocker/shiny base image
FROM rocker/shiny:latest

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && apt-get clean

# Install R packages from CRAN
COPY requirements.txt /requirements.txt
RUN R -e "packages <- scan('/requirements.txt', what = character()); install.packages(packages)"

# Copy the Shiny app to the image
COPY . /srv/shiny-server/

# Make sure the app is runnable by any user
RUN chown -R shiny:shiny /srv/shiny-server

# Expose the port where the Shiny app will run
EXPOSE 3838

# Run the Shiny app
CMD ["/usr/bin/shiny-server"]
