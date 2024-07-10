# Base R Shiny image
FROM rocker/shiny

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# Create a directory in the container
RUN mkdir /home/shiny-app

# Set the CRAN repository
RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org/'))"

# Install renv
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Copy the Shiny app files and renv settings
WORKDIR /home/shiny-app/
COPY app.R .
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY www/ www/
COPY .Renviron .Renviron

# Restore the R environment with renv
RUN R -e "renv::restore()"

# Expose the Shiny application port
EXPOSE 8180

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('/home/shiny-app')"]
