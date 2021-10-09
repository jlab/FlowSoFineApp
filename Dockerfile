FROM rocker/r-ver:4.0.3

RUN apt-get update && apt-get install -y \
    sudo \
    libssl-dev \
    libcurl4-gnutls-dev \
    wget \
    libtool \
    zlib1g \
    zlib1g-dev \
    git

RUN git clone https://github.com/JonasKup/FlowSoFine.git

RUN R -e "install.packages(c('devtools'))"

RUN R -e "library('devtools'); devtools::install_git('FlowSoFine', upgrade='always')"


RUN git clone https://github.com/JonasKup/FlowSoFineApp.git

RUN sed -i "s/\\\(/function\(/" /FlowSoFineApp/inst/shiny-app/FlowSoFineApp/R/permanova_tab.R
RUN R -e "library('devtools'); devtools::install_git('FlowSoFineApp', upgrade='always')"

CMD ["R", "-e", "library('FlowSoFineApp'); shiny::runApp('/FlowSoFineApp/inst/shiny-app/FlowSoFineApp', host = '0.0.0.0', port = 3838)"]

