ARG R_VERSION=4.2.0
FROM r-base:${R_VERSION}

# install devtools
RUN R -e "install.packages('deSolve')"
RUN R -e "install.packages('RColorBrewer')"
RUN R -e "install.packages('zoo')"
RUN R -e "install.packages('xts')"

# COPY the catflow package
COPY ./Catflow ./Catflow

# install Catflow
RUN R -e "install.packages('./Catflow', repos=NULL, type='source')"
RUN rm -rf ./Catflow

# build working area
RUN mkdir -p /Catflow-TEST/out
COPY  ./run.R /Catflow-TEST/run.R

# create tool infrastructure
RUN mkdir /spec
COPY ./tools.yaml /spec/tools.yaml


WORKDIR /Catflow-TEST

# run container
CMD ["Rscript", "run.R"]