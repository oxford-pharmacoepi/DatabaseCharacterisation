# THIS CODE IS TO RUN THE DATABASE CHARCTERISATION IN THE EUNOMIA
# SYNTHETIC DATABASE. IF YOU WANT TO RUN THE CODE IN YOUR OWN DATA PLEASE
# USE THE FILE CodeToRun.R

# renv::activate()
# renv::restore()

library(CDMConnector)
library(DBI)
library(log4r)
library(dplyr)
library(here)
library(readr)
library(glue)
library(PatientProfiles)
library(IncidencePrevalence)
library(omopgenerics)

# database metadata and connection details
# The name/ acronym for the database
dbName <- "GiBleed"

pathEunomia <- here("Eunomia")
if (!dir.exists(pathEunomia)) {
  dir.create(pathEunomia)
}
downloadEunomiaData(datasetName = dbName, pathToData = pathEunomia)
Sys.setenv("EUNOMIA_DATA_FOLDER" = pathEunomia)

# Database connection details
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below
# https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html 
# for more details.
db <- dbConnect(duckdb::duckdb(), eunomia_dir())

# The name of the schema that contains the OMOP CDM with patient-level data
cdmSchema <- "main"

# The name of the schema where results tables will be created and a prefix for
# all the permanent tables created
writeSchema <- c(schema = "main", prefix = "mc_")

# minimum counts that can be displayed according to data governance
minCellCount <- 8

# Run the study
source(here("RunStudy.R"))

# after the study is run you should have a zip folder in your output folder to share
cli::cli_alert_success("Study finished")
