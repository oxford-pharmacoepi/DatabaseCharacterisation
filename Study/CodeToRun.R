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
library(CohortCharacteristics)
library(testthat)

# database metadata and connection details
# The name/ acronym for the database
dbName <- "...."

# Database connection details
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below
# https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html 
# for more details.
db <- dbConnect("...")

# The name of the schema that contains the OMOP CDM with patient-level data
cdmSchema <- "..."

# The name of the schema where results tables will be created and a prefix for
# all the permanent tables created
writeSchema <- c(schema = "...", prefix = "...")

# minimum counts that can be displayed according to data governance
minCellCount <- "..."

# Set sampleValue as TRUE if you would like to run it in a 10K sample. Otherwise, set it to FALSE
sampleValue <- "..." # TRUE OR FALSE 

# Run the study
source(here("RunStudy.R"))

# after the study is run you should have a zip folder in your output folder to share
cli::cli_alert_success("Study finished")
