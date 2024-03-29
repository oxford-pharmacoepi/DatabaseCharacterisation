# study parameters ----
ageGroups <- list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, Inf))

# create logger ----
resultsFolder <- here("Results", dbName)
if (!dir.exists(resultsFolder)) {
  dir.create(resultsFolder, recursive = T)
}
loggerName <- gsub(":| |-", "_", paste0("log ", Sys.time(),".txt"))
logger <- create.logger()
logfile(logger) <- here(resultsFolder, loggerName)
level(logger) <- "INFO"
info(logger, "LOG CREATED")

# create cdm object ----
info(logger, "CREATING CDM OBJECT")
cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdmSchema, 
  writeSchema = writeSchema, 
  cdmName = dbName,
  achillesSchema = achillesSchema
)
info(logger, "CDM OBJECT CREATED")

# create and export snapshot
info(logger, "EXPORT SNAPSHOT")
summary(cdm) |>
  suppress(minCellCount = minCellCount) |>
  write_csv(file = here(resultsFolder, glue("{cdmName(cdm)}_snapshot.csv")))
info(logger, "SNAPHSOT EXPORTED")

# source functions
source(here("Analyses", "functions.R"))

# run analyses ----
info(logger, "RUN ANALYSES")
source(here("Analyses", "1-SummariseClinicalTables.R"))

source(here("Analyses", "1-LargeScaleCharacteristics.R"))

info(logger, "ANALYSES FINISHED")

# export results ----
info(logger, "EXPORTING RESULTS")
zip(
  zipfile = file.path(paste0(resultsFolder, "/Results_", cdmName(cdm), ".zip")),
  files = list.files(resultsFolder, full.names = TRUE)
)