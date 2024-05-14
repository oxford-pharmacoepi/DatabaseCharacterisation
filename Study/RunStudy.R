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
  cdmName = dbName
)
info(logger, "CDM OBJECT CREATED")

# correct eunomia
if (dbName == "GiBleed") {
  cdm$observation_period <- cdm$observation_period |>
    dplyr::inner_join(
      cdm$person |>
        dplyr::select("person_id") |>
        dplyr::distinct(),
      by = "person_id"
    ) |>
    dplyr::compute(name = "observation_period", temporary = FALSE)
}

# create and export snapshot
info(logger, "EXPORT SNAPSHOT")
summary(cdm) |>
  suppress(minCellCount = minCellCount) |>
  write_csv(file = here(resultsFolder, glue("{cdmName(cdm)}_snapshot.csv")))
info(logger, "SNAPHSOT EXPORTED")

# source functions
source(here("Analyses", "functions.R"))

# run analyses ----
info(logger, "1 - SUMMARISE CLINICAL TABLES")
source(here("Analyses", "1-SummariseClinicalTables.R"))
info(logger, "1 - CLINICAL TABLES SUMMARISED")

info(logger, "2 - CHARACTERISE INDIVIDUALS AT INDEX DATE")
source(here("Analyses", "2-CharacterisationAtIndex.R"))
info(logger, "2 - INDIVIDUALS AT INDEX DATE CHARACTERISED")

info(logger, "3 - SUMMARISE INDIVIDUALS FOLLOWUP")
source(here("Analyses", "3-SummariseFollowup.R"))
info(logger, "3 - INDIVIDUALS FOLLOWUP SUMMARISED")

info(logger, "ANALYSES FINISHED")

# export results ----
info(logger, "EXPORTING RESULTS")
zip(
  zipfile = file.path(paste0(resultsFolder, "/", cdmName(cdm), ".zip")),
  files = list.files(resultsFolder, full.names = TRUE)
)