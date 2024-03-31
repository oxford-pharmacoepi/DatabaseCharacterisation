
# quality checks ----
info(logger, "start quality checks")
tables <- c(
  "observation_period", "visit_occurrence", "condition_occurrence",
  "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
  "observation", "death"
)

qualityChecks <- emptySummarisedResult()
for (table in tables) {
  info(logger, paste0("quality checks for: ", table))
  qualityChecks <- qualityChecks |>
    bind(summaryQuality(cdm[[table]]))
}
qualityChecks |>
  suppress(minCellCount = minCellCount) |>
  write_csv(file = here(resultsFolder, glue("{cdmName(cdm)}_quality_checks.csv")))
info(logger, "quality checks finished")

# observation period ----
info(logger, "observation period summary")
overlapCounts(cdm$observation_period) |>
  suppress(minCellCount = minCellCount) |>
  write_csv(file = here(resultsFolder, glue("{cdmName(cdm)}_observation_period.csv")))
info(logger, "observation period summarised")

# counts ----
info(logger, "start incidence counts")
tables <- c(
  "visit_occurrence", "condition_occurrence",
  "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
  "observation", "death"
)

incident <- emptySummarisedResult()
for (table in tables) {
  info(logger, paste0("incident counts for: ", table))
  incident <- incident |>
    bind(incidenceCounts(cdm[[table]]))
}
incident |>
  suppress(minCellCount = minCellCount) |>
  write_csv(file = here(resultsFolder, glue("{cdmName(cdm)}_incident_counts.csv")))
info(logger, "incident counts done")

# most common records ----
info(logger, "concept counts")

tables <- c(
  "visit_occurrence", "condition_occurrence",
  "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
  "observation", "death"
)

conceptCounts <- emptySummarisedResult()
for (table in tables) {
  info(logger, paste0("concept counts for: ", table))
  conceptCounts <- conceptCounts |>
    bind(summaryCodeCounts(cdm[[table]], ageGroups))
}
conceptCounts |>
  suppress(minCellCount = minCellCount) |>
  write_csv(file = here(resultsFolder, glue("{cdmName(cdm)}_concept_counts.csv")))
info(logger, "concept counts done")

