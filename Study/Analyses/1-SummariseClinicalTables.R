
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

# incident counts ----
info(logger, "denominator for counts")
denominator <- overlapCounts(cdm$observation_period)

info(logger, "start incident counts")
tables <- c(
  "observation_period", "visit_occurrence", "condition_occurrence",
  "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
  "observation", "death"
)

incident <- emptySummarisedResult()
for (table in tables) {
  info(logger, paste0("incident counts for: ", table))
  incident <- incident |>
    bind(summariseIncidentCounts(cdm[[table]], denominator))
}
incident |>
  suppress(minCellCount = minCellCount) |>
  write_csv(file = here(resultsFolder, glue("{cdmName(cdm)}_incident_counts.csv")))
info(logger, "incident counts done")

# overlap counts ----
info(logger, "start overlap counts")
tables <- c(
  "visit_occurrence", "condition_occurrence", "drug_exposure", "device_exposure"
)

overlap <- emptySummarisedResult()
for (table in tables) {
  info(logger, paste0("overlap counts for: ", table))
  overlap <- overlap |>
    bind(summariseOverlapCounts(cdm[[table]], denominator))
}
overlap |>
  suppress(minCellCount = minCellCount) |>
  write_csv(file = here(resultsFolder, glue("{cdmName(cdm)}_overlap_counts.csv")))
info(logger, "overlap counts done")

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

