
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
    union_all(summaryQuality(cdm[[table]]))
}
qualityChecks |>
  suppress(minCellCount = minCellCount) |>
  write_csv(file = here(resultsFolder, glue("{cdmName(cdm)}_quality_checks.csv")))
info(logger, "quality checks finished")

# trends ----
