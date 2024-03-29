# summarise followup ----
info(logger, "summarise followup")
summariseFollowUp(cdm) |>
  omopgenerics::suppress(minCellCount = minCellCount) |>
  write_csv(file = here(resultsFolder, glue("{cdmName(cdm)}_followup.csv")))
info(logger, "followup summarised")

# summarise persondays ----
info(logger, "summarise person days")
result <- summarisePersonDays(
  cdm = cdm, ageGroup = ageGroups, byYear = TRUE, bySex = TRUE
)
result |>
  omopgenerics::suppress(minCellCount = minCellCount) |>
  write_csv(file = here(resultsFolder, glue("{cdmName(cdm)}_persondays.csv")))
info(logger, "person days summarised")

