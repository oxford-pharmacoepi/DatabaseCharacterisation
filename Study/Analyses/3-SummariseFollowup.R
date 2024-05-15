# summarise followup ----
info(logger, "summarise followup")
summariseFollowUp(cdm) |>
  exportSummarisedResult(
    fileName = glue("{cdmName(cdm)}_followup.csv"),
    path = here(resultsFolder)
  )
info(logger, "followup summarised")

# summarise person-days ----
info(logger, "summarise person days")
result <- summarisePersonDays(
  cdm = cdm, ageGroup = c(list(c(0, 150)), ageGroups), byYear = TRUE, bySex = TRUE
) 

result |> 
  exportSummarisedResult(
    fileName = glue("{cdmName(cdm)}_persondays.csv"),
    path = here(resultsFolder)
  )
info(logger, "person days summarised")


  