info(logger, 'create denominator cohorts')
cdm <- generateYearCohortSet(cdm = cdm, name = "denominator")

info(logger, 'summarise characteristics')
cdm$denominator |>
  CohortCharacteristics::summariseCharacteristics(ageGroup = ageGroups) |>
  dplyr::mutate(
    "strata_name" = dplyr::if_else(
      .data$group_level == "overall", "overall", "year"
    ),
    "strata_level" = .data$group_level,
    "group_level" = "general population",
  ) |>
  write_csv(file = here(resultsFolder, glue("{cdmName(cdm)}_characteristics.csv")))
info(logger, 'characteristics summarised')
