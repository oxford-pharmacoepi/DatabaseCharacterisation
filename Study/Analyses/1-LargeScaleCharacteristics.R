info(logger, 'create denominator cohorts')
cdm <- generateAgeCohortSet(
  cdm = cdm, name = "den", overwrite = TRUE, ageGroup = ageStrata
)
cdm$den <- cdm$den %>%
  mutate(
    cohort_start_date = as.Date(cohort_start_date),
    cohort_end_date = as.Date(cohort_end_date)
  ) %>%
  addSex() %>%
  addAge(ageGroup = ageStrata)

info(logger, 'summarise lsc')
lsc <- cdm$den %>%
  summariseLargeScaleCharacteristics(
    strata = list("sex", "age_group", c("sex", "age_group")),
    window = list(c(0, Inf)),
    censorDate = "cohort_end_date",
    eventInWindow = c("condition_occurrence", "observation", "measurement", "procedure_occurrence", "device_exposure"),
    episodeInWindow = c("drug_exposure"),
    minimumFrequency = 0.005
  )

info(logger, 'export lsc')
write.csv(
  x = lsc,
  file = here("Results", paste0(cdmName(cdm), "_lsc", ".csv")),
  row.names = FALSE
)

info(logger, 'summarise characteristics')
characteristics <- cdm$den %>%
  summariseCharacteristics(ageGroup = ageStrata, strata = list("sex", "age_group", c("sex", "age_group")))

info(logger, 'export characteristics')
write.csv(
  x = characteristics,
  file = here("Results", paste0(cdmName(cdm), "_characteristics", ".csv")),
  row.names = FALSE
)
