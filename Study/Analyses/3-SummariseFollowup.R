result <- summarisePersonDays(cdm, ageGroup = ageStrata,
                              byYear = TRUE, bySex = TRUE)

result %>%
  write.csv(
  file = here("Results", paste0(cdmName(cdm), "_summary_person_days.csv")),
  row.names = FALSE
)
