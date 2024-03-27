x <- list()
for (tab in c(
  "observation_period", "drug_exposure", "condition_occurrence", "observation",
  "measurement", "procedure_occurrence", "device_exposure"
)) {
  info(logger, paste0('incident ', tab))
  x[[tab]] <- monthlyIncident(cdm, tab)
}
x %>%
  bind_rows(.id = "tab") %>%
  mutate(cdm_name = cdmName(cdm)) %>%
  write.csv(
    file = here("Results", paste0(cdmName(cdm), "_incident_counts.csv")),
    row.names = FALSE
  )

x <- list()
for (tab in c(
  "observation_period", "drug_exposure", "condition_occurrence",
  "device_exposure"
)) {
  info(logger, paste0('ongoing ', tab))
  x[[tab]] <- monthlyOngoing(cdm, tab)
}
x %>%
  bind_rows(.id = "table") %>%
  mutate(cdm_name = cdmName(cdm)) %>%
  write.csv(
    file = here("Results", paste0(cdmName(cdm), "_ongoing_counts.csv")),
    row.names = FALSE
  )

info(logger, "histogram year of birth sex")
x <- cdm$person %>%
  inner_join(cdm$observation_period %>% select("person_id") %>% distinct(), by = "person_id") %>%
  group_by(gender_concept_id, year_of_birth) %>%
  summarise(n = as.numeric(n()), .groups = "drop") %>%
  collect()
x <- x %>%
  union_all(
    x %>%
      group_by(year_of_birth) %>%
      summarise(n = sum(.data$n), .groups = "drop") %>%
      mutate(gender_concept_id = as.numeric(NA))
  ) %>%
  union_all(
    x %>%
      group_by(gender_concept_id) %>%
      summarise(n = sum(.data$n), .groups = "drop") %>%
      mutate(year_of_birth = as.numeric(NA))
  ) %>%
  mutate(n = if_else(n < 5, as.numeric(NA), .data$n)) %>%
  mutate(cdm_name = cdmName(cdm))
write.csv(
  x = x,
  file = here("Results", paste0(cdmName(cdm), "_year_sex.csv")),
  row.names = FALSE
)

info(logger, "follow-up summary")
x <- cdm$observation_period %>%
  inner_join(cdm$person %>% select("person_id") %>% distinct(), by = "person_id") %>%
  mutate(obs_time = !!datediff("observation_period_start_date", "observation_period_end_date")) %>%
  pull(obs_time)
x <- tibble(obs_weeks = x %/% 7) %>%
  group_by(obs_weeks) %>%
  summarise(n = as.numeric(n()), .groups = "drop") %>%
  mutate(n = if_else(n < 5, as.numeric(NA), n)) %>%
  mutate(cdm_name = cdmName(cdm))
write.csv(
  x = x,
  file = here("Results", paste0(cdmName(cdm), "_followup.csv")),
  row.names = FALSE
)

x <- list()
for (tab in c(
  "observation_period", "drug_exposure", "condition_occurrence", "observation",
  "measurement", "procedure_occurrence", "device_exposure", "person"
)) {
  info(logger, paste0('summary ', tab))
  x[[tab]] <- summaryTable(cdm, tab)
}
x %>%
  bind_rows(.id = "table") %>%
  mutate(cdm_name = cdmName(cdm)) %>%
  write.csv(
    file = here("Results", paste0(cdmName(cdm), "_summary_counts.csv")),
    row.names = FALSE
  )
