library(here)
library(readr)
library(omopgenerics)
library(readr)
library(dplyr)
library(tidyr)
library(visOmopResults)

x <- list.files(here("data"))
y <- x[tools::file_ext(x) == "zip"]
results <- emptySummarisedResult()
for (k in seq_along(y)) {
  folder <- tempdir()
  files <- unzip(zipfile = here("data", y[k]), exdir = folder)
  files <- files[tools::file_ext(files) == "csv"]
  for (i in seq_along(files)) {
    results <- results |>
      bind(
        read_csv(files[i], show_col_types = FALSE) |> newSummarisedResult()
      )
  }
  unlink(folder)
}

x <- x[tools::file_ext(x) == "csv"]
for (i in seq_along(x)) {
  results <- results |>
    bind(
      read_csv(x[i], show_col_types = FALSE) |> newSummarisedResult()
    )
}

snapshot <- results |>
  set
  filter(result_type == "cdm_snapshot") |>
  select(cdm_name, variable_name, estimate_name, estimate_value)

overallSummary <- results |>
  filter(result_type == "summarised_omop_table") |>
  select(-c(result_id, result_type, starts_with(c("package", "strata", "additional")))) |>
  splitGroup()

opResult <- results |>
  filter(result_type == "summarised_overlap_counts") |>
  filter(strata_name == "year") |>
  splitGroup() |>
  splitAdditional() |>
  mutate(strata_level = as.Date(paste0(strata_level, "-01-01"))) |>
  select(-c("result_id", "result_type", "variable_level", starts_with(c("package"))))

incidentCounts <- results |>
  filter(result_type == "summarised_incident_counts") |>
  filter(strata_name == "year") |>
  splitGroup() |>
  splitAdditional() |>
  mutate(variable_name = paste0(variable_name, "_", estimate_name)) |>
  mutate(strata_level = as.Date(paste0(strata_level, "-01-01"))) |>
  select(-c("result_id", "result_type", "variable_level", "estimate_name", starts_with(c("package"))))

conceptCounts <- results |>
  filter(result_type == "summarised_code_counts") |>
  select(-starts_with(c("result", "package"))) |>
  splitAll() 

characteristicsAtEntry <- results |>
  filter(result_type %in% c("summarised_characteristics", "summarised_demographics")) |>
  filter(strata_name == "overall")

characteristicsYear <- results |>
  filter(result_type %in% c("summarised_characteristics", "summarised_demographics")) |>
  filter(strata_name != "overall") |>
  arrange(as.numeric(strata_level))

summaryFollowup <- results |>
  filter(result_type == "summarised_followup") |>
  splitStrata() |>
  select(!starts_with(c("result", "package", "group", "additional")))

summaryPersonDays <- results |>
  filter(result_type == "summarised_person_days") |>
  splitAll() |>
  select(!starts_with(c("result", "package", "population", "variable_level"))) |>
  mutate(age_group = if_else(age_group == "0 to 150", "overall", age_group)) |>
  mutate(
    age_group = factor(age_group, levels = c("overall", "0 to 19", "20 to 39", "40 to 59", "60 to 79", "80 to 150")),
    sex = factor(sex, levels = c("overall", "Female", "Male")),
    year = factor(year, levels = c("overall", as.character(1900:2050)))
  ) |>
  arrange(age_group, sex, year)

save(
  snapshot, overallSummary, incidentCounts, conceptCounts, opResult, 
  characteristicsAtEntry, characteristicsYear, summaryFollowup,
  summaryPersonDays,
  file = here("mergedResults.RData")
)
