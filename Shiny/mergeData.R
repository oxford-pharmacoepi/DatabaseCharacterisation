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
  filter(result_type == "cdm_snapshot") |>
  select(cdm_name, variable_name, estimate_name, estimate_value)

overallSummary <- results |>
  filter(result_type == "summarised_omop_table") |>
  select(-c(result_id, result_type, starts_with(c("package", "strata", "additional")))) |>
  splitGroup()

save(snapshot, overallSummary, file = here("mergedResults.RData"))
