library(here)
library(readr)
library(omopgenerics)
library(readr)
library(dplyr)
library(tidyr)

x <- list.files(here("data"))
x <- x[tools::file_ext(x) == "zip"]
results <- emptySummarisedResult()
for (k in seq_along(x)) {
  folder <- tempdir()
  files <- unzip(zipfile = here("data", x[k]), exdir = folder)
  files <- files[tools::file_ext(files) == "csv"]
  for (i in seq_along(files)) {
    results <- results |>
      bind(
        read_csv(files[i], show_col_types = FALSE) |> newSummarisedResult()
      )
  }
  unlink(folder)
}

snapshot <- results |>
  filter(result_type == "cdm_snapshot") |>
  select(cdm_name, variable_name, estimate_name, estimate_value)

save(snapshot, file = here("mergedResults.RData"))
