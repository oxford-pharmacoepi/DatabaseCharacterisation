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
    bind(summaryQuality(cdm[[table]]))
}
qualityChecks |>
  exportSummarisedResult(fileName = glue("{cdmName(cdm)}_quality_checks.csv"), 
                         path = here(resultsFolder))

info(logger, "quality checks finished")

# observation period ----
info(logger, "observation period summary")
overlapCounts(cdm$observation_period) |>
  omopgenerics::exportSummarisedResult(fileName = glue("{cdmName(cdm)}_observation_period.csv"), 
                                       path = here(resultsFolder))
info(logger, "observation period summarised")

# counts ----
info(logger, "start incidence counts")
tables <- c(
  "visit_occurrence", "condition_occurrence",
  "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
  "observation", "death"
)

incident <- emptySummarisedResult()
for (table in tables) {
  info(logger, paste0("incident counts for: ", table))
  incident <- incident |>
    bind(incidenceCounts(cdm[[table]]))
}
incident |> 
  omopgenerics::exportSummarisedResult(fileName = glue("{cdmName(cdm)}_incident_counts.csv"), 
                                       path = here(resultsFolder))
info(logger, "incident counts done")

# most common records ----
info(logger, "concept counts")

tables <- c(
  "visit_occurrence", "condition_occurrence",
  "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
  "observation", "death"
)

if(replaceSpecialCharacters == TRUE){
  concept_names <- cdm[["concept"]] |>
    select("concept_name", "concept_id") |>
    distinct() |>
    collect() |>
    mutate(concept_name = iconv(concept_name, to = "UTF-8", sub = ".")) |>
    mutate(concept_name = if_else(row_number() == 1, "M\\+.ni\\+.re's", concept_name)) |>
    filter(grepl("\\+.",concept_name)) |>
    mutate(concept_name = gsub("M\\+.ni\\+.re's","Meniere's",concept_name),
           concept_name = gsub("Sj\\+.gren","Sjogren's", concept_name),
           concept_name = gsub("Boutonni\\+.re","Boutonnier", concept_name),
           concept_name = gsub("Ch\\+.diak","Chediak", concept_name),
           concept_name = gsub("Cura\\+.ao", "Curasao", concept_name),
           concept_name = gsub("Cr\\+.ole","Creole", concept_name),
           concept_name = gsub("D\\+.j\\+.", "Deja", concept_name),
           concept_name = gsub("Sch+.nlein", "Schonlein", concept_name),
           concept_name = gsub("S\\+.quard", "Sequard", concept_name),
           concept_name = gsub("Waldenstr\\+.m", "Waldenstrom", concept_name),
           concept_name = gsub("Caf\\+.","Cafe", concept_name)) 
  
  cdm[["concept"]] <- cdm[["concept"]] |>
    mutate(concept_name = if_else(
      concept_id %in% concept_names$concept_id,
      paste0("concept_",as.character(concept_id)),
      concept_name)
    ) |>
    compute()
}

conceptCounts <- emptySummarisedResult()
for (table in tables) {
  info(logger, paste0("concept counts for: ", table))
  conceptCounts <- conceptCounts |>
    bind(summaryCodeCounts(cdm[[table]], ageGroups))
}

if(replaceSpecialCharacters == TRUE){
  conceptCounts <- conceptCounts |>
    filter(grepl("concept_", variable_name)) |>
    select(-c("variable_name")) |>
    inner_join(
      concept_names |> 
        rename(variable_name = concept_name, variable_level = concept_id) |>
        mutate(variable_level = as.character(variable_level)),
      by = "variable_level"
    )  |>
    full_join(
      conceptCounts |>
        filter(!grepl("concept_", variable_name)),
      by = c("result_id", "cdm_name", "group_name", "group_level","strata_name",
             "strata_level", "variable_level", "estimate_name","estimate_type",
             "estimate_value","additional_name", "additional_level", "variable_name")
    )
}

conceptCounts |>
  omopgenerics::exportSummarisedResult(fileName = glue("{cdmName(cdm)}_concept_counts.csv"), 
                                       path = here(resultsFolder))

info(logger, "concept counts done")

