
strataByYear <- function(cohort, bySex) {
  years <- cohort |>
    dplyr::summarise(
      min = min(.data$cohort_start_date, na.rm = TRUE),
      max = max(.data$cohort_end_date, na.rm = TRUE)
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      min = lubridate::year(.data$min), max = lubridate::year(.data$max)
    )
  years <- seq(years$min, years$max, by = 1)
  result <- list()
  for (k in seq_along(years)) {
    x <- cohort |> correctCohort(years[[k]])
    res <- x |> summaryFollowUp(strata = "age_group")
    if (bySex) {
      res <- res |>
        dplyr::bind_rows(
          x |> summaryFollowUp(strata = c("age_group", "sex"))
        )
    }
    result[[k]] <- res |> dplyr::mutate("year" = .env$years[[k]])
  }
  result <- dplyr::bind_rows(result)
  return(result)
}
correctCohort <- function(cohort, year) {
  startDate <- as.Date(paste0(year, "/01/01"))
  endDate <- as.Date(paste0(year, "/12/31"))
  cohort |>
    dplyr::mutate(
      "cohort_start_date" = dplyr::if_else(
        .data$cohort_start_date <= .env$startDate,
        .env$startDate,
        .data$cohort_start_date
      ),
      "cohort_end_date" = dplyr::if_else(
        .data$cohort_end_date >= .env$endDate,
        .env$endDate,
        .data$cohort_end_date
      )
    ) |>
    dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date)
}
summaryFollowUp <- function(cohort, strata) {
  x <- cohort %>%
    dplyr::mutate("person_days" = as.numeric(!!CDMConnector::datediff(
      start = "cohort_start_date", end = "cohort_end_date", interval = "day"
    )) + 1) %>%
    dplyr::select(dplyr::any_of(c(
      "age_group", "sex", "year", "subject_id", "person_days"
    ))) |>
    dplyr::collect()
  if (nrow(x) > 0) {
    res <- x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(strata))) |>
      dplyr::summarise(
        "count_number_subjects" = dplyr::n_distinct(.data$subject_id),
        "count_number_records" = dplyr::n(),
        "min_person_days" = min(.data$person_days, na.rm = TRUE),
        "q25_person_days" = stats::quantile(.data$person_days, probs = 0.25, na.rm = TRUE),
        "median_person_days" = stats::median(.data$person_days, na.rm = TRUE),
        "q75_person_days" = stats::quantile(.data$person_days, probs = 0.75, na.rm = TRUE),
        "max_person_days" = max(.data$person_days, na.rm = TRUE),
        "total_person_days" = sum(.data$person_days, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    res <- dplyr::tibble()
  }
  return(res)
}
summarisePersonDays  <- function(cdm,
                                 ageGroup = NULL,
                                 byYear = FALSE,
                                 bySex = FALSE) {
  # check input
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assertLogical(byYear, any.missing = FALSE, len = 1)
  checkmate::assertLogical(bySex, any.missing = FALSE, len = 1)
  
  prefix <- omopgenerics::tmpPrefix()
  tmp1 <- omopgenerics::uniqueTableName(prefix = prefix)
  if (is.null(ageGroup)) ageGroup <- list(c(0, 150))
  ageGroup[[length(ageGroup)]][2] <- 150
  
  # create denominator cohort
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm, name = tmp1, ageGroup = ageGroup, sex = "Both",
    daysPriorObservation = 0, requirementInteractions = TRUE
  )
  
  set <- omopgenerics::settings(cdm[[tmp1]]) |>
    dplyr::select("cohort_definition_id", "age_group")
  tmp2 <- omopgenerics::uniqueTableName(prefix = prefix)
  cdm <- omopgenerics::insertTable(cdm = cdm, name = tmp2, table = set)
  cdm[[tmp1]] <- cdm[[tmp1]] |>
    dplyr::inner_join(cdm[[tmp2]], by = "cohort_definition_id") |>
    dplyr::compute(name = tmp1, temporary = FALSE)
  
  # results by age group
  result <- cdm[[tmp1]] |> summaryFollowUp(strata = c("age_group"))
  
  # results by sex
  if (bySex == TRUE) {
    cdm[[tmp1]] <- cdm[[tmp1]] |> 
      addDemographics(
        age = FALSE, priorObservation = FALSE, futureObservation = FALSE
      )
    result <- result |>
      dplyr::bind_rows(
        cdm[[tmp1]] |> summaryFollowUp(strata = c("age_group", "sex"))
      )
  }
  
  if (byYear == TRUE) {
    result <- result |> dplyr::bind_rows(strataByYear(cdm[[tmp1]], bySex))
  }
  
  # tidy result
  if (byYear) {
    result <- result |> dplyr::mutate("year" = dplyr::if_else(
      is.na(.data$year), "overall", as.character(.data$year)
    ))
  } else {
    result <- result |> dplyr::mutate("year" = "overall")
  }
  if (bySex) {
    result <- result |> dplyr::mutate("sex" = dplyr::if_else(
      is.na(.data$sex), "overall", as.character(.data$sex)
    ))
  } else {
    result <- result |> dplyr::mutate("sex" = "overall")
  }
  
  result <- result |>
    tidyr::pivot_longer(
      cols = !dplyr::any_of(c("age_group", "sex", "year")),
      values_to = "estimate_value"
    ) |>
    tidyr::separate_wider_delim(
      cols = "name", delim = "_", too_many = "merge",
      names = c("estimate_name", "variable_name")
    ) |>
    dplyr::mutate("age_group" = dplyr::if_else(
      .data$age_group == "0 to 150", "overall", .data$age_group
    ))
  
  result <- result |>
    visOmopResults::uniteStrata(cols = c("age_group", "sex", "year"))|>
    dplyr::mutate(
      "result_id" = as.integer(1),
      "cdm_name" = omopgenerics::cdmName(cdm),
      "result_type" = "summarised_person_days",
      "package_name" = "DatabaseCharacterisation",
      "package_version" = "0.0.0", # as.character(utils::packageVersion("OmopSketch")),
      "group_name" = "population",
      "group_level" = "overall",
      "variable_level" = NA_character_,
      "estimate_type" = dplyr::if_else(
        .data$estimate_name %in% c("count", "min", "max", "total"), "integer",
        "numeric"
      ),
      "estimate_value" = as.character(.data$estimate_value),
      "additional_name" = "overall",
      "additional_level" = "overall"
    ) |>
    omopgenerics::newSummarisedResult()
  
  # drop created tables
  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(prefix))
  
  return(result)
}
summaryQuality <- function(table) {
  
  name <- tableName(table)
  concept <- standardConcept(name)
  type <- typeConcept(name)
  start <- startDate(name)
  
  cdm <- omopgenerics::cdmReference(table)
  
  den <- cdm$person |> dplyr::ungroup() |> dplyr::tally() |> dplyr::pull()
  records <- table |>
    dplyr::ungroup() |>
    dplyr::summarise(
      "number_records-count" = dplyr::n(),
      "number_subjects-count" = dplyr::n_distinct(.data$person_id),
    ) |>
    dplyr::collect() |>
    # dplyr::mutate("number_subjects-percentage" = 100 * .data[["number_subjects-count"]] / .env$den) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(), 
      names_to = c("variable_name", "estimate_name"), 
      names_sep = "-", 
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      "variable_level" = NA_character_,
      "estimate_type" = dplyr::if_else(
        .data$estimate_name == "count", "integer", "percentage"
      )
    )
  totalrecords <- as.numeric(records$estimate_value[records$variable_name == "number_records"])
  recordName <- "records_per_person"
  
  nIndividuals <- table |>
    group_by(person_id) |>
    tally(name = recordName) |>
    right_join(cdm$person |> select("person_id"), by = "person_id") |>
    mutate(!!recordName := if_else(is.na(.data[[recordName]]), 0, .data[[recordName]])) |>
    collect() |>
    mutate(!!recordName := as.integer(.data[[recordName]])) |>
    summariseResult(
      variables = recordName, 
      estimates = c("mean", "sd", "median", "q25", "q75", "q05", "q95"),
      counts = F
    )
  
  if (name == "observation_period") {
    x <- table |> mutate(in_observation = 1)
  } else {
    x <- table |>
      addInObservation(indexDate = start)
  }
  if (!is.null(concept)) {
    x <- x |>
      mutate(
        !!concept := dplyr::if_else(is.na(.data[[concept]]), 0L, as.integer(.data[[concept]])),
        mapped = if_else(.data[[concept]] == 0, "No", "Yes")
      ) |>
      left_join(
        cdm$concept |>
          select("domain_id", !!concept := "concept_id"),
        by = concept
      )
  } else {
    x <- x |>
      mutate(mapped = "Yes", domain_id = "No domain")
  }
  x <- x |>
    left_join(
      cdm$concept |>
        select("type_name" = "concept_name", !!type := "concept_id"),
      by = type,
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      "in_observation", "mapped", "domain_id", "type_name", type
    )))) |>
    tally() |>
    collect() |>
    dplyr::mutate(
      "n" = as.numeric(.data$n),
      "type_name" = dplyr::if_else(
        is.na(.data$type_name), as.character(.data[[type]]), .data$type_name 
      )) |>
    dplyr::select(-dplyr::all_of(type))
  
  res <- records |>
    dplyr::union_all(
      nIndividuals |>
        select(variable_name, variable_level, estimate_name, estimate_type, estimate_value)
    ) |>
    union_all(
      x |>
        group_by(in_observation) |>
        summarise(estimate_value = sum(n)) |>
        mutate(
          variable_name = "In observation",
          variable_level = if_else(in_observation == 1, "Yes", "No")
        ) |>
        select(variable_name, variable_level, estimate_value) |>
        union_all(
          x |>
            group_by(mapped) |>
            summarise(estimate_value = sum(n)) |>
            mutate(
              variable_name = "Mapped",
              variable_level = mapped
            ) |>
            select(variable_name, variable_level, estimate_value)
        ) |>
        union_all(
          x |>
            group_by(domain_id) |>
            summarise(estimate_value = sum(n)) |>
            mutate(
              variable_name = "Domain",
              variable_level = domain_id
            ) |>
            select(variable_name, variable_level, estimate_value)
        ) |>
        union_all(
          x |>
            group_by(type_name) |>
            summarise(estimate_value = sum(n)) |>
            mutate(
              variable_name = "Record type",
              variable_level = type_name
            ) |>
            select(variable_name, variable_level, estimate_value)
        ) |>
        rename("count" = "estimate_value") |>
        mutate("percentage" = 100 * .data$count / .env$totalrecords) |>
        tidyr::pivot_longer(
          cols = c("count", "percentage"), 
          names_to = "estimate_name", 
          values_to = "estimate_value"
        ) |>
        mutate(
          estimate_value = as.character(estimate_value),
          estimate_type = dplyr::if_else(
            .data$estimate_name == "count", "integer", "percentage"
          )
        )
    ) |>
    mutate(
      result_id = as.integer(1),
      cdm_name = cdmName(cdmReference(table)),
      result_type = "summarised_omop_table", 
      package_name = "omopSketch",
      package_version = "0.0.0",
      group_name = "omop_table",
      group_level = name,
      strata_name = "overall",
      strata_level = "overall",
      additional_name = "overall", 
      additional_level = "overall"
    ) |>
    mutate(variable_name = if_else(is.na(variable_name), "NA", variable_name)) |>
    newSummarisedResult()
  return(res)
}
startDate <- function(name) {
  switch(
    name,
    "observation_period" = "observation_period_start_date",
    "visit_occurrence" = "visit_start_date",
    "condition_occurrence" = "condition_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "procedure_occurrence" = "procedure_date",
    "device_exposure" = "device_exposure_start_date",
    "measurement" = "measurement_date",
    "observation" = "observation_date",
    "death" = "death_date"
  )
}
endDate <- function(name) {
  switch(
    name,
    "observation_period" = "observation_period_end_date",
    "visit_occurrence" = "visit_end_date",
    "condition_occurrence" = "condition_end_date",
    "drug_exposure" = "drug_exposure_end_date",
    "procedure_occurrence" = "procedure_date",
    "device_exposure" = "device_exposure_end_date",
    "measurement" = "measurement_date",
    "observation" = "observation_date",
    "death" = "death_date"
  )
}
standardConcept <- function(name) {
  switch(
    name,
    "observation_period" = NULL,
    "visit_occurrence" = "visit_concept_id",
    "condition_occurrence" = "condition_concept_id",
    "condition_era" = "condition_concept_id",
    "concept" = "concept_id",
    "concept_synonym" = "concept_id",
    "drug_exposure" = "drug_concept_id",
    "drug_era" = "drug_concept_id",
    "dose_era" = "drug_concept_id",
    "drug_strength" = "drug_concept_id",
    "procedure_occurrence" = "procedure_concept_id",
    "device_exposure" = "device_concept_id",
    "measurement" = "measurement_concept_id",
    "observation" = "observation_concept_id",
    "death" = "cause_concept_id"
  )
}
typeConcept <- function(name) {
  switch(
    name,
    "observation_period" = "period_type_concept_id",
    "visit_occurrence" = "visit_type_concept_id",
    "condition_occurrence" = "condition_type_concept_id",
    "drug_exposure" = "drug_type_concept_id",
    "procedure_occurrence" = "procedure_type_concept_id",
    "device_exposure" = "device_type_concept_id",
    "measurement" = "measurement_type_concept_id",
    "observation" = "observation_type_concept_id",
    "death" = "death_type_concept_id"
  )
}
incidenceCounts <- function(table) {
  name <- omopgenerics::tableName(table)
  date <- startDate(name)
  tablePrefix <- omopgenerics::tmpPrefix()
  table <- table |>
    dplyr::select(dplyr::all_of(date), "person_id")
  if (name != "observation_period") {
    table <- table |>
      filterInObservation(indexDate = date)
  }
  x <- table |>
    rename("incidence_date" = all_of(date)) %>%
    mutate("incidence_year" = !!datepart("incidence_date", "year")) |>
    group_by(incidence_year) %>%
    summarise("estimate_value" = dplyr::n(), .groups = "drop") |>
    collect() |>
    dplyr::mutate(
      "estimate_value" = as.integer(.data$estimate_value),
      "variable_name" = "incidence_records"
    ) |>
    dplyr::rename("year" = "incidence_year") |>
    visOmopResults::uniteStrata(cols = "year") |>
    dplyr::mutate(
      "result_id" = as.integer(1),
      "cdm_name" = omopgenerics::cdmName(omopgenerics::cdmReference(table)),
      "result_type" = "summarised_incident_counts",
      "package_name" = "omopSketch",
      "package_version" = "0.0.0",
      "group_name" = "omop_table",
      "group_level" = name,
      "estimate_value" = as.character(.data$estimate_value),
      "variable_level" = NA_character_,
      "estimate_name" = "count",
      "estimate_type" = "integer",
      "additional_name" = "overall",
      "additional_level" = "overall"
    ) |>
    omopgenerics::newSummarisedResult()
  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))
  return(x)
}
summariseIncidentCounts <- function(table, denominator) {
  if (table |> dplyr::tally() |> dplyr::pull() == 0) {
    return(omopgenerics::emptySummarisedResult())
  }
  incidenceCounts(table) |>
    dplyr::inner_join(
      denominator |>
        dplyr::select(
          "strata_name", "strata_level", "variable_name", 
          "percentage" = "estimate_value"
        ) |>
        dplyr::mutate("variable_name" = gsub(
          "overlap", "incident", .data$variable_name
        )),
      by = c("strata_name", "strata_level", "variable_name")
    ) |>
    dplyr::mutate("percentage" = as.character(
      100 * as.numeric(.data$estimate_value) / as.numeric(.data$percentage)
    )) |>
    dplyr::rename("count" = "estimate_value") |>
    dplyr::select(-c("estimate_name")) |>
    tidyr::pivot_longer(
      cols = c("count", "percentage"), 
      names_to = "estimate_name", 
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      "estimate_type" = dplyr::case_when(
        .data$estimate_name == "count" ~ "integer", 
        .data$variable_name == "incident_records" ~ "numeric",
        .data$variable_name == "incident_subjects" ~ "percentage"
      ),
      "estimate_name" = dplyr::case_when(
        .data$estimate_name == "count" ~ "count", 
        .data$variable_name == "incident_records" ~ "mean",
        .data$variable_name == "incident_subjects" ~ "percentage"
      ),
      "estimate_value" = dplyr::if_else(
        .data$estimate_name == "mean", 
        as.character(as.numeric(.data$estimate_value)/100),
        .data$estimate_value
      )
    ) |>
    omopgenerics::newSummarisedResult()
}
summariseOverlapCounts <- function(table, denominator) {
  if (table |> dplyr::tally() |> dplyr::pull() == 0) {
    return(omopgenerics::emptySummarisedResult())
  }
  overlapCounts(table) |>
    dplyr::inner_join(
      denominator |>
        dplyr::select(
          "strata_name", "strata_level", "variable_name", 
          "percentage" = "estimate_value"
        ),
      by = c("strata_name", "strata_level", "variable_name")
    ) |>
    dplyr::mutate("percentage" = as.character(
      100 * as.numeric(.data$estimate_value) / as.numeric(.data$percentage)
    )) |>
    dplyr::rename("count" = "estimate_value") |>
    dplyr::select(-c("estimate_name")) |>
    tidyr::pivot_longer(
      cols = c("count", "percentage"), 
      names_to = "estimate_name", 
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(
      "estimate_type" = dplyr::case_when(
        .data$estimate_name == "count" ~ "integer", 
        .data$variable_name == "overlap_records" ~ "numeric",
        .data$variable_name == "overlap_subjects" ~ "percentage"
      ),
      "estimate_name" = dplyr::case_when(
        .data$estimate_name == "count" ~ "count", 
        .data$variable_name == "overlap_records" ~ "mean",
        .data$variable_name == "overlap_subjects" ~ "percentage"
      ),
      "estimate_value" = dplyr::if_else(
        .data$estimate_name == "mean", 
        as.character(as.numeric(.data$estimate_value)/100),
        .data$estimate_value
      )
    ) |>
    omopgenerics::newSummarisedResult()
}
overlapCounts <- function(table) {
  tablePrefix <- omopgenerics::tmpPrefix()
  name <- omopgenerics::tableName(table)
  start_date <- startDate(name)
  end_date <- endDate(name)
  table <- table |>
    dplyr::select(dplyr::all_of(c(start_date, end_date)), "person_id") |>
    dplyr::filter(!is.na(.data[[end_date]]))
  minYear <- table |>
    dplyr::summarise(x = min(.data[[start_date]], na.rm = TRUE)) |>
    dplyr::pull() |>
    format("%Y") |>
    as.numeric()
  overlapRecordsYear <- table %>%
    mutate(
      "start" = !!datepart(start_date, "year"),
      "end" = !!datepart(end_date, "year")
    ) |>
    countRecords() |>
    dplyr::rename("year" = "group", "overlap_records" = "n")
  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))
  
  x <- overlapRecordsYear |>
    tidyr::pivot_longer(
      cols = "overlap_records",
      names_to = "variable_name", 
      values_to = "estimate_value"
    ) |>
    visOmopResults::uniteStrata("year") |>
    dplyr::mutate(
      "result_id" = as.integer(1),
      "cdm_name" = omopgenerics::cdmName(omopgenerics::cdmReference(table)),
      "result_type" = "summarised_overlap_counts",
      "package_name" = "omopSketch",
      "package_version" = "0.0.0",
      "group_name" = "omop_table",
      "group_level" = name,
      "estimate_value" = as.character(.data$estimate_value),
      "variable_level" = NA_character_,
      "estimate_name" = "count",
      "estimate_type" = "integer",
      "additional_name" = "overall",
      "additional_level" = "overall"
    ) |>
    omopgenerics::newSummarisedResult()
  
  return(x)
}
countRecords <- function(x) {
  tablePrefix <- omopgenerics::tmpPrefix()
  x <- x |>
    dplyr::group_by(.data$start, .data$end) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.integer)) |>
    dplyr::compute(
      name = omopgenerics::uniqueTableName(tablePrefix), temporary = FALSE
    )
  nm <- omopgenerics::uniqueTableName(tablePrefix)
  cdm <- omopgenerics::insertTable(
    cdm = cdm, 
    name = nm, 
    table = dplyr::tibble("group" = seq(
      x |> 
        dplyr::summarise(x = min(.data$start, na.rm = TRUE)) |> 
        dplyr::pull(),
      x |> 
        dplyr::summarise(x = max(.data$end, na.rm = TRUE)) |> 
        dplyr::pull()
    ))
  )
  x <- x |>
    dplyr::cross_join(cdm[[nm]]) |>
    dplyr::filter(.data$start <= .data$group & .data$end >= .data$group) |>
    dplyr::group_by(.data$group) |>
    dplyr::summarise(n = sum(.data$n, na.rm = TRUE)) %>%
    dplyr::collect()
  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))
  return(x)
}
solveOverlap <- function(x) {
  x |> 
    dplyr::select("person_id", "date" = "start") |>
    dplyr::mutate("id" = -1) |>
    dplyr::union_all(
      x |> 
        dplyr::select("person_id", "date" = "end") |>
        dplyr::mutate("id" = 1)
    ) |>
    dplyr::group_by(.data$person_id) |>
    dplyr::arrange(.data$date, .data$id) |>
    dplyr::mutate(
      "cum_id" = cumsum(.data$id),
      "idd" = cumsum(as.numeric(.data$cum_id == -1 & .data$id == -1))
    ) |>
    dplyr::arrange() |>
    dplyr::ungroup() |>
    dplyr::mutate("colname" = dplyr::case_when(
      .data$id == -1 & .data$cum_id == -1 ~ "start",
      .data$id == 1 & .data$cum_id == 0 ~ "end"
    )) |>
    dplyr::filter(!is.na(colname)) |>
    dplyr::select(-c("id", "cum_id")) |>
    tidyr::pivot_wider(names_from = "colname", values_from = "date") |>
    dplyr::select(-"idd")
}
summaryCodeCounts <- function(table, ageGroups) {
  if (table |> dplyr::tally() |> dplyr::pull() == 0) {
    return(omopgenerics::emptySummarisedResult())
  }
  name <- omopgenerics::tableName(table)
  x <- table |>
    addDemographics(
      indexDate = startDate(name), 
      ageGroup = ageGroups, 
      futureObservation = FALSE
    ) |>
    dplyr::filter(!is.na(.data$prior_observation)) |>
    dplyr::rename("concept_id" = !!standardConcept(name)) %>%
    dplyr::mutate(
      "year" = !!datepart(date = startDate(name), interval = "year"),
      "concept_id" = dplyr::if_else(is.na(.data$concept_id), 0L, as.integer(.data$concept_id))
    ) |>
    dplyr::group_by(.data$concept_id, .data$age_group, .data$sex, .data$year) |>
    dplyr::tally() |>
    dplyr::collect()
  strata <- list(
    character(),
    "sex",
    "age_group",
    "year",
    c("sex", "age_group"),
    c("sex", "year"),
    c("age_group", "year"),
    c("sex", "age_group", "year")
  )
  res <- list()
  for (k in seq_along(strata)) {
    res[[k]] <- x |>
      dplyr::group_by(dplyr::across(c("concept_id", strata[[k]]))) |>
      dplyr::summarise("estimate_value" = sum(.data$n), .groups = "drop") |>
      visOmopResults::uniteStrata(cols = strata[[k]])
  }
  res <- res |>
    dplyr::bind_rows() |>
    left_join(
      cdm$concept |> 
        dplyr::select("concept_id", "variable_name" = "concept_name") |> 
        dplyr::collect(), 
      by = "concept_id"
    ) |>
    dplyr::rename("variable_level" = "concept_id") |>
    dplyr::mutate(
      "result_id" = as.integer(1),
      "cdm_name" = omopgenerics::cdmName(omopgenerics::cdmReference(table)),
      "result_type" = "summarised_code_counts",
      "package_name" = "omopSketch",
      "package_version" = "0.0.0",
      "group_name" = "omop_table",
      "group_level" = name,
      "variable_level" = as.character(.data$variable_level),
      "estimate_value" = as.character(.data$estimate_value),
      "estimate_name" = "count",
      "estimate_type" = "integer",
      "additional_name" = "overall",
      "additional_level" = "overall"
    ) |>
    omopgenerics::newSummarisedResult()
  return(res)
}
generateYearCohortSet <- function(cdm, name) {
  tablePrefix <- omopgenerics::tmpPrefix()
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm, name = name
  )
  year <- cdm[[name]] |>
    dplyr::summarise(
      "start" = min(.data$cohort_start_date, na.rm = TRUE),
      "end" = max(.data$cohort_end_date, na.rm = TRUE)
    ) |>
    dplyr::collect()
  year <- seq(
    year$start |> format("%Y") |> as.numeric(),
    year$end |> format("%Y") |> as.numeric()
  )
  nmax <- 20
  denominators <- list()
  for (k in 1:ceiling(length(year)/nmax)) {
    yeark <- year[(1+(k-1)*nmax):min(k*nmax, length(year))]
    startDates <- "dplyr::if_else(
      .data$cohort_start_date <= as.Date('{yeark}-01-01'),
      as.Date('{yeark}-01-01'), .data$cohort_start_date
    )" |>
      glue::glue() |>
      rlang::parse_exprs() |>
      rlang::set_names(glue::glue("cohort_start_date_{yeark}"))
    endDates <- "dplyr::if_else(
      .data$cohort_end_date >= as.Date('{yeark}-12-31'),
      as.Date('{yeark}-12-31'), .data$cohort_end_date
    )" |>
      glue::glue() |>
      rlang::parse_exprs() |>
      rlang::set_names(glue::glue("cohort_end_date_{yeark}"))
    denominators[[k]] <- cdm[[name]] |>
      dplyr::mutate(!!!startDates, !!!endDates) |>
      dplyr::select(-c(
        "cohort_definition_id", "cohort_start_date", "cohort_end_date"
      ))  |>
      dplyr::compute(
        name = omopgenerics::uniqueTableName(tablePrefix), temporary = FALSE
      ) |>
      tidyr::pivot_longer(
        cols = -"subject_id", 
        names_to = c(".value", "cohort_definition_id"), 
        names_pattern = "(cohort_start_date|cohort_end_date)_(\\d+)"
      ) |>
      dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) |>
      dplyr::compute(
        name = omopgenerics::uniqueTableName(tablePrefix), temporary = FALSE
      )
  }
  denominators <- base::Reduce(dplyr::union_all, denominators) |> 
    dplyr::compute(
      name = omopgenerics::uniqueTableName(tablePrefix), temporary = FALSE
    ) |>
    dplyr::mutate("cohort_definition_id" = as.integer(.data$cohort_definition_id) - !!year[1] + 2) |>
    dplyr::union_all(
      cdm[[name]] |>
        dplyr::compute(
          name = omopgenerics::uniqueTableName(tablePrefix), temporary = FALSE
        )
    ) |>
    dplyr::compute(name = name, temporary = FALSE) |>
    omopgenerics::newCohortTable(
      cohortSetRef = dplyr::tibble(
        "cohort_definition_id" = c(1, year-year[1]+2), 
        "cohort_name" = c("overall", year)
      ),
      cohortAttritionRef = NULL
    )
  omopgenerics::dropTable(cdm = cdm, name = dplyr::starts_with(tablePrefix))
  return(cdm)
}
summariseFollowUp <- function(cdm) {
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm, name = "denominator"
  )
  res <- cdm[["denominator"]] |>
    addDemographics(
      priorObservation = FALSE, ageGroup = ageGroups
    ) |>
    dplyr::rename("age_group_at_entry" = "age_group") |>
    dplyr::select("age_group_at_entry", "sex", "future_observation") |>
    dplyr::collect()
  sep <- round(max(res$future_observation)/100)
  res <- res |>
    dplyr::mutate("bin" = floor(.data$future_observation/.env$sep))
  strata <- list(
    character(), "sex", "age_group_at_entry", c("sex", "age_group_at_entry")
  )
  result <- list()
  for (k in seq_along(strata)) {
    result[[k]] <- res |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("bin", strata[[k]])))) |>
      dplyr::tally() |>
      dplyr::ungroup() |>
      visOmopResults::uniteStrata(cols = strata[[k]])
  }
  result <- result |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      "variable_level" = paste0(
        as.character((.data$bin*sep)), 
        " to ", 
        as.character((.data$bin+1)*sep - 1)
      ),
      "variable_name" = "follow-up",
      "estimate_value" = as.character(.data$n),
      "estimate_name" = "count",
      "estimate_type" = "integer"
    ) |>
    dplyr::select(-c("bin", "n")) |>
    dplyr::mutate(
      "result_id" = as.integer(1),
      "result_type" = "summarised_followup",
      "cdm_name" = omopgenerics::cdmName(cdm),
      "package_name" = "omopSketch",
      "package_version" = "0.0.0",
      "group_name" = "cohort_name",
      "group_level" = "denominator",
      "additional_name" = "overall",
      "additional_level" = "overall"
    ) |>
    omopgenerics::newSummarisedResult()
  return(result)
}
# addInObservation2 <- function(x, indexDate, nameStyle = "in_observation") {
#   cdm <- omopgenerics::cdmReference(x)
#   id <- c("person_id", "subject_id")
#   id <- id[id %in% colnames(x)]
#   x |>
#     dplyr::left_join(
#       x |>
#         dplyr::select(dplyr::all_of(c(id, "date" = indexDate))) |>
#         dplyr::inner_join(
#           cdm$observation_period |>
#             dplyr::select(
#               !!id := "person_id", 
#               "start" = "observation_period_start_date",
#               "end" = "observation_period_end_date"
#             ),
#           by = id
#         ) |>
#         dplyr::filter(.data$date >= .data$start & .data$date <= .data$end) |>
#         dplyr::mutate(!!nameStyle := 1),
#       by = "person_id"
#     ) |>
#     dplyr::mutate(!!nameStyle := dplyr::if_else(
#       is.na(.data[[nameStyle]]), 0, 1
#     ))
# }
filterInObservation <- function(x, indexDate) {
  cdm <- omopgenerics::cdmReference(x)
  id <- c("person_id", "subject_id")
  id <- id[id %in% colnames(x)]
  x |>
    dplyr::inner_join(
      cdm$observation_period |>
        dplyr::select(
          !!id := "person_id", 
          "start" = "observation_period_start_date",
          "end" = "observation_period_end_date"
        ),
      by = id
    ) |>
    dplyr::filter(
      .data[[indexDate]] >= .data$start & .data[[indexDate]] <= .data$end
    )
}
addDemographics2 <- function(x, 
                             indexDate = "cohort_start_date", 
                             sex = TRUE, 
                             age = TRUE,
                             ageGroup = NULL,
                             priorObservation = TRUE,
                             futureObservation = TRUE) {
  cdm <- omopgenerics::cdmReference(x)
  idx <- colnames(x)[1]
  id <- c("person_id", "subject_id")
  id <- id[id %in% colnames(x)]
  
  if (futureObservation | priorObservation) {
    qq <- c(
      "as.integer(local(CDMConnector::datediff(start = 'start', end = indexDate, interval = 'day')))",
      "as.integer(local(CDMConnector::datediff(start = indexDate, end = 'end_obs', interval = 'day')))"
    )
    qq <- qq[c(priorObservation, futureObservation)]
    nms <- c("prior_observation", "future_observation")
    nms <- nms[c(priorObservation, futureObservation)]
    qq <- qq |>
      rlang::parse_exprs() |>
      rlang::set_names(nms)
    x <- x |>
      dplyr::left_join(
        x |>
          dplyr::select(dplyr::all_of(c(idx, id, indexDate))) |>
          dplyr::inner_join(
            cdm$observation_period |>
              dplyr::select(
                !!id := "person_id", 
                "start" = "observation_period_start_date",
                "end_obs" = "observation_period_end_date"
              ),
            by = id
          ) |>
          dplyr::filter(
            .data$start <= .data[[indexDate]] & .data$end_obs >= .data[[indexDate]]
          ) %>%
          dplyr::mutate(!!!qq) |>
          dplyr::select(-"start", -"end_obs"),
        by = c(idx, id, indexDate)
      )
  }
  
  if (sex | age | !is.null(ageGroup)) {
    xx <- x |>
      dplyr::select(dplyr::all_of(c(idx, id, indexDate))) |>
      dplyr::inner_join(
        cdm$person |> 
          dplyr::select(
            !!id := "person_id", "gender_concept_id", "year_of_birth", 
            "month_of_birth", "day_of_birth"
          ) |>
          dplyr::mutate(
            "sex" = dplyr::case_when(
              .data$gender_concept_id == 8507 ~ "Male",
              .data$gender_concept_id == 8532 ~ "Female",
              .default = "None"
            ),
            "day_of_birth" = dplyr::if_else(
              is.na(.data$day_of_birth), 1, .data$day_of_birth
            ),
            "month_of_birth" = dplyr::if_else(
              is.na(.data$month_of_birth), 1, .data$month_of_birth
            ),
            "birth_date" = as.Date(paste0(
              as.character(as.integer(.data$year_of_birth)), "-", 
              as.character(as.integer(.data$month_of_birth)), "-",
              as.character(as.integer(.data$day_of_birth))
            ))
          ) |>
          dplyr::select(dplyr::all_of(c(id, "sex", "birth_date"))),
        by = id
      )
    if (age | !is.null(ageGroup)) {
      xx <- xx %>%
        dplyr::mutate(
          "age" = !!CDMConnector::datediff("birth_date", indexDate, interval = "year")
        )
      if (!is.null(ageGroup)) {
        agq <- ageGroupQuery(ageGroup)
        xx <- xx |>
          dplyr::mutate(!!!agq)
      }
    }
    cols <- c("birth_date", "sex", "age")[c(TRUE, !sex, !age)]
    xx <- xx |> dplyr::select(!dplyr::any_of(cols))
    x <- x |>
      dplyr::left_join(xx, by = c(idx, id, indexDate))
  }
  
  return(x)
}
ageGroupQuery <- function(ageGroup) {
  q <- "dplyr::case_when("
  for (k in seq_along(ageGroup)) {
    q <- paste0(
      q, ".data$age >= ", ageGroup[[k]][1], " & .data$age <= ", 
      ageGroup[[k]][2], " ~ '", ageGroup[[k]][1], " to ", ageGroup[[k]][2], "', " 
    )
  }
  q <- paste0(q, ".default = 'None')")
  q <- q |> rlang::parse_exprs() |> rlang::set_names("age_group")
  return(q)
}
