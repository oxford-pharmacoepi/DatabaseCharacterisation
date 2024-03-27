generateAgeCohortSet <- function(cdm,
                                 name,
                                 ageGroup = list(c(0, 19), c(20, Inf)),
                                 targetCohortTable = NULL,
                                 targetCohortId = NULL,
                                 overwrite = TRUE) {
  ages <- unlist(ageGroup, recursive = TRUE)
  ageBreak <- ages + rep(c(0, 1), length(ages)/2)
  ageBreak <- unique(ageBreak)
  
  if (is.null(targetCohortTable)) {
    x <- cdm[["observation_period"]] |>
      dplyr::select(
        "subject_id" = "person_id",
        "cohort_start_date" = "observation_period_start_date",
        "cohort_end_date" = "observation_period_end_date"
      ) |>
      dplyr::mutate("cohort_definition_id" = 1)
    set <- dplyr::tibble(
      cohort_definition_id = 1, cohort_name = "age_cohort"
    )
  } else {
    x <- cdm[[targetCohortTable]]
    set <- CDMConnector::cohortSet(x)
    if (!is.null(targetCohortId)) {
      x <- x |>
        dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
      set <- set |>
        dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
    }
  }
  
  ageBreaks <- ageBreak[!is.infinite(ageBreak) & ageBreak > 0]
  plus1yr <- glue::glue(
    "CDMConnector::dateadd('date_of_birth',{ageBreaks},interval = 'year')"
  ) %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue("start_{seq_along(ageBreaks) + 1}"))
  minus1d <- glue::glue(
    "CDMConnector::dateadd('start_{seq_along(ageBreaks) + 1}',-1, interval = 'day')"
  ) %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue("enddd_{seq_along(ageBreaks)}"))
  x <- x %>%
    PatientProfiles::addDateOfBirth() %>%
    dplyr::mutate(!!!plus1yr) %>%
    dplyr::mutate(!!!minus1d, !!paste0("enddd_", length(ageBreaks) + 1) := .data$cohort_end_date) %>%
    dplyr::rename("start_1" = "date_of_birth", "obs_start" = "cohort_start_date", "obs_end" = "cohort_end_date") %>%
    dplyr::compute()
  
  x <- x %>%
    tidyr::pivot_longer(dplyr::starts_with(c("start", "enddd"))) %>%
    dplyr::mutate(
      date = dplyr::if_else(
        substr(.data$name, 1, 5) == "start",
        "cohort_start_date",
        "cohort_end_date"
      ),
      obs_id = substr(.data$name, 7, 7)
    ) %>%
    dplyr::select(-"name") %>%
    tidyr::pivot_wider(names_from = "date", values_from = "value") %>%
    dplyr::mutate(
      "cohort_start_date" = dplyr::if_else(
        .data$cohort_start_date > .data$obs_start,
        .data$cohort_start_date,
        .data$obs_start
      ),
      "cohort_end_date" = dplyr::if_else(
        .data$cohort_end_date > .data$obs_end,
        .data$obs_end,
        .data$cohort_end_date
      )
    ) %>%
    dplyr::filter(.data$cohort_start_date <= .data$cohort_end_date) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    )  %>%
    dplyr::compute(
      name = name, temporary = FALSE,
      overwrite = TRUE
    )
  
  cdm[[name]] <- x %>%
    omopgenerics::newCohortTable(
      cohortSetRef = set)

  return(cdm)
}
monthlyIncident <- function(cdm, tab) {
  date <- switch(
    tab,
    "observation_period" = "observation_period_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "condition_occurrence" = "condition_start_date",
    "observation" = "observation_date",
    "measurement" = "measurement_date",
    "procedure_occurrence" = "procedure_date",
    "device_exposure" = "device_exposure_start_date",
  )
  if (tab != "observation_period") {
    x <- cdm[[tab]] %>%
      addInObservation(indexDate = date) %>%
      filter(in_observation == 1)
  } else {
    x <- cdm[[tab]]
  }
  x <- x %>%
    rename("incidence_date" = all_of(date)) %>%
    mutate(
      "incidence_month" = !!datepart("incidence_date", "month"),
      "incidence_year" = !!datepart("incidence_date", "year")
    ) %>%
    group_by(incidence_month, incidence_year) %>%
    summarise(n = as.numeric(n()), .groups = "drop") %>%
    collect()
  x <- x %>%
    union_all(
      x %>%
        group_by(incidence_year) %>%
        summarise(n = sum(n), .groups = "drop") %>%
        mutate(incidence_month = as.numeric(NA))
    ) %>%
    mutate(n = if_else(n < 5, as.numeric(NA), .data$n))
  return(x)
}
monthlyOngoing <- function(cdm, tab) {
  dateS <- switch(
    tab,
    "observation_period" = "observation_period_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "condition_occurrence" = "condition_start_date",
    "device_exposure" = "device_exposure_start_date",
  )
  dateE <- switch(
    tab,
    "observation_period" = "observation_period_end_date",
    "drug_exposure" = "drug_exposure_end_date",
    "condition_occurrence" = "condition_end_date",
    "device_exposure" = "device_exposure_end_date",
  )
  if (tab != "observation_period") {
    x <- cdm[[tab]] %>%
      addInObservation(indexDate = dateS) %>%
      filter(in_observation == 1)
  } else {
    x <- cdm[[tab]]
  }
  x <- x %>%
    select("start_date" = all_of(dateS), "end_date" = all_of(dateE)) %>%
    filter(!is.na(end_date)) %>%
    mutate(
      "start_date_month" = as.numeric(!!datepart("start_date", "month")),
      "start_date_year" = as.numeric(!!datepart("start_date", "year")),
      "end_date_month" = as.numeric(!!datepart("end_date", "month")),
      "end_date_year" = as.numeric(!!datepart("end_date", "year"))
    ) %>%
    group_by(start_date_month, start_date_year, end_date_month, end_date_year) %>%
    summarise(n = as.numeric(n()), .groups = "drop") %>%
    compute()
  time <- expand.grid(
    ongoing_month = 1:12,
    ongoing_year = seq(
      x %>% pull("start_date_year") %>% min(),
      x %>% pull("end_date_year") %>% max()
    ),
    id = 1
  ) %>%
    mutate(ongoing_date = as.Date(paste0(ongoing_year, "/", ongoing_month, "/01"))) %>%
    select(-ongoing_month, -ongoing_year)
  x <- x %>%
    mutate(id = 1) %>%
    inner_join(time, copy = TRUE, by = "id", relationship = "many-to-many") %>%
    mutate(
      start_date = as.Date(paste0(start_date_year, "/", start_date_month, "/01")),
      end_date = as.Date(paste0(end_date_year, "/", end_date_month, "/01"))
    ) %>%
    filter(ongoing_date >= start_date & ongoing_date <= end_date) %>%
    group_by(ongoing_date) %>%
    summarise(n = sum(.data$n), .groups = "drop") %>%
    collect() %>%
    mutate(n = if_else(n < 5, as.numeric(NA), .data$n))
  return(x)
}
summaryTable <- function(cdm, tab) {
  concept <- switch(
    tab,
    "observation_period" = "period_type_concept_id",
    "drug_exposure" = "drug_concept_id",
    "condition_occurrence" = "condition_concept_id",
    "observation" = "observation_concept_id",
    "measurement" = "measurement_concept_id",
    "procedure_occurrence" = "procedure_concept_id",
    "device_exposure" = "device_concept_id",
    "person" = "gender_concept_id"
  )
  if (!tab %in% c("observation_period", "person")) {
    date <- switch(
      tab,
      "drug_exposure" = "drug_exposure_start_date",
      "condition_occurrence" = "condition_start_date",
      "observation" = "observation_date",
      "measurement" = "measurement_date",
      "procedure_occurrence" = "procedure_date",
      "device_exposure" = "device_exposure_start_date",
    )
    x <- cdm[[tab]] %>%
      addInObservation(indexDate = date)
  } else {
    x <- cdm[[tab]] %>% mutate(in_observation = 1)
  }
  x %>%
    rename(concept_id = all_of(concept)) %>%
    summarise(
      number_records = n(),
      number_concepts = n_distinct(concept_id),
      number_persons = n_distinct(person_id),
      number_in_observation = sum(in_observation, na.rm = TRUE)
    ) %>%
    collect()
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
  
  # create denominator cohort
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm, name = tmp1, ageGroup = ageGroup, sex = "Both",
    daysPriorObservation = 0, requirementInteractions = TRUE,
    overwrite = TRUE
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
      PatientProfiles::addSex()
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
      "cdm_name" = omopgenerics::cdmName(cdm),
      "result_type" = "summarised_person_days",
      "package_name" = "DatabaseCharacterisation",
      "package_version" = "0.0.0", #as.character(utils::packageVersion("OmopSketch")),
      "group_name" = "population",
      "group_level" = "overall",
      "variable_level" = NA_character_,
      "estimate_type" = dplyr::if_else(
        .data$estimate_name %in% c("count",  "total"), "integer",
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
      PatientProfiles::addSex()
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
