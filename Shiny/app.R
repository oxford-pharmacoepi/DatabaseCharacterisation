
# renv::activate()
# renv::restore()

library(shiny)
library(shinydashboard)
library(fresh)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(gt)
library(dplyr)
library(tidyr)
library(visOmopResults)
library(readr)
library(ggplot2)
library(here)
library(CohortCharacteristics)

load(here("mergedResults.RData"))

source(here("functions.R"))

# theme ----
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#073042" 
  ),
  adminlte_sidebar(
    dark_bg = "#1f8cad",
    dark_hover_bg = "#1c5570",
    dark_color = "white"
  ), 
  adminlte_global(
    content_bg = "#eaebea" 
  ),
  adminlte_vars(
    border_color = "#112446",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446"
  )
)

# ui ----
ui <- dashboardPage(
  dashboardHeader(title = "db Characaterisation"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background"
      ),
      menuItem(
        text = "Database Metadata",
        tabName = "database_snapshot"
      ),
      menuItem(
        text = "Clinical tables",
        tabName = "clinical_tables",
        menuSubItem(
          text = "Overall summary",
          tabName = "overall_summary"
        ),
        menuSubItem(
          text = "Counts",
          tabName = "incident_counts_trends"
        )
      ),
      menuItem(
        text = "Observation period",
        tabName = "observation_period",
        menuSubItem(
          text = "Individuals in observation",
          tabName = "individuals_observation"
        ),
        menuSubItem(
          text = "Follow-up histogram",
          tabName = "overall_followup"
        ),
        menuSubItem(
          text = "Person days contribution",
          tabName = "person_days"
        )
      ),
      menuItem(
        text = "Counts",
        tabName = "counts",
        # menuSubItem(
        #   text = "Incidence",
        #   tabName = "incidence"
        # ),
        # menuSubItem(
        #   text = "Prevalence",
        #   tabName = "prevalence"
        # ),
        menuSubItem(
          text = "Concept counts",
          tabName = "concept_counts"
        )
      ),
      menuItem(
        text = "Individuals characteristics",
        tabName = "individuals_char",
        menuSubItem(
          text = "Characteristics at entry",
          tabName = "characteristics_entry"
        ),
        menuSubItem(
          text = "Characteristics per year",
          tabName = "characteristics_year"
        )
      )
    )
  ),
  ## body ----
  dashboardBody(
    use_theme(mytheme),
    tabItems(
      ### background  ------
      tabItem(
        tabName = "background",
        a(
          img(src = "logo_HDS.png", align = "right", width = "200px"),
          href = "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology/",
          target = "_blank"
        ),
        h3("Database characterisation"),
        h5("This is a visualisation for database characterisation."),
        h5(
          "The code can be found at: ",
          a(
            "https://github.com/oxford-pharmacoepi/DatabaseCharacterisation", 
            href = "https://github.com/oxford-pharmacoepi/DatabaseCharacterisation"
          ),
          "."
        ),
        h5(
          "The study was conducted with synthetic data: ",
          a("Eunomia.", href = "https://github.com/OHDSI/Eunomia"),
          "The mock synthetic data is available using the ",
          a("CodeToRunEunomia.R", href = "https://github.com/oxford-pharmacoepi/DatabaseCharacterisation/blob/main/Study/CodeToRunEunomia.R"),
          " or you can download it from: ",
          a("https://example-data.ohdsi.dev/GiBleed.zip", href = "https://example-data.ohdsi.dev/GiBleed.zip")
        ),
        h5(
          "This project was founded by: ",
          a("HDRUK", href = "https://www.hdruk.ac.uk/"),
          "."
        ),
        a(
          img(src = "hdruk_main_rgb_transparentpng.png", align = "left", width = "200px"),
          href = "https://www.hdruk.ac.uk/",
          target = "_blank"
        )
      ),
      ### cdm snapshot ----
      tabItem(
        tabName = "database_snapshot",
        h5("Metadata of each database"),
        selectors(
          data = snapshot, 
          prefix = "db", 
          columns = c("cdm_name"), 
          multiple = TRUE
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tidy table",
            downloadButton("snapshot_tidy_download", "Download csv"),
            dataTableOutput("snapshot_tidy") %>% withSpinner()
          ),
          tabPanel(
            "Formatted table",
            downloadButton("snapshot_formatted_download", "Download word"),
            gt_output("snapshot_formatted") %>% withSpinner()
          )
        )
      ),
      ### overall summary ----
      tabItem(
        tabName = "overall_summary",
        h5("Overall summary"),
        selectors(
          data = overallSummary, 
          prefix = "os", 
          columns = c("cdm_name", "omop_table"), 
          multiple = TRUE
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tidy table",
            downloadButton("os_tidy_download", "Download csv"),
            dataTableOutput("os_tidy") %>% withSpinner()
          ),
          tabPanel(
            "Formatted table",
            downloadButton("os_formatted_download", "Download word"),
            gt_output("os_formatted") %>% withSpinner()
          )
        )
      ),
      ### concept counts ----
      tabItem(
        tabName = "concept_counts",
        h5("Concept counts"),
        selectors(
          data = conceptCounts, 
          prefix = "cc", 
          columns = c("cdm_name", "omop_table", "sex", "age_group", "year"),
          default = list("sex" = "overall", "age_group" = "overall", "year" = "overall"),
          multiple = TRUE
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tidy table",
            downloadButton("cc_tidy_download", "Download csv"),
            dataTableOutput("cc_tidy") %>% withSpinner()
          ),
          tabPanel(
            "Formatted table",
            downloadButton("cc_formatted_download", "Download word"),
            gt_output("cc_formatted") %>% withSpinner()
          )
        )
      ),
      ### characteristics at entry ----
      tabItem(
        tabName = "characteristics_entry",
        h5("Characteristics at entry"),
        selectors(
          data = characteristicsAtEntry, 
          prefix = "ce", 
          columns = c("cdm_name"),
          multiple = TRUE
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tidy table",
            downloadButton("ce_tidy_download", "Download csv"),
            dataTableOutput("ce_tidy") %>% withSpinner()
          ),
          tabPanel(
            "Formatted table",
            downloadButton("ce_formatted_download", "Download word"),
            gt_output("ce_formatted") %>% withSpinner()
          )
        )
      ),
      ### characteristics year ----
      tabItem(
        tabName = "characteristics_year",
        h5("Characteristics of individuals in observation at a certain year"),
        selectors(
          data = characteristicsYear, 
          prefix = "cy", 
          columns = c("cdm_name", "strata_level"),
          multiple = TRUE
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tidy table",
            downloadButton("cy_tidy_download", "Download csv"),
            dataTableOutput("cy_tidy") %>% withSpinner()
          ),
          tabPanel(
            "Formatted table",
            downloadButton("cy_formatted_download", "Download word"),
            gt_output("cy_formatted") %>% withSpinner()
          )
        )
      ),
      ### summary followup ----
      tabItem(
        tabName = "overall_followup",
        h5("Histogram of follow-up"),
        selectors(
          data = summaryFollowup, 
          prefix = "fu", 
          columns = c("cdm_name", "sex", "age_group_at_entry"),
          default = list(sex = "overall", age_group_at_entry = "overall"),
          multiple = TRUE
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tidy table",
            downloadButton("fu_tidy_download", "Download csv"),
            dataTableOutput("fu_tidy") %>% withSpinner()
          ),
          tabPanel(
            "Formatted table",
            downloadButton("fu_formatted_download", "Download word"),
            gt_output("fu_formatted") %>% withSpinner()
          ),
          tabPanel(
            "Histogram plot",
            plotOutput("fu_plot") %>% withSpinner()
          )
        )
      ),
      ### summary person-days ----
      tabItem(
        tabName = "person_days",
        h5("Person days"),
        selectors(
          data = summaryPersonDays, 
          prefix = "pd", 
          columns = c("cdm_name", "sex", "age_group", "year"),
          default = list(sex = "overall", age_group = "overall", year = "overall"),
          multiple = TRUE
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tidy table",
            downloadButton("pd_tidy_download", "Download csv"),
            dataTableOutput("pd_tidy") %>% withSpinner()
          ),
          tabPanel(
            "Formatted table",
            downloadButton("pd_formatted_download", "Download word"),
            gt_output("pd_formatted") %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            pickerInput(
              inputId = "pd_x",
              label = "x", 
              choices = c("cdm_name", "age_group", "sex", "year"),
              selected = c("age_group", "sex"),
              multiple = T,
              inline = TRUE
            ),
            pickerInput(
              inputId = "pd_facet",
              label = "Facet by", 
              choices = c("cdm_name", "age_group", "sex", "year"),
              selected = c("cdm_name", "year"),
              multiple = T,
              inline = TRUE
            ),
            pickerInput(
              inputId = "pd_color",
              label = "Color by", 
              choices = c("cdm_name", "age_group", "sex", "year"),
              selected = c("age_group", "sex"),
              multiple = T,
              inline = TRUE
            ),
            plotOutput("pd_plot") %>% withSpinner()
          )
        )
      ),
      ### incident counts trend ----
      tabItem(
        tabName = "incident_counts_trends",
        h5("Counts per table"),
        selectors(
          data = incidentCounts, 
          prefix = "ic", 
          columns = c("cdm_name", "omop_table"), 
          multiple = TRUE
        ),
        selectors(
          data = incidentCounts, 
          prefix = "ic", 
          columns = c("strata_name"), 
          multiple = FALSE
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tidy table",
            downloadButton("ic_tidy_download", "Download csv"),
            dataTableOutput("ic_tidy") %>% withSpinner()
          ),
          tabPanel(
            "Formatted table",
            downloadButton("ic_formatted_download", "Download word"),
            gt_output("ic_formatted") %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            plotOutput("ic_plot") %>% withSpinner()
          )
        )
      ),
      ### incident counts trend ----
      tabItem(
        tabName = "individuals_observation",
        h5("Individuals in observation"),
        selectors(
          data = opResult, 
          prefix = "op", 
          columns = c("cdm_name"), 
          multiple = TRUE
        ),
        selectors(
          data = opResult, 
          prefix = "op", 
          columns = c("strata_name"), 
          multiple = FALSE
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tidy table",
            downloadButton("op_tidy_download", "Download csv"),
            dataTableOutput("op_tidy") %>% withSpinner()
          ),
          tabPanel(
            "Formatted table",
            downloadButton("op_formatted_download", "Download word"),
            gt_output("op_formatted") %>% withSpinner()
          ),
          tabPanel(
            "Plot",
            plotOutput("op_plot") %>% withSpinner()
          )
        )
      )
      ### end ----
    )
  )
)

# server ----
server <- function(input, output) {
  ## cdm snapshot ----
  getSnapshot <- reactive({
    snapshot |>
      filterData(prefix = "db", input) |>
      pivot_wider(
        names_from = c("variable_name", "estimate_name"), 
        values_from = "estimate_value"
      )
  })
  output$snapshot_tidy <- renderDataTable({
    datatable(getSnapshot(), options = list(scrollX = TRUE))
  })
  output$snapshot_tidy_download <- downloadHandler(
    filename = "cdm_snapshot.csv",
    content = function(file) {
      getSnapshot() %>% write_csv(file = file)
    }
  )
  getGtSnapshot <- reactive({
    snapshot |>
      filterData(prefix = "db", input) |>
      formatHeader(header = "cdm_name") |>
      rename("Variable" = "variable_name", "Estimate" = "estimate_name") |>
      gtTable()
  })
  output$snapshot_formatted <- render_gt({
    getGtSnapshot()
  })
  output$snapshot_formatted_download <- downloadHandler(
    filename = "cdm_snapshot.docx",
    content = function(file) {
      getGtSnapshot() %>% gtsave(filename = file)
    }
  )
  ## omop table ----
  getOsTidy <- reactive({
    overallSummary |>
      filterData("os", input)
  })
  output$os_tidy <- renderDataTable({
    getOsTidy() |>
      datatable()
  })
  output$os_tidy_download  <- downloadHandler(
    filename = "omop_table_summary.csv",
    content = function(file) {
      getOsTidy() %>% write_csv(file = file)
    }
  )
  getOsFormatted <- reactive({
    overallSummary |>
      filterData("os", input) |>
      formatEstimateValue() |>
      formatEstimateName(
        estimateNameFormat = c(
          "N (%)" = "<count> (<percentage>%)",
          "N" = "<count>",
          "median [IQR]" = "<median> [<q25> - <q75>]",
          "mean (sd)" = "<mean> (<sd>)"
        ), 
        keepNotFormatted = FALSE
      ) |>
      formatHeader(header = c("cdm_name")) |>
      select(-"estimate_type") |>
      rename(
        "Variable" = "variable_name", "Level" = "variable_level",
        "Estimate" = "estimate_name"
      ) |>
      gtTable(
        groupNameCol = c("omop_table"), 
        colsToMergeRows = c("Variable", "Level")
      )
      
  })
  output$os_formatted <- render_gt({
    getOsFormatted()
  })
  output$os_formatted_download <- downloadHandler(
    filename = "omop_tables_summary.docx",
    content = function(file) {
      getOsFormatted() %>% gtsave(filename = file)
    }
  )
  ## incident counts ----
  getIcTidy <- reactive({
    incidentCounts |>
      filterData("ic", input) |>
      mutate(estimate_value = as.numeric(estimate_value)) |>
      select("omop_table", "strata_name", "strata_level", "variable_name", "cdm_name", "estimate_value") |>
      pivot_wider(names_from = cdm_name, values_from = estimate_value)
  })
  output$ic_tidy <- renderDataTable({
    getIcTidy() 
  })
  output$ic_tidy_download  <- downloadHandler(
    filename = "omop_table_incident_counts.csv",
    content = function(file) {
      getIcTidy() |>
        write_csv(file = file)
    }
  )
  getIcFormatted <- reactive({
    incidentCounts |>
      filterData("ic", input) |>
      mutate(estimate_name = NA) |>
      formatEstimateValue() |>
      select(-estimate_name) |>
      arrange(strata_level) |>
      mutate(Time = as.character(strata_level)) |>
      formatHeader(header = c("cdm_name")) |>
      select(-"estimate_type", -"strata_name", -"strata_level") |>
      rename("Variable" = "variable_name") |>
      gtTable(
        groupNameCol = c("omop_table"), 
        colsToMergeRows = c("Variable")
      )
  })
  output$ic_formatted <- render_gt(getIcFormatted())
  output$ic_formatted_download <- downloadHandler(
    filename = "omop_table_incident_counts.docx",
    content = function(file) {
      getIcFormatted() |>
        gtsave(filename = file)
    }
  )
  output$ic_plot <- renderPlot({
    incidentCounts |>
      filterData("ic", input) |>
      mutate(count = as.numeric(estimate_value), time = strata_level) |>
      ggplot(aes(x = time, y = count, group = omop_table, color = omop_table)) +
      geom_point() +
      geom_line() +
      facet_wrap(facets = "cdm_name")
  })
  ## code counts tidy ----
  getCcTidy <- reactive({
    conceptCounts |>
      filterData("cc", input) |>
      mutate(estimate_value = as.numeric(estimate_value))
  })
  output$cc_tidy <- renderDataTable({
    getCcTidy() 
  })
  output$cc_tidy_download <- downloadHandler(
    filename = "omop_table_code_counts.csv",
    content = function(file) {
      getCcTidy() |>
        write_csv(file = file)
    }
  )
  ## code counts gt ----
  getCcFormatted <- reactive({
    conceptCounts |>
      filterData("cc", input) |>
      arrange(desc(as.numeric(estimate_value))) |>
      formatEstimateValue() |>
      formatHeader(header = c("cdm_name")) |>
      select(-"estimate_type", -"estimate_name") |>
      rename("Concept name" = "variable_name", "Concept id" = "variable_level") |>
      gtTable(
        groupNameCol = c("omop_table"), 
        colsToMergeRows = c("Concept name", "Concept id")
      )
  })
  output$cc_formatted <- render_gt(getCcFormatted())
  output$cc_formatted_download <- downloadHandler(
    filename = "omop_table_code_counts.docx",
    content = function(file) {
      getCcFormatted() |>
        gtsave(filename = file)
    }
  )
  #
  ## characteristics entry tidy ----
  getCeTidy <- reactive({
    characteristicsAtEntry |>
      filterData("ce", input) |>
      select(-starts_with(c("result", "package", "group", "strata", "additional")))
  })
  output$ce_tidy <- renderDataTable({
    getCeTidy()
  })
  output$ce_tidy_download <- downloadHandler(
    filename = "characteristics_at_entry.csv",
    content = function(file) {
      getCeTidy() |>
        write_csv(file = file)
    }
  )
  ## characteristics entry gt ----
  output$ce_formatted <- render_gt({
    characteristicsAtEntry |>
      filterData("ce", input) |>
      CohortCharacteristics::tableCharacteristics(header = "cdm_name")
  })
  output$ce_formatted_download <- downloadHandler(
    filename = "characteristics_at_entry.docx",
    content = function(file) {
      characteristicsAtEntry |>
        filterData("ce", input) |>
        CohortCharacteristics::tableCharacteristics(header = "cdm_name") |>
        gtsave(filename = file)
    }
  )
  ## characteristics year tidy ----
  getCyTidy <- reactive({
    characteristicsYear |>
      filterData("ye", input) |>
      splitStrata() |>
      select(-starts_with(c("result", "package", "group", "additional")))
  })
  output$cy_tidy <- renderDataTable({
    getCyTidy()
  })
  output$cy_tidy_download <- downloadHandler(
    filename = "characteristics_year.csv",
    content = function(file) {
      getCyTidy() |>
        write_csv(file = file)
    }
  )
  ## characteristics year gt ----
  output$cy_formatted <- render_gt({
    characteristicsYear |>
      # filterData("cy", input) |>
      CohortCharacteristics::tableCharacteristics(header = c("cdm_name", "strata"))
  })
  output$cy_formatted_download <- downloadHandler(
    filename = "characteristics_year.docx",
    content = function(file) {
      characteristicsYear |>
        filterData("cy", input) |>
        CohortCharacteristics::tableCharacteristics(header = c("cdm_name", "strata")) |>
        gtsave(filename = file)
    }
  )
  ## individuals followup tidy ----
  getFuTidy <- reactive({
    summaryFollowup |>
      filterData("fu", input)
  })
  output$fu_tidy <- renderDataTable({
    getFuTidy()
  })
  output$fu_tidy_download <- downloadHandler(
    filename = "followup_histogram.csv",
    content = function(file) {
      getFuTidy() |>
        write_csv(file = file)
    }
  )
  ## individuals followup gt ----
  getFuFormatted <- reactive({
    summaryFollowup |>
      filterData("fu", input) |>
      formatEstimateValue() |>
      formatHeader(header = c("cdm_name", "sex", "age_group_at_entry")) |>
      select(-"estimate_type", -"estimate_name") |>
      gtTable(colsToMergeRows = c("variable_name"), na = "<5")
  })
  output$fu_formatted <- render_gt(getFuFormatted())
  output$fu_formatted_download <- downloadHandler(
    filename = "summarised_followup.docx",
    content = function(file) {
      getFuFormatted() |>
        gtsave(filename = file)
    }
  )
  ## individuals followup plot ----
  getFuPlot <- reactive({
    summaryFollowup |>
      filterData("fu", input) |>
      rename(number_individuals = estimate_value) |>
      separate(col = "variable_level", into = c("x1", "x2"), sep = " to ") |>
      mutate(across(c("x1", "x2", "number_individuals"), as.numeric)) |>
      mutate(followup_years = (x1 + x2)/2/365.25) |>
      ggplot(aes(x = followup_years, y = number_individuals)) +
      geom_bar(stat = "identity") + 
      facet_grid(sex + age_group_at_entry ~ cdm_name)
  })
  output$fu_plot <- renderPlot({
    getFuPlot()
  })
  ## person days tidy ----
  getPdTidy <- reactive({
    summaryPersonDays |> filterData("pd", input)
  })
  output$pd_tidy <- renderDataTable(getPdTidy())
  output$pd_tidy_download <- downloadHandler(
    filename = "summary_person_days.csv",
    content = function(file) {
      getPdTidy() |>
        write_csv(file = file)
    }
  )
  ## person days gt ----
  getPdFormatted <- reactive({
    summaryPersonDays |> 
      filterData("pd", input) |>
      formatEstimateValue() |>
      formatEstimateName(estimateNameFormat = c(
        "N" = "<count>",
        "median [IQR]" = "<median> [<q25> - <q75>]"
      )) |>
      select(-"estimate_type") |>
      formatHeader(header = c("cdm_name", "year")) |>
      mutate(group = paste0("Age group: ", age_group, "; Sex: ", sex)) |>
      select(-"age_group", -"sex") |>
      rename("Variable" = "variable_name", "Estimate" = "estimate_name") |>
      gtTable(groupNameCol = "group", colsToMergeRows = "Variable")
  })
  output$pd_formatted <- render_gt(getPdFormatted())
  output$pd_formatted_download <- downloadHandler(
    filename = "summary_person_days.docx",
    content = function(file) {
      getPdFormatted() |>
        gtsave(filename = file)
    }
  )
  ## person days plot ----
  getPdPlot <- reactive({
    x <- summaryPersonDays |> 
      filter(estimate_name == "total") |>
      filterData("pd", input) |>
      mutate(person_days = as.numeric(estimate_value)) |>
      unite(col = "group", c("cdm_name", "sex", "age_group", "year"), remove = F)
    if (length(input$pd_x) > 0) {
      x <- x |>
        unite(col = "x", input$pd_x, remove = F)
    } else {
      x <- x |>
        mutate(x = "")
    }
    if (length(input$pd_facet) > 0) {
      x <- x |>
        unite(col = "facet", input$pd_facet, remove = F)
    } else {
      x <- x |>
        mutate(facet = "")
    }
    if (length(input$pd_color) > 0) {
      x <- x |>
        unite(col = "color", input$pd_color, remove = F)
    } else {
      x <- x |>
        mutate(color = "")
    }
    x |>
      ggplot(aes(x = x, y = person_days, group = group, color = color, fill = color)) +
      geom_bar(stat = "identity") +
      facet_wrap(facets = "facet")
  })
  output$pd_plot <- renderPlot({getPdPlot()})
  ## individuals in observation ----
  getOpTidy <- reactive({
    opResult |>
      filterData("op", input) |>
      mutate(estimate_value = as.numeric(estimate_value))
  })
  output$op_tidy <- renderDataTable({
    getOpTidy() 
  })
  output$op_tidy_download  <- downloadHandler(
    filename = "observation_period_counts.csv",
    content = function(file) {
      getOpTidy() |>
        write_csv(file = file)
    }
  )
  getOpFormatted <- reactive({
    opResult |>
      filterData("op", input) |>
      mutate(estimate_name = NA) |>
      formatEstimateValue() |>
      select(-estimate_name) |>
      arrange(strata_level) |>
      mutate(Time = as.character(strata_level)) |>
      formatHeader(header = c("cdm_name")) |>
      select(-"estimate_type", -"strata_name", -"strata_level", -"omop_table") |>
      rename("Variable" = "variable_name") |>
      gtTable(colsToMergeRows = c("Variable"))
  })
  output$op_formatted <- render_gt(getOpFormatted())
  output$op_formatted_download <- downloadHandler(
    filename = "observation_period_counts.docx",
    content = function(file) {
      getOpFormatted() |>
        gtsave(filename = file)
    }
  )
  output$op_plot <- renderPlot({
    opResult |>
      filterData("op", input) |>
      mutate(individuals_in_observation = as.numeric(estimate_value), time = strata_level) |>
      ggplot(aes(x = time, y = individuals_in_observation, color = cdm_name)) +
      geom_point() +
      geom_line()
  })
  ## end ----
}

# app ----
shinyApp(ui = ui, server = server)
