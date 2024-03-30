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

load(here("mergedResults.RData"))

source(here("functions.R"))

# theme ----
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#0c0e0c" 
  ),
  adminlte_sidebar(
    # width = "400px",
    dark_bg = "#78B7C5",
    dark_hover_bg = "#3B9AB2",
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
  dashboardHeader(title = "database characaterisation"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background"
      ),
      menuItem(
        text = "Database details",
        tabName = "cdm_snapshot",
        menuSubItem(
          text = "Database snapshot",
          tabName = "database_snapshot"
        )
      ),
      menuItem(
        text = "Summary tables",
        tabName = "summary_tables",
        menuSubItem(
          text = "Overall summary",
          tabName = "overall_summary"
        ),
        menuSubItem(
          text = "Incident counts trends",
          tabName = "incident_counts_trends"
        ),
        menuSubItem(
          text = "Ongoing counts trends",
          tabName = "ongoingt_counts_trends"
        ),
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
      ),
      menuItem(
        text = "Individuals followup",
        tabName = "individuals_followup",
        menuSubItem(
          text = "Overall follow-up",
          tabName = "overall_followup"
        ),
        menuSubItem(
          text = "Person days summary",
          tabName = "person_days"
        )
      )
    )
  ),
  ## body ----
  dashboardBody(
    use_theme(mytheme),
    tabItems(
      # background  ------
      tabItem(
        tabName = "background",
        a(
          img(src = "hds_logo.png", align = "right", width = "200px"),
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
        h5("The study was conducted in Eunomia.")
      ),
      # cdm snapshot ----
      tabItem(
        tabName = "database_snapshot",
        h5("Metadata of each database"),
        pickerInput(
          inputId = "db_cdm_name", 
          label = "Select database name",
          selected = snapshot$cdm_name |> unique(),
          choices = snapshot$cdm_name |> unique(),
          multiple = F, 
          inline = T
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
      )
      # end ----
    )
  )
)

# server ----
server <- function(input, output) {
  # cdm snapshot ----
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
  # end ----
}

# app ----
shinyApp(ui = ui, server = server)
