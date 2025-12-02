# Source - https://stackoverflow.com/a/48434444
# Posted by Cristian E. Nuno, modified by community. See post 'Timeline' for change history
# Retrieved 2025-11-26, License - CC BY-SA 3.0

# install necessary packages
# install.packages( pkgs = c( "devtools", "shiny", "shinydashboard" ) )
# install the development version of leaflet from Github
# devtools::install_github( repo = "rstudio/leaflet" )


# load necessary packages
library(readr)
library(tigris)
library(tibble)
library(dplyr)
library(sociome)
library(ggplot2)
# library(ggiraph)
library(shinyvalidate)
library(shiny)
library(bslib)

options(tigris_use_cache = TRUE)

# block_groups <- block_groups(cb = TRUE, year = 2020)

# library(tidyverse)
# counties_tbl <-
#     map_dfr(
#         c(2000, 2010, 2020),
#         ~tigris::counties(cb = TRUE, year = .x) |>
#             as_tibble() |>
#             reframe(
#                 year = .x,
#                 state_fips = STATEFP,
#                 county_fips = COUNTYFP,
#                 county_name = NAME
#             )
#     ) |>
#     filter(!(state_fips %in% c(60, 78, 69, 66))) |>
#     inner_join(
#         distinct(tidycensus::fips_codes, state_name, state_fips = state_code),
#         by = "state_fips"
#     ) |>
#     select(year, state_fips, state_name, county_fips, county_name)
# states <- deframe(distinct(counties_tbl, state_name, state_fips))
# saveRDS(counties_tbl, here::here("data", "counties_tbl.rds"))
# saveRDS(states, here::here("data", "states.rds"))
states <- readRDS("data/states.rds")
counties_tbl <- readRDS("data/counties_tbl.rds")

current_year <- as.POSIXlt(Sys.Date())$year + 1900

dataset_options_tbl <-
    bind_rows(
        tibble(dataset = "acs5", year = 2009:current_year),
        tibble(dataset = "acs1", year = 2007:current_year),
        tibble(dataset = "acs3", year = 2007:2013),
        tibble(dataset = "decennial", year = c(2000, 2010, 2020))
    )


# User interface ----
ui <- page_sidebar(
    title = "sociome",
    sidebar = sidebar(
        radioButtons(
            "geography",
            "Select level of geography for which you want ADIs:",
            choices =
                c("state", "county", "tract", "block group", ZCTA = "zcta")
        ),
        numericInput("year", "Year:", value = 2020),
        radioButtons(
            "ref_area_type",
            label = "Select reference area selection method",
            choices = c("States", "Counties", "GEOIDs")
        ),
        uiOutput("ref_area_spec_ui"),
        uiOutput("dataset_spec_ui"),
        input_task_button("execute_get_adi", "Calculate ADI"),
        downloadButton("download_tbl", "Download ADI data"),
        downloadButton("download_plot", "Download ADI plot")
    ),
    card(
        card_header("Area Deprivation Index (ADI)"),
        plotOutput("plot")
    )
)

# Server logic
server <- function(input, output) {

    # Year
    year_validator <- InputValidator$new()
    year_validator$add_rule(
        "year",
        sv_in_set(c(2000, 2007:current_year), set_limit = Inf)
    )
    year_validator$enable()
    dec_year <- reactive({
        req(year_validator$is_valid())
        trunc(input$year / 10) * 10
    })

    output$ref_area_spec_ui <- renderUI({
        switch(
            input$ref_area_type,
            States =
                selectInput(
                    "states",
                    "Select one or more states and/or territories:",
                    choices = states,
                    multiple = TRUE
                ),
            Counties =
                tagList(
                    selectInput(
                        "states",
                        "Select a state and/or territory:",
                        choices = states,
                        multiple = FALSE
                    ),
                    uiOutput("counties")
                )
        )
    })

    county_choices <- reactive({
        req(input$ref_area_type == "Counties")
        counties_tbl |>
        filter(year == dec_year(), state_fips == input$states) |>
        select(county_name, county_fips) |>
        arrange(county_name) |>
        deframe()
    })

    output$counties <- renderUI({
        selectInput(
            "counties",
            "Select one or more counties:",
            choices = county_choices(),
            multiple = TRUE
        )
    })

    output$dataset_spec_ui <- renderUI({
        req(year_validator$is_valid())
        radioButtons(
            "dataset_spec",
            "Select the desired data set:",
            choices =
                dataset_options_tbl[
                    dataset_options_tbl$year == input$year,
                    "dataset",
                    drop = TRUE
                ]
        )
    })

    adi <- reactive({
        req(year_validator$is_valid())
        nonshifted_results <-
            get_adi(
                input$geography,
                state = input$states,
                county = input$counties,
                year = input$year,
                dataset = input$dataset_spec,
                geometry = TRUE,
                keep_indicators = TRUE
            )
        if (any(input$states %in% c("02", "15", "72"))) {
            shift_geometry(nonshifted_results)
        } else {
            nonshifted_results
        }
    }) |>
        bindEvent(input$execute_get_adi)

    output$plot <- renderPlot({
        ggplot(adi()) +
            geom_sf(aes(fill = ADI)) +
            scale_fill_viridis_c(direction = -1, na.value = "red")
    })

    output$download_tbl <- downloadHandler(
        "area_deprivation_indices.csv",
        function(filepath) write_csv(adi(), filepath, na = "")
    )

    output$download_plot <- downloadHandler(
        "area_deprivation_plot.png",
        function(x) ggsave(x, dpi = "retina")
    )
}

# Run the app
shinyApp(ui, server)
