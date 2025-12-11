library(readr)
library(tigris)
library(tibble)
library(stringr)
library(dplyr)
library(sociome)
library(ggplot2)
library(ggiraph)
library(reactable)
library(shinyvalidate)
library(shiny)
library(bslib)
source("helpers.R")
options(tigris_use_cache = TRUE)
states <- readRDS("data/states.rds")
counties_tbl <- readRDS("data/counties_tbl.rds")

current_year <- as.POSIXlt(Sys.Date())$year + 1900

dataset_options_tbl <-
    bind_rows(
        tibble(dataset = "acs5", year = 2009:current_year),
        tibble(dataset = "acs1", year = setdiff(2007:current_year, 2020)),
        tibble(dataset = "acs3", year = 2007:2013),
        tibble(dataset = "decennial", year = c(2000, 2010, 2020))
    )


# User interface ----
ui <- page_sidebar(
    title = "sociome",
    sidebar = sidebar(
        numericInput("year", "Year:", value = 2020),
        selectInput(
            "geography",
            "I want an ADI value for each",
            choices =
                c("state", "county", "tract", "block group", ZCTA = "zcta")
        ),
        uiOutput("ref_area_ui"),
        uiOutput("dataset_spec_ui"),
        input_task_button("execute_get_adi", "Calculate ADI"),
        uiOutput("measure_selection_ui"),
        uiOutput("download_buttons")
    ),
    navset_tab(
        nav_panel(title = "plot", girafeOutput("plot")),
        nav_panel(title = "data", reactableOutput("tbl"))
    )
)

# Server logic
server <- function(input, output, session) {

    # Year
    year_validator <- InputValidator$new()
    year_validator$add_rule("year", sv_required())
    year_validator$add_rule(
        "year",
        sv_in_set(c(2000, 2007:current_year), set_limit = Inf)
    )
    year_validator$enable()
    dec_year <- reactive({
        req(year_validator$is_valid())
        trunc(input$year / 10) * 10
    })

   # Geography
    output$ref_area_type_ui <- renderUI({
        switch(
            input$geography,
            state =
                selectInput(
                    "ref_area_type",
                    label = NULL,
                    choices = c("in this list:" = "multiple_states_ref_area",
                                "from these GEOIDs:" = "geoids_ref_area")
                ),
            county = ,
            tract = ,
            "block group" =
                selectInput(
                    "ref_area_type",
                    label = NULL,
                    choices =
                        c("in these states:" = "multiple_states_ref_area",
                          "in a list of counties in one state:" = "counties_ref_area",
                          "from these GEOIDs:" = "geoids_ref_area")
                )
        )
    })
    ref_area_type <- reactive({
        switch(
            input$geography,
            zcta = "zctas_ref_area",
            input$ref_area_type
        )
    })


    ############################################################################
    # Reference area

    output$ref_area_ui <- renderUI({
        tagList(
            switch(input$geography, zcta = NULL, uiOutput("ref_area_type_ui")),
            uiOutput("ref_area_spec_ui")
        )
    })


    output$ref_area_spec_ui <- renderUI({
        switch(
            ref_area_type(),
            multiple_states_ref_area =
                tagList(
                    selectInput(
                        "state",
                        label = NULL,
                        choices = states,
                        multiple = TRUE
                    ),
                    actionButton("select_all_states", "Select all states"),
                    actionButton("clear_all_states", "Clear all states")
                ),
            counties_ref_area =
                tagList(
                    selectInput(
                        "state",
                        label = "State:",
                        choices = states,
                        multiple = FALSE
                    ),
                    uiOutput("county_ui")
                ),
            geoids_ref_area =
                textAreaInput(
                    "geoid",
                    label = NULL,
                    placeholder = "Enter any combination of 2-, 5-, 11-, and 12-digit GEOIDs separated by , ; ' \" | or whitespace."
                ),
            zctas_ref_area =
                textAreaInput(
                    "zcta",
                    label = "in this list:",
                    placeholder = "Enter 5-digit ZCTAs separated by , ; ' \" | or whitespace. \nElements under 5 digits will match all ZCTAs beginnig with those digits."
                )
        )
    }) |>
        bindEvent(ref_area_type())

    # Multiple states: select all button
    observe({
        updateSelectizeInput(
            session,
            "state",
            selected = union(input$state, states)
        )
    }) |>
        bindEvent(input$select_all_states)
    # Multiple states: clear all button
    observe({
        updateSelectizeInput(session, "state", selected = character())
    }) |>
        bindEvent(input$clear_all_states)

    # County_choices
    county_choices <- reactive({
        req(ref_area_type() == "counties_ref_area")
        counties_tbl |>
            filter(year == dec_year(), state_fips == input$state) |>
            select(county_name, county_fips) |>
            arrange(county_name) |>
            deframe()
    })

    output$county_ui <- renderUI({
        tagList(
            selectInput(
                "county",
                label = "Counties:",
                choices = county_choices(),
                multiple = TRUE
            ),
            actionButton("select_all_counties", "Select all counties"),
            actionButton("clear_all_counties", "Clear all counties")
        )
    })

    # County select all button
    observe({
        updateSelectizeInput(
            session,
            "county",
            selected = union(input$county, county_choices())
        )
    }) |>
        bindEvent(input$select_all_counties)
    # County clear all button
    observe({
        updateSelectizeInput(session, "county", selected = character())
    }) |>
        bindEvent(input$clear_all_counties)


    ############################################################################
    ############################################################################
    # GEOID free text
    geoid_validator <- InputValidator$new()
    geoid_validator$condition(~identical(ref_area_type(), "geoids_ref_area"))
    geoid_validator$add_rule("geoid", sv_required("Cannot be blank."))
    geoid_validator$add_rule(
        "geoid",
        sv_regex(
            pattern = "^[\\s,;'\"|]*(\\d{2}|\\d{5}|\\d{11,12})([\\s,;'\"|]+(\\d{2}|\\d{5}|\\d{11,12}))*[\\s,'\";|]*$",
            message = "GEOIDs must each be 2, 5, 11, or 12 digits, separated by , ; ' \" | or whitespace.",
            perl = TRUE
        )
    )
    geoid_validator$enable()
    geoid_vector <- reactive({
        req(geoid_validator$is_valid())
        as.character(str_extract_all(input$geoid, "\\d+", simplify = TRUE))
    })


    ############################################################################
    ############################################################################
    # ZCTA free text
    zcta_validator <- InputValidator$new()
    zcta_validator$condition(
        ~identical(ref_area_type(), "zctas_ref_area") && nchar(input$zcta)
    )
    zcta_validator$add_rule("zcta", sv_required(message = NULL))
    zcta_validator$add_rule(
        "zcta",
        sv_regex(
            pattern = "^[\\s,;'\"|]*(\\d{1,5})([\\s,;'\"|]+(\\d{1,5}))*[\\s,;'\"|]*$",
            message = "Element must be between 1 and 5 digits, separated by , ; ' \" | or whitespace.",
            perl = TRUE
        )
    )
    zcta_validator$enable()
    zcta_vector <- reactive({
        req(zcta_validator$is_valid())
        as.character(str_extract_all(input$zcta, "\\d+", simplify = TRUE))
    })


    ############################################################################
    # data set
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



    ############################################################################
    # Calculate ADI
    adi_sf_tbl <- reactive({
        req(year_validator$is_valid())

        switch(
            ref_area_type(),
            multiple_states_ref_area = {
                state <- input$state
                county <- geoid <- zcta <- NULL
            },
            counties_ref_area = {
                state <- input$state
                county <- input$county
                geoid <- zcta <- NULL
            },
            geoids_ref_area = {
                req(geoid_validator$is_valid())
                geoid <- geoid_vector()
                state <- county <- zcta <- NULL
            },
            zctas_ref_area = {
                req(zcta_validator$is_valid())
                zcta <- zcta_vector()
                state <- county <- geoid <- NULL
            }
        )

        adi_results <-
            get_adi(
                input$geography,
                state = state,
                county = county,
                geoid = geoid,
                zcta = zcta,
                year = input$year,
                dataset = input$dataset_spec,
                geometry = TRUE,
                keep_indicators = TRUE
            )

        if (contains_ak_hi_pr(ref_area_type(), state, geoid, zcta)) {
            shift_geometry(adi_results)
        } else {
            adi_results
        }
    }) |>
        bindEvent(input$execute_get_adi)

    ############################################################################
    # Render plot
    output$measure_selection_ui <- renderUI({
        selectInput(
            "measure",
            "Plotted measure:",
            choices =
                c("ADI",
                  "Financial Strength" = "Financial_Strength",
                  "Economic Hardship and Inequality" =
                      "Economic_Hardship_and_Inequality",
                  "Educational Attainment" = "Educational_Attainment"
                )
        )
    }) |>
        bindEvent(adi_sf_tbl())

    ggobj <- reactive({
        measure_sym <- sym(input$measure)

        legend_label <-
            c(ADI = "ADI",
              Financial_Strength = "Financial\nStrength",
              Economic_Hardship_and_Inequality =
                  "Economic\nHardship\nand\nInequality",
              Educational_Attainment = "Educational\nAttainment"
            )[input$measure] |>
            unname()

        ggplot(
            mutate(
                adi_sf_tbl(),
                tooltip = paste0(NAME, ": ", round(!!measure_sym, 2))
            ),
            aes(fill = !!measure_sym)
        ) +
            geom_sf_interactive(aes(tooltip = tooltip, data_id = NAME)) +
            scale_fill_viridis_c(direction = -1, na.value = "red", name = legend_label)
    }) |>
        bindEvent(input$measure, adi_sf_tbl(), ignoreInit = TRUE)

    output$plot <- renderGirafe({
        girafe(
            ggobj = ggobj(),
            options = list(
                opts_selection(css = ""),
                opts_hover(css = "fill:cyan;")
            )
        )
    })

    ############################################################################
    # Render table
    output$tbl <- renderReactable({
        adi_sf_tbl() |>
            as_tibble() |>
            select(
                GEOID, NAME, ADI,
                Financial_Strength,
                Economic_Hardship_and_Inequality,
                Educational_Attainment
            ) |>
            reactable()
    })

    ############################################################################
    # Downloading
    output$download_buttons <- renderUI({
        tagList(
            downloadButton("download_plot", "Download plot"),
            downloadButton("download_tbl", "Download data")
        )
    }) |>
        bindEvent(adi_sf_tbl())

    output$download_tbl <- downloadHandler(
        "area_deprivation_indices.csv",
        function(filepath)
            adi_sf_tbl() |>
            as_tibble() |>
            select(!geometry) |>
            write_csv(filepath, na = "")
    )

    output$download_plot <- downloadHandler(
        "area_deprivation_plot.png",
        function(x) ggsave(x, dpi = "retina")
    )
}

# Run the app
shinyApp(ui, server)
