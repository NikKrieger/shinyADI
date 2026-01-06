


#' Shifts Geometry of [get_adi()] Results According to Its States
#'
#' Looks at which states are contained in the [get_adi()] results and shifts its
#' geometry accordingly.
#'
#' When more than one state is on the map and any of them are Alaska, Hawaii, or
#' Puerto Rico, then [tigris::shift_geometry()] is applied.
#'
#' When only Alaska is in the results, `st_transform(crs = "EPSG:3338")` is
#' applied.
#'
#' Otherwise, adi_results is returned unchanged.
#'
#' @param adi_results An [sf] object from [get_adi()].
#' @param geography A string indicating the level of geography that was
#'   requested.
#'
#' @returns An [sf] object.
shift_geo2 <- function(adi_results, geography) {
  states <-
    switch(
      geography,
      zcta =
        c("006" = "72", "007" = "72", "009" = "72",
          "967" = "15", "968" = "15",
          "995" = "02", "996" = "02", "997" = "02", "998" = "02", "999" = "02")[
            substr(adi_results[["GEOID"]], 1L, 3L)
          ] |>
        unname() |>
        unique(),
      unique(substr(adi_results[["GEOID"]], 1L, 2L))
    )

  if (identical(states, "02")) {
    st_transform(adi_results, "EPSG:3338")
  } else if (
    length(states) > 1L && any(match(states, c("02", "15", "72"), nomatch = 0))
  ) {
    shift_geometry(adi_results, position = "outside")
  } else {
    adi_results
  }
}


# This code was used to obtain the states and counties_tbl objects
#
# library(tidycensus)
# library(tigris)
# library(tidyverse)
# # Try to grab tigris::counties() for every year 2000-2025
# counties_results <-
#   lapply(
#     set_names(2000:2025),
#     \(yr) {
#       tryCatch(
#         tigris::counties(cb = TRUE, year = yr) |>
#           as_tibble() |>
#           reframe(
#             year = yr,
#             state_fips = STATEFP,
#             county_fips = COUNTYFP,
#             county_name = NAME
#           ) |>
#           filter(state_fips %in% sociome::state_geoids),
#         error = identity
#       )
#     }
#   )
#
# counties_tbl <-
#   counties_results |>
#   keep(is_tibble) |>
#   map_at(
#     c("2010", "2013"),
#     mutate,
#     # These two years need to be re-encoded from latin1 to UTF-8
#     county_name = iconv(county_name, "latin1", "UTF-8")
#   ) |>
#   list_rbind() |>
#   arrange(state_fips, year, county_fips) |>
#   # Make a unique row for each state/year combination. Each row has a nested
#   # data frame containing that state's counties in that year.
#   nest(counties = c(county_fips, county_name)) |>
#   # Remove the state+year combo if its county list if it is the same as the
#   # prior year's county list.
#   filter(
#     row_number() == 1 |
#       state_fips != lag(state_fips) |
#       !map2_lgl(counties, lag(counties), identical)
#   ) |>
#   unnest(counties) |>
#   arrange(state_fips, year, county_fips)
#
# saveRDS(counties_tbl, here::here("data", "counties_tbl.rds"))
