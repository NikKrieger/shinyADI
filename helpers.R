


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
#     select(year, state_fips, state_name, county_fips, county_name) |>
#     arrange(desc(year), state_name, county_name)
# states <- deframe(distinct(counties_tbl, state_name, state_fips))
# saveRDS(counties_tbl, here::here("data", "counties_tbl.rds"))
# saveRDS(states, here::here("data", "states.rds"))
