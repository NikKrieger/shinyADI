
contains_ak_hi_pr <- function(ref_area_type, state, geoid, zcta) {
  switch(
    ref_area_type,
    multiple_states_ref_area = ,
    counties_ref_area =
      is.null(state) || any(match(state, c("02", "15", "72"), 0)),
    geoids_ref_area =
      any(match(str_sub(geoid, 1, 2), c("02", "15", "72"), 0)),
    zctas_ref_area =
      is.null(zcta) ||
      any(
        match(
          str_sub(zcta, 1, 3),
          c("9", "96", "99", "967", "968", "995", "996", "997", "998", "999"),
          0
        )
      )
  )
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
