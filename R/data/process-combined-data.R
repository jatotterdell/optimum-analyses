# Functions used to derive specific data sets from the raw combined data
source(file.path("R", "util.R"))

get_baseline_data <- function(dat, unblind = FALSE) {
  if (unblind) {
    rnd <- left_join(
      select_form(dat, "randomisation"),
      select_form(dat, "allocations"),
      join_by(rand)
    )
  } else {
    rnd <- select_form(dat, "randomisation")
  }
  rnd |>
    left_join(
      select_form(dat, "demographics"),
      join_by(record_id)
    ) |>
    left_join(
      select_form(dat, "birth_history"),
      join_by(record_id)
    )
}

get_skin_prick_data <- function(dat) {}
