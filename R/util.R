suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
})

read_stage1_randomisation_list <- function() {
  st1rands <- read_csv(
    file.path(Sys.getenv("RDS_PATH"), "data", "rand", "OPTIMUM_STAGE1_randomisation_list.csv"),
    show_col_types = FALSE
  )
  st1rands |>
    rename(
      rand = RANDNO,
      blockno = BLOCKNO,
      trt = TREATMENT
    ) |>
    mutate(
      rand = as.character(rand),
      blockno = as.character(blockno),
      rand_site = "PCH",
      rand_stage = 1
    )
}

read_stage2_randomisation_list <- function() {
  fn <- grep(
    "STAGE2",
    list.files(file.path(Sys.getenv("RDS_PATH"), "data", "rand"), full.names = TRUE),
    value = TRUE
  )
  read_csv(fn, show_col_types = FALSE) |>
    rename(
      rand = RANDNO,
      blockno = BLOCKNO,
      trt = TREATMENT
    ) |>
    mutate(
      rand = as.character(rand),
      rand_site = case_when(
        substr(rand, 1, 2) == "21" ~ "PCH",
        substr(rand, 1, 2) == "22" ~ "CHW",
        substr(rand, 1, 2) == "23" ~ "MCRI",
        substr(rand, 1, 2) == "24" ~ "SCHN",
        .default = NA_character_
      ),
      rand_stage = 2
    )
}

read_randomisation_lists <- function() {
  st1_rand <- read_stage1_randomisation_list()
  st2_rand <- read_stage2_randomisation_list()
  bind_rows(st1_rand, st2_rand)
}

select_form <- function(dat, frm) {
  filter(dat, form == frm) |>
    pull(data) |>
    pluck(1)
}
