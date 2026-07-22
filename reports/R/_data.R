# Raw REDCap/Medrio data ----
com_file <- file.path(Sys.getenv("RDS_PATH"), config::get("combined_data_file"))
dat_raw <- qs2::qd_read(com_file)


# Baseline data ----
dat_base <- get_baseline_data(dat_raw, TRUE)
dat_spt <- get_skin_prick_long(dat_raw)
dat_ecz <- get_eczema_data(dat_raw)
dat_fa <- get_food_allergy(dat_raw)
dat_fc <- get_ofc_long(dat_raw)

# For 4629-92 we do not consider their walnut allergy as valid
# therefore, this outcome is excluded here
dat_fa <- dat_fa |>
  filter(!(outfrafood == "walnut" & record_id == "4629-92"))

# Fix wrong coding of delt in stage 2
dat_base <- dat_base |>
  mutate(
    delt = replace_values(
      delt,
      "Forceps/ vacuum assisted delivery" ~ "Forceps/vacuum assisted delivery"
    )
  )


# Visit data -----
dat_vs <- select_form(dat_raw, "participant_assessment") |>
  select(record_id, visdat, windyn, windreas, windothspec, visit, visage) |>
  mutate(
    visdat = if_else(
      record_id == "5785-83" & visit == 2,
      as_date("2023-03-30"),
      visdat
    )
  )

dat_grid <- dat_base |>
  crossing(
    visage = factor(
      unique(dat_vs$visage),
      levels = c(
        "4-month",
        "6-month",
        "6-month + 72 hrs",
        "7-month",
        "12-month",
        "18-month",
        "19-month"
      )
    )
  ) |>
  arrange(
    str_rank(record_id, numeric = TRUE),
    str_rank(visage, numeric = TRUE)
  ) |>
  mutate(discage = interval(birthdat, discdat) %/% months(1))

dat_grid_vs <- dat_grid |>
  left_join(dat_vs, join_by(record_id, visage)) |>
  mutate(record_id = fct_inorder(record_id), visage = fct_inorder(visage)) |>
  mutate(
    age_weeks = interval(birthdat, visdat) %/% weeks(1),
    age_months = interval(birthdat, visdat) %/% months(1)
  ) |>
  mutate(
    visit_gap = interval(lag(visdat), visdat) %/% days(1),
    .by = record_id
  ) |>
  mutate(visit_attended = !is.na(visdat))

# Check for obvious inconsistencies with an aim to correct them
# dat_grid_vs |>
#   filter(visage != "6-month + 72 hrs") |>
#   mutate(
#     visit_gap = case_when(
#       visage == "4-month" ~ interval(visdat1, visdat) %/% days(1),
#       .default = interval(lag(visdat), visdat) %/% days(1)
#     ),
#     .by = subjid
#   ) |>
#   select(record_id, subjid, rand_stage, trt, visage, age_months, visdat, visit_gap, windyn, windreas) |>
#   mutate(visit_age_diff = age_months - as.numeric(gsub("-month", "", visage))) |>
#   filter(visit_age_diff != 0) |>
#   arrange(visit_age_diff) |>
#   print(n = Inf)
# # Wrong year entered
# dat_grid_vs |>
#   filter(record_id == "5785-139") |>
#   select(birthdat, visdat1, visage, visdat)
# dat_grid_vs |>
#   filter(record_id == "5785-186") |>
#   select(birthdat, visdat1, visage, visdat)
# dat_grid_vs |>
#   filter(record_id == "5785-354") |>
#   select(birthdat, visdat1, visage, visdat)
# dat_grid_vs |>
#   filter(record_id == "5785-368") |>
#   select(birthdat, visdat1, visage, visdat)
# dat_grid_vs |>
#   filter(record_id == "5785-10") |>
#   select(subjid, birthdat, visdat1, visage, visdat)
# dat_grid_vs |>
#   filter(record_id == "5785-86") |>
#   select(subjid, birthdat, visdat1, visage, visdat)

# Corrections to dates
dat_grid_vs <- dat_grid_vs |>
  mutate(
    visdat = replace_when(
      visdat,
      record_id == "5785-139" & visage == "18-month" ~ date("2024-01-17"),
      record_id == "5785-186" & visage == "12-month" ~ date("2024-01-08"),
      record_id == "5785-354" & visage == "18-month" ~ date("2025-01-31"),
      record_id == "5785-368" & visage == "18-month" ~ date("2025-01-31")
    )
  ) |>
  # Reacalculate
  mutate(
    age_weeks = interval(birthdat, visdat) %/% weeks(1),
    age_months = interval(birthdat, visdat) %/% months(1)
  ) |>
  mutate(
    visit_gap = interval(lag(visdat), visdat) %/% days(1),
    .by = record_id
  ) |>
  mutate(visit_attended = !is.na(visdat))

# Food and household questionnaire data -----
dat_fhq <- select_form(dat_raw, "food_and_household_questionnaire")

dat_grid <- dat_base |>
  crossing(
    visit_age = factor(
      unique(dat_fhq$visit_age),
      levels = c(
        "6-week",
        "4-month",
        "6-month",
        "7-month",
        "9-month",
        "12-month",
        "15-month",
        "18-month",
        "19-month"
      )
    )
  ) |>
  arrange(
    str_rank(record_id, numeric = TRUE),
    visit_age
  ) |>
  mutate(discage = interval(birthdat, discdat) %/% months(1)) |>
  select(-fecurr, -fedog, -fecat, -fedcyn)

dat_grid_fhq <- dat_grid |>
  left_join(dat_fhq, join_by(record_id, visit_age)) |>
  mutate(
    record_id = fct_inorder(record_id),
    visit_age = fct_inorder(visit_age)
  ) |>
  mutate(
    age_weeks = interval(birthdat, fedat) %/% weeks(1),
    age_months = interval(birthdat, fedat) %/% months(1)
  ) |>
  mutate(
    visit_gap = interval(lag(fedat), fedat) %/% days(1),
    .by = record_id
  ) |>
  mutate(fhq_completed = !is.na(fedat)) |>
  filter(
    !(rand_stage == 2 & visit_age %in% c("4-month", "7-month", "19-month"))
  ) |>
  select(
    record_id,
    birthdat,
    trt,
    rand_stage,
    rand_site,
    discdat,
    disc_age_mth,
    visit_age,
    visit_gap,
    age_weeks,
    age_months,
    all_of(names(dat_fhq)),
    fhq_completed
  )


# Skin prick test and eczema -----
dat_spt_pos <- summarise_spt_positive(dat_spt)
dat_spt_pos_any <- dat_spt_pos |>
  summarise(
    any_pos_spt = any(any_spt_pos_1mm & spt_age < 19),
    .by = record_id
  )

dat_spt_food_pos <- dat_spt |>
  filter(!(spt_tested %in% c("D.pteronyssinus", "cat dander", "perennial ryegrass"))) |>
  summarise_spt_positive()
dat_spt_food_pos_any <- dat_spt_food_pos |>
  summarise(
    any_pos_spt = any(any_spt_pos_1mm & spt_age < 19),
    .by = record_id
  )

# Dataset used for SPT outcome analyses
dat_spt_analysis <- dat_spt |>
  summarise(
    across(
      spt_0mm:spt_3mm,
      ~ if_else(
        all(!priyn) | all(spt_age > 18),
        NA,
        any(.x == 1 & spt_age < 19, na.rm = TRUE)
      )
    ),
    .by = c(record_id, trt)
  ) |>
  right_join(
    select(
      dat_base,
      record_id,
      rand_site,
      gender,
      bfed,
      fborn,
      parinc_imp,
      ces,
      fha
    ),
    join_by(record_id)
  )

dat_ecz_spt <- dat_ecz |>
  left_join(dat_spt_pos_any, join_by(record_id))

# Dataset used for Eczema outcome analyses
dat_ecz_analysis <- dat_ecz_spt |>
  filter(!out_ecz_preexisting) |>
  mutate(
    ecz_6m = ecz &
      !out_ecz_preexisting &
      outageval_months2 <= 6 &
      any_pos_spt,
    ecz_12m = ecz &
      !out_ecz_preexisting &
      outageval_months2 <= 12 &
      any_pos_spt,
    ecz_18m = ecz &
      !out_ecz_preexisting &
      outageval_months2 <= 18 &
      any_pos_spt
  ) |>
  select(record_id, trt, ecz_6m, ecz_12m, ecz_18m) |>
  left_join(
    dat_base |>
      select(record_id, rand_site, gender, bfed, fborn, parinc_imp, ces, fha),
    join_by(record_id)
  )


# More data and visit checks ----
dat_ofc_raw <- select_form(dat_raw, "food_challenge")
dat_out_raw <- select_form(dat_raw, "outcome_report")
dat_spt_raw <- select_form(dat_raw, "skin_prick_test")
dat_pa_raw <- select_form(dat_raw, "participant_assessment")
dat_pe_raw <- select_form(dat_raw, "physical_examination")

dat_vs_spt <- dat_grid_vs |>
  select(
    record_id,
    subjid,
    trt,
    birthdat,
    streas,
    stetrreas,
    ippvspec,
    discdat,
    disc_age_mth,
    visage,
    visdat,
    age_months,
    visit_attended,
    windyn,
    windreas,
    windothspec
  ) |>
  filter(visage == "12-month") |>
  left_join(
    dat_spt_raw |>
      filter(is.na(unvisyn)) |>
      filter(row_number() == 1, .by = record_id) |>
      select(record_id, priyn, prinspec, pridat),
    join_by(record_id)
  ) |>
  mutate(
    priage = time_length(interval(birthdat, pridat), "months")
  )

# dat_grid_vs |>
#   filter(rand_stage == 2) |>
#   select(
#     record_id,
#     subjid,
#     birthdat,
#     visdat1,
#     visage,
#     visdat,
#     visit_gap,
#     visit_attended,
#     windyn,
#     windreas,
#     windothspec
#   ) |>
#   filter(visage == "12-month") |>
#   mutate(
#     visage_obs = time_length(interval(birthdat, visdat), "months")
#   ) |>
#   left_join(dat_spt_raw) |>
#   mutate(priage_obs = time_length(interval(birthdat, pridat), "months")) |>
#   filter(!visit_attended, priyn)

# Missed visits
# dat_grid_vs |>
#   filter(visage %in% c("12-month", "18-month")) |>
#   select(subjid, trt, visage, visit_attended) |>
#   pivot_wider(names_from = visage, values_from = visit_attended) |>
#   count(trt, `12-month`, `18-month`)

# Early Terminations

# dat_base |>
#   select(subjid, disc_age_mth, streas, stetrreas, ippvspec) |>
#   filter(streas != "Completed protocol") |>
#   filter_out(grepl("deviation", stetrreas)) |>
#   print(n = Inf)

# How many infants had no SPT

# What was the earliest termination which had an outcome reported?
# dat_base |>
#   select(record_id, subjid, streas, stetrreas, birthdat, discdat, disc_age_mth) |>
#   left_join(
#     dat_out_raw |>
#       filter(outallyn) |>
#       select(record_id, outalltp, outrepdat, outawardat, outdiagdat, outfrasource)
#   ) |>
#   arrange(disc_age_mth) |>
#   mutate(across(outrepdat:outdiagdat, ~ time_length(interval(birthdat, .x), "months"), .names = "age_{.col}")) |>
#   print(n = Inf)
