# Raw REDCap/Medrio data ----
com_file <- file.path(Sys.getenv("RDS_PATH"), config::get("combined_data_file"))
dat_raw <- qread(com_file)


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
    trt,
    rand_stage,
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
