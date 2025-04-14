library(REDCapTidieR)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(labelled)
library(qs)

readRenviron(".env")

st1_path <- file.path(Sys.getenv("RDS_PATH"), config::get("raw_data_stage1_path"))
st2_path <- file.path(
  Sys.getenv("RDS_PATH"),
  config::get("raw_data_stage2_path"),
  config::get("raw_data_stage2_file")
)
st2_data <- qread(st2_path)

combine_randomisation <- function(st2_data, st1_path) {
  st2_rand <- extract_tibble(st2_data, "randomisation") |>
    select(-redcap_event, -form_status_complete) |>
    rename(site = redcap_data_access_group)
  st1_rand <- read_delim(
    file.path(st1_path, "VAX V1.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(medrioid, subjectid, site, rand, randdattim) |>
    mutate(
      medrioid = as.character(medrioid),
      randdattim = as_datetime(randdattim, format = "%d-%b-%Y %H:%M")
    ) |>
    rename(
      record_id = medrioid,
      subjid = subjectid
    )
  rand <- bind_rows(st1_rand, st2_rand)
  var_label(rand) <- var_label(st2_rand)
  rand
}

combine_study_termination <- function(st2_data, st1_path) {
  st2_st <- extract_tibble(st2_data, "study_termination") |>
    select(-redcap_event, -form_status_complete, -redcap_data_access_group, -deemail)
  st1_st <- read_delim(
    file.path(stage1_path, "ST.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(medrioid, discdat, streas, stetrreas, ippvspec, wcipnw, wcipspec) |>
    mutate(
      medrioid = as.character(medrioid),
      discdat = as_date(discdat, format = "%d-%b-%Y"),
      wcipnw = wcipnw == "Yes"
    ) |>
    rename(record_id = medrioid, wcipnwreas = wcipspec)
  st <- bind_rows(st1_st, st2_st)
  var_label(st) <- var_label(st2_st)
  st
}

combine_demographics <- function(st2_data, st1_path) {
  st2_demo <- extract_tibble(st2_data, "demographics") |>
    select(-redcap_event, -form_status_complete, -redcap_data_access_group, -deemail)
  st1_demo <- read_delim(
    file.path(stage1_path, "DEMO.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(
      medrioid,
      ptinit,
      visdat1,
      brthdat,
      gender,
      cobmther,
      cobfther,
      etomthr,
      etofthr,
      etoinf,
      p1typ,
      edumoth,
      p2typ,
      edufath,
      income,
      gpinf
    ) |>
    mutate(
      medrioid = as.character(medrioid),
      visdat1 = as_date(visdat1, format = "%d-%b-%Y"),
      brthdat = as_date(brthdat, format = "%d-%b-%Y"),
      gpinf = gpinf == "Yes",
      income = gsub("t0", "to", income),
      # In REDCap, education is stored as "parent 1" and "parent 2"
      # rather than as "mother" and "father"
      # Recode to make consistent
      edupar1 = if_else(p1typ == "Mother", edumoth, edufath),
      edupar2 = case_when(
        p2typ == "Mother" & p1typ == "Father" ~ edumoth,
        # There are two cases where both parents are mothers
        p2typ == "Mother" & p1typ == "Mother" ~ edufath,
        .default = edufath
      )
    ) |>
    select(-edumoth, -edufath) |>
    rename(
      record_id = medrioid,
      birthdat = brthdat,
      relp1 = p1typ,
      relp2 = p2typ,
      bcmoth = cobmther,
      bcfath = cobfther,
      parinc = income,
      gpinfyn = gpinf
    )

  parinc_levels <- c(
    "< $18,000",
    "$18,000 to $37,000",
    "$37,001 to $87,000",
    "$87,001 to $180,000",
    ">$180,000"
  )
  edupar_levels <- c(
    "Primary school",
    "Secondary school",
    "TAFE or trade certificate (including diploma)",
    "Bachelor-level university degree",
    "Post-graduate university qualification"
  )
  demo <- bind_rows(st1_demo, st2_demo) |>
    mutate(
      parinc = factor(parinc, levels = parinc_levels),
      edupar1 = factor(edupar1, levels = edupar_levels),
      edupar2 = factor(edupar2, levels = edupar_levels)
    )
  var_label(demo) <- var_label(st2_demo)
  demo
}

combine_birth_history <- function(st2_data, st1_path) {
  st2_bh <- extract_tibble(st2_data, "birth_history") |>
    select(-redcap_event, -form_status_complete, -redcap_data_access_group) |>
    mutate(
      gestwp = as.numeric(gestwp),
      gestdp = as.numeric(gestdp),
      prevnum = as.numeric(prevnum),
      prevdat1 = as_date(prevdat1, format = "%Y-%m-%d")
    )
  st1_bh <- read_delim(
    file.path(stage1_path, "BH.txt")
  ) |>
    rename_with(tolower) |>
    select(-ends_with("_coded"))
  st1_bh1 <- st1_bh |>
    filter(row_number() == 1, .by = medrioid) |>
    select(
      medrioid, delt, mgrav, mpara, inmpertvp, gestwp, gestdp, vacad, prevpert, minfvp, gestwi, gestdi,
      gestadelw, gestadeld, ipab, neoab, aps1, aps5, wgt, len, inhc, sibna
    ) |>
    mutate(
      inmpertvp = inmpertvp == "Yes",
      prevpert = prevpert == "Yes",
      minfvp = minfvp == "Yes",
      ipab = ipab == "Yes",
      neoab = neoab == "Yes"
    )
  st1_bhpp <- st1_bh |>
    filter(!is.na(vargroup1row)) |>
    mutate(
      prevdat = as_date(prevdat, format = "%d-%b-%Y"),
      vargroup1row = if_else(subjectid == "01041", 1, vargroup1row)
    ) |>
    rename(prevnum = vargroup1row) |>
    select(medrioid, prevnum, prevdat, prevac) |>
    rename(prevdat1 = prevdat, prevac1 = prevac) |>
    mutate(prevac1 = factor(prevac1, levels = levels(st2_bh$prevac1)))
  st1_bhsibs <- st1_bh |>
    select(medrioid, sibna:sibage) |>
    group_by(medrioid) |>
    filter(any(sibna == "Yes")) |>
    mutate(sibnum = max(vargroup2row, na.rm = TRUE)) |>
    ungroup() |>
    filter(is.na(sibna), !is.na(vargroup2row)) |>
    select(-sibna, -sibord) |>
    pivot_wider(
      names_from = vargroup2row,
      values_from = sibage,
      names_prefix = "sibage"
    )
  st1_out <- left_join(st1_bh1, st1_bhpp, join_by(medrioid)) |>
    left_join(st1_bhsibs, join_by(medrioid)) |>
    mutate(
      sibnum = factor(case_when(
        sibna == "Not Applicable (No Siblings)" ~ "Not Applicable",
        sibna == "Unknown" ~ "Unknown",
        .default = as.character(sibnum)
      ), levels = levels(st2_bh$sibnum))
    )
  bh <- bind_rows(st2_bh, st1_out)
  var_label(bh) <- var_label(st2_bh)
  bh
}

combine_medical_history <- function() {
  st2_mh <- extract_tibble(st2_data, "medical_history") |>
    select(-redcap_event, -form_status_complete, -redcap_data_access_group, -mhplanspec) |>
    mutate(mhenddat1 = as_date(mhenddat1, format = "%Y-%m-%d"))
  st1_mh <- read_delim(
    file.path(st1_path, "MH.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(-c(
      ends_with("_coded"),
      visit, form, subjectvisitformid, formentrydate, subjectstatus, site, subjectid, mhstdat_p, mhenddat_p
    )) |>
    mutate(medrioid = as.character(medrioid))
  st1_mh_1 <- st1_mh |>
    filter(is.na(vargroup1row)) |>
    select(medrioid, mhyn, mhplan) |>
    mutate(mhyn = mhyn == "Yes", mhplan = mhplan == "Yes")
  st1_mh_2 <- st1_mh |>
    filter(!is.na(vargroup1row)) |>
    select(-mhyn, -mhplan) |>
    mutate(
      mhstdat = as_date(mhstdat, format = "%d-%b-%Y"),
      mhenddat = as_date(mhenddat, format = "%d-%b-%Y")
    ) |>
    mutate(mhnum = max(vargroup1row), .by = medrioid) |>
    pivot_wider(
      names_from = vargroup1row,
      values_from = c(mhdistp, mhdiag, mhstat, mhstdat, mhenddat),
      names_sep = "",
      names_vary = "slowest"
    )
  st1_out <- left_join(st1_mh_1, st1_mh_2, join_by(medrioid)) |>
    mutate(mhnum = if_else(is.na(mhnum), 0, mhnum)) |>
    rename(record_id = medrioid)
  st <- bind_rows(st2_mh, st1_out)
  var_label(st) <- var_label(st2_mh)
  st
}

combine_physical_exam_v1 <- function(st2_data, st1_path) {
  st2_pe <- extract_tibble(st2_data, "physical_examination_v1") |>
    select(-redcap_event, -form_status_complete, -redcap_data_access_group)
  st1_pe <- read_delim(
    file.path(st1_path, "PE.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower)
  st1_pe1 <- st1_pe |>
    filter(is.na(vargroup1row)) |>
    select(medrioid, tempv1, inwv1, inlv1, inhcv1)
  st1_pe2 <- st1_pe |>
    filter(!is.na(vargroup1row)) |>
    select(medrioid, vargroup1row, peres, pespec) |>
    pivot_wider(names_from = vargroup1row, values_from = c(peres, pespec), names_sep = "")
  st1_out <- left_join(st1_pe1, st1_pe2, join_by(medrioid)) |>
    rename(record_id = medrioid) |>
    mutate(record_id = as.character(record_id))
  st <- bind_rows(st2_pe, st1_out)
  var_label(st) <- var_label(st2_pe)
  st
}

combine_food_household <- function(st2_data, st1_path) {
  st2_food <- st2_data |>
    extract_tibble("food_and_household_questionnaire")
  st2_food_v1 <- st2_food |>
    filter(redcap_event == "visit_1") |>
    select(-redcap_event, -redcap_data_access_group) |>
    mutate(fedat = as_date(fedat)) |>
    select(record_id, fecurr, febfever, febfform, fethickyn, fesolyn, fefadiag, fesupp, ferash, fecat, fedog, fedcyn)
  st1_food_v1 <- read_delim(
    file.path(st1_path, "FOOD V1.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(-ends_with("_coded"))
  st1_food_v1_1 <- st1_food_v1 |>
    filter(is.na(vargroup1row) & is.na(vargroup2row) & is.na(vargroup3row)) |>
    select(medrioid, fecurr, febfever, febfform, fethickyn, fesolyn, fefadiag, fesupp, ferash, fecat, fedog, fedcyn) |>
    mutate(
      record_id = as.character(medrioid),
      febfever = febfever == "Yes",
      febfform = febfform == "Yes",
      fethickyn = fethickyn == "Yes",
      fesupp = fesupp == "Yes",
      ferash = ferash == "Yes",
      fedcyn = fedcyn == "Yes"
    ) |>
    select(-medrioid)
  food_v1 <- bind_rows(st2_food_v1, st1_food_v1_1)
  var_label(food_v1) <- var_label(st2_food_v1)
  food_v1
}

dat_rand <- combine_randomisation(st2_data, st1_path)
dat_st <- combine_study_termination(st2_data, st1_path)
dat_demo <- combine_demographics(st2_data, st1_path)
dat_bh <- combine_birth_history(st2_data, st1_path)
dat_mh <- combine_medical_history()
dat_pe <- combine_physical_exam_v1(st2_data, st1_path)
dat_food <- combine_food_household(st2_data, st1_path)

optimum_data <- tibble(
  form = c(
    "randomisation",
    "study_termination",
    "demographics",
    "birth_history"
  ),
  data = list(
    dat_rand,
    dat_st,
    dat_demo,
    dat_bh
  )
)
