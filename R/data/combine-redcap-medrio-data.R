library(REDCapTidieR)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
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

combine_randomisation <- function() {
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
  rand <- bind_rows(st1_rand, st2_rand) |>
    mutate(rand = as.character(rand))
  var_label(rand) <- var_label(st2_rand)
  rand
}

combine_study_termination <- function() {
  st2_st <- extract_tibble(st2_data, "study_termination") |>
    select(-redcap_event, -form_status_complete, -redcap_data_access_group)
  st1_st <- read_delim(
    file.path(st1_path, "ST.txt"),
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

combine_demographics <- function() {
  st2_demo <- extract_tibble(st2_data, "demographics") |>
    select(-redcap_event, -form_status_complete, -redcap_data_access_group, -deemail)
  st1_demo <- read_delim(
    file.path(st1_path, "DEMO.txt"),
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
  demo <- bind_rows(st2_demo, st1_demo) |>
    mutate(
      parinc = factor(parinc, levels = parinc_levels),
      edupar1 = factor(edupar1, levels = edupar_levels),
      edupar2 = factor(edupar2, levels = edupar_levels)
    )
  var_label(demo) <- var_label(st2_demo)
  demo
}

combine_birth_history <- function() {
  st2_bh <- extract_tibble(st2_data, "birth_history") |>
    select(-redcap_event, -form_status_complete, -redcap_data_access_group) |>
    mutate(
      gestwp = as.numeric(gestwp),
      gestdp = as.numeric(gestdp),
      prevnum = as.numeric(prevnum),
      prevdat1 = as_date(prevdat1, format = "%Y-%m-%d")
    )
  st1_bh <- read_delim(
    file.path(st1_path, "BH.txt")
  ) |>
    rename_with(tolower) |>
    select(-ends_with("_coded"))
  st1_bh1 <- st1_bh |>
    filter(row_number() == 1, .by = medrioid) |>
    select(
      medrioid, delt, mgrav, mpara, inmpertvp, gestwp, gestdp, vacad, prevpert, minfvp, gestwi, gestdi,
      gestadelw, gestadeld, ipab, neoab, aps1, aps5, wgt, len, inhc, sibna
    ) |>
    rename(mpar = mpara) |>
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
      ), levels = levels(st2_bh$sibnum)),
      record_id = as.character(medrioid)
    ) |>
    select(-medrioid, -sibna)
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

combine_medications <- function() {
  st2_meds <- extract_tibble(st2_data, "medications") |>
    select(record_id, cmyn, vacyn)
  st1_meds <- read_delim(
    file.path(st1_path, "MEDS.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(medrioid, cmedyn, vacyn) |>
    mutate(record_id = as.character(medrioid), cmyn = cmedyn == "Yes", vacyn = vacyn == "Yes") |>
    select(-medrioid, -cmedyn)
  meds <- bind_rows(st2_meds, st1_meds)
  var_label(meds) <- var_label(st2_meds)
  meds
}

combine_family_history_atopy <- function() {
  st2_fha <- extract_tibble(st2_data, "family_history_of_atopy") |>
    select(-redcap_event, -redcap_data_access_group, -form_status_complete)
  st1_fha <- read_delim(
    file.path(st1_path, "FHA.txt"),
    show_col_types = FALSE,
    na = c("", "NA", "N/A")
  ) |>
    rename_with(tolower) |>
    select(-c(ends_with("_coded"), subjectid, site, subjectstatus, visit, form, formentrydate))
  st1_fha_1 <- st1_fha |>
    filter(is.na(vargroup1row)) |>
    select(medrioid:fhasib4fa) |>
    rename(
      fhasibast1 = fhasib1ast,
      fhasibast2 = fhasib2ast,
      fhasibast3 = fhasib3ast,
      fhasibast4 = fhasib4ast,
      fhasibecz1 = fhasib1ecz,
      fhasibecz2 = fhasib2ecz,
      fhasibecz3 = fhasib3ecz,
      fhasibecz4 = fhasib4ecz,
      fhasibar1 = fhasib1ar,
      fhasibar2 = fhasib2ar,
      fhasibar3 = fhasib3ar,
      fhasibar4 = fhasib4ar,
      fhasibfa1 = fhasib1fa,
      fhasibfa2 = fhasib2fa,
      fhasibfa3 = fhasib3fa,
      fhasibfa4 = fhasib4fa,
    )
  fha <- bind_rows(st2_fha, st1_fha_1)
  var_label(fha) <- var_label(st2_fha)
  fha
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

combine_vax_admin_v1 <- function() {
  st2_vaxv1 <- extract_tibble(st2_data, "vaccine_administration_v1") |>
    select(-redcap_event, -form_status_complete, -redcap_data_access_group, -sdadmnam) |>
    mutate(sdadmdattim = as_datetime(paste(sdadmdat, sdadmtim), format = "%Y-%m-%d %H:%M:%S")) |>
    select(-c(sdadmdat, sdadmtim)) |>
    relocate(sdadmdattim, .after = record_id) |>
    mutate(
      vacblloc = "Right Thigh IM injection",
      vacpnloc = "Left Thigh IM injection",
      vacrort = if_else(vacrort___1, "Oral", NA)
    ) |>
    select(-c(vacblloc___1, vacblloc___99, vaciplocspec, vacpnloc___1, vacrort___1))
  st1_vaxv1 <- read_delim(
    file.path(st1_path, "VAX V1.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(-c(
      ends_with("_coded"),
      subjectid, subjectstatus, site, visit, form, formentrydate, rand, subjectvisitformid
    )) |>
    mutate(
      record_id = as.character(medrioid),
      sdadmdattim = as_datetime(randdattim, format = "%d-%b-%Y %H:%M"),
      vacpara = vacpara == "Yes",
      vacblpentbat = as.character(vacblpentbat),
      vacprotyn = vacprotyn == "Yes",
      vacobyn = vacobyn == "Yes",
      vacobae = vacobae == "Yes"
    ) |>
    select(-medrioid, -randdattim, -vacrottp) |>
    rename(vacadyn = vacprotyn, vacadreas = vacprotreas)
  vaxv1 <- bind_rows(st2_vaxv1, st1_vaxv1)
  v_labs <- var_label(st2_vaxv1)
  v_labs$sdadmdattim <- "Date of vaccine administration"
  v_labs$vacblloc <- "Location of blinded study vaccine"
  v_labs$vacpnloc <- "Location of Prevenar 13 (pneumococcal)"
  v_labs$vacrort <- "Route of administration Rotarix"
  var_label(vaxv1) <- v_labs
  vaxv1
}

combine_participant_assessment <- function() {
  st2_pa <- extract_tibble(st2_data, "participant_assessment") |>
    mutate(
      visit = gsub("visit_", "", redcap_event)
    ) |>
    select(-c(redcap_event, redcap_data_access_group, form_status_complete)) |>
    mutate(
      visdat = as_date(visdat, format = "%Y-%m-%d"),
      visage = case_when(
        visit == 2 ~ "12-month",
        visit == 3 ~ "18-month",
        visit == 4 ~ "19-month"
      )
    )
  st1_pa <- read_delim(
    file.path(st1_path, "V2-5.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(
      -ends_with("_coded"),
      -subjectid, -site, -subjectstatus, -form, -subjectvisitformid, -mthpostv5, -formentrydate
    ) |>
    mutate(
      record_id = as.character(medrioid),
      visit = gsub("Visit ", "", visit),
      visdat = as_date(visdat, format = "%d-%b-%Y"),
      visage = case_when(
        visit == 2 ~ "4-month",
        visit == 3 ~ "6-month",
        visit == 4 ~ "6-month + 72 hrs",
        visit == 5 ~ "7-month",
        visit == 6 ~ "12-month",
        visit == 7 ~ "18-month",
        visit == 8 ~ "19-month"
      ),
      windyn = windyn == "Yes",
      diaretyn = diaretyn == "Yes",
      v4yn = v4yn == "Yes",
      v6yn = v6yn == "Yes",
      v6conyn = v6conyn == "Yes"
    ) |>
    select(-medrioid)
  pa <- bind_rows(st2_pa, st1_pa)
  v_labs <- var_label(st2_pa)
  v_labs$visage <- "Scheduled visit age"
  v_labs$v4yn <- "Visit 4 not applicable"
  v_labs$v6yn <- "Visit 6 not attended"
  v_labs$diaretyn <- "Has diary card been returned"
  v_labs$diarespec <- "Reason diary card not returned"
  var_label(pa) <- v_labs
  pa
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

combine_outcome_report <- function() {
  st2_out <- extract_tibble(st2_data, "outcome_report") |>
    select(-c(redcap_event, redcap_data_access_group, form_status_complete)) |>
    rename(record_id_num = redcap_form_instance) |>
    select(-outageval, -outageunit)
  st1_out <- read_delim(
    file.path(st1_path, "outcomes.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(
      -ends_with("_coded"),
      -subjectvisitformid, -subjectid, -site, -subjectstatus, -visit, -form, -formentrydate
    ) |>
    mutate(
      record_id = as.character(medrioid),
      outallyn = outallyn == "No",
      outfraeldose = as.character(outfraeldose),
      outfrarashyn = outfrarashyn == "Yes",
      outfrasoth1yn = outfrasoth1yn == "Yes",
      outeczmedyn = outeczmedyn == "Yes",
      outeczscoryn = outeczscoryn == "Yes",
      across(outrepdat:outbirthdat, ~ as_date(.x, format = "%d-%b-%Y"))
    ) |>
    mutate(record_id_num = row_number(), .by = record_id) |>
    select(-medrioid, -outbirthdat, -ouagewk, -outagemnth, -outageyrs)
  out <- bind_rows(st2_out, st1_out)
  var_label(out) <- var_label(st2_out)
  out
}

combine_skin_prick_test <- function() {
  # There are two sources of skin prick tests:
  #    - the scheduled visit at 12-months of age (visit 2 in REDCap, visit 6 in Medrio)
  #    - at any unscheduled visit
  # Each child should have a "scheduled" SPT, and may have none or multiple "unscheduled".
  # When joining the two datasets, I will just refer to these as SPT's as
  # "spt_occasion" with values of "scheduled" or "unscheduled"
  #
  # Note that there are some difference between Medrio and REDCap here.
  # In REDCap, "prireact[1-11]" may be "Yes", "No", or "Not done"
  # If it is "No", then "prires[1-11]" is "NA", but presumably would be
  # considered "0" (no reaction)
  # In Medrio, "prind" only indicates whether the test was not done
  # confusingly, via a negative. So "No" means the test was done
  # and "Yes" means the test was not done.
  # If a test was done and "prires" is 0, then presumably this is equivalent
  # to a "prireact" of "No" in REDCap.
  # However, there are a couple of cases in REDCap where "prireact" is "Yes"
  # but "prires" is 0.
  # So, I will likely ignore "prireact" other than w.r.t "Not done" and just
  # focus on the actual reported diameter in "prires", filling this to be 0
  # if "prireact" was "No".
  #
  # Additional tested allergens may be added in REDCap, somewhat confusingly:
  #  priallspec, prireact9, prires9 - relate to the first optional allergen
  #  priallspec_2, prireact10, prires10 - relate to the second optional allergen
  #  etc.
  #  priallspec_5, prireact13, prires13 - relate to the 5th optional allergen
  # I will rename these to avoid some confusion, e.g. priallspec9, priallspec10, etc.
  #
  # Extra allergens were rare in Medrio, only two subjects, each with one additiona allergen
  # will map these to prireact9, prires9, and priallspec9
  st2_spt <- extract_tibble(st2_data, "skin_prick_test") |>
    filter((unvisyn & unvisreas___1) | is.na(unvisyn)) |>
    mutate(spt_occasion = if_else(redcap_event == "visit_2", "scheduled", "unscheduled")) |>
    select(-redcap_event, -redcap_data_access_group, -redcap_form_instance, -form_status_complete) |>
    mutate(
      prinegres = as.numeric(prinegres),
      prireact8 = if_else(prireact8 == "N/A", "Not Done", prireact8)
    ) |>
    select(-c(unvisyn, unvisdat, unvisreas___1, unvisreas___2, unvisreas___99, unvisreasoth)) |>
    rename(
      priallspec9 = priallspec,
      priallspec10 = priallspec_2,
      priallspec11 = priallspec_3,
      priallspec12 = priallspec_4,
      priallspec13 = priallspec_5
    )
  # Per above, fill in "0" prires if prireact is "No"
  st2_spt_fill <- st2_spt |>
    mutate(
      across(
        starts_with("prires"),
        ~ if_else(get(str_replace(cur_column(), "prires", "prireact")) == "No", 0, .)
      )
    )

  var_label(st2_spt)$prinegres <- "Negative control mean wheal diameter (mm)"
  var_label(st2_spt)$prireact8 <- "Sesame reaction"

  st1_spt <- read_delim(
    file.path(st1_path, "PRICK.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(
      -ends_with("_coded"),
      -subjectvisitformid, -subjectid, -site, -subjectstatus, -formentrydate, -visit, -form
    ) |>
    mutate(spt_occasion = "scheduled")

  st1_un <- read_delim(
    file.path(st1_path, "UN_VISIT.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(
      -ends_with("_coded"),
      -subjectvisitformid, -subjectid, -site, -subjectstatus, -formentrydate
    ) |>
    filter(any(unvisyn == "Yes" & unvisreas == "Skin Prick Test"), .by = medrioid) |>
    mutate(spt_occasion = "unscheduled") |>
    select(
      medrioid, spt_occasion, priyn_un, prispec_un, pridat_un,
      vargroup1row, priall_un, prind_un, priallspec_un, prires_un
    ) |>
    rename(
      priyn = priyn_un,
      prispec = prispec_un,
      pridat = pridat_un,
      priall = priall_un,
      prind = prind_un,
      priallspec = priallspec_un,
      prires = prires_un
    )

  st1_spt_all <- bind_rows(st1_spt, st1_un) |>
    mutate(spt_num = cumsum(is.na(vargroup1row)), .by = medrioid)

  st1_spt_1 <- st1_spt_all |>
    filter(is.na(vargroup1row)) |>
    select(medrioid, spt_occasion, spt_num, priyn, prispec, pridat) |>
    mutate(
      priyn = priyn == "Yes",
      pridat = as_date(pridat, format = "%d-%b-%Y")
    ) |>
    rename(prinspec = prispec)

  st1_spt_2 <- st1_spt_all |>
    filter(!is.na(vargroup1row)) |>
    select(medrioid, spt_occasion, spt_num, vargroup1row, priall, prind, priallspec, prires)
  st1_spt_ctr <- st1_spt_2 |>
    filter(vargroup1row < 3) |>
    select(medrioid, spt_occasion, spt_num, vargroup1row, prires) |>
    pivot_wider(names_from = vargroup1row, values_from = prires) |>
    rename(prinegres = `1`, priposres = `2`)
  st1_spt_tst <- st1_spt_2 |>
    filter(between(vargroup1row, 3, 10)) |>
    select(medrioid, spt_occasion, spt_num, vargroup1row, prind, prires) |>
    mutate(vargroup1row = vargroup1row - 2) |>
    rename(prireact = prind) |>
    mutate(prireact = case_when(
      prireact == "Yes" ~ "Not Done",
      prireact == "No" & prires == 0 ~ "No",
      prireact == "No" & prires > 0 ~ "Yes",
      .default = NA_character_
    )) |>
    pivot_wider(
      names_from = vargroup1row,
      values_from = c(prireact, prires),
      names_sep = "",
      names_vary = "slowest"
    )
  st1_spt_oth <- st1_spt_2 |>
    filter(vargroup1row > 10) |>
    filter(!is.na(priall)) |>
    select(medrioid, spt_occasion, spt_num, vargroup1row, prind, prind, priallspec, prires) |>
    mutate(vargroup1row = vargroup1row - 2) |>
    rename(prireact = prind) |>
    mutate(prireact = case_when(
      prireact == "Yes" ~ "Not Done",
      prireact == "No" & prires == 0 ~ "No",
      prireact == "No" & prires > 0 ~ "Yes",
      .default = NA_character_
    )) |>
    pivot_wider(
      names_from = vargroup1row,
      values_from = c(prireact, priallspec, prires),
      names_sep = "",
      names_vary = "slowest"
    )

  st1_spt_out <- st1_spt_1 |>
    left_join(st1_spt_ctr, join_by(medrioid, spt_occasion, spt_num)) |>
    left_join(st1_spt_tst, join_by(medrioid, spt_occasion, spt_num)) |>
    left_join(st1_spt_oth, join_by(medrioid, spt_occasion, spt_num)) |>
    mutate(record_id = as.character(medrioid)) |>
    select(-medrioid)

  spt <- bind_rows(st2_spt_fill, st1_spt_out)
  var_label(spt) <- var_label(st2_spt_fill)
  spt
}

dat_rand <- combine_randomisation()
dat_st <- combine_study_termination()
dat_demo <- combine_demographics()
dat_bh <- combine_birth_history()
dat_mh <- combine_medical_history()
dat_spt <- combine_skin_prick_test()

optimum_data <- list(
  "randomisation" = dat_rand,
  "demographics" = dat_demo,
  "birth_history" = dat_bh,
  "medical_history" = dat_mh,
  "skin_prick_test" = dat_spt
)
qsave(enframe(optimum_data, name = "form", value = "data"), file.path("data", "optimum-data.qs"))
