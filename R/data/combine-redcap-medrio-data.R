suppressPackageStartupMessages({
  library(REDCapTidieR)
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(stringr)
  library(tibble)
  library(lubridate)
  library(labelled)
  library(qs)
})

readRenviron(".env")

st1_path <- file.path(
  Sys.getenv("RDS_PATH"),
  config::get("raw_data_stage1_path")
)
st2_path <- file.path(
  Sys.getenv("RDS_PATH"),
  config::get("raw_data_stage2_path")
)
st2_file <- file.path(st2_path, config::get("raw_data_stage2_file"))
com_file <- file.path(Sys.getenv("RDS_PATH"), config::get("combined_data_file"))
st2_data <- qread(st2_file)

combine_randomisation <- function() {
  st2_rand <- extract_tibble(st2_data, "randomisation") |>
    select(-redcap_event, -form_status_complete) |>
    rename(site = redcap_data_access_group)
  st1_rand <- read_delim(
    file.path(st1_path, "VAX V1.txt"),
    show_col_types = FALSE,
    col_select = c(MedrioID, SubjectID, Site, RAND, RANDDATTIM)
  ) |>
    rename_with(tolower) |>
    mutate(
      subjectid = gsub("^01", "01-", subjectid),
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
    select(-redcap_event, -form_status_complete, -redcap_data_access_group) |>
    mutate(discdat = as_date(discdat, format = "%Y-%m-%d"))
  st1_st <- read_delim(
    file.path(st1_path, "ST.txt"),
    show_col_types = FALSE,
    col_select = c(
      MedrioID,
      DISCDAT,
      STREAS,
      STETRREAS,
      IPPVSPEC,
      WCIPNW,
      WCIPSPEC
    ),
    col_types = list(MedrioID = "c", DISCDAT = col_date(format = "%d-%b-%Y"))
  ) |>
    rename_with(tolower) |>
    mutate(
      wcipnw = wcipnw == "Yes"
    ) |>
    rename(record_id = medrioid, wcipnwreas = wcipspec)
  st <- bind_rows(st1_st, st2_st)
  var_label(st) <- var_label(st2_st)
  st
}

combine_demographics <- function() {
  st2_demo <- extract_tibble(st2_data, "demographics") |>
    select(
      -redcap_event,
      -form_status_complete,
      -redcap_data_access_group,
      -deemail
    )
  st1_demo <- read_delim(
    file.path(st1_path, "DEMO.txt"),
    show_col_types = FALSE,
    col_select = c(
      MedrioID,
      VISDAT1,
      BRTHDAT,
      GENDER,
      starts_with("COB"),
      starts_with("ETO"),
      starts_with("EDU"),
      P1TYP,
      P2TYP,
      INCOME,
      GPINF,
      -ends_with("_CODED")
    ),
    col_types = list(
      MedrioID = "c",
      VISDAT1 = col_date(format = "%d-%b-%Y"),
      BRTHDAT = col_date(format = "%d-%b-%Y")
    )
  ) |>
    rename_with(tolower) |>
    mutate(
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
      gestwp = if_else(gestwp == "MI", NA_character_, gestwp),
      gestwp = as.numeric(gestwp),
      gestdp = if_else(gestdp == "MI", NA_character_, gestdp),
      gestdp = as.numeric(gestdp),
      prevnum = if_else(prevnum == "MI", NA_character_, prevnum),
      prevnum = as.numeric(prevnum),
      prevdat1 = if_else(prevdat1 %in% c("MI", "UNK"), NA_character_, prevdat1),
      prevdat1 = as_date(prevdat1, format = "%Y-%m-%d")
    )
  suppressWarnings(
    st1_bh <- read_delim(
      file.path(st1_path, "BH.txt"),
      show_col_types = FALSE,
      col_select = c(
        -ends_with("_CODED"),
        -SubjectID,
        -Site,
        -SubjectStatus,
        -Visit,
        -Form,
        -SubjectVisitFormID
      )
    ) |>
      rename_with(tolower)
  )
  st1_bh1 <- st1_bh |>
    filter(row_number() == 1, .by = medrioid) |>
    select(
      medrioid,
      delt,
      mgrav,
      mpara,
      inmpertvp,
      gestwp,
      gestdp,
      vacad,
      prevpert,
      minfvp,
      gestwi,
      gestdi,
      gestadelw,
      gestadeld,
      ipab,
      neoab,
      aps1,
      aps5,
      wgt,
      len,
      inhc,
      sibna
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
      vargroup1row = if_else(medrioid == "41", 1, vargroup1row)
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
      sibnum = factor(
        case_when(
          sibna == "Not Applicable (No Siblings)" ~ "Not Applicable",
          sibna == "Unknown" ~ "Unknown",
          .default = as.character(sibnum)
        ),
        levels = levels(st2_bh$sibnum)
      ),
      record_id = as.character(medrioid)
    ) |>
    select(-medrioid, -sibna)
  bh <- bind_rows(st2_bh, st1_out)
  var_label(bh) <- var_label(st2_bh)
  bh
}

combine_medical_history <- function() {
  st2_mh <- extract_tibble(st2_data, "medical_history") |>
    select(
      -redcap_event,
      -form_status_complete,
      -redcap_data_access_group,
      -mhplanspec
    ) |>
    mutate(
      mhenddat1 = if_else(mhenddat1 == "MI", NA_character_, mhenddat1),
      mhenddat1 = as_date(mhenddat1, format = "%Y-%m-%d")
    )
  suppressWarnings(
    st1_mh <- read_delim(
      file.path(st1_path, "MH.txt"),
      show_col_types = FALSE,
      col_types = list(MedrioID = "c"),
      col_select = -c(
        ends_with("_CODED"),
        Visit,
        Form,
        SubjectVisitFormID,
        SubjectStatus,
        Site,
        SubjectID,
        FormEntryDate,
        MHENDDAT_P,
        MHSTDAT_P
      )
    ) |>
      rename_with(tolower)
  )
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
    show_col_types = FALSE,
    col_select = c(MedrioID, CMEDYN, VACYN),
    col_types = c(MedrioID = "c")
  ) |>
    rename_with(tolower) |>
    rename(record_id = medrioid) |>
    mutate(cmyn = cmedyn == "Yes", vacyn = vacyn == "Yes") |>
    select(-cmedyn)
  meds <- bind_rows(st2_meds, st1_meds)
  var_label(meds) <- var_label(st2_meds)
  meds
}

combine_family_history_atopy <- function() {
  st2_fha <- extract_tibble(st2_data, "family_history_of_atopy") |>
    select(-redcap_event, -redcap_data_access_group, -form_status_complete)
  suppressWarnings(
    st1_fha <- read_delim(
      file.path(st1_path, "FHA.txt"),
      show_col_types = FALSE,
      col_select = -c(
        ends_with("_CODED"),
        SubjectID,
        Site,
        SubjectStatus,
        Visit,
        Form,
        FormEntryDate,
        SubjectVisitFormID
      ),
      na = c("", "NA", "N/A")
    ) |>
      rename_with(tolower)
  )
  st1_fha_1 <- st1_fha |>
    filter(is.na(vargroup1row)) |>
    select(medrioid:fhasib4fa) |>
    rename(
      record_id = medrioid,
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
    ) |>
    mutate(record_id = as.character(record_id))
  fha <- bind_rows(st2_fha, st1_fha_1) |>
    mutate(
      across(fhafthast:fhasibast6, ~ if_else(.x == "N/A", NA_character_, .x)),
      across(fhafatecz:fhasibecz6, ~ if_else(.x == "N/A", NA_character_, .x)),
      across(fhafatar:fhasibar6, ~ if_else(.x == "N/A", NA_character_, .x)),
      across(fhafatfa:fhasibfa6, ~ if_else(.x == "N/A", NA_character_, .x))
    ) |>
    rowwise() |>
    mutate(
      fhaast = case_when(
        any(c_across(fhafthast:fhasibast6) == "Yes", na.rm = TRUE) ~ 1,
        all(
          c_across(fhafthast:fhasibast6) == "No" |
            c_across(fhafthast:fhasibast6) == "Unknown",
          na.rm = TRUE
        ) ~
          0,
        TRUE ~ NA_real_
      ),
      fhaecz = case_when(
        any(c_across(fhafatecz:fhasibecz6) == "Yes", na.rm = TRUE) ~ 1,
        all(
          c_across(fhafatecz:fhasibecz6) == "No" |
            c_across(fhafatecz:fhasibecz6) == "Unknown",
          na.rm = TRUE
        ) ~
          0,
        TRUE ~ NA_real_
      ),
      fhaar = case_when(
        any(c_across(fhafatar:fhasibar6) == "Yes", na.rm = TRUE) ~ 1,
        all(
          c_across(fhafatar:fhasibar6) == "No" |
            c_across(fhafatar:fhasibar6) == "Unknown",
          na.rm = TRUE
        ) ~
          0,
        TRUE ~ NA_real_
      ),
      fhafa = case_when(
        any(c_across(fhafatfa:fhasibfa6) == "Yes", na.rm = TRUE) ~ 1,
        all(
          c_across(fhafatfa:fhasibfa6) == "No" |
            c_across(fhafatfa:fhasibfa6) == "Unknown",
          na.rm = TRUE
        ) ~
          0,
        TRUE ~ NA_real_
      ),
      fha_raw = case_when(
        any(c(fhaast, fhaecz, fhaar, fhafa) == 1, na.rm = TRUE) ~ 1,
        any(is.na(c(fhaast, fhaecz, fhaar, fhafa))) ~ NA_real_,
        TRUE ~ 0
      ),
      fha = factor(
        case_when(
          any(c(fhaast, fhaecz, fhaar, fhafa) == 1, na.rm = TRUE) ~ "Yes",
          TRUE ~ "No"
        ),
        levels = c("Yes", "No")
      )
    ) |>
    ungroup()
  var_label(fha) <- var_label(st2_fha)
  fha
}

combine_physical_exam_v1 <- function(st2_data, st1_path) {
  st2_pe <- extract_tibble(st2_data, "physical_examination_v1") |>
    select(-redcap_event, -form_status_complete, -redcap_data_access_group)
  suppressWarnings(
    st1_pe <- read_delim(
      file.path(st1_path, "PE.txt"),
      show_col_types = FALSE
    ) |>
      rename_with(tolower)
  )
  st1_pe1 <- st1_pe |>
    filter(is.na(vargroup1row)) |>
    select(medrioid, tempv1, inwv1, inlv1, inhcv1)
  st1_pe2 <- st1_pe |>
    filter(!is.na(vargroup1row)) |>
    select(medrioid, vargroup1row, peres, pespec) |>
    pivot_wider(
      names_from = vargroup1row,
      values_from = c(peres, pespec),
      names_sep = ""
    )
  st1_out <- left_join(st1_pe1, st1_pe2, join_by(medrioid)) |>
    rename(record_id = medrioid) |>
    mutate(record_id = as.character(record_id))
  st <- bind_rows(st2_pe, st1_out)
  var_label(st) <- var_label(st2_pe)
  st
}

combine_vax_admin_v1 <- function() {
  st2_vaxv1 <- extract_tibble(st2_data, "vaccine_administration_v1") |>
    select(
      -redcap_event,
      -form_status_complete,
      -redcap_data_access_group,
      -sdadmnam
    ) |>
    mutate(
      sdadmdattim = as_datetime(
        paste(sdadmdat, sdadmtim),
        format = "%Y-%m-%d %H:%M:%S"
      )
    ) |>
    select(-c(sdadmdat, sdadmtim)) |>
    relocate(sdadmdattim, .after = record_id) |>
    mutate(
      vacblloc = "Right Thigh IM injection",
      vacpnloc = "Left Thigh IM injection",
      vacrort = if_else(vacrort___1, "Oral", NA)
    ) |>
    select(
      -c(vacblloc___1, vacblloc___99, vaciplocspec, vacpnloc___1, vacrort___1)
    )
  st1_vaxv1 <- read_delim(
    file.path(st1_path, "VAX V1.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(
      -c(
        ends_with("_coded"),
        subjectid,
        subjectstatus,
        site,
        visit,
        form,
        formentrydate,
        rand,
        subjectvisitformid
      )
    ) |>
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

combine_vax_admin_v3 <- function() {
  # In stage 2, 18-month vax was visit 3
  st2_vaxv3 <- extract_tibble(st2_data, "vaccine_administration_v3") |>
    select(
      -redcap_event,
      -form_status_complete,
      -redcap_data_access_group,
      -vac7admtim,
      -vac7obtim
    ) |>
    mutate(
      vac7admdat = as_date(vac7admdat)
    )
  # In stage 1, 18-month vax was visit 7
  st1_visv3 <- read_delim(
    file.path(st1_path, "V2-5.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    filter(visit == "Visit 7") |>
    select(medrioid, visdat) |>
    mutate(visdat = as_date(visdat, format = "%d-%b-%Y"))
  st1_vaxv3 <- read_delim(
    file.path(st1_path, "VAX V7.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(
      -c(
        ends_with("_coded"),
        subjectid,
        subjectstatus,
        site,
        visit,
        form,
        formentrydate,
        subjectvisitformid
      )
    )
  st1_vaxv3 <- left_join(
    st1_visv3,
    st1_vaxv3,
    join_by(medrioid)
  ) |>
    rename(
      vac7admdat = visdat,
      vac7iploc = vac7ipvloc,
      record_id = medrioid,
      vac7hibexp = vac7hepexp
    ) |>
    mutate(
      record_id = as.character(record_id),
      vac7adyn = vac7adyn == "Yes",
      vac7obyn = vac7obyn == "Yes",
      vac7obae = vac7obae == "Yes"
    )
  bind_rows(st1_vaxv3, st2_vaxv3)
}

combine_participant_assessment <- function() {
  # For PCH participants, expect assessment at visit 2, 3, and 4
  # For all others, only at visit 2 and 3.
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
      ),
      visdat = if_else(
        record_id == "5785-83" & visit == "2",
        date("2023-03-30"),
        visdat
      )
    )
  st1_pa <- read_delim(
    file.path(st1_path, "V2-5.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(
      -ends_with("_coded"),
      -subjectid,
      -site,
      -subjectstatus,
      -form,
      -subjectvisitformid,
      -mthpostv5,
      -formentrydate
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

combine_food_household <- function() {
  # The most relevant field here is probably:
  #   - fefadiag, new diagnosis of food allergy
  #   - feecz, new diagnosis of eczema
  # and the associated fields (type, date)
  # Other fields relate to feeding: breastfeeding, formuala feeding, introduction of solids
  # Currently, just focus on food allergy and eczema fields
  st2_food <- st2_data |>
    extract_tibble("food_and_household_questionnaire") |>
    mutate(
      fedat = if_else(fedat == "MI", NA_character_, fedat),
      fedat = as_date(fedat)
    ) |>
    mutate(
      fe_phone = grepl("ques", redcap_event),
      visit_age = case_when(
        redcap_event == "visit_1" ~ "6-week",
        redcap_event == "6_month_food_quest" ~ "6-month",
        redcap_event == "9_month_food_quest" ~ "9-month",
        redcap_event == "visit_2" ~ "12-month",
        redcap_event == "15_month_food_ques" ~ "15-month",
        redcap_event == "visit_3" ~ "18-month",
        .default = NA_character_
      )
    )

  st2_food <- st2_food |>
    select(
      record_id,
      visit_age,
      fe_phone,
      fedat,
      fecurr,
      febfever,
      febfform,
      fethickyn,
      fesolyn,
      fefadiag,
      fefadiagspec,
      fefadiadat,
      fesupp,
      ferash,
      fecat,
      fedog,
      fedcyn,
      feecz,
      feeczdat
    ) |>
    mutate(
      across(
        c(febfever, febfform, fethickyn, fesupp, ferash, fedcyn),
        ~ case_match(.x, FALSE ~ "No", TRUE ~ "Yes")
      )
    )

  # st2_food_v1 <- st2_food |>
  #   filter(redcap_event == "visit_1") |>
  #   select(-redcap_event, -redcap_data_access_group) |>
  #   mutate(fedat = as_date(fedat)) |>
  #   select(
  #     record_id,
  #     fecurr,
  #     febfever,
  #     febfform,
  #     fethickyn,
  #     fesolyn,
  #     fefadiag,
  #     fesupp,
  #     ferash,
  #     fecat,
  #     fedog,
  #     fedcyn
  #   )
  # st2_ecz <- st2_food |>
  #   select(
  #     record_id,
  #     redcap_event,
  #     redcap_survey_timestamp,
  #     visit_age,
  #     fe_phone,
  #     fedat,
  #     fecurr,
  #     fefadiag,
  #     fefadiagspec,
  #     fefadiadat,
  #     feecz,
  #     feeczdat
  #   ) |>
  #   select(-redcap_event)

  # For Medrio, the food questionnaires are split between specific visits
  # The variable names differ, but for the most part the same information
  # is being collected
  # Further, the "fedat" would come from the "visit date" or "phone contact date"
  # as stored in "V1, V2-5, V6-8, PHONE".
  suppressWarnings(
    st1_food_v1 <- read_delim(
      file.path(st1_path, "FOOD V1.txt"),
      show_col_types = FALSE,
      col_select = -ends_with("_CODED")
    ) |>
      rename_with(tolower)
  )
  suppressWarnings(
    st1_food_v2 <- read_delim(
      file.path(st1_path, "FOOD V2-5.txt"),
      show_col_types = FALSE,
      col_select = -ends_with("_CODED")
    ) |>
      rename_with(tolower)
  )
  suppressWarnings(
    st1_food_v3 <- read_delim(
      file.path(st1_path, "FOOD V6-8.txt"),
      show_col_types = FALSE,
      col_select = -ends_with("_CODED")
    ) |>
      rename_with(tolower) |>
      select(-fefdagun)
  )
  # For the date fields
  # Phone contact dates
  st1_phone <- read_delim(
    file.path(st1_path, "PHONE.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(-ends_with("_coded")) |>
    select(medrioid, visit, condat) |>
    mutate(condat = as_date(condat, format = "%d-%b-%Y")) |>
    rename(fedat = condat)
  # Visit dates
  st1_visits_v1 <- read_delim(
    file.path(st1_path, "DEMO.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(-ends_with("_coded")) |>
    select(medrioid, visit, visdat1) |>
    mutate(visdat1 = as_date(visdat1, format = "%d-%b-%Y")) |>
    rename(fedat = visdat1)
  st1_visits_v2 <- read_delim(
    file.path(st1_path, "V2-5.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(-ends_with("_coded")) |>
    select(medrioid, visit, visdat, windyn, windreas, windothspec) |>
    mutate(visdat = as_date(visdat, format = "%d-%b-%Y")) |>
    rename(fedat = visdat)
  st1_visits <- bind_rows(st1_visits_v1, st1_visits_v2) |>
    bind_rows(st1_phone)

  st1_food_v2 <- st1_food_v2 |>
    rename_with(~ gsub("fe2", "fe", .x))
  st1_food_v3 <- st1_food_v3 |>
    rename_with(~ gsub("fe6", "fe", .x))

  st1_food <- bind_rows(
    st1_food_v1,
    st1_food_v2,
    st1_food_v3
  ) |>
    filter(is.na(vargroup1row & is.na(vargroup2row) & is.na(vargroup3row))) |>
    select(
      medrioid,
      subjectid,
      visit,
      fecurr,
      febfever,
      febfform,
      fethickyn,
      fesolyn,
      fefadiag,
      fefadiagspec,
      fefadiagstag,
      fesupp,
      ferash,
      fecat,
      fedog,
      fedcyn,
      feecx,
      feeczag,
      feexstun
    ) |>
    left_join(st1_visits, join_by(medrioid, visit)) |>
    rename(
      feecz = feecx,
      feeczstun = feexstun
    ) |>
    arrange(medrioid) |>
    mutate(
      record_id = as.character(medrioid),
      fe_phone = grepl("phone", visit),
      visit_age = case_when(
        visit == "Visit 1" ~ "6-week",
        visit == "Visit 2" ~ "4-month",
        visit == "Visit 3" ~ "6-month",
        visit == "Visit 4" ~ "6-month + 72 hrs",
        visit == "Visit 5" ~ "7-month",
        visit == "9 month phone contact" ~ "9-month",
        visit == "Visit 6" ~ "12-month",
        visit == "15 month phone contact" ~ "15-month",
        visit == "Visit 7" ~ "18-month",
        visit == "Visit 8" ~ "19-month",
        .default = NA_character_
      )
    ) |>
    select(-visit, -medrioid)

  # Eczema diagnoses at visit 1
  # st1_ecz_v1 <- st1_food_v1 |>
  #   filter(is.na(vargroup1row) & is.na(vargroup2row) & is.na(vargroup3row)) |>
  #   select(medrioid, visit, fecurr, fefadiag, feecx) |>
  #   left_join(st1_visits, join_by(medrioid, visit)) |>
  #   rename(feecz = feecx)
  # # Eczema diagnoses at visits 2 - 5
  # st1_ecz_v2 <- st1_food_v2 |>
  #   filter(is.na(vargroup1row) & is.na(vargroup2row) & is.na(vargroup3row)) |>
  #   select(
  #     medrioid,
  #     visit,
  #     fe2curr,
  #     fe2fadiag,
  #     fe2fadiagspec,
  #     fe2fadiagstag,
  #     fe2ecx,
  #     fe2eczag,
  #     fe2exstun
  #   ) |>
  #   rename(
  #     fecurr = fe2curr,
  #     fefadiag = fe2fadiag,
  #     fefadiagspec = fe2fadiagspec,
  #     fefadiagstag = fe2fadiagstag,
  #     feecz = fe2ecx,
  #     feeczag = fe2eczag,
  #     feeczstun = fe2exstun
  #   ) |>
  #   left_join(st1_visits, join_by(medrioid, visit))
  # # Eczema diagnoses at phone contact and visits 6 - 8
  # st1_ecz_v3 <- st1_food_v3 |>
  #   filter(is.na(vargroup1row) & is.na(vargroup2row) & is.na(vargroup3row)) |>
  #   select(
  #     medrioid,
  #     visit,
  #     fe6fadiag,
  #     fe6fadiagspec,
  #     fe6fadiagstag,
  #     fe6ecx,
  #     fe6eczag
  #   ) |>
  #   rename(
  #     fefadiag = fe6fadiag,
  #     fefadiagspec = fe6fadiagspec,
  #     fefadiagstag = fe6fadiagstag,
  #     feecz = fe6ecx,
  #     feeczag = fe6eczag
  #   ) |>
  #   left_join(st1_visits, join_by(medrioid, visit))
  # st1_ecz <- bind_rows(st1_ecz_v1, st1_ecz_v2, st1_ecz_v3) |>
  #   arrange(medrioid) |>
  #   mutate(
  #     record_id = as.character(medrioid),
  #     fe_phone = grepl("phone", visit),
  #     visit_age = case_when(
  #       visit == "Visit 1" ~ "6-week",
  #       visit == "Visit 2" ~ "4-month",
  #       visit == "Visit 3" ~ "6-month",
  #       visit == "Visit 4" ~ "6-month + 72 hrs",
  #       visit == "Visit 5" ~ "7-month",
  #       visit == "9 month phone contact" ~ "9-month",
  #       visit == "Visit 6" ~ "12-month",
  #       visit == "15 month phone contact" ~ "15-month",
  #       visit == "Visit 7" ~ "18-month",
  #       visit == "Visit 8" ~ "19-month",
  #       .default = NA_character_
  #     )
  #   ) |>
  #   select(-visit, -medrioid)

  # ecz <- bind_rows(st2_ecz, st1_ecz)
  # var_label(ecz) <- var_label(st2_ecz)
  # ecz

  st_food <- bind_rows(st2_food, st1_food) |>
    mutate(
      fecat = if_else(fecat == "N/A", "No", fecat),
      fedog = if_else(fedog == "N/A", "No", fedog)
    )
  var_label(st_food) <- var_label(st2_food)
  st_food
}

combine_outcome_report <- function() {
  st2_out <- extract_tibble(st2_data, "outcome_report") |>
    select(-c(redcap_event, redcap_data_access_group)) |>
    rename(record_id_num = redcap_form_instance) |>
    mutate(
      outcome_num = row_number(),
      no_outcome_report = all(is.na(outallyn) & is.na(outalltp)),
      .by = record_id
    ) |>
    filter(!(record_id_num > 1 & is.na(outalltp))) |>
    mutate(
      outfrraspec = as.numeric(outfrraspec),
      outfrarxntm = as.numeric(outfrarxntm),
      outdiagdat = as_date(outdiagdat),
      outawardat = as_date(outawardat),
      outageval = as.numeric(outageval),
      outageval_months = case_when(
        outageunit == "Months" ~ outageval,
        outageunit == "Weeks" ~ outageval / 4.345,
        outageunit == "Years" ~ outageval * 12
      ),
      outageval_weeks = outageval_months * 4.345,
      outallyn2 = case_when(
        no_outcome_report ~ NA,
        !is.na(outalltp) ~ TRUE,
        is.na(outalltp) ~ FALSE
      )
    )
  st1_out <- read_delim(
    file.path(st1_path, "outcomes.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(
      -ends_with("_coded"),
      -subjectvisitformid,
      -subjectid,
      -site,
      -subjectstatus,
      -visit,
      -form,
      -formentrydate
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
    mutate(
      no_outcome_report = all(is.na(outallyn) & is.na(outalltp)),
      .by = record_id
    ) |>
    mutate(
      outageval_months = 12 * outageyrs + outagemnth + ouagewk / 4.345,
      outageval_weeks = outageval_months * 4.345,
      outallyn2 = case_when(
        no_outcome_report ~ NA,
        !is.na(outalltp) ~ TRUE,
        is.na(outalltp) ~ FALSE
      )
    ) |>
    mutate(outcome_num = row_number(), .by = record_id) |>
    select(-medrioid)
  out <- bind_rows(st2_out, st1_out)
  var_label(out) <- var_label(st2_out)
  out
}

combine_other_immuno_data <- function() {
  st1_imm <- read_delim(
    file.path(st1_path, "oth_immun_data.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(
      -ends_with("_coded"),
      -subjectvisitformid,
      -subjectid,
      -site,
      -subjectstatus,
      -formentrydate,
      -visit,
      -form
    ) |>
    filter(is.na(othimmtp) | othimmtp != "No other immunological data") |>
    mutate(othpridat = as_date(othpridat, format = "%d-%b-%Y")) |>
    rename(record_id = medrioid) |>
    mutate(record_id = as.character(record_id))

  st1_imm_1 <- st1_imm |>
    filter(is.na(vargroup1row)) |>
    select(record_id, othpridat, othprinegres, othpriposres) |>
    rename_with(~ gsub("oth", "", .x))
  st1_imm_2 <- st1_imm |>
    filter(!is.na(vargroup1row)) |>
    mutate(
      othpriallspec = tolower(if_else(
        is.na(othpriallspec),
        othpriall,
        othpriallspec
      ))
    ) |>
    select(record_id, vargroup1row, othpriallspec, othprires) |>
    rename_with(~ gsub("oth", "", .x))
  st1_spt_oth <- left_join(
    st1_imm_1,
    st1_imm_2,
    join_by(record_id)
  ) |>
    mutate(
      spt_occasion = "other"
    ) |>
    filter(record_id != "136") |>
    select(-vargroup1row) |>
    rename(
      spt_neg = prinegres,
      spt_pos = priposres,
      spt_tested = priallspec,
      spt_result = prires
    )

  st2_spt_oth <- extract_tibble(st2_data, "other_immunological_data") |>
    filter(othimmtp___1) |>
    select(
      -redcap_event,
      -redcap_data_access_group,
      -starts_with("othimmtp___")
    ) |>
    rename_with(~ gsub("oth", "", .x)) |>
    rename(prilocun = priloc) |>
    select(record_id, starts_with("pri")) |>
    mutate(
      priallspec1 = tolower(if_else(is.na(priallspec1), priall1, priallspec1)),
      priallspec2 = tolower(if_else(is.na(priallspec2), priall2, priallspec2)),
      priallspec3 = tolower(if_else(is.na(priallspec3), priall3, priallspec3)),
      priallspec4 = tolower(if_else(is.na(priallspec4), priall4, priallspec4)),
      priallspec5 = tolower(if_else(is.na(priallspec5), priall5, priallspec5)),
      priallspec6 = tolower(if_else(is.na(priallspec6), priall6, priallspec6)),
      priallspec7 = tolower(if_else(is.na(priallspec7), priall7, priallspec7)),
      priallspec8 = tolower(if_else(is.na(priallspec8), priall8, priallspec8)),
      priallspec9 = tolower(if_else(is.na(priallspec9), priall9, priallspec9)),
      priallspec10 = tolower(if_else(
        is.na(priallspec10),
        priall10,
        priallspec10
      ))
    ) |>
    select(-matches("priall[1-9]")) |>
    pivot_longer(
      priallspec1:prires10,
      names_pattern = "pri(allspec|res)",
      names_to = c(".value")
    ) |>
    rename(
      spt_neg = prinegres,
      spt_pos = priposres,
      spt_tested = allspec,
      spt_result = res
    ) |>
    mutate(
      spt_occasion = "other"
    ) |>
    filter(!is.na(spt_tested))
  bind_rows(st1_spt_oth, st2_spt_oth)
}

combine_skin_prick_test <- function() {
  # There are three sources of skin prick tests:
  #    - the scheduled visit at 12-months of age (visit 2 in REDCap, visit 6 in Medrio)
  #    - at any unscheduled visit
  #    - other immunological data
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
    mutate(
      spt_occasion = if_else(
        redcap_event == "visit_2",
        "scheduled",
        "unscheduled"
      )
    ) |>
    select(
      -redcap_event,
      -redcap_data_access_group,
      -redcap_form_instance,
      -form_status_complete
    ) |>
    mutate(
      prinegres = if_else(prinegres == "MI", NA_character_, prinegres),
      prinegres = as.numeric(prinegres),
      priposres = if_else(priposres == "UNK", NA_character_, priposres),
      priposres = as.numeric(priposres),
      prireact8 = if_else(prireact8 == "N/A", "Not Done", prireact8)
    ) |>
    # select(
    #   -c(
    #     unvisyn,
    #     unvisdat,
    #     unvisreas___1,
    #     unvisreas___2,
    #     unvisreas___99,
    #     unvisreasoth
    #   )
    # ) |>
    rename(
      priallspec9 = priallspec,
      priallspec10 = priallspec_2,
      priallspec11 = priallspec_3,
      priallspec12 = priallspec_4,
      priallspec13 = priallspec_5
    ) |>
    arrange(record_id, pridat) |>
    mutate(spt_num = row_number(), .by = record_id)
  # Per above, fill in "0" prires if prireact is "No"
  st2_spt_fill <- st2_spt |>
    mutate(
      across(
        starts_with("prires"),
        ~ if_else(
          get(str_replace(cur_column(), "prires", "prireact")) == "No",
          0,
          .
        )
      )
    )

  var_label(st2_spt)$prinegres <- "Negative control mean wheal diameter (mm)"
  var_label(st2_spt)$prireact8 <- "Sesame reaction"

  suppressWarnings(
    st1_spt <- read_delim(
      file.path(st1_path, "PRICK.txt"),
      show_col_types = FALSE
    ) |>
      rename_with(tolower) |>
      select(
        -ends_with("_coded"),
        -subjectvisitformid,
        -subjectid,
        -site,
        -subjectstatus,
        -formentrydate,
        -visit,
        -form
      ) |>
      mutate(spt_occasion = "scheduled")
  )
  suppressWarnings(
    st1_un <- read_delim(
      file.path(st1_path, "UN_VISIT.txt"),
      show_col_types = FALSE
    ) |>
      rename_with(tolower) |>
      select(
        -ends_with("_coded"),
        -subjectvisitformid,
        -subjectid,
        -site,
        -subjectstatus,
        -formentrydate
      ) |>
      filter(
        any(unvisyn == "Yes" & unvisreas == "Skin Prick Test"),
        .by = medrioid
      ) |>
      mutate(spt_occasion = "unscheduled") |>
      select(
        medrioid,
        spt_occasion,
        unvisyn,
        unvisdat,
        unvisreas,
        unsreasoth,
        priyn_un,
        prispec_un,
        pridat_un,
        vargroup1row,
        priall_un,
        prind_un,
        priallspec_un,
        prires_un
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
  )

  st1_spt_all <- bind_rows(st1_spt, st1_un) |>
    mutate(spt_num = cumsum(is.na(vargroup1row)), .by = medrioid)

  st1_spt_1 <- st1_spt_all |>
    filter(is.na(vargroup1row)) |>
    select(
      medrioid,
      spt_occasion,
      unvisyn,
      unvisdat,
      unvisreas,
      unsreasoth,
      spt_num,
      priyn,
      prispec,
      pridat
    ) |>
    mutate(
      priyn = priyn == "Yes",
      pridat = as_date(pridat, format = "%d-%b-%Y"),
      unvisyn = unvisyn == "Yes",
      unvisdat = as_date(unvisdat, format = "%d-%b-%Y")
    ) |>
    rename(prinspec = prispec)

  st1_spt_2 <- st1_spt_all |>
    filter(!is.na(vargroup1row)) |>
    select(
      medrioid,
      spt_occasion,
      spt_num,
      vargroup1row,
      priall,
      prind,
      priallspec,
      prires
    )
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
    mutate(
      prireact = case_when(
        prireact == "Yes" ~ "Not Done",
        prireact == "No" & prires == 0 ~ "No",
        prireact == "No" & prires > 0 ~ "Yes",
        .default = NA_character_
      )
    ) |>
    pivot_wider(
      names_from = vargroup1row,
      values_from = c(prireact, prires),
      names_sep = "",
      names_vary = "slowest"
    )
  st1_spt_oth <- st1_spt_2 |>
    filter(vargroup1row > 10) |>
    filter(!is.na(priall)) |>
    select(
      medrioid,
      spt_occasion,
      spt_num,
      vargroup1row,
      prind,
      prind,
      priallspec,
      prires
    ) |>
    mutate(vargroup1row = vargroup1row - 2) |>
    rename(prireact = prind) |>
    mutate(
      prireact = case_when(
        prireact == "Yes" ~ "Not Done",
        prireact == "No" & prires == 0 ~ "No",
        prireact == "No" & prires > 0 ~ "Yes",
        .default = NA_character_
      )
    ) |>
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

combine_food_challenge <- function() {
  st2_fc <- extract_tibble(st2_data, "food_challenge") |>
    select(-redcap_event, -redcap_data_access_group, -form_status_complete) |>
    rename(ofc_num = redcap_form_instance) |>
    filter(ofcyn)
  # Here there are only 3 OFC records, so just restrict to those
  # They are for "egg" and "milk"
  # In REDCap, "Egg" corresponds to ofcfoodtp2 and "Milk" to ofcfoodtp3
  # Here I just map the 3 records to these.
  suppressWarnings(
    st1_fc <- read_delim(
      file.path(st1_path, "CHALL.txt"),
      show_col_types = FALSE
    ) |>
      rename_with(tolower) |>
      select(-ends_with("_coded")) |>
      select(
        -subjectid,
        -site,
        -subjectstatus,
        -visit,
        -form,
        -formentrydate,
        -subjectvisitformid
      ) |>
      filter(vargroup1row == 1) |>
      rename(record_id = medrioid, ofc_num = vargroup1row) |>
      mutate(
        record_id = as.character(record_id),
        ofcyn = TRUE,
        ofcdat = as_date(fcdat, format = "%d-%b-%Y"),
        ofcfoodtp2 = if_else(grepl("egg", fcall), fcres, NA_character_),
        ofcfoodtp3 = if_else(grepl("milk", fcall), fcres, NA_character_)
      ) |>
      select(-fcyn, -fcall, -fcres, -fcdat)
  )
  fc <- bind_rows(st2_fc, st1_fc)
  var_label(fc) <- var_label(st2_fc)
  fc
}

combine_adverse_events <- function() {
  st2_ae <- extract_tibble(st2_data, "adverse_events") |>
    filter(!is.na(aeterm)) |>
    arrange(record_id, aestdat, redcap_form_instance) |>
    mutate(ae_num = row_number(), .by = record_id) |>
    relocate(ae_num, .after = record_id) |>
    select(
      -redcap_event,
      -redcap_form_instance,
      -redcap_data_access_group,
      -form_status_complete,
      -aeyn
    ) |>
    mutate(
      aestdat = if_else(aestdat == "UNK", NA_character_, aestdat),
      aestdat = as_date(aestdat, format = "%Y-%m-%d"),
      aeenddat = if_else(aeenddat == "UNK", NA_character_, aeenddat),
      aeenddat = as_date(aeenddat, format = "%Y-%m-%d")
    )
  suppressWarnings(
    st1_ae <- read_delim(
      file.path(st1_path, "AE.txt"),
      show_col_types = FALSE
    ) |>
      rename_with(tolower) |>
      select(
        -ends_with("_coded"),
        -subjectid,
        -site,
        -subjectstatus,
        -visit,
        -form,
        -formentrydate,
        -subjectvisitformid
      ) |>
      filter(!is.na(vargroup1row)) |>
      mutate(
        record_id = as.character(medrioid),
        aestdat = as_date(aestdat, format = "%d-%b-%Y"),
        aeenddat = as_date(aeenddat, format = "%d-%b-%Y"),
        aeongo = aeongo == "Yes",
        aemeadv = aemeadv == "Yes",
        aecm = aecm == "Yes"
      ) |>
      select(-ends_with("_p"), -aeyn, -medrioid) |>
      rename(
        ae_num = vargroup1row,
        aeterm = aetermtxt,
        aemedadv = aemeadv,
        aecmyn = aecm
      )
  )

  # For Stage 1 MedDRA coded terms
  # LLT: lowest level terms
  # PT: preferred terms
  # HLT: high level terms
  # HLGT: high level group terms
  # SOCs: system organ classes
  dat_rand <- combine_randomisation() |>
    select(record_id, subjid)
  st1_med <- read_xlsx(
    file.path(
      st1_path,
      "2024-12",
      "OPTIMUM Medrio CodingReport 17 Dec 2024.xlsx"
    )
  ) |>
    rename_with(tolower) |>
    select(
      -site,
      -group,
      -visit,
      -form,
      -`coded date`,
      -`coded by`,
      -`coding status`,
      -`coding dictionary`
    ) |>
    rename(
      subjid = `subject id`,
      aeterm = `verbatim term`
    ) |>
    select(-ends_with(" id")) |>
    mutate(
      subjid = gsub("^01", "01-", subjid)
    ) |>
    # One record has the same aeterm twice, which results in many-to-many join
    # Just keep distinct coded terms
    distinct()
  st1_ae_coded <- st1_ae |>
    left_join(
      dat_rand,
      join_by(record_id)
    ) |>
    left_join(st1_med, join_by(subjid, aeterm)) |>
    rename(
      aecode1 = pt
    ) |>
    select(-subjid)

  ae <- bind_rows(st2_ae, st1_ae_coded)
  var_label(ae) <- var_label(st2_ae)
  var_label(ae)$aecode1 <- "Preferred term"
  var_label(ae)$ae_num <- "Adverse event number"
  var_label(ae)$aestdat <- "Start date"
  var_label(ae)$aeenddat <- "End date"
  var_label(ae)$llt <- "MedDRA Lowest level term"
  var_label(ae)$hlt <- "MedDRA High level term"
  var_label(ae)$hlgt <- "MedDRA High level group term"
  var_label(ae)$soc <- "MedDRA System organ class"
  ae
}

combine_sae <- function() {
  st2_sae <- extract_tibble(st2_data, "sae_reporting_log") |>
    select(-redcap_event, -redcap_data_access_group, -form_status_complete) |>
    rename(sae_num = redcap_form_instance) |>
    filter((is.na(saeyn) & sae_num > 1) | saeyn) |>
    mutate(
      saedat = as_date(saedat, format = "%Y-%m-%d"),
      saestdat = as_date(saestdat, format = "%Y-%m-%d"),
      saerpdat = as_date(saerpdat, format = "%Y-%m-%d"),
    )
  st2_sae
}

combine_blood_collection <- function() {
  # st1_ec <- read_delim(
  #   file.path(st1_path, "EC.txt"),
  #   show_col_types = FALSE
  # )
  # st1_conschg <- read_delim(
  #   file.path(st1_path, "CONSCHG.txt"),
  #   show_col_types = FALSE
  # )
  st1_bc <- read_delim(
    file.path(st1_path, "BC.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(
      -ends_with("_coded"),
      -subjectid,
      -site,
      -subjectstatus,
      -form,
      -formentrydate,
      -subjectvisitformid
    ) |>
    mutate(
      lbipdat = as_date(lbipdat, format = "%d-%b-%Y"),
      lbipvst = case_when(
        is.na(lbipvst) ~ NA,
        lbipvst == "Yes" ~ TRUE,
        lbipvst == "No" ~ FALSE
      ),
      visage = case_when(
        visit == "Visit 3" ~ "6-month",
        visit == "Visit 4" ~ "6-month + 72hrs",
        visit == "Visit 5" ~ "7-month",
        visit == "Visit 7" ~ "18-month",
        visit == "Visit 8" ~ "19-month"
      )
    ) |>
    rename(lbiptim = lbiptm) |>
    select(-visit) |>
    rename(record_id = medrioid) |>
    mutate(record_id = as.character(record_id))
  st2_bc <- extract_tibble(st2_data, "blood_sample_collection") |>
    filter(
      redcap_data_access_group == "Perth Children's Hospital",
      as.numeric(substr(record_id, 6, 8)) <= 150
    ) |>
    select(-c(redcap_data_access_group, form_status_complete)) |>
    rowwise() |>
    mutate(
      lbsite = paste0(
        c("Antecubital fossa", "Hand", "Left", "Right", "Both")[
          c(lbsite___1, lbsite___2, lbsite___3, lbsite___4, lbsite___5)
        ],
        collapse = ","
      )
    ) |>
    ungroup() |>
    mutate(
      lbvol = if_else(lbvol == "08", 0.8, as.numeric(lbvol)),
      visage = case_when(
        redcap_event == "visit_3" ~ "18-month",
        redcap_event == "visit_4" ~ "19-month"
      )
    ) |>
    select(-redcap_event)
  bc <- bind_rows(st1_bc, st2_bc)
  bc |>
    mutate(
      visage = factor(
        visage,
        levels = c("6-month", "7-month", "18-month", "19-month")
      )
    )
}

combine_igg <- function() {
  units_igg <- tribble(
    ~antigen   ,
    ~units     ,
    ~ref       ,
    "HBsAg"    ,
    "mIU/mL"   ,
            10 ,
    "Hib-PRP"  ,
    "ng/mL"    ,
          1000 ,
    "PnPs 1"   ,
    "ng/mL"    ,
           350 ,
    "PnPs 3"   ,
    "ng/mL"    ,
           350 ,
    "PnPs 4"   ,
    "ng/mL"    ,
           350 ,
    "PnPs 5"   ,
    "ng/mL"    ,
           350 ,
    "PnPs 6A"  ,
    "ng/mL"    ,
           350 ,
    "PnPs 6B"  ,
    "ng/mL"    ,
           350 ,
    "PnPs 7F"  ,
    "ng/mL"    ,
           350 ,
    "PnPs 9V"  ,
    "ng/mL"    ,
           350 ,
    "PnPs 11A" ,
    "ng/mL"    ,
           350 ,
    "PnPs 14"  ,
    "ng/mL"    ,
           350 ,
    "PnPs 18C" ,
    "ng/mL"    ,
           350 ,
    "PnPs 19A" ,
    "ng/mL"    ,
           350 ,
    "PnPs 19F" ,
    "ng/mL"    ,
           350 ,
    "PnPs 23F" ,
    "ng/mL"    ,
           350 ,
    "DT"       ,
    "mIU/mL"   ,
           100 ,
    "FHA"      ,
    "mIU/mL"   ,
          5000 ,
    "FIM2/3"   ,
    "mIU/mL"   ,
          5000 ,
    "PRN"      ,
    "mIU/mL"   ,
          5000 ,
    "PT"       ,
    "mIU/mL"   ,
          5000 ,
    "TT"       ,
    "mIU/mL"   ,
           100
  ) |>
    mutate(antigen = fct_inorder(antigen))

  # Stage 1 IgG concentrations
  igg_st1_pth <- file.path(st1_path, "2022-09-12_OPTIMUM_IgG_clean.xlsx")
  igg_st1_sht <- excel_sheets(igg_st1_pth)
  igg_st1_raw <- lapply(igg_st1_sht, read_excel, path = igg_st1_pth)
  names(igg_st1_raw) <- igg_st1_sht
  names(igg_st1_raw)[1] <- "DTaP"
  igg_st1 <- left_join(
    igg_st1_raw[[1]],
    igg_st1_raw[[2]],
    join_by(SampleID, `Subject ID`, Visit)
  ) |>
    left_join(igg_st1_raw[[3]], join_by(SampleID, `Subject ID`, Visit)) |>
    pivot_longer(-(1:3), names_to = "antigen", values_to = "concentration") |>
    rename(subjid = `Subject ID`, visit = Visit) |>
    select(-SampleID) |>
    mutate(
      antigen = gsub(" \\([0-9]*\\)", "", antigen),
      visage = case_when(
        visit == "V3" ~ "6-month",
        visit == "V5" ~ "7-month",
        visit == "V7" ~ "18-month",
        visit == "V8" ~ "19-month"
      )
    ) |>
    select(-visit) |>
    complete(
      subjid = paste0("01-", str_pad(1:150, 3, pad = "0")),
      visage,
      antigen,
      fill = list(concentration = NA)
    )

  # Stage 2 IgG concentrations
  igg_st2_pth <- file.path(st2_path, "20250701_OPTIMUM-02.xlsx")
  igg_st2_sht <- excel_sheets(igg_st2_pth)
  igg_st2_units <- read_excel(igg_st2_pth, igg_st2_sht[[2]])
  igg_st2_raw <- read_excel(igg_st2_pth, igg_st2_sht[[1]], na = c("", "N/A"))
  igg_st2 <- igg_st2_raw |>
    pivot_longer(-(1:3), names_to = "antigen", values_to = "concentration") |>
    rename(subjid = `Subject ID`, visit = Visit) |>
    select(-`ID + Visit`) |>
    mutate(
      visage = case_when(
        visit == "V3" ~ "18-month",
        visit == "V4" ~ "19-month"
      )
    ) |>
    select(-visit) |>
    filter(!is.na(concentration)) |>
    complete(
      subjid = paste0("02-", 151:300),
      nesting(visage, antigen),
      fill = list(concentration = NA)
    )

  igg <- bind_rows(igg_st1, igg_st2) |>
    mutate(
      antigen = gsub("HiB", "Hib", antigen),
      antigen = factor(
        antigen,
        levels = c(
          "HBsAg",
          "Hib-PRP",
          "PnPs 1",
          "PnPs 3",
          "PnPs 4",
          "PnPs 5",
          "PnPs 6A",
          "PnPs 6B",
          "PnPs 7F",
          "PnPs 9V",
          "PnPs 11A",
          "PnPs 14",
          "PnPs 18C",
          "PnPs 19A",
          "PnPs 19F",
          "PnPs 23F",
          "DT",
          "FHA",
          "FIM2/3",
          "PRN",
          "PT",
          "TT"
        )
      ),
      group = case_when(
        antigen %in% c("HBsAg", "Hib-PRP") ~ "Other",
        antigen %in% c("DT", "FHA", "FIM2/3", "PRN", "PT", "TT") ~ "Pertussis",
        antigen %in%
          c(
            "PnPs 1",
            "PnPs 3",
            "PnPs 4",
            "PnPs 5",
            "PnPs 6A",
            "PnPs 6B",
            "PnPs 7F",
            "PnPs 9V",
            "PnPs 11A",
            "PnPs 14",
            "PnPs 18C",
            "PnPs 19A",
            "PnPs 19F",
            "PnPs 23F"
          ) ~
          "Pneumococcal"
      ),
      type = case_when(
        group == "Other" ~ "HHB",
        group == "Pertussis" ~ "DTaP",
        group == "Pneumococcal" ~ "PnPs"
      ),
      visage = factor(
        visage,
        levels = c("6-month", "7-month", "18-month", "19-month")
      )
    ) |>
    arrange(subjid, group, antigen, visage) |>
    left_join(units_igg, join_by(antigen)) |>
    mutate(
      positive = as.numeric(concentration > ref),
      # Hib-PRP we use age-specific threshold
      positive = case_when(
        antigen != "Hib-PRP" ~ positive,
        is.na(concentration) ~ NA_real_,
        visage %in% c("6-month", "7-month") & concentration >= 150 ~ 1,
        visage %in% c("18-month", "19-month") & concentration >= 1000 ~ 1,
        TRUE ~ 0
      )
    )

  transform_igg <- function(igg) {
    igg |>
      mutate(
        concentration = case_when(
          group == "Pneumococcal" ~ concentration / 1e3, # ng/mL to ug/mL
          group == "Pertussis" ~ concentration / 1e3, # mIU/mL to IU/mL
          antigen == "HBsAg" ~ concentration / 1e3, # mIU/mL to IU/mL
          antigen == "Hib-PRP" ~ concentration / 1e3, # ng/mL to ug/mL
          antigen == "DT" ~ concentration / 1e3, # mIU/mL to IU/mL
          antigen == "TT" ~ concentration / 1e3, # mIU/mL to IU/mL
          TRUE ~ concentration
        ),
        log_concentration = log(concentration, base = 10),
        units = case_when(
          group == "Pneumococcal" ~ "µg/mL",
          group == "Pertussis" ~ "IU/mL",
          antigen == "HBsAg" ~ "IU/mL",
          antigen == "Hib-PRP" ~ "µg/mL",
          antigen == "DT" ~ "IU/mL",
          antigen == "TT" ~ "IU/mL",
          TRUE ~ units
        ),
        ref = case_when(
          group == "Pneumococcal" ~ ref / 1e3,
          group == "Pertussis" ~ ref / 1e3,
          antigen == "HBsAg" ~ ref / 1e3,
          antigen == "Hib-PRP" ~ ref / 1e3,
          antigen == "DT" ~ ref / 1e3,
          antigen == "TT" ~ ref / 1e3,
          TRUE ~ ref
        )
      )
  }

  igg_trans <- igg |>
    transform_igg()
  igg_trans
}

# There is only IgE data for stage 1,
# but for completeness sake, include it here
combine_ige <- function() {
  ige <- read_delim(
    file.path(st1_path, "Lab results IGE 2.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(
      !ends_with("_coded"),
      -site,
      -subjectstatus,
      -visit,
      -form,
      -formentrydate,
      -subjectvisitformid
    ) |>
    rename(
      lbresigewholev8 = lbresigwholev8,
      lbresigetotalv8 = lbresigtotalv8
    )
  ige_long <- ige |>
    select(-ends_with("com"), -lbresigesamptpv8) |>
    pivot_longer(
      starts_with("lbresige"),
      names_pattern = "lbresige(tet|eggw|whole|total|pean|cash|milk|dust|cat|rye)(v3|v5|v8)",
      names_to = c("allergen", "visit"),
      values_to = "ige"
    ) |>
    mutate(
      visit = as.numeric(gsub("v", "", visit)),
      visit_age = factor(
        visit,
        levels = c(3, 5, 8),
        labels = c("6-month", "7-month", "19-month")
      ),
      allergen = factor(
        allergen,
        levels = c(
          "total",
          "tet",
          "eggw",
          "whole",
          "cash",
          "cat",
          "dust",
          "milk",
          "pean",
          "rye"
        ),
        labels = c(
          "Total IgE",
          "Tetanus-toxoid",
          "Egg white",
          "Whole egg",
          "Cashew",
          "Cat",
          "Dust",
          "Cow's milk",
          "Peanut",
          "Rye"
        )
      )
    )
  ige_com <- ige |>
    select(subjectid, lbresigev3com, lbresigev5com, lbresigev8com) |>
    pivot_longer(
      lbresigev3com:lbresigev8com,
      names_pattern = "lbresige(v3|v5|v8)com",
      names_to = "visit",
      values_to = "comment"
    ) |>
    mutate(
      visit = as.numeric(gsub("v", "", visit)),
      visit_age = factor(
        visit,
        levels = c(3, 5, 8),
        labels = c("6-month", "7-month", "19-month")
      )
    )
  ige_long |>
    left_join(
      ige_com,
      join_by(subjectid, visit, visit_age)
    ) |>
    mutate(
      subjectid = gsub("^01", "01-", subjectid)
    ) |>
    relocate(visit_age, .after = visit) |>
    rename(
      record_id = medrioid,
      subjid = subjectid
    ) |>
    mutate(record_id = as.character(record_id))
}

combine_diary <- function() {
  # Stage 1 vaccination locations check, 6-week and 18-month only
  st1_vax1 <- read_delim(
    file.path(st1_path, "VAX V1.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(-ends_with("_coded")) |>
    select(medrioid, vacblloc, vacpnloc, vacprotyn, vacprotreas) |>
    mutate(
      across(
        ends_with("loc"),
        ~ gsub("Thigh", "Leg", gsub(" IM injection", "", .x))
      )
    )
  st1_vax7 <- read_delim(
    file.path(st1_path, "VAX V7.txt"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower) |>
    select(-ends_with("_coded")) |>
    select(
      medrioid,
      vac7ipvloc,
      vac7mmrloc,
      vac7hibloc,
      vac7adyn,
      vac7adspec
    ) |>
    mutate(
      across(
        ends_with("loc"),
        ~ gsub(
          "Deltoid",
          "Arm",
          gsub("Thigh|thigh", "Leg", gsub(" (IM|SC) injection", "", .x))
        )
      )
    )

  # Stage 2 vaccination locations check
  # Only first 150 at PCH, but due to delays, 153 had diary card
  st2_vax1 <- extract_tibble(st2_data, "vaccine_administration_v1") |>
    filter(
      substr(record_id, 1, 4) == "4629",
      as.numeric(gsub("4629-", "", record_id)) <= 153
    ) |>
    select(
      record_id,
      sdadmdat,
      vacpara,
      vacparastr,
      vacparadose,
      vacadyn,
      vacadreas
    ) |>
    mutate(visit = 1)
  st2_vax3 <- extract_tibble(st2_data, "vaccine_administration_v3") |>
    filter(
      substr(record_id, 1, 4) == "4629",
      as.numeric(gsub("4629-", "", record_id)) <= 153
    ) |>
    select(
      record_id,
      vac7admdat,
      vac7iploc,
      vac7mmrloc,
      vac7hibloc,
      vac7adyn,
      vac7adspec
    ) |>
    rename(
      sdadmdat = vac7admdat,
      vacadyn = vac7adyn,
      vacadreas = vac7adspec
    ) |>
    mutate(
      sdadmdat = as_date(sdadmdat)
    ) |>
    pivot_longer(
      ends_with("loc"),
      names_pattern = "vac7(ip|mmr|hib)loc",
      names_to = "vaccine",
      values_to = "location"
    ) |>
    mutate(
      visit = 3,
      vaxloc = gsub(
        "deltoid|Deltoid",
        "Arm",
        gsub("thigh|Thigh", "Leg", gsub(" (IM|SC) injection", "", location))
      ),
      vaccine = case_match(
        vaccine,
        "ip" ~ "DTPa-IPV",
        "mmr" ~ "MMR",
        "hib" ~ "HiB"
      ),
    )

  st2_dc1 <- extract_tibble(st2_data, "diary_card_data_page_1")
  st2_dc2 <- extract_tibble(st2_data, "diary_card_data_page_2")
  st2_dc <- left_join(
    st2_dc1,
    st2_dc2,
    join_by(record_id, redcap_event, redcap_data_access_group)
  ) |>
    filter(
      substr(record_id, 1, 4) == "4629",
      as.numeric(gsub("4629-", "", record_id)) <= 153
    ) |>
    mutate(
      visit = case_match(redcap_event, "visit_1" ~ 1, "visit_3" ~ 3),
      vaxage = case_match(visit, 1 ~ "6-week", 3 ~ "18-month"),
      # Fix some locations
      solloca1 = if_else(
        visit == 1 & solloca1 == "Right Leg" & solloca2 == "Right Leg",
        "Left Leg",
        solloca1
      )
    )

  st2_dc_shared <- st2_dc |>
    select(
      record_id,
      visit,
      vaxage,
      diaretyn,
      diaimmscale,
      solvacdat,
      matches("solloca")
    ) |>
    pivot_longer(
      contains("solloc"),
      names_pattern = "solloca([1-3])",
      names_to = "vaxloc_id",
      values_to = "vaxloc"
    ) |>
    filter(!(visit == 1 & vaxloc_id == 3)) |>
    mutate(
      # Participant has "Right Leg" for two diary responses, don't know which is which
      vaxloc = if_else(
        record_id == "4629-106" & visit == 3 & vaxloc == "Right Leg",
        NA_character_,
        vaxloc
      )
    ) |>
    left_join(
      select(st2_vax3, record_id, visit, vaxloc, vaccine) |>
        filter(!is.na(vaxloc)),
      join_by(record_id, visit, vaxloc)
    ) |>
    mutate(
      vaccine = case_when(
        visit == 1 & vaxloc == "Left Leg" ~ "PCv13",
        visit == 1 & vaxloc == "Right Leg" ~ "aP/wP",
        TRUE ~ vaccine
      )
    )

  st2_temp <- st2_dc |>
    select(record_id, visit, vaxage, matches("temp")) |>
    pivot_longer(
      matches("temp"),
      names_pattern = "soltemp([0-6])",
      names_to = "day",
      values_to = "temp"
    ) |>
    mutate(
      temp = as.numeric(if_else(temp == "MI", NA_character_, temp)),
      temp_fac = cut(
        temp,
        c(-Inf, seq(38, 41, 0.5), Inf),
        include.lowest = TRUE,
        right = FALSE,
        labels = c(
          "None (<38)",
          "38.0-38.4",
          "38.5-38.9",
          "39.0-39.4",
          "39.5-39.9",
          "40.0-40.4",
          "40.5-40.9",
          "41.0 or more"
        )
      )
    ) |>
    complete(
      record_id,
      nesting(visit, vaxage),
      day
    )

  st2_sol <- st2_dc |>
    select(record_id, visit, vaxage, matches("sol[0-6]int")) |>
    pivot_longer(
      contains("sol"),
      names_pattern = "sol([0-6])int([1-6])",
      names_to = c("day", "term"),
      values_to = "intensity"
    ) |>
    mutate(
      term = factor(
        term,
        labels = c(
          "Irritability",
          "Vomiting",
          "Diarrhoea",
          "Decreased feeding",
          "Drowsiness",
          "Restlessness"
        )
      ),
      intensity = factor(
        intensity,
        levels = 0:3,
        labels = c("None", "Mild", "Moderate", "Severe")
      )
    ) |>
    complete(
      record_id,
      nesting(visit, vaxage),
      term,
      day
    )

  st2_pain <- st2_dc |>
    select(record_id, visit, vaxage, matches("pain")) |>
    pivot_longer(
      contains("pain"),
      names_pattern = "d([0-6])pain([1-3])",
      names_to = c("day", "vaxloc_id"),
      values_to = "pain"
    ) |>
    mutate(
      pain = factor(
        pain,
        levels = 0:3,
        labels = c("None", "Mild", "Moderate", "Severe")
      )
    ) |>
    complete(
      record_id,
      nesting(visit, vaxage, vaxloc_id),
      day
    ) |>
    filter(!(visit == 1 & vaxloc_id == 3))

  st2_isr <- st2_dc |>
    select(record_id, visit, vaxage, matches("siz")) |>
    pivot_longer(
      contains("siz"),
      names_pattern = c("(rd|sw|hr)d([0-6])siz([1-3])"),
      names_to = c("term", "day", "vaxloc_id"),
      values_to = "size"
    ) |>
    mutate(
      term = factor(
        term,
        levels = c("rd", "sw", "hr"),
        labels = c("Erythema", "Swelling", "Induration")
      ),
      size_fac = cut(
        size,
        breaks = c(-Inf, 0, 10, 25, 50, Inf),
        include.lowest = FALSE,
        right = TRUE,
        labels = c("None", ">0 to 10", ">10 to 25", ">25 to 50", ">50")
      )
    ) |>
    complete(
      record_id,
      nesting(visit, vaxage, vaxloc_id),
      term,
      day
    ) |>
    filter(!(visit == 1 & vaxloc_id == 3))

  # fmt: skip
  tribble(
    ~ "reaction", ~ "data",
    "shared", st2_dc_shared,
    "temp", st2_temp,
    "sol", st2_sol,
    "pain", st2_pain,
    "isr", st2_isr
  )
}

read_stage1_randomisation_list <- function() {
  st1rands <- read_csv(
    file.path(
      Sys.getenv("RDS_PATH"),
      "data",
      "rand",
      "OPTIMUM_STAGE1_randomisation_list.csv"
    ),
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
    list.files(
      file.path(Sys.getenv("RDS_PATH"), "data", "rand"),
      full.names = TRUE
    ),
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

combine_treatment_lists <- function() {
  st1_rand <- read_stage1_randomisation_list()
  st2_rand <- read_stage2_randomisation_list()
  bind_rows(st1_rand, st2_rand)
}

writeLines("Processing forms...", stdout())
dat_rand <- combine_randomisation()
dat_st <- combine_study_termination()
dat_pass <- combine_participant_assessment()
dat_demo <- combine_demographics()
dat_bh <- combine_birth_history()
dat_mh <- combine_medical_history()
dat_vax_v1 <- combine_vax_admin_v1()
dat_vax_v3 <- combine_vax_admin_v3()
dat_spt <- combine_skin_prick_test()
dat_oth_imm <- combine_other_immuno_data()
dat_fc <- combine_food_challenge()
dat_ae <- combine_adverse_events()
dat_sae <- combine_sae()
dat_out <- combine_outcome_report()
dat_food <- combine_food_household()
dat_fha <- combine_family_history_atopy()
dat_bc <- combine_blood_collection()
dat_igg <- combine_igg()
dat_ige <- combine_ige()
dat_diary <- combine_diary()
dat_trt <- combine_treatment_lists()

optimum_data <- list(
  "randomisation" = dat_rand,
  "allocations" = dat_trt,
  "demographics" = dat_demo,
  "birth_history" = dat_bh,
  "medical_history" = dat_mh,
  "vaccine_administration_v1" = dat_vax_v1,
  "vaccine_administration_v3" = dat_vax_v3,
  "skin_prick_test" = dat_spt,
  "other_immunological" = dat_oth_imm,
  "food_challenge" = dat_fc,
  "adverse_events" = dat_ae,
  "serious_adverse_events" = dat_sae,
  "study_termination" = dat_st,
  "participant_assessment" = dat_pass,
  "outcome_report" = dat_out,
  "food_and_household_questionnaire" = dat_food,
  "family_history_of_atopy" = dat_fha,
  "blood_collection" = dat_bc,
  "igg" = dat_igg,
  "ige" = dat_ige,
  "diary" = dat_diary
)

qsave(enframe(optimum_data, name = "form", value = "data"), com_file)
writeLines("Successfully combined databases.", stdout())
