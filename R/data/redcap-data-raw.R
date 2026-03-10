library(REDCapTidieR)
library(REDCapR)
library(dplyr)
library(qs)

options(redcaptidier.allow.mdc = TRUE)

readRenviron(".env")

# Some test records remain in the REDCap database.
# Need to filter these out
filter_test_records <- function(dat) {
  filter(dat, !record_id %in% c("3", "23", "24", "25", "26"))
}

# Unfortunately, REDCapTidieR does not export labelled MedDRA coded terms
# Need to extract adverse events separately for these and add them in
get_ae_medra_terms <- function() {
  aecodes <- redcap_read(
    redcap_uri = Sys.getenv("REDCAP_URI"),
    token = Sys.getenv("REDCAP_KEY"),
    forms = "adverse_events",
    events = "adverse_events_arm_1",
    raw_or_label = "label",
    verbose = FALSE
  )$data
  aecodes |>
    select(record_id, redcap_repeat_instance, aecode1, aecode2, aecode3) |>
    rename(redcap_form_instance = redcap_repeat_instance)
}

# The same for the SAE log
get_sae_medra_terms <- function() {
  aecodes <- redcap_read(
    redcap_uri = Sys.getenv("REDCAP_URI"),
    token = Sys.getenv("REDCAP_KEY"),
    forms = "sae_reporting_log",
    events = "sae_log_arm_1",
    raw_or_label = "label",
    verbose = FALSE
  )$data
  aecodes |>
    select(
      record_id,
      redcap_repeat_instance,
      saemeddra,
      saemeddra_2,
      saemeddra_3
    ) |>
    rename(redcap_form_instance = redcap_repeat_instance)
}

get_mh_medra_terms <- function() {
  redcap_read(
    redcap_uri = Sys.getenv("REDCAP_URI"),
    token = Sys.getenv("REDCAP_KEY"),
    forms = "medical_history",
    events = "visit_1_arm_1",
    raw_or_label = "label",
    verbose = FALSE
  )$data |>
    select(record_id, starts_with("mhcode")) |>
    rename_with(~ gsub("mhcode", "mhcodelab", .x), starts_with("mhcode"))
}

get_redcap_data <- function(append = "") {
  data_path <- file.path(Sys.getenv("RDS_PATH"), "data", "raw", "stage2")

  redcap_forms <- c(
    "demographics",
    "eligibility_criteria",
    "infant_eligibility_criteria",
    "participant_assessment",
    "randomisation",
    "birth_history",
    "medical_history",
    "medications",
    "family_history_of_atopy",
    "review_of_participants_6week_check",
    "physical_examination_v1",
    "vaccine_administration_v1",
    "vaccine_administration_v2",
    "vaccine_administration_v3",
    "nonstudy_vaccination_log",
    "food_and_household_questionnaire",
    "skin_prick_test",
    "food_challenge",
    "blood_sample_collection",
    "physical_examination",
    "study_termination",
    "outcome_report",
    "primary_outcome_status",
    "concomitant_medications",
    "diary_card_data_page_1",
    "diary_card_data_page_2",
    "adverse_events",
    "sae_reporting_log",
    "other_immunological_data",
    "antibody_results"
  )

  dat_raw <- read_redcap(
    Sys.getenv("REDCAP_URI"),
    Sys.getenv("REDCAP_KEY"),
    raw_or_label = "raw",
    allow_mixed_structure = TRUE,
    forms = redcap_forms
  ) |>
    make_labelled() |>
    mutate(redcap_data = lapply(redcap_data, filter_test_records))

  dat_labelled <- read_redcap(
    Sys.getenv("REDCAP_URI"),
    Sys.getenv("REDCAP_KEY"),
    raw_or_label = "label",
    allow_mixed_structure = TRUE,
    forms = redcap_forms
  ) |>
    make_labelled() |>
    mutate(redcap_data = lapply(redcap_data, filter_test_records))

  # Fix MedDRA code labels for aecode1-3
  dat_ae_codes <- get_ae_medra_terms()
  dat_ae <- dat_labelled |>
    extract_tibble("adverse_events") |>
    select(-starts_with("aecode")) |>
    left_join(dat_ae_codes, join_by(record_id, redcap_form_instance)) |>
    relocate(starts_with("aecode"), .after = aeterm)
  dat_labelled <- dat_labelled |>
    mutate(
      redcap_data = if_else(
        redcap_form_name == "adverse_events",
        list(dat_ae),
        redcap_data
      )
    )
  # Add MeDRA codes for SAE
  dat_sae_codes <- get_sae_medra_terms()
  dat_sae <- dat_labelled |>
    extract_tibble("sae_reporting_log") |>
    select(-starts_with("saemeddra")) |>
    left_join(dat_sae_codes, join_by(record_id, redcap_form_instance)) |>
    relocate(starts_with("saemeddra"), .after = saeaeterm)
  dat_labelled <- dat_labelled |>
    mutate(
      redcap_data = case_when(
        redcap_form_name == "adverse_events" ~ list(dat_ae),
        redcap_form_name == "sae_reporting_log" ~ list(dat_sae),
        .default = redcap_data
      )
    )

  # Add MeDRA codes for medical history terms
  dat_mh_codes <- get_mh_medra_terms()
  dat_mh <- dat_labelled |>
    extract_tibble("medical_history") |>
    left_join(dat_mh_codes, join_by(record_id)) |>
    relocate(mhcodelab1, .after = mhcode1) |>
    relocate(mhcodelab2, .after = mhcode2) |>
    relocate(mhcodelab3, .after = mhcode3) |>
    relocate(mhcodelab4, .after = mhcode4) |>
    relocate(mhcodelab5, .after = mhcode5) |>
    relocate(mhcodelab6, .after = mhcode6) |>
    relocate(mhcodelab7, .after = mhcode7) |>
    relocate(mhcodelab8, .after = mhcode8)
  dat_labelled <- dat_labelled |>
    mutate(
      redcap_data = if_else(
        redcap_form_name == "medical_history",
        list(dat_mh),
        redcap_data
      )
    )

  qsave(dat_raw, file.path(data_path, paste0("redcap-raw", append, ".qs")))
  qsave(
    dat_labelled,
    file.path(data_path, paste0("redcap-labelled", append, ".qs"))
  )
}

get_redcap_data(paste0("-", Sys.Date()))
