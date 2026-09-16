# Previously consent data (consent-sonia.R) was provided.
# Now, for those with the appropriate consent we would like
# a set of data for the WACRF grant analyses.
# That is, for: OPTIMUM Lab Protocol- Immunobiology of infant vaccine responses
#
# The protocol listed the following fields
# - level of consent
# - age at first pertussis dose
# - randomly assigned treatment group
# - demographic information
# - sex (male/female)
# - breastfeeding status (exclusive/partial/none)
# - birth order (first born yes/no)
# - combined parental income
# - mode of delivery (caesarean/vaginal)
# - antigen specific IgG concentrations at 6, 7, 18, and 19 months old
#
# The purpose of this script is to generate the required data

# --- SETUP

library(dplyr)
library(lubridate)

com_file <- file.path(Sys.getenv("RDS_PATH"), config::get("combined_data_file"))
dat_raw <- qs2::qd_read(com_file)

# We only want stage 1 participants
treatment <- left_join(
  select_form(dat_raw, "randomisation"),
  select_form(dat_raw, "allocations"),
  join_by(rand)
) |>
  filter(rand_stage == 1) |>
  select(record_id, subjid, randdattim, trt)

stage1_subjects <- function(x, var = record_id) {
  inner_join(x, select(treatment, {{ var }}), join_by({{ var }}))
}

# Only use data with appropriate consent level
consent <- select_form(dat_raw, "consent")

# Baseline data
dem <- select_form(dat_raw, "demographics") |>
  select(record_id, birthdat, visdat1, gender, parinc) |>
  mutate(vis1_age_weeks = time_length(interval(birthdat, visdat1), "weeks")) |>
  select(record_id, vis1_age_weeks, gender, parinc) |>
  stage1_subjects()
bh <- select_form(dat_raw, "birth_history") |>
  mutate(
    first_born = recode_values(sibnum, c("Not Applicable", "Unknown", NA) ~ "Yes", default = "No"),
    delt = replace_values(delt, "Forceps/ vacuum assisted delivery" ~ "Forceps/vacuum assisted delivery"),
    caesarean = recode_values(
      delt,
      c("Elective caesarean section", "Emergency caesarean section") ~ "Yes",
      c("Vaginal delivery", "Forceps/vacuum assisted delivery") ~ "No"
    )
  ) |>
  select(record_id, first_born, caesarean) |>
  stage1_subjects()
fh <- select_form(dat_raw, "food_and_household_questionnaire") |>
  filter(visit_age == "6-week") |>
  select(record_id, fecurr) |>
  mutate(
    feeding_status = recode_values(
      fecurr,
      "Exclusively Breastfed" ~ "Exclusive",
      c("Both breastfed and formula-fed", "Both breastfed and started on solids") ~ "Partial",
      "Exclusively formula-fed" ~ "None"
    )
  ) |>
  select(record_id, feeding_status) |>
  stage1_subjects()

join_key <- join_by(record_id)
baseline <- dem |>
  left_join(bh, join_key) |>
  left_join(fh, join_key)

# Outcomes are the IgG concentrations
igg <- select_form(dat_raw, "igg") |>
  select(subjid, visage, antigen, concentration, units) |>
  stage1_subjects(subjid)
