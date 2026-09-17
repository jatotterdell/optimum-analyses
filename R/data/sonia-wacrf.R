# Previously consent data (consent-sonia.R) was provided.
# Now, for those with the appropriate consent we would like
# a set of data for the WACRF grant analyses.
# That is, for: OPTIMUM Lab Protocol- Immunobiology of infant vaccine responses
#
# The protocol listed the following fields
# - level of consent
# - age at first pertussis dose
# - randomly assigned treatment group
# - sex (male/female)
# - breastfeeding status (exclusive/partial/none)
# - birth order (first born yes/no)
# - combined parental income
# - mode of delivery (caesarean/vaginal)
# - antigen specific IgG concentrations at 6, 7, 18, and 19 months old
# - maternal age (not available)
# - Tdap boosters (number, type, gestational age at in preceding pregnancy)
#   - inmpertvp
#   - gestwp
#   - gestdp
#   - vacad
#   - prevpert
#   - prevnum
#   - prevdat1-5 prevac1-5
# - other maternal vaccinations
#   - minfvp
#   - gestwi
#   - gestdi
# - maternal antibiotics
#   - ipab
# - maternal parity
# -
#
# The purpose of this script is to generate the required data

# --- SETUP

library(here)
library(dplyr)
library(lubridate)

readRenviron(here(".env"))

source(here("R", "util.R"))

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

# CONSENT ----
consent <- select_form(dat_raw, "consent")
consent <- left_join(
  consent,
  select(select_form(dat_raw, "demographics"), record_id, birthdat),
  join_by(record_id)
) |>
  mutate(
    cons_age_weeks = time_length(interval(birthdat, cons_date), "weeks"),
    cons_age_months = time_length(interval(birthdat, cons_date), "months"),
    cons_rnaseq = cons_spec == "Parent has consented to this study + future research INCLUDING gene expression studies",
    cons_cellular = cons_spec %in%
      c(
        "Parent has consented to this study + future research INCLUDING gene expression studies",
        "Parent has consented to this study + future research EXCLUDING gene expression studies"
      )
  ) |>
  select(-birthdat)

# BASELINE ----
vax1 <- select_form(dat_raw, "vaccine_administration_v1") |>
  select(record_id, vacpara) |>
  stage1_subjects()
dem <- select_form(dat_raw, "demographics") |>
  select(record_id, birthdat, visdat1, gender, parinc) |>
  mutate(vis1_age_weeks = time_length(interval(birthdat, visdat1), "weeks")) |>
  select(record_id, birthdat, vis1_age_weeks, gender, parinc) |>
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
  select(
    record_id,
    mpar,
    inmpertvp,
    gestwp,
    gestdp,
    vacad,
    prevpert,
    prevnum,
    matches("prevdat[1-2]"),
    matches("prevac[1-2]"),
    minfvp,
    gestwi,
    gestdi,
    ipab,
    gestadelw,
    gestadeld,
    delt,
    caesarean,
    wgt,
    neoab,
    first_born
  ) |>
  stage1_subjects()
fh <- select_form(dat_raw, "food_and_household_questionnaire") |>
  filter(visit_age == "6-week") |>
  select(record_id, fecurr, febfever, febfform) |>
  stage1_subjects()

join_key <- join_by(record_id)
baseline <- treatment |>
  left_join(vax1, join_key) |>
  left_join(dem, join_key) |>
  left_join(bh, join_key) |>
  left_join(fh, join_key)


# BLOOD COLLECTION ----
bc <- select_form(dat_raw, "blood_collection") |>
  stage1_subjects() |>
  filter(visage != "6-month + 72hrs") |>
  arrange(record_id, visage) |>
  select(record_id, visage, lbipvst, lbncireas, lbvol, lbsite, lbipdat, lbiptim) |>
  mutate(
    lbipdat = replace_when(lbipdat, record_id == "96" & visage == "7-month" ~ date("2020-01-10"))
  )
bc <- left_join(
  select(baseline, record_id, subjid, birthdat),
  bc,
  join_by(record_id)
) |>
  mutate(
    bc_age_months = time_length(interval(birthdat, lbipdat), "months"),
    # If lbipdat is missing, just fill in with the planned date for consent merge
    bc_age_months = replace_when(bc_age_months, is.na(bc_age_months) ~ as.numeric(gsub("-month", "", visage)))
  )

bc_consent <- left_join(bc, consent, join_by(record_id, closest(bc_age_months >= cons_age_months)))

# IGG CONCENTRATIONS ----
igg <- select_form(dat_raw, "igg") |>
  select(subjid, visage, antigen, concentration, units) |>
  stage1_subjects(subjid)
igg <- left_join(
  bc_consent,
  igg,
  join_by(subjid, visage)
) |>
  arrange(subjid, antigen, visage)

data_dictionary <- tribble(
  ~field            , ~description                                                                                             ,
  "record_id"       , "Record ID"                                                                                              ,
  "subjid"          , "Subject ID"                                                                                             ,
  "randdattim"      , "Datetime of randomisation (datetime of vaccination at visit 1)"                                         ,
  "trt"             , "Assigned treatment group"                                                                               ,
  "vacpara"         , "Paracetamol given?"                                                                                     ,
  "vis1_age_weeks"  , "Age at visit 1 vaccination (weeks)"                                                                     ,
  "gender"          , "Gender"                                                                                                 ,
  "parinc"          , "Combined parental income status"                                                                        ,
  "mpar"            , "Birth mother parity"                                                                                    ,
  "inmpertvp"       , "Maternal pertussis vaccination during preceding pregnancy?"                                             ,
  "gestwp"          , "Pertussis vaccine at gestation (weeks)"                                                                 ,
  "gestdp"          , "Pertussis vaccine at gestation (days)"                                                                  ,
  "vacad"           , "Maternal pertussis vaccine administered"                                                                ,
  "prevpert"        , "Were other maternal pertussis vaccination within the last 5 years (not including preceeding pregnancy)" ,
  "prevnum"         , "Number of maternal pertussis vaccines in previous 5 years)"                                             ,
  "prevdat1"        , "Previous pertussis vaccine date 1"                                                                      ,
  "prevac1"         , "Previous pertussis vaccine administered 1"                                                              ,
  "prevdat2"        , "Previous pertussis vaccine date 2"                                                                      ,
  "prevac2"         , "Previous pertussis vaccine administered 2"                                                              ,
  "minfvp"          , "Maternal seasonal influenza vaccination during pregnancy?"                                              ,
  "gestwi"          , "Gestation (weeks) for maternal influenza vaccine"                                                       ,
  "gestdi"          , "Gestation (days) for maternal influenza vaccine"                                                        ,
  "ipab"            , "Intrapartum antibiotics?"                                                                               ,
  "gestadelw"       , " Gestational age at delivery (weeks)"                                                                   ,
  "gestadeld"       , " Gestational age at delivery (days)"                                                                    ,
  "delt"            , "Delivery type"                                                                                          ,
  "wgt"             , "Birth weight (grams)"                                                                                   ,
  "neoab"           , "Neonatal systemic antibiotics (first 28 days of life)?"                                                 ,
  "first_born"      , "First born child"                                                                                       ,
  "fecurr"          , "Child is currently being... (breastfeeding status)"                                                     ,
  "febfever "       , "Ever breastfed?"                                                                                        ,
  "febfform"        , "Has your child ever been given formula?"                                                                ,
  "cons_seq"        , "Consent sequence"                                                                                       ,
  "cons_type"       , "Type of consent sequence"                                                                               ,
  # "cons_date"       , "Date of consent"                                                                                        ,
  "cons_spec"       , "Consent specification"                                                                                  ,
  "cons_age_weeks"  , "Age at consent (weeks)"                                                                                 ,
  "cons_age_months" , "Age at consent (months)"                                                                                ,
  "cons_ranseq"     , "Consent for RNAseq"                                                                                     ,
  "cons_cellular"   , "Consent for cellular analysis"                                                                          ,
  "visage"          , "Scheduled visit age"                                                                                    ,
  "lbipvst"         , "Has a Blood Sample been obtained?"                                                                      ,
  "lbncireas"       , "Reason not collected"                                                                                   ,
  "lbvol"           , " Volume obtained (mL)"                                                                                  ,
  "lbsite"          , "Collection site"                                                                                        ,
  "bc_age_months"   , "Age at blood collection (months)"                                                                       ,
  "antigen"         , "Antigen type"                                                                                           ,
  "concentration"   , "IgG concentration"                                                                                      ,
  "units"           , "Concentration units"
)

datasets <- list(
  "consent" = select(consent, -cons_date),
  "baseline" = select(baseline, -birthdat),
  "igg" = select(igg, -c(birthdat, lbipdat, cons_date)),
  "dictionary" = data_dictionary
)
