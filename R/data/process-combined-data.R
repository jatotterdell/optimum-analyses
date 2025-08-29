# Functions used to derive specific data sets from the raw combined data
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

source(file.path("R", "util.R"))

# Derive a "baseline" dataset, including:
# - randomisation
# - demographics
# - study termination (for date of loss-to-follow-up if applicable)
# - any other baseline form fields as required
get_baseline_data <- function(dat_raw, unblind = FALSE) {
  if (unblind) {
    rnd <- left_join(
      select_form(dat_raw, "randomisation"),
      select_form(dat_raw, "allocations"),
      join_by(rand)
    )
  } else {
    rnd <- select_form(dat_raw, "randomisation")
  }
  rnd |>
    mutate(randdat = date(randdattim)) |>
    select(
      record_id,
      subjid,
      rand_site,
      rand_stage,
      rand,
      randdattim,
      randdat,
      trt
    ) |>
    left_join(
      select_form(dat_raw, "study_termination"),
      join_by(record_id)
    ) |>
    left_join(
      select_form(dat_raw, "demographics") |>
        select(-ptinit, -calcagem, -starts_with("eto")),
      join_by(record_id)
    ) |>
    mutate(
      v1_age_wk = interval(birthdat, visdat1) %/% weeks(1),
      rand_age_wk = interval(birthdat, randdat) %/% weeks(1),
      disc_age_mth = interval(birthdat, discdat) %/% months(1)
    ) |>
    left_join(
      select_form(dat_raw, "birth_history") |>
        select(-starts_with("sibage"), -matches("(prevac|prevdat)[4-5]")),
      join_by(record_id)
    )
}

get_food_allergy <- function(dat_raw) {
  # Source of truth is outcome report
  # But cross check FHQ
  dat_base <- select_form(dat_raw, "demographics")
  dat_out <- select_form(dat_raw, "outcome_report") |>
    arrange(str_rank(record_id, numeric = TRUE))
  dat_fhq <- select_form(dat_raw, "food_and_household_questionnaire")
  dat_ofc <- select_form(dat_raw, "food_challenge") |>
    arrange(str_rank(record_id, numeric = TRUE))

  # Oral food challenge
  dat_out_ofc <- dat_out |>
    filter(outalltp == "Oral food challenge")

  # Anaphylaxis
  dat_out_ana <- dat_out |>
    filter(outalltp == "Anaphylaxis")

  fa_out <- dat_base |>
    select(record_id, birthdat, visdat1) |>
    left_join(
      dat_out |>
        filter(outalltp != "Eczema") |>
        select(
          record_id,
          outallyn,
          outallyn2,
          outalltp,
          outsev,
          outrepdat,
          outawardat,
          outdiagdat,
          outageval,
          outageunit,
          outfrasource,
          outfrasrcespec,
          outfrafood,
          outfraexptp,
          outfrafdamt,
          outfracrdose,
          outfraeldose,
          outfrarxntm,
          outfrarashyn,
          outfrraspec,
          outfrasym1:outfrasym13
        ),
      join_by(record_id)
    ) |>
    mutate(
      diag_age_weeks = interval(birthdat, outdiagdat) %/% weeks(1),
      diag_age_months = interval(birthdat, outdiagdat) %/% months(1)
    )
}

# Derive an "eczema" dataset which considers
# all sources of information:
# - outcome report
# - food and household questionnaire
# - medical history
# - AESI (stage 1 only)
get_eczema_data <- function(dat_raw) {
  dat_base <- select_form(dat_raw, "demographics")
  dat_out <- select_form(dat_raw, "outcome_report")
  dat_fhq <- select_form(dat_raw, "food_and_household_questionnaire")
  dat_mh <- select_form(dat_raw, "medical_history")
  dat_ae <- select_form(dat_raw, "adverse_events")

  # Source of truth is outcome report.
  # However, want to check FHQ and MH for existing eczema and to cross-check outcomes
  ecz_out <- dat_base |>
    select(record_id, birthdat, visdat1) |>
    left_join(
      dat_out |>
        filter(!(!is.na(outallyn) & !outallyn & record_id_num > 1)) |>
        filter(outalltp == "Eczema" | !outallyn) |>
        arrange(record_id, outdiagdat) |>
        select(
          record_id,
          outallyn,
          outalltp,
          outsev,
          outrepdat,
          outawardat,
          outdiagdat,
          outageval,
          outageunit,
          outageval_weeks,
          outageval_months,
          outeczsrce,
          outeczsrcoth,
          outeczmedyn,
          outeczmedtp,
          outeczscoryn,
          outeczscoradno,
          outeczscrorate
        ),
      join_by(record_id)
    ) |>
    mutate(
      v1_age_weeks = interval(birthdat, visdat1) %/% weeks(1),
      ecz_age_weeks = interval(birthdat, outdiagdat) %/% weeks(1),
      ecz_age_months = interval(birthdat, outdiagdat) %/% months(1),
      # If outageval (first allergy) is missing, use the clinician diagnosis date
      outageval_weeks2 = if_else(
        is.na(outageval_weeks) & outalltp == "Eczema",
        ecz_age_weeks,
        outageval_weeks
      ),
      outageval_months2 = if_else(
        is.na(outageval_months) & outalltp == "Eczema",
        ecz_age_months,
        outageval_months
      )
    )

  # Medical history of eczema
  ecz_mh <- dat_mh |>
    filter(
      if_any(starts_with("mhcodelab"), ~ tolower(.) == "eczema") |
        if_any(starts_with("mhdiag"), ~ tolower(.) == "eczema")
    ) |>
    pivot_longer(
      mhdistp1:mhenddat8,
      names_pattern = "mh(distp|diag|code|codelab|stat|stdat|enddat)([1-8])",
      names_to = c(".value", "mh_seq")
    ) |>
    filter(
      grepl("eczema", tolower(diag)) | grepl("eczema", tolower(codelab))
    ) |>
    select(record_id, diag, stat, stdat, enddat) |>
    rename(
      mh_diag = diag,
      mh_stat = stat,
      mh_stdat = stdat,
      mh_enddat = enddat
    ) |>
    mutate(
      mh_diag = tolower(mh_diag),
      mh_ecz = 1
    )

  ecz_fhq <- dat_fhq |>
    arrange(str_rank(record_id, numeric = TRUE), fedat) |>
    select(record_id, visit_age, fedat, feecz, feeczdat, feeczag, feeczstun) |>
    filter(
      feecz %in%
        c(
          "Yes",
          "N/A (child had already been diagnosed with eczema before last visit)",
          "N/A (child has already been diagnosed with eczema before the last visit)"
        )
    ) |>
    # Note 5785-100 has 6 and 9 month survey mixed up,
    # Need to fix in source or fix here prior to this filter step
    filter(row_number() == 1, .by = record_id) |>
    mutate(
      feecz = if_else(grepl("N/A ", feecz), "Previous diagnosis", feecz),
    ) |>
    rename(
      fhq_vis_age = visit_age,
      fhq_date = fedat,
      fhq_ecz = feecz,
      fhq_ecz_date = feeczdat,
      fhq_ecz_age = feeczag,
      fhq_ecz_age_u = feeczstun
    )

  # Only applicable for stage 1
  ecz_ae <- dat_ae |>
    filter(grepl("eczema", tolower(aeterm))) |>
    select(record_id, aestdat) |>
    rename(ae_stdat = aestdat) |>
    mutate(ae_ecz = 1)

  # Merge all sources together and create derived fields
  out <- ecz_out |>
    left_join(ecz_ae, join_by(record_id)) |>
    left_join(ecz_mh, join_by(record_id)) |>
    left_join(ecz_fhq, join_by(record_id)) |>
    mutate(
      mh_ecz_age_weeks = interval(birthdat, mh_stdat) %/% weeks(1),
      fhq_ecz_age_weeks = case_when(
        !is.na(fhq_ecz_date) ~ interval(birthdat, fhq_ecz_date) %/% weeks(1),
        fhq_ecz_age_u == "Weeks" ~ fhq_ecz_age,
        fhq_ecz_age_u == "Months" ~ fhq_ecz_age * 4,
        !is.na(fhq_ecz_age_u) ~ fhq_ecz_age
      ),
      fhq_ecz_age_months = case_when(
        !is.na(fhq_ecz_date) ~ interval(birthdat, fhq_ecz_date) %/% months(1),
        fhq_ecz_age_u == "Months" ~ fhq_ecz_age,
        fhq_ecz_age_u == "Weeks" ~ fhq_ecz_age / 4,
        !is.na(fhq_ecz_age_u) ~ fhq_ecz_age / 4
      ),
      # Was eczema outcome first allergy prior to enrolment?
      out_ecz_preexisting = outageval_weeks2 <= v1_age_weeks,
      # Any pre-existing reported in medical history
      mh_ecz_preexisting = !is.na(mh_stdat) & (mh_stdat <= visdat1),
      # Any pre-existing reported on FHQ
      fhq_ecz_preexisting = !is.na(fhq_ecz_date) & (fhq_ecz_date <= visdat1),
      # Any pre-existing reported?
      ecz_preexisting = out_ecz_preexisting |
        mh_ecz_preexisting |
        fhq_ecz_preexisting,
      # Eczema outcome reported?
      ecz = !is.na(outalltp)
    )
}

get_skin_prick_long <- function(dat_raw) {
  dat_rand <- select_form(dat_raw, "randomisation")
  dat_allo <- select_form(dat_raw, "allocations")
  dat_base <- select_form(dat_raw, "demographics")
  dat_st <- select_form(dat_raw, "study_termination")
  c_allergens <- c(
    "D.pteronyssinus",
    "cat dander",
    "perennial ryegrass",
    "whole egg",
    "cashew",
    "cow's milk",
    "peanut",
    "sesame"
  )
  dat_spt <- select_form(dat_raw, "skin_prick_test") |>
    filter(!is.na(priyn)) |> # Exclude 2 "records" with no data
    filter(!(spt_occasion == "unscheduled" & !priyn)) # Exclude 1 "record" with no data

  # Transform negative/positive controls to long format
  # And constant fields
  dat_spt_1 <- dat_spt |>
    select(
      record_id,
      spt_occasion,
      spt_num,
      unvisyn,
      unvisdat,
      priyn,
      prinspec,
      pridat,
      prinegres:priposres
    ) |>
    pivot_longer(
      prinegres:priposres,
      names_pattern = "pri(neg|pos)res",
      names_to = c(".value")
    ) |>
    rename(spt_neg = neg, spt_pos = pos) |>
    # There's one infant where the SPT date was at 28 months of age
    # This is 18 months later than the reported visit date
    # Use `unvisdat` instead of `pridat` for that participant
    # In all but 3 infants the two fields are equal
    # In those other 2, the difference is only about a week
    mutate(
      pridat = if_else(record_id == "4629-115", unvisdat, pridat)
    )

  # Transform standard panel to long format
  dat_spt_2 <- dat_spt |>
    select(record_id, spt_num, prires1:prireact8) |>
    pivot_longer(
      prires1:prireact8,
      names_pattern = "pri(res|react)([1-9])",
      names_to = c(".value", "spt_tested")
    ) |>
    rename(spt_result = res, spt_reaction = react) |>
    mutate(
      spt_tested = factor(spt_tested, labels = c_allergens)
    )

  # Transform extra allergens tested to long format
  dat_spt_3 <- dat_spt |>
    select(record_id, spt_num, prireact9:prires13) |>
    pivot_longer(
      prireact9:prires13,
      names_pattern = "pri(react|allspec|res)",
      names_to = c(".value")
    ) |>
    filter(!is.na(allspec)) |>
    rename(spt_tested = allspec, spt_reaction = react, spt_result = res) |>
    mutate(spt_tested = tolower(spt_tested))

  # Merge all SPT fields and some baseline fields
  dat_rand |>
    select(record_id, subjid, rand) |>
    left_join(
      select(dat_allo, rand, trt, rand_site, rand_stage),
      join_by(rand)
    ) |>
    left_join(
      dat_base |>
        select(record_id, birthdat, visdat1),
      join_by(record_id)
    ) |>
    left_join(
      select(dat_st, record_id, discdat, streas, stetrreas),
      join_by(record_id)
    ) |>
    left_join(dat_spt_1, join_by(record_id)) |>
    # Fill in for those with missing records
    mutate(
      spt_occasion = replace_na(spt_occasion, "scheduled"),
      spt_num = replace_na(spt_num, 1),
      priyn = replace_na(priyn, FALSE)
    ) |>
    left_join(
      bind_rows(dat_spt_2, dat_spt_3) |>
        mutate(spt_tested = fct_inorder(spt_tested)),
      join_by(record_id, spt_num)
    ) |>
    mutate(
      dis_age = interval(birthdat, discdat) %/% months(1),
      spt_age = interval(birthdat, pridat) %/% months(1),
      spt_0mm = as.numeric(spt_result > 0),
      spt_1mm = as.numeric(spt_result > spt_neg + 1),
      spt_3mm = as.numeric(spt_result >= spt_neg + 3)
    ) |>
    arrange(str_rank(record_id, numeric = TRUE))
}

get_skin_prick_data <- function(dat) {
  dat_spt <- select_form(dat, "skin_prick_test")
  dat_spt <- dat_spt |>
    mutate(
      across(
        starts_with("prires"),
        ~ . > 0,
        .names = "{gsub('prires', 'prisens_0mm_', {.col})}"
      ),
      across(
        starts_with("prires"),
        ~ . > prinegres + 1,
        .names = "{gsub('prires', 'prisens_1mm_', {.col})}"
      ),
      n_prisens_1mm = rowSums(pick(starts_with("prisens_1mm_")), na.rm = TRUE),
      any_prisens_1mm = n_prisens_1mm > 0,
      across(
        starts_with("prires"),
        ~ . >= prinegres + 3,
        .names = "{gsub('prires', 'prisens_3mm_', {.col})}"
      ),
      n_prisens_3mm = rowSums(pick(starts_with("prisens_3mm_")), na.rm = TRUE),
      any_prisens_3mm = n_prisens_3mm > 0,
    )
  # Collect all positive skin prick test allergens
  c_allergens <- c(
    "D.pteronyssinus",
    "cat dander",
    "perennial ryegrass",
    "whole egg",
    "cashew",
    "cow's milk",
    "peanut",
    "sesame"
  )
  dat_spt_str <- dat_spt |>
    select(
      record_id,
      spt_num,
      contains("_0mm_"),
      contains("_1mm_"),
      contains("_3mm_"),
      contains("priallspec")
    ) |>
    rowwise() |>
    mutate(
      pri_0mm_str = str_replace_all(
        str_replace_all(
          paste(
            c(c_allergens, c_across(priallspec9:priallspec13))[c_across(
              prisens_0mm_1:prisens_0mm_13
            )],
            collapse = ", "
          ),
          ", NA",
          ""
        ),
        "NA, ",
        ""
      ),
      pri_1mm_str = str_replace_all(
        str_replace_all(
          paste(
            c(c_allergens, c_across(priallspec9:priallspec13))[c_across(
              prisens_1mm_1:prisens_1mm_13
            )],
            collapse = ", "
          ),
          ", NA",
          ""
        ),
        "NA, ",
        ""
      ),
      pri_3mm_str = str_replace_all(
        str_replace_all(
          paste(
            c(c_allergens, c_across(priallspec9:priallspec13))[c_across(
              prisens_3mm_1:prisens_3mm_13
            )],
            collapse = ", "
          ),
          ", NA",
          ""
        ),
        "NA, ",
        ""
      )
    ) |>
    select(record_id, spt_num, pri_0mm_str, pri_1mm_str, pri_3mm_str)
  dat_spt |>
    left_join(dat_spt_str, join_by(record_id, spt_num))
}
