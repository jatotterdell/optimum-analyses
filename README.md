# OPTIMUM Analyses

Repository for the final analyses of the OPTIMUM trial data.

## Data Processing

The raw Medrio data was manually exported using the web portal and is stored on the RDS in `\\shared.sydney.edu.au\research-data\PRJ-OPTIMUM\data\raw\stage1`.

The raw REDCap data is exported using the API via `R/data/redcap-data-raw.R` and stored in `\\shared.sydney.edu.au\research-data\PRJ-OPTIMUM\data\raw\stage2`.

The two datasets are combined into tibbles for each form via `R/data/combine-redcap-medrio-data.R`.

Forms to be mapped between MedDRIO and REDCap (and their current status) are:

- [x] randomisation
- [x] demographics
- [x] study_termination
- [x] birth_history
- [x] medical_history
- [x] medications
- [ ] family_history_of_atopy
- [x] physical_examination_v1
- [x] vaccine_administration_v1
- [x] participant_assessment
  - visit 2, 3, and 4
- [ ] food_and_household_questionnaire
  - visit 1, 2, and 3, 6-month, 9-month, 15-month phone contact
- [ ] skin_prick_test
- [ ] physical_examination
- [ ] vaccine_administration_v2
- [ ] vaccine_administration_v3
- [ ] outcome_report
- [ ] primary_outcome_status
- [ ] nonstudy_vaccination_log
- [ ] sae_reporting_log
- [ ] adverse_events
- [ ] concomitant_medications
- [ ] diary_card_data_page_1
- [ ] diary_card_data_page_2
- [ ] other_immunological_data
