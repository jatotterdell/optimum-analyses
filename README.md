# OPTIMUM Analyses

Repository for the final analyses of the OPTIMUM trial data.

## Data Processing

The stage 1 data for OPTIMUM (first 150 participants) was stored in a Medrio database. 
The raw Medrio data was manually exported using the web portal and stored on the RDS in `\\shared.sydney.edu.au\research-data\PRJ-OPTIMUM\data\raw\stage1`.

The stage 2 data was stored in a REDCap database. 
The raw REDCap data is exported using the API via `R/data/redcap-data-raw.R` and stored in `\\shared.sydney.edu.au\research-data\PRJ-OPTIMUM\data\raw\stage2`.

The two databases have some differences between them in the available forms, fields, codings, and form structure. 
The necessary forms from the two databases are combined into dataframes, one for each form, via `R/data/combine-redcap-medrio-data.R`.
This is then stored as `PRJ-OPTIMUM/data/derived/optimum-data.qs"`.

In particular, the schedule of assessment was different between stage 1 and 2.

**In stage 1**:

- visit 1 (6-weeks)
- visit 2 (4-months)
- visit 3 (6-months)
- visit 4 (6-months + 72 hrs) [optional visit]
- visit 5 (7-months)
- visit 6 (12-months)
- visit 7 (18-months)
- visit 8 (19-months)

**In stage 2**:

- visit 1 (6-weeks)
- visit 2 (12-months)
- visit 3 (18-months)
- visit 4 (19-months) [first 150 at PCH only]

Further data processing functions (e.g. deriving outcomes) are defined in `R/data/process-combined-data.R`.

Forms to be mapped between Medrio and REDCap (and their current status) are:

- [x] randomisation
- [x] demographics
- [x] study_termination
- [x] birth_history
- [x] medical_history
- [x] medications
- [x] family_history_of_atopy
- [x] physical_examination_v1
- [x] vaccine_administration_v1
- [x] participant_assessment
  - visit 2, 3, and 4
- [x] food_and_household_questionnaire
  - visit 1, 2, and 3, 6-month, 9-month, 15-month phone contact
- [x] skin_prick_test
- [x] food_challenge
- [ ] physical_examination
- [ ] vaccine_administration_v2
- [x] vaccine_administration_v3
- [x] outcome_report
- [x] primary_outcome_status
- [x] nonstudy_vaccination_log
- [x] sae_reporting_log
- [x] adverse_events
- [ ] concomitant_medications
- [ ] diary_card_data_page_1
- [ ] diary_card_data_page_2
- [x] other_immunological_data

## Reporting

Immunogenicity and reactogenecity (IgE, IgG, and diary card solicited adverse reactions responses) have previously been reported elsewhere. 
The focus here is on summarising the characteristics of all sampled participants and analyses of the remaining outcomes to be reported. 
The relevant elements are (see [https://doi.org/10.1136/bmjopen-2020-042838](https://doi.org/10.1136/bmjopen-2020-042838)):

- [x] sample characteristics
  - [x] at baseline
- [x] primary outcome of IgE mediated food allergy by 18 months of age
- secondary outcomes:
  - [x] new onset eczema by 6 or 12 months of age with positive skin prick test
  - [x] sensitisation to at least one allergen by 12 and up to 18 months of age
    - [x] 1mm greater than negative control
    - [x] 3mm greater than or equal to negative control
- [x] SAE listing
- [x] AE summaries
- [ ] diary card data
- [x] IgG data (in a standalone report)

## Notebooks

Files in notebooks were working documents for interactive checking of outcome derivations and can be ignored.
These were not kept up to date with current data processing so may not run without error and are not used for trial reporting.
