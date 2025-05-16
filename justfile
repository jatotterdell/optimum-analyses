# List recipes
default:
  just --list --unsorted

# Extract raw REDCap data using API and save dated qs file
extract-raw-redcap-data:
  Rscript R/data/redcap-data-raw.R

# Combine REDCap and Medrio data
combine-redcap-medrio-data:
  Rscript R/data/combine-redcap-medrio-data.R