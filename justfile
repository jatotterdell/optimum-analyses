# List recipes
default:
  just --list --unsorted

# Mount RDS data to location
mount-rds:
  sudo mount -t drvfs '\\shared.sydney.edu.au\research-data' /mnt/share

# Extract raw REDCap data using API and save dated qs file
extract-raw-redcap-data:
  Rscript R/data/redcap-data-raw.R

# Combine REDCap and Medrio data
combine-redcap-medrio-data:
  Rscript R/data/combine-redcap-medrio-data.R

render-manuscript:
  quarto render

render-report:
  quarto render --profile report reports/igg-revisited.qmd
