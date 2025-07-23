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

# Render manuscript
render-manuscript:
  quarto render

# Render IgG report
render-report-igg:
  quarto render --profile report reports/igg-revisited.qmd

# Render report investigating PRN stage 1 and 2 differences
render-report-prn:
  quarto render --profile report reports/stage1-vs-stage2.qmd
