suppressPackageStartupMessages({
  # Data
  library(here)
  # library(qs)
  library(qs2)
  library(dplyr)
  library(forcats)
  library(lubridate)
  library(labelled)
  library(arrow)

  # Figures
  library(ggplot2)
  library(ggdist)
  library(showtext)
  library(patchwork)
  library(legendry)
  library(ggh4x)
  library(scales)
  library(ggsurvfit)

  # Tables
  library(gt)
  library(gtsummary)

  # Models
  library(cmdstanr)
  library(posterior)
  library(brms)
  library(tidybayes)
  library(distributional)
  library(brmsmargins)
  library(survival)
})

options(brms.backend = "cmdstanr", mc.cores = 8)

readRenviron(here(".env"))
source(here("R", "util.R"))
source(here("R", "data", "process-combined-data.R"))

sysfonts::font_add(
  family = "TeX Gyre Pagella",
  regular = "texgyrepagella-regular.otf",
  bold = "texgyrepagella-bold.otf",
  italic = "texgyrepagella-italic.otf",
  bolditalic = "texgyrepagella-bolditalic.otf"
)

theme_set(
  theme_bw(base_size = 10, base_family = "TeX Gyre Pagella") +
    theme(panel.grid.minor = element_blank())
)

theme_gtsummary_compact(font_size = 10)

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
