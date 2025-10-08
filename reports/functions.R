gmc <- function(x, ...) 10^mean(log10(x), ...)
gsd <- function(x, ...) 10^sd(log10(x), ...)
expit <- function(x) 1 / (1 + exp(-x))
logit <- function(x) log(x) - log(1 - x)
OR <- function(x, y) x / (1 - x) / (y / (1 - y))
rho <- function(omega) omega^2 / (omega^2 + pi^2 / 3)

fmt_num <- function(x) {
  ifelse(
    abs(x) < 10,
    sprintf("%.2f", x),
    ifelse(
      abs(x) < 100,
      sprintf("%.1f", x),
      sprintf("%.0f", x)
    )
  )
}

fmt_med_iqr <- function(x) {
  sprintf(
    "%.0f (%.0f–%.0f)",
    median(x, na.rm = TRUE),
    quantile(x, 0.25, na.rm = TRUE),
    quantile(x, 0.75, na.rm = TRUE)
  )
}

miss_pattern_trt_tab <- function(dat, var = concentration) {
  pic <- c("X", "-")
  pal <- setNames(c("black", "red"), pic)
  tab <- dat |>
    to_factor() |>
    mutate(missing = pic[1 + as.integer(is.na({{ var }}))]) |>
    select(subjid, stage, trt, visage, missing) |>
    pivot_wider(
      names_from = visage,
      values_from = missing,
      values_fill = "-"
    ) |>
    count(stage, trt, `6-month`, `7-month`, `18-month`, `19-month`) |>
    arrange(trt, desc(n)) |>
    mutate(p = n / sum(n)) |>
    pivot_wider(
      names_from = "trt",
      values_from = c("n", "p"),
      values_fill = 0
    ) |>
    arrange(
      stage,
      desc(`6-month`),
      desc(`7-month`),
      desc(`18-month`),
      desc(`19-month`)
    ) |>
    mutate(stage = paste("Stage", stage))

  gt(tab, groupname_col = "stage") |>
    fmt_percent(starts_with("p_"), decimals = 0) |>
    cols_merge(ends_with("_aP"), pattern = "<<{1} ({2})>>") |>
    cols_merge(ends_with("_wP"), pattern = "<<{1} ({2})>>") |>
    cols_label(
      `n_aP` = md("aP<br>(n = 150)"),
      `n_wP` = md("wP<br>(n = 150)")
    ) |>
    cols_align(align = "center", columns = 1:2) |>
    tab_spanner(
      label = "Count (%)",
      columns = 3:4
    ) |>
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = everything())
    )
}

make_stan_data <- function(dd, form = ~trt) {
  tmp <- dd |>
    filter(row_number() == 1, .by = subjid) |>
    select(-visage) |>
    distinct() |>
    to_factor(drop_unused_labels = TRUE)
  X <- model.matrix(form, data = tmp)
  Y <- dd |>
    select(subjid, visage, log_concentration) |>
    spread(visage, log_concentration) |>
    select(-subjid) |>
    mutate(across(everything(), ~ replace_na(.x, -99))) |>
    as.matrix()
  R <- dd |>
    mutate(
      R = case_when(
        is.na(log_concentration) ~ 1,
        !is.na(log_concentration) ~ 0
      )
    ) |>
    select(subjid, visage, R) |>
    spread(visage, R) |>
    select(-subjid) |>
    as.matrix()
  out <- list(
    N = nrow(tmp),
    J = ncol(Y),
    X = X,
    P = ncol(X),
    Y = Y,
    R = R
  )
  return(out)
}

igg_gmc_summary_tab <- function(
  dat,
  rn = "visage",
  var = concentration,
  var2 = positive
) {
  tab <- dat |>
    summarise(
      obs = sum(!is.na({{ var }})),
      gmc = gmc({{ var }}, na.rm = TRUE),
      gsd = gsd({{ var }}, na.rm = TRUE),
      q1 = quantile({{ var }}, probs = 0.25, na.rm = TRUE),
      q3 = quantile({{ var }}, probs = 0.75, na.rm = TRUE),
      min = min({{ var }}, na.rm = TRUE),
      max = max({{ var }}, na.rm = TRUE),
      pos = mean({{ var2 }}, na.rm = TRUE),
      .by = c(antigen, sym(rn), trt)
    ) |>
    pivot_wider(
      names_from = trt,
      values_from = c(obs, gmc, gsd, q1, q3, min, max, pos),
      names_vary = "slowest"
    ) |>
    to_factor() |>
    arrange(antigen, rn)

  gt(tab, groupname_col = "antigen", rowname_col = rn) |>
    fmt_integer(starts_with("obs")) |>
    fmt_number(starts_with(c("gmc", "gsd")), decimals = 2) |>
    fmt(starts_with(c("q1", "q3")), fns = fmt_num) |>
    fmt(starts_with(c("min", "max")), fns = fmt_num) |>
    fmt_percent(starts_with(c("pos")), decimals = 0) |>
    cols_merge(
      columns = c(min_aP, max_aP),
      pattern = "{1}–{2}"
    ) |>
    cols_merge(
      columns = c(min_wP, max_wP),
      pattern = "{1}–{2}"
    ) |>
    cols_merge(
      columns = c(q1_aP, q3_aP),
      pattern = "{1}–{2}"
    ) |>
    cols_merge(
      columns = c(q1_wP, q3_wP),
      pattern = "{1}–{2}"
    ) |>
    cols_label(
      obs_aP = "n",
      obs_wP = "n",
      gmc_aP = "GMC",
      gmc_wP = "GMC",
      gsd_aP = "GSD",
      gsd_wP = "GSD",
      q1_aP = "Q1–Q3",
      q1_wP = "Q1–Q3",
      min_aP = "Min–Max",
      min_wP = "Min–Max",
      pos_aP = md("S+"),
      pos_wP = md("S+")
    ) |>
    # tab_stub_indent(
    #   rows = everything(),
    #   indent = 1
    # ) |>
    # tab_spanner(
    #   label = md("**aP**<br>(n = 150)"),
    #   columns = ends_with("_aP")
    # ) |>
    # tab_spanner(
    #   label = md("**wP**<br>(n = 150)"),
    #   columns = ends_with("_wP")
    # ) |>
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    ) |>
    tab_footnote(
      footnote = "Geometric mean concentration",
      locations = cells_column_labels(columns = starts_with("gmc"))
    ) |>
    tab_footnote(
      footnote = "Geometric standard deviation",
      locations = cells_column_labels(columns = starts_with("gsd"))
    ) |>
    tab_footnote(
      footnote = "Q1 - 25th sample percentile, Q3 - 75th sample percentile",
      locations = cells_column_labels(columns = starts_with("q1"))
    ) |>
    tab_footnote(
      footnote = "Seroprotection at specified levels",
      locations = cells_column_labels(columns = starts_with("pos"))
    ) |>
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = everything())
    )
}

posterior_gmr_table <- function(gmr) {
  sum_gmr <- gmr |>
    arrange(antigen) |>
    mutate(
      antigen = fct_inorder(antigen),
      age = fct_inorder(age),
      group = fct_inorder(group)
    ) |>
    arrange(antigen, group, age) |>
    mutate(
      mean = mean(trtwP),
      sd = sd(trtwP),
      med = median(trtwP),
      lo = quantile(trtwP, 0.025)[1, ],
      hi = quantile(trtwP, 0.975)[1, ],
      pr = Pr(trtwP > 1),
      pr_ninf = if_else(group == "GMR", Pr(trtwP > 2 / 3), NA_real_)
    ) |>
    select(-trtwP, -group)

  out_tab <- gt(sum_gmr, groupname_col = "antigen", rowname_col = "age") |>
    fmt_number(2:9, decimals = 2) |>
    cols_merge(
      columns = c(mean, sd),
      pattern = "<<{1} ± {2}>>"
    ) |>
    cols_merge(
      columns = c(lo, hi),
      pattern = "<<({1}, {2})>>"
    ) |>
    cols_label(
      age = "Age",
      mean = md("Mean ± std"),
      med = "Median",
      lo = "95% CrI",
      pr = "Pr(> 1)",
      pr_ninf = "Pr(> 2/3)"
    ) |>
    tab_stub_indent(
      rows = everything(),
      indent = 1
    ) |>
    cols_align(
      columns = 2,
      align = "left"
    ) |>
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "---"
    ) |>
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = everything())
    )
  return(out_tab)
}
