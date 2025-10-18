# =======================================================================
# SPC.R — Statistical Process Control (SPC) Toolkit for Datacenter Timeseries
# Works with long data containing at least:
# ts, asset_id, asset_type, server_id, rack_id, room_id,
# asset_temp_inlet_c, rack_inlet_c, room_temp_c, server_temp_c
# Optional: t_idx (integer time index). If missing, it's derived from ts.
# =======================================================================

# ---- Libraries --------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(moments)

# ---- Theme ------------------------------------------------------------
set_theme <- function(){
  theme_set(
    theme_classic(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(fill = NA, color = "grey"),
        plot.margin = margin(r = 0)
      )
  )
}

# ---- Helpers ----------------------------------------------------------
# Ensure we have an integer time index (t_idx). If absent, derive from ts "t_###".
ensure_t_idx <- function(df){
  if (!"t_idx" %in% names(df)) {
    stopifnot("ts" %in% names(df))
    df <- df %>% mutate(t_idx = as.integer(sub("^t_", "", .data$ts)))
  }
  df
}

describe <- function(x){
  tibble(x) %>%
    summarize(
      mean = mean(x, na.rm = TRUE),
      sd   = sd(x, na.rm = TRUE),
      skew = moments::skewness(x, na.rm = TRUE),
      kurtosis = moments::kurtosis(x, na.rm = TRUE)
    ) %>%
    mutate(
      caption = paste0(
        "Process Mean: ", round(mean,2), " | ",
        "SD: ", round(sd,2), " | ",
        "Skewness: ", round(skew,2), " | ",
        "Kurtosis: ", round(kurtosis,2)
      )
    )
}

# Safe subgroup summarizer (handles NA/Inf and variable subgroup sizes)
.get_subgroup_stats <- function(x, y) {
  tibble(x = x, y = y) %>%
    group_by(x) %>%
    summarise(
      n_valid = sum(is.finite(y)),
      xbar = ifelse(n_valid > 0, mean(y[is.finite(y)], na.rm = TRUE), NA_real_),
      r    = ifelse(n_valid > 0, diff(range(y[is.finite(y)], na.rm = TRUE)), NA_real_),
      s    = ifelse(n_valid > 1, sd(y[is.finite(y)], na.rm = TRUE), NA_real_),
      nw   = n_valid,
      df   = pmax(nw - 1, 0),
      .groups = "drop"
    )
}

# ---- Subgroup/Total stats --------------------------------------------
get_stat_s <- function(x,y){
  stat <- .get_subgroup_stats(x, y)
  sigma_t <- sd(y, na.rm = TRUE)
  stat %>%
    mutate(
      xbbar   = mean(xbar, na.rm = TRUE),
      sigma_s = sqrt(sum(df * s^2, na.rm = TRUE) / pmax(sum(df, na.rm = TRUE), 1)),
      sigma_t = sigma_t,
      se      = sigma_s / sqrt(pmax(nw, 1)),
      upper   = xbbar + 3*se,
      lower   = xbbar - 3*se
    )
}

get_stat_t <- function(x,y){
  stat <- .get_subgroup_stats(x, y)
  sigma_t <- sd(y, na.rm = TRUE)
  stat %>%
    summarize(
      xbbar   = mean(xbar, na.rm = TRUE),
      rbar    = mean(r,    na.rm = TRUE),
      sbar    = mean(s,    na.rm = TRUE),
      sigma_s = sqrt(sum(df * s^2, na.rm = TRUE) / pmax(sum(df, na.rm = TRUE), 1)),
      sigma_t = sigma_t,
      n       = sum(nw, na.rm = TRUE)
    )
}

# ---- Control constants via simulation (pooled across reps) -----------
dn <- function(n = 12, reps = 1e4){
  tibble(rep = 1:reps) %>%
    group_by(rep) %>%
    summarise(r = abs(diff(range(rnorm(n, 0, 1)))), .groups = "drop") %>%
    summarise(
      d2 = mean(r),
      d3 = sd(r),
      D3 = pmax(0, 1 - 3*(d3/d2)),
      D4 = 1 + 3*(d3/d2)
    )
}

bn <- function(n, reps = 1e4){
  tibble(rep = 1:reps) %>%
    group_by(rep) %>%
    summarise(s = sd(rnorm(n, 0, 1)), .groups = "drop") %>%
    summarise(
      b2 = mean(s),
      b3 = sd(s),
      C4 = b2,
      A3 = 3 / (b2 * sqrt(n)),
      B3 = pmax(0, 1 - 3 * b3/b2),
      B4 = 1 + 3 * b3/b2
    )
}

# ---- Limits (Xbar, S, R, MR) -----------------------------------------
limits_avg <- function(x,y){
  stat_s <- .get_subgroup_stats(x, y) %>% filter(nw >= 2)
  if (nrow(stat_s) == 0)
    return(stat_s %>% mutate(lower = NA_real_, upper = NA_real_, sbar = NA_real_, xbbar = NA_real_))
  constants <- stat_s %>%
    distinct(nw) %>%
    rowwise() %>%
    mutate(tmp = list(bn(n = nw, reps = 1e4))) %>%
    unnest_wider(tmp) %>%
    ungroup() %>%
    select(nw, A3)
  stat_s %>%
    left_join(constants, by = "nw") %>%
    mutate(
      sbar  = sqrt(sum(df * s^2, na.rm=TRUE) / pmax(sum(df, na.rm=TRUE), 1)),
      xbbar = mean(xbar, na.rm=TRUE),
      lower = xbbar - A3 * sbar,
      upper = xbbar + A3 * sbar
    )
}

limits_s <- function(x,y){
  stat_s <- .get_subgroup_stats(x, y) %>% filter(nw >= 2)
  if (nrow(stat_s) == 0)
    return(stat_s %>% mutate(lower = NA_real_, upper = NA_real_, sbar = NA_real_))
  constants <- stat_s %>%
    distinct(nw) %>%
    rowwise() %>%
    mutate(tmp = list(bn(n = nw, reps = 1e4))) %>%
    unnest_wider(tmp) %>%
    ungroup() %>%
    select(nw, B3, B4)
  stat_s %>%
    left_join(constants, by = "nw") %>%
    mutate(
      sbar  = mean(s, na.rm=TRUE),
      lower = B3 * sbar,
      upper = B4 * sbar
    )
}

limits_r <- function(x,y){
  stat_s <- .get_subgroup_stats(x, y) %>% filter(nw >= 2)
  if (nrow(stat_s) == 0)
    return(stat_s %>% mutate(lower = NA_real_, upper = NA_real_, rbar = NA_real_))
  constants <- stat_s %>%
    distinct(nw) %>%
    rowwise() %>%
    mutate(tmp = list(dn(n = nw, reps = 1e4))) %>%
    unnest_wider(tmp) %>%
    ungroup() %>%
    select(nw, D3, D4)
  stat_s %>%
    left_join(constants, by = "nw") %>%
    mutate(
      rbar  = mean(r, na.rm=TRUE),
      lower = D3 * rbar,
      upper = D4 * rbar
    )
}

# Proper MR(2): LCL=0, UCL=3.267*MRbar
limits_mr <- function(x,y){
  data  <- tibble(x=x,y=y)
  data2 <- data %>% reframe(x = x[-1], mr = abs(diff(y)))
  mrbar <- mean(data2$mr, na.rm = TRUE)
  tibble(
    x     = data2$x,
    mr    = data2$mr,
    mrbar = mrbar,
    lower = 0,
    upper = 3.267 * mrbar
  )
}

# ---- Plot guards ------------------------------------------------------
.ggx_empty <- function(msg = "No subgroups with ≥ 2 valid points"){
  ggplot() + annotate("text", x = 0, y = 0, label = msg) + theme_void(base_size = 14)
}

# ---- Plots (Xbar, S, R, MR) ------------------------------------------
ggxbar <- function(x,y, xlab = "Subgroup", ylab = "Average", show_labels = TRUE){
  stat_s <- get_stat_s(x, y)
  stat_t <- get_stat_t(x, y)
  lims   <- limits_avg(x, y)
  if (nrow(lims) == 0 || all(!is.finite(lims$upper))) return(.ggx_empty())
  
  labels <- tibble(
    x    = max(lims$x, na.rm = TRUE),
    xb   = mean(lims$xbbar, na.rm = TRUE),
    up   = max(lims$upper,  na.rm = TRUE),
    lo   = min(lims$lower,  na.rm = TRUE)
  ) %>% pivot_longer(c(xb, up, lo), names_to = "name", values_to = "value")
  
  gg <- ggplot() +
    geom_hline(data = stat_t, aes(yintercept = xbbar), color = "lightgrey") +
    geom_ribbon(data = lims, aes(x = x, ymin = lower, ymax = upper),
                fill = "steelblue", alpha = 0.2) +
    geom_line(data = stat_s, aes(x = x, y = xbar), linewidth = 0.5) +
    geom_point(data = stat_s, aes(x = x, y = xbar), size = 2) +
    labs(x = xlab, y = ylab, subtitle = "Average Chart")
  if (isTRUE(show_labels)) {
    labmap <- c(xb = "xbbar", up = "+3 s", lo = "-3 s")
    gg <- gg + geom_label(
      data = labels,
      aes(x = x, y = value, label = paste0(labmap[name], " = ", round(value,2))),
      hjust = 1, alpha = 0.5
    )
  }
  gg
}

ggs <- function(x,y, xlab = "Subgroup", ylab = "Standard Deviation", show_labels = TRUE){
  lims <- limits_s(x, y)
  stat <- .get_subgroup_stats(x, y)
  if (nrow(lims) == 0 || all(!is.finite(lims$upper))) return(.ggx_empty())
  
  labels <- tibble(
    x    = max(lims$x, na.rm = TRUE),
    sb   = mean(lims$sbar, na.rm = TRUE),
    up   = max(lims$upper,  na.rm = TRUE),
    lo   = min(lims$lower,  na.rm = TRUE)
  ) %>% pivot_longer(c(sb, up, lo), names_to = "name", values_to = "value")
  
  gg <- ggplot() +
    geom_hline(yintercept = mean(lims$sbar, na.rm = TRUE), color = "lightgrey") +
    geom_ribbon(data = lims, aes(x = x, ymin = lower, ymax = upper),
                fill = "steelblue", alpha = 0.2) +
    geom_line(data = stat, aes(x = x, y = s), linewidth = 0.5) +
    geom_point(data = stat, aes(x = x, y = s), size = 3) +
    labs(x = xlab, y = ylab, subtitle = "Standard Deviation Chart")
  if (isTRUE(show_labels)) {
    labmap <- c(sb = "sbar", up = "+3 s", lo = "-3 s")
    gg <- gg + geom_label(
      data = labels,
      aes(x = x, y = value, label = paste0(labmap[name], " = ", round(value,2))),
      hjust = 1, alpha = 0.5
    )
  }
  gg
}

ggr <- function(x,y, xlab = "Subgroup", ylab = "Range", show_labels = TRUE){
  lims <- limits_r(x, y)
  stat <- .get_subgroup_stats(x, y)
  if (nrow(lims) == 0 || all(!is.finite(lims$upper))) return(.ggx_empty())
  
  labels <- tibble(
    x    = max(lims$x, na.rm = TRUE),
    rb   = mean(lims$rbar, na.rm = TRUE),
    up   = max(lims$upper,  na.rm = TRUE),
    lo   = min(lims$lower,  na.rm = TRUE)
  ) %>% pivot_longer(c(rb, up, lo), names_to = "name", values_to = "value")
  
  gg <- ggplot() +
    geom_hline(yintercept = mean(lims$rbar, na.rm = TRUE), color = "lightgrey") +
    geom_ribbon(data = lims, aes(x = x, ymin = lower, ymax = upper),
                fill = "steelblue", alpha = 0.2) +
    geom_line(data = stat, aes(x = x, y = r), linewidth = 1) +
    geom_point(data = stat, aes(x = x, y = r), size = 5) +
    labs(x = xlab, y = ylab, subtitle = "Range Chart")
  if (isTRUE(show_labels)) {
    labmap <- c(rb = "rbar", up = "+3 s", lo = "-3 s")
    gg <- gg + geom_label(
      data = labels,
      aes(x = x, y = value, label = paste0(labmap[name], " = ", round(value,2))),
      hjust = 1
    )
  }
  gg
}

# ---- Individuals–MR (single asset) -----------------------------------
imr_stats <- function(metric_vec) {
  stopifnot(is.numeric(metric_vec))
  x  <- as.numeric(metric_vec)
  n  <- length(x)
  if (n < 3) stop("I–MR needs at least 3 points.")
  mr <- abs(diff(x))
  mrbar <- mean(mr, na.rm = TRUE)
  
  # MR(2) constants
  d2 <- 1.128; D3 <- 0.000; D4 <- 3.267
  sigma <- mrbar / d2
  xbar  <- mean(x, na.rm = TRUE)
  
  tibble(
    xbar = xbar,
    mrbar = mrbar,
    sigma = sigma,
    I_LCL = xbar - 3*sigma,
    I_UCL = xbar + 3*sigma,
    MR_LCL = D3 * mrbar,
    MR_UCL = D4 * mrbar
  )
}

ggi_mr_one <- function(df, metric_col, t_col = "t_idx",
                       xlab = "Time (t)", ylab_i = "Individuals", ylab_mr = "Moving Range",
                       show_labels = TRUE) {
  df <- df %>% filter(!is.na(asset_id))
  stopifnot(dplyr::n_distinct(df$asset_id) == 1)
  df <- df %>% arrange(.data[[t_col]])
  x  <- df[[metric_col]]
  t  <- df[[t_col]]
  
  st <- imr_stats(x)
  d_i  <- tibble(t = t, x = x)
  d_mr <- tibble(t = t[-1], mr = abs(diff(x)))
  
  g_i <- ggplot() +
    geom_hline(aes(yintercept = st$xbar), color = "grey50") +
    geom_hline(aes(yintercept = st$I_LCL), linetype = 2, color = "firebrick") +
    geom_hline(aes(yintercept = st$I_UCL), linetype = 2, color = "firebrick") +
    geom_line(data = d_i, aes(x = t, y = x)) +
    geom_point(data = d_i, aes(x = t, y = x)) +
    labs(x = xlab, y = ylab_i,
         subtitle = paste0("Individuals (", metric_col, ")"),
         caption  = sprintf("Xbar=%.2f | LCL=%.2f | UCL=%.2f", st$xbar, st$I_LCL, st$I_UCL))
  
  g_mr <- ggplot() +
    geom_hline(aes(yintercept = st$mrbar), color = "grey50") +
    geom_hline(aes(yintercept = st$MR_LCL), linetype = 2, color = "steelblue4") +
    geom_hline(aes(yintercept = st$MR_UCL), linetype = 2, color = "steelblue4") +
    geom_line(data = d_mr, aes(x = t, y = mr)) +
    geom_point(data = d_mr, aes(x = t, y = mr)) +
    labs(x = xlab, y = ylab_mr,
         subtitle = "Moving Range (lag 1)",
         caption  = sprintf("MRbar=%.2f | LCL=%.2f | UCL=%.2f", st$mrbar, st$MR_LCL, st$MR_UCL))
  
  ggarrange(g_i, g_mr, ncol = 1, heights = c(2,1))
}

ggi_mr <- function(df, asset_id, metric_col = "rack_inlet_c", show_labels = TRUE) {
  stopifnot(is.character(asset_id), length(asset_id) == 1)
  d <- df %>%
    filter(!is.na(.data$asset_id), .data$asset_id == asset_id) %>%
    ensure_t_idx() %>%
    arrange(t_idx)
  
  # allow use standalone and inside shiny
  validate <- if (!requireNamespace("shiny", quietly = TRUE)) function(...) invisible(NULL) else shiny::validate
  need     <- if (!requireNamespace("shiny", quietly = TRUE)) function(...) TRUE else shiny::need
  validate(
    need(nrow(d) > 1, "Not enough points for Individuals–MR (need > 1)."),
    need(dplyr::n_distinct(d$asset_id) == 1, "Please pick a single asset.")
  )
  
  ggi_mr_one(d, metric_col = metric_col, t_col = "t_idx",
             ylab_i = paste0("Individuals (", metric_col, ")"),
             ylab_mr = paste0("Moving Range (", metric_col, ")"),
             show_labels = show_labels)
}

# ---- X̄–S base and wrappers ------------------------------------------
spc_xbar_s <- function(df, metric_col = "rack_inlet_c",
                       subgroup_col = "t_idx",
                       xlab = "Subgroup", ylab_mean = "Average", ylab_sd = "SD",
                       show_labels = TRUE) {
  stopifnot(all(c(metric_col, subgroup_col) %in% names(df)))
  x <- df[[subgroup_col]]
  y <- df[[metric_col]]
  
  p_xbar <- ggxbar(x = x, y = y, xlab = xlab,
                   ylab = paste0(ylab_mean, " (", metric_col, ")"),
                   show_labels = show_labels)
  p_s    <- ggs   (x = x, y = y, xlab = xlab,
                   ylab = paste0(ylab_sd,   " (", metric_col, ")"),
                   show_labels = show_labels)
  ggarrange(p_xbar, p_s, ncol = 1, heights = c(2,1))
}

# For single assets, create time-window subgroups so Xbar–S makes sense
spc_asset_xbar_s_window <- function(df, asset_id, metric_col = "rack_inlet_c", m = 5){
  d <- df %>%
    filter(asset_id == !!asset_id) %>%
    ensure_t_idx() %>%
    arrange(t_idx) %>%
    mutate(subgroup = floor((row_number() - 1L) / m))
  spc_xbar_s(d, metric_col = metric_col, subgroup_col = "subgroup",
             xlab = paste0("Subgroup (", m, " pts)"))
}

spc_room <- function(df, room_id, metric_col = "rack_inlet_c", show_labels = TRUE) {
  d <- df %>%
    filter(.data$room_id == room_id) %>%
    ensure_t_idx()
  spc_xbar_s(d, metric_col = metric_col, show_labels = show_labels)
}

spc_rack <- function(df, rack_id, metric_col = "rack_inlet_c", show_labels = TRUE) {
  d <- df %>%
    filter(.data$rack_id == rack_id) %>%
    ensure_t_idx()
  spc_xbar_s(d, metric_col = metric_col, show_labels = show_labels)
}

spc_server <- function(df, server_id, metric_col = "server_temp_c", show_labels = TRUE) {
  d <- df %>%
    filter(.data$server_id == server_id) %>%
    ensure_t_idx()
  spc_xbar_s(d, metric_col = metric_col, show_labels = show_labels)
}
