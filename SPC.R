#' @name SPC_compat.R
#' @title Statistical Process Control (SPC) Toolkit for Datacenter Timeseries
#' @description
#' Below are several functions that you can use or customize to perform statistical process control.

library(tidyr)
library(dplyr) # for data wrangling
library(readr) # for reading in data
library(ggplot2) # for visualization
library(ggpubr) # for combining plots
library(moments) # for skewness and kurtosis

#' @name set_theme
#' @title Set Theme of All ggplots
set_theme = function(){

  # By running theme_set()
  theme_set(
    # we tell ggplot to give EVERY plot this theme
    theme_classic(base_size = 14) +
      # With these theme traits, including
      theme(
        # Putting the legend on the bottom, if applicable
        legend.position = "bottom",
        # horizontally justify plot subtitle and caption in center
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        # Getting rid of busy axis ticks
        axis.ticks = element_blank(),
        # Getting rid of busy axis lines
        axis.line = element_blank(),
        # Surrounding the plot in a nice grey border
        panel.border = element_rect(fill = NA, color = "grey"),
        # Remove the right margin, for easy joining of plots
        plot.margin = margin(r = 0)
      )
  )

}

#' @name describe
#' @title Describe a vector `x`
#' @param x [numeric] a numeric vector of observed metric values
describe = function(x){
  # Put our vector x in a tibble
  tibble(x) %>%
    # Calculate summary statistics
    summarize(
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      # We'll use the moments package for these two
      skew = skewness(x, na.rm = TRUE),
      kurtosis = kurtosis(x, na.rm = TRUE)) %>%
    # Let's add a caption, that compiles  all these statistics
    mutate(
      # We'll paste() the following together
      caption = paste(
        # Listing the name of each stat, then reporting its value and rounding it, then separating with " | "
        "Process Mean: ", mean %>% round(2), " | ",
        "SD: ", sd %>% round(2), " | ",
        "Skewness: ", skew %>% round(2), " | ",
        "Kurtosis: ", kurtosis %>% round(2),
        # Then make sure no extra spaces separate each item
        sep = "")) %>%
    return()
}

# example
# describe(x = rnorm(1000,0,1))


#' @name ensure_t_idx
#' @title Ensure integer time index (t_idx) exists
#' @param df [data.frame] long/thin timeseries data with a `ts` column (e.g., "t_001")
ensure_t_idx = function(df){
  if (!"t_idx" %in% names(df)) {
    stopifnot("ts" %in% names(df))
    df = df %>% mutate(t_idx = as.integer(sub("^t_", "", .data$ts)))
  }
  return(df)
}

#' @name .get_subgroup_stats
#' @title Safe subgroup summarizer
#' @description Handles NA/Inf and variable subgroup sizes.
#' @param x [numeric] subgroup labels (e.g., time index)
#' @param y [numeric] metric values
.get_subgroup_stats = function(x, y){
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

# ---- Subgroup/Total statistics ---------------------------------------
#' @name get_stat_s
#' @title Get Subgroup Statistics (with pooled sigma_s and ±3*SE limits)
#' @param x [numeric] subgroup labels
#' @param y [numeric] metric values
get_stat_s = function(x,y){
  stat = .get_subgroup_stats(x, y)
  sigma_t = sd(y, na.rm = TRUE)
  stat %>%
    mutate(
      xbbar   = mean(xbar, na.rm = TRUE),
      sigma_s = sqrt(sum(df * s^2, na.rm = TRUE) / pmax(sum(df, na.rm = TRUE), 1)),
      sigma_t = sigma_t,
      se      = sigma_s / sqrt(pmax(nw, 1)),
      upper   = xbbar + 3*se,
      lower   = xbbar - 3*se
    ) %>%
    return()
}

#' @name get_stat_t
#' @title Get Total Statistics (xbbar, rbar, sbar, sigma_s, sigma_t, n)
#' @param x [numeric] subgroup labels
#' @param y [numeric] metric values
get_stat_t = function(x,y){
  stat = .get_subgroup_stats(x, y)
  sigma_t = sd(y, na.rm = TRUE)
  stat %>%
    summarize(
      xbbar   = mean(xbar, na.rm = TRUE),
      rbar    = mean(r,    na.rm = TRUE),
      sbar    = mean(s,    na.rm = TRUE),
      sigma_s = sqrt(sum(df * s^2, na.rm = TRUE) / pmax(sum(df, na.rm = TRUE), 1)),
      sigma_t = sigma_t,
      n       = sum(nw, na.rm = TRUE)
    ) %>%
    return()
}

# How do we approximate sigma-short?
dn = function(n = 12, reps = 1e4){
  # For 10,0000 reps
  tibble(rep = 1:reps) %>%
    # For each rep,
    group_by(rep) %>%
    # Simulate the ranges of n values
    summarize(r = rnorm(n = n, mean = 0, sd = 1) %>% range() %>% diff() %>% abs()) %>%
    ungroup() %>%
    # And calculate...
    summarize(
      # Mean range
      d2 = mean(r),
      # standard deviation of ranges
      d3 = sd(r),
      # and constants for obtaining lower and upper ci for rbar
      D3 = 1 - 3*(d3/d2), # sometimes written D3
      D4 = 1 + 3*(d3/d2), # sometimes written D4
      # Sometimes D3 goes negative; we need to bound it at zero
      D3 = if_else(D3 < 0, true = 0, false = D3) ) %>%
    return()
}

#dn(n = 12)

#Let's write a function bn() to calculate our B3 and B4 statistics for any subgroup size n
bn = function(n, reps = 1e4){
  tibble(rep = 1:reps) %>%
    group_by(rep) %>%
    summarize(s = rnorm(n, mean = 0, sd = 1) %>% sd()) %>%
    summarize(b2 = mean(s),
              b3 = sd(s),
              C4 = b2, # this is sometimes called C4
              A3 = 3 / (b2 * sqrt( n  )),
              B3 = 1 - 3 * b3/b2,
              B4 = 1 + 3 * b3/b2,
              # bound B3 at 0, since we can't have a standard deviation below 0
              B3 = if_else(B3 < 0, true = 0, false = B3)) %>%
    return()
}

# For a subgroup of size 12
# stat = bn(n = 12)
# # Statistic of interest
# sbar = 2.5
# # Lower Control Limit
# sbar * stat$B3
# # Upper control limit
# sbar * stat$B4


#' @name limits_avg
#' @title Get Upper and Lower Control Limits for an Averages Chart (using control constants)
#' @param x [numeric] subgroup labels
#' @param y [numeric] metric values
#' @note Uses pooled sbar with A3; guards for nw < 2
limits_avg = function(x,y){
  stat_s = .get_subgroup_stats(x, y) %>% filter(nw >= 2)
  if (nrow(stat_s) == 0) {
    return(stat_s %>% mutate(lower = NA_real_, upper = NA_real_, sbar = NA_real_, xbbar = NA_real_))
  }
  constants = stat_s %>%
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
    ) %>%
    return()
}

#' @name limits_s
#' @title Get Upper and Lower Control Limits for a Standard Deviation Chart (using control constants)
#' @param x [numeric] subgroup labels
#' @param y [numeric] metric values
limits_s = function(x,y){
  stat_s = .get_subgroup_stats(x, y) %>% filter(nw >= 2)
  if (nrow(stat_s) == 0) {
    return(stat_s %>% mutate(lower = NA_real_, upper = NA_real_, sbar = NA_real_))
  }
  constants = stat_s %>%
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
    ) %>%
    return()
}

#' @name limits_r
#' @title Get Upper and Lower Control Limits for a Range Chart (using control constants)
#' @param x [numeric] subgroup labels
#' @param y [numeric] metric values
limits_r = function(x,y){
  stat_s = .get_subgroup_stats(x, y) %>% filter(nw >= 2)
  if (nrow(stat_s) == 0) {
    return(stat_s %>% mutate(lower = NA_real_, upper = NA_real_, rbar = NA_real_))
  }
  constants = stat_s %>%
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
    ) %>%
    return()
}


# ---- Plot guards ------------------------------------------------------
#' @name .ggx_empty
#' @title Empty placeholder plot with message
.ggx_empty = function(msg = "No subgroups with ≥ 2 valid points"){
  ggplot() + annotate("text", x = 0, y = 0, label = msg) + theme_void(base_size = 14)
}

#' @name ggxbar
#' @title Average Control Chart with ggplot
#' @param x [numeric] subgroup labels
#' @param y [numeric] metric values
#' @param xlab [string] label for x-axis
#' @param ylab [string] label for y-axis
ggxbar = function(x,y, xlab = "Subgroup", ylab = "Average", show_labels = TRUE){
  stat_s = get_stat_s(x, y)
  stat_t = get_stat_t(x, y)
  lims   = limits_avg(x, y)
  if (nrow(lims) == 0 || all(!is.finite(lims$upper))) return(.ggx_empty())
  
  labels = tibble(
    x  = max(lims$x, na.rm = TRUE),
    xb = mean(lims$xbbar, na.rm = TRUE),
    up = max(lims$upper,  na.rm = TRUE),
    lo = min(lims$lower,  na.rm = TRUE)
  ) %>% pivot_longer(c(xb, up, lo), names_to = "name", values_to = "value")
  
  gg = ggplot() +
    geom_hline(data = stat_t, mapping = aes(yintercept = xbbar), color = "lightgrey") +
    geom_ribbon(data = lims, mapping = aes(x = x, ymin = lower, ymax = upper),
                fill = "steelblue", alpha = 0.2) +
    geom_line(data = stat_s, mapping = aes(x = x, y = xbar), linewidth = 0.5) +
    geom_point(data = stat_s, mapping = aes(x = x, y = xbar), size = 2) +
    labs(x = xlab, y = ylab, subtitle = "Average Chart")
  
  if (isTRUE(show_labels)){
    labmap = c(xb = "xbbar", up = "+3 s", lo = "-3 s")
    gg = gg + geom_label(
      data = labels,
      mapping = aes(x = x, y = value, label = paste0(labmap[name], " = ", round(value,2))),
      hjust = 1, alpha = 0.5
    )
  }
  return(gg)
}

#' @name ggs
#' @title Standard Deviation Chart with ggplot
#' @param x [numeric] subgroup labels
#' @param y [numeric] metric values
#' @param xlab [string] label for x-axis
#' @param ylab [string] label for y-axis
ggs = function(x,y, xlab = "Subgroup", ylab = "Standard Deviation", show_labels = TRUE){
  lims = limits_s(x, y)
  stat = .get_subgroup_stats(x, y)
  if (nrow(lims) == 0 || all(!is.finite(lims$upper))) return(.ggx_empty())
  
  labels = tibble(
    x  = max(lims$x, na.rm = TRUE),
    sb = mean(lims$sbar, na.rm = TRUE),
    up = max(lims$upper,  na.rm = TRUE),
    lo = min(lims$lower,  na.rm = TRUE)
  ) %>% pivot_longer(c(sb, up, lo), names_to = "name", values_to = "value")
  
  gg = ggplot() +
    geom_hline(yintercept = mean(lims$sbar, na.rm = TRUE), color = "lightgrey") +
    geom_ribbon(data = lims, mapping = aes(x = x, ymin = lower, ymax = upper),
                fill = "steelblue", alpha = 0.2) +
    geom_line(data = stat, mapping = aes(x = x, y = s), linewidth = 0.5) +
    geom_point(data = stat, mapping = aes(x = x, y = s), size = 3) +
    labs(x = xlab, y = ylab, subtitle = "Standard Deviation Chart")
  
  if (isTRUE(show_labels)){
    labmap = c(sb = "sbar", up = "+3 s", lo = "-3 s")
    gg = gg + geom_label(
      data = labels,
      mapping = aes(x = x, y = value, label = paste0(labmap[name], " = ", round(value,2))),
      hjust = 1, alpha = 0.5
    )
  }
  return(gg)
}

#' @name ggr
#' @title Range Chart with ggplot
#' @param x [numeric] subgroup labels
#' @param y [numeric] metric values
#' @param xlab [string] label for x-axis
#' @param ylab [string] label for y-axis
ggr = function(x,y, xlab = "Subgroup", ylab = "Range", show_labels = TRUE){
  lims = limits_r(x, y)
  stat = .get_subgroup_stats(x, y)
  if (nrow(lims) == 0 || all(!is.finite(lims$upper))) return(.ggx_empty())
  
  labels = tibble(
    x  = max(lims$x, na.rm = TRUE),
    rb = mean(lims$rbar, na.rm = TRUE),
    up = max(lims$upper,  na.rm = TRUE),
    lo = min(lims$lower,  na.rm = TRUE)
  ) %>% pivot_longer(c(rb, up, lo), names_to = "name", values_to = "value")
  
  gg = ggplot() +
    geom_hline(yintercept = mean(lims$rbar, na.rm = TRUE), color = "lightgrey") +
    geom_ribbon(data = lims, mapping = aes(x = x, ymin = lower, ymax = upper),
                fill = "steelblue", alpha = 0.2) +
    geom_line(data = stat, mapping = aes(x = x, y = r), linewidth = 1) +
    geom_point(data = stat, mapping = aes(x = x, y = r), size = 5) +
    labs(x = xlab, y = ylab, subtitle = "Range Chart")
  
  if (isTRUE(show_labels)){
    labmap = c(rb = "rbar", up = "+3 s", lo = "-3 s")
    gg = gg + geom_label(
      data = labels,
      mapping = aes(x = x, y = value, label = paste0(labmap[name], " = ", round(value,2))),
      hjust = 1
    )
  }
  return(gg)
}

# ---- Individuals–Moving Range (single asset) -------------------------
#' @name imr_stats
#' @title I–MR constants and limits for a numeric vector
#' @param metric_vec [numeric] the Individuals series (>= 3 points)
imr_stats = function(metric_vec){
  stopifnot(is.numeric(metric_vec))
  x  = as.numeric(metric_vec)
  n  = length(x)
  if (n < 3) stop("I–MR needs at least 3 points.")
  mr = abs(diff(x))
  mrbar = mean(mr, na.rm = TRUE)
  
  # MR(2) constants
  d2 = 1.128; D3 = 0.000; D4 = 3.267
  sigma = mrbar / d2
  xbar  = mean(x, na.rm = TRUE)
  
  tibble(
    xbar = xbar,
    mrbar = mrbar,
    sigma = sigma,
    I_LCL = xbar - 3*sigma,
    I_UCL = xbar + 3*sigma,
    MR_LCL = D3 * mrbar,
    MR_UCL = D4 * mrbar
  ) %>% return()
}

#' @name ggi_mr_one
#' @title Individuals–MR chart for a single asset (by t_idx)
#' @param df [data.frame] filtered to a single asset_id
#' @param metric_col [string] metric column name
#' @param t_col [string] time index column name (default t_idx)
#' @note Produces a stacked Individuals and MR plot
ggi_mr_one = function(df, metric_col, t_col = "t_idx",
                      xlab = "Time (t)", ylab_i = "Individuals", ylab_mr = "Moving Range",
                      show_labels = TRUE){
  df = df %>% filter(!is.na(asset_id))
  stopifnot(dplyr::n_distinct(df$asset_id) == 1)
  df = df %>% arrange(.data[[t_col]])
  x  = df[[metric_col]]
  t  = df[[t_col]]
  
  st  = imr_stats(x)
  d_i = tibble(t = t, x = x)
  d_mr = tibble(t = t[-1], mr = abs(diff(x)))
  
  g_i = ggplot() +
    geom_hline(aes(yintercept = st$xbar), color = "grey50") +
    geom_hline(aes(yintercept = st$I_LCL), linetype = 2, color = "firebrick") +
    geom_hline(aes(yintercept = st$I_UCL), linetype = 2, color = "firebrick") +
    geom_line(data = d_i, mapping = aes(x = t, y = x)) +
    geom_point(data = d_i, mapping = aes(x = t, y = x)) +
    labs(x = xlab, y = ylab_i,
         subtitle = paste0("Individuals (", metric_col, ")"),
         caption  = sprintf("Xbar=%.2f | LCL=%.2f | UCL=%.2f", st$xbar, st$I_LCL, st$I_UCL))
  
  g_mr = ggplot() +
    geom_hline(aes(yintercept = st$mrbar), color = "grey50") +
    geom_hline(aes(yintercept = st$MR_LCL), linetype = 2, color = "steelblue4") +
    geom_hline(aes(yintercept = st$MR_UCL), linetype = 2, color = "steelblue4") +
    geom_line(data = d_mr, mapping = aes(x = t, y = mr)) +
    geom_point(data = d_mr, mapping = aes(x = t, y = mr)) +
    labs(x = xlab, y = ylab_mr,
         subtitle = "Moving Range (lag 1)",
         caption  = sprintf("MRbar=%.2f | LCL=%.2f | UCL=%.2f", st$mrbar, st$MR_LCL, st$MR_UCL))
  
  ggarrange(g_i, g_mr, ncol = 1, heights = c(2,1))
}

#' @name ggi_mr
#' @title Convenience wrapper for Individuals–MR by asset_id
#' @param df [data.frame] long/thin dataset
#' @param asset_id [string] specific asset id
#' @param metric_col [string] metric column (default "rack_inlet_c")
#' @note Ensures t_idx exists and validates single-asset selection
ggi_mr = function(df, asset_id, metric_col = "rack_inlet_c", show_labels = TRUE){
  stopifnot(is.character(asset_id), length(asset_id) == 1)
  d = df %>%
    filter(!is.na(.data$asset_id), .data$asset_id == asset_id) %>%
    ensure_t_idx() %>%
    arrange(t_idx)
  
  validate = if (!requireNamespace("shiny", quietly = TRUE)) function(...) invisible(NULL) else shiny::validate
  need     = if (!requireNamespace("shiny", quietly = TRUE)) function(...) TRUE else shiny::need
  validate(
    need(nrow(d) > 1, "Not enough points for Individuals–MR (need > 1)."),
    need(dplyr::n_distinct(d$asset_id) == 1, "Please pick a single asset.")
  )
  
  ggi_mr_one(d, metric_col = metric_col, t_col = "t_idx",
             ylab_i = paste0("Individuals (", metric_col, ")"),
             ylab_mr = paste0("Moving Range (", metric_col, ")"),
             show_labels = show_labels)
}

# ---- Xbar–S base and hierarchical wrappers ---------------------------
#' @name spc_xbar_s
#' @title Xbar–S panel (Average + SD charts)
#' @param df [data.frame] data
#' @param metric_col [string] metric column name
#' @param subgroup_col [string] subgroup column (default t_idx)
spc_xbar_s = function(df, metric_col = "rack_inlet_c",
                      subgroup_col = "t_idx",
                      xlab = "Subgroup", ylab_mean = "Average", ylab_sd = "SD",
                      show_labels = TRUE){
  stopifnot(all(c(metric_col, subgroup_col) %in% names(df)))
  x = df[[subgroup_col]]
  y = df[[metric_col]]
  
  p_xbar = ggxbar(x = x, y = y, xlab = xlab,
                  ylab = paste0(ylab_mean, " (", metric_col, ")"),
                  show_labels = show_labels)
  p_s    = ggs   (x = x, y = y, xlab = xlab,
                  ylab = paste0(ylab_sd,   " (", metric_col, ")"),
                  show_labels = show_labels)
  ggarrange(p_xbar, p_s, ncol = 1, heights = c(2,1))
}

#' @name spc_asset_xbar_s_window
#' @title Rolling-window Xbar–S for a single asset (create pseudo-subgroups)
#' @param m [integer] subgroup window size (default 5)
spc_asset_xbar_s_window = function(df, asset_id, metric_col = "rack_inlet_c", m = 5){
  d = df %>%
    filter(asset_id == !!asset_id) %>%
    ensure_t_idx() %>%
    arrange(t_idx) %>%
    mutate(subgroup = floor((row_number() - 1L) / m))
  spc_xbar_s(d, metric_col = metric_col, subgroup_col = "subgroup",
             xlab = paste0("Subgroup (", m, " pts)"))
}

#' @name spc_room
#' @title Room-level Xbar–S wrapper
spc_room = function(df, room_id, metric_col = "rack_inlet_c", show_labels = TRUE){
  d = df %>% filter(.data$room_id == room_id) %>% ensure_t_idx()
  spc_xbar_s(d, metric_col = metric_col, show_labels = show_labels)
}

#' @name spc_rack
#' @title Rack-level Xbar–S wrapper
spc_rack = function(df, rack_id, metric_col = "rack_inlet_c", show_labels = TRUE){
  d = df %>% filter(.data$rack_id == rack_id) %>% ensure_t_idx()
  spc_xbar_s(d, metric_col = metric_col, show_labels = show_labels)
}

#' @name spc_server
#' @title Server-level Xbar–S wrapper
spc_server = function(df, server_id, metric_col = "server_temp_c", show_labels = TRUE){
  d = df %>% filter(.data$server_id == server_id) %>% ensure_t_idx()
  spc_xbar_s(d, metric_col = metric_col, show_labels = show_labels)
}
