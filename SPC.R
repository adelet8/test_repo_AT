# =======================================================================
# Statistical Process Control (SPC) Toolkit for Datacenter GPU Timeseries
# Works with: datacenter_assets_timeseries_8h_GPU_<ROOM>.csv
# Schema must include: ts, asset_id, asset_type, server_id, rack_id, room_id,
# asset_power_w, server_temp_c (plus temps/humidity/capacity cols).
# =======================================================================

# ---- Libraries --------------------------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)
library(moments)
library(tidyr)

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

# ---- Descriptive helper ----------------------------------------------
describe <- function(x){
  tibble(x) %>%
    summarize(
      mean = mean(x, na.rm = TRUE),
      sd   = sd(x, na.rm = TRUE),
      skew = skewness(x, na.rm = TRUE),
      kurtosis = kurtosis(x, na.rm = TRUE)
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

# ---- Subgroup statistics ---------------------------------------------
get_stat_s <- function(x,y){
  data <- tibble(x=x,y=y)
  
  sigma_t <- sd(y)
  
  output <- data %>%
    group_by(x) %>%
    summarize(
      xbar = mean(y),
      r    = max(y) - min(y),
      s    = sd(y),
      nw   = n(),
      df   = nw - 1,
      .groups = "drop"
    )
  
  output %>%
    mutate(
      xbbar   = mean(xbar),
      # pooled within-group SD (robust for unequal subgroup sizes)
      sigma_s = sqrt(sum(df * s^2) / sum(df)),
      sigma_t = sigma_t,
      se      = sigma_s / sqrt(nw),
      upper   = mean(xbar) + 3*se,
      lower   = mean(xbar) - 3*se
    )
}

get_stat_t <- function(x,y){
  data <- tibble(x=x,y=y)
  sigma_t <- sd(y)
  
  stat_s <- data %>%
    group_by(x) %>%
    summarize(
      xbar = mean(y),
      r    = max(y) - min(y),
      s    = sd(y),
      nw   = n(),
      df   = nw - 1,
      .groups = "drop"
    )
  
  stat_s %>%
    summarize(
      xbbar   = mean(xbar),
      rbar    = mean(r),
      sbar    = mean(s),
      sigma_s = sqrt(sum(s^2) / sum(nw)),
      sigma_t = sigma_t,
      n       = sum(nw)
    )
}

# ---- Control constants via simulation --------------------------------
dn <- function(n = 12, reps = 1e4){
  tibble(rep = 1:reps) %>%
    group_by(rep) %>%
    summarize(r = diff(range(rnorm(n, 0, 1))) %>% abs(), .groups="drop") %>%
    summarize(
      d2 = mean(r),
      d3 = sd(r),
      D3 = pmax(0, 1 - 3*(d3/d2)),
      D4 = 1 + 3*(d3/d2)
    )
}

bn <- function(n, reps = 1e4){
  tibble(rep = 1:reps) %>%
    group_by(rep) %>%
    summarize(s = sd(rnorm(n, 0, 1)), .groups = "drop") %>%
    summarize(
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
  data <- tibble(x=x,y=y)
  
  stat_s <- data %>%
    group_by(x) %>%
    summarize(xbar = mean(y), s = sd(y), nw = n(), df = nw-1, .groups="drop")
  
  constants <- stat_s %>%
    distinct(nw) %>%
    rowwise() %>%
    mutate(A3 = bn(n = nw, reps = 1e4)$A3) %>%
    ungroup()
  
  stat_s <- stat_s %>% left_join(constants, by = "nw") %>%
    mutate(
      sbar = sqrt(sum(df * s^2, na.rm=TRUE) / sum(df, na.rm=TRUE)),
      xbbar = mean(xbar, na.rm=TRUE),
      # FIX: proper lower/upper around xbbar
      lower = xbbar - A3 * sbar,
      upper = xbbar + A3 * sbar
    )
  
  stat_s
}

limits_s <- function(x,y){
  data <- tibble(x=x,y=y)
  
  stat_s <- data %>%
    group_by(x) %>%
    summarize(s = sd(y), nw = n(), df = nw-1, .groups="drop")
  
  constants <- stat_s %>%
    distinct(nw) %>%
    rowwise() %>%
    do(bn(n = .$nw, reps = 1e4)) %>%
    mutate(nw = unique(stat_s$nw)) %>%
    ungroup()
  
  stat_s %>%
    left_join(constants, by = "nw") %>%
    mutate(
      sbar = mean(s, na.rm=TRUE),
      lower = B3 * sbar,
      upper = B4 * sbar
    )
}

limits_r <- function(x,y){
  data <- tibble(x=x,y=y)
  
  stat_s <- data %>%
    group_by(x) %>%
    summarize(r = max(y) - min(y), nw = n(), df = nw-1, .groups="drop")
  
  constants <- stat_s %>%
    distinct(nw) %>%
    rowwise() %>%
    do(dn(n = .$nw, reps = 1e4)) %>%
    mutate(nw = unique(stat_s$nw)) %>%
    ungroup()
  
  stat_s %>%
    left_join(constants, by = "nw") %>%
    mutate(
      rbar  = mean(r, na.rm=TRUE),
      lower = D3 * rbar,
      upper = D4 * rbar
    )
}

limits_mr <- function(x,y){
  data <- tibble(x=x,y=y)
  
  data2 <- data %>%
    reframe(x = x[-1], mr = abs(diff(y)))
  
  # d2 for MR with window size 2
  d2 <- 1.128
  
  stat <- data2 %>%
    mutate(
      mrbar = mean(mr, na.rm=TRUE),
      sigma_s = mrbar / d2,
      n = 1,
      se = sigma_s / sqrt(n),
      upper = mrbar + 3 * se,
      lower = 0
    )
  
  stat
}

# ---- Plots (Xbar, S, R, MR) ------------------------------------------
ggxbar <- function(x,y, xlab = "Time (Subgroups)", ylab = "Average"){
  data  <- tibble(x=x, y=y)
  stat_s <- get_stat_s(x = data$x, y = data$y)
  
  labels <- stat_s %>%
    reframe(
      x    = c(max(x), max(x), max(x)),
      type = c("xbbar", "upper", "lower"),
      name = c("xbbar", "+3 s", "-3 s"),
      value= c(mean(xbbar), max(upper), min(lower))
    ) %>%
    mutate(value = round(value, 2),
           text  = paste0(name, " = ", value))
  
  stat_t <- get_stat_t(x = x, y = y)
  
  ggplot() +
    geom_hline(data = stat_t, aes(yintercept = xbbar), color = "lightgrey") +
    geom_ribbon(data = stat_s, aes(x = x, ymin = lower, ymax = upper),
                fill = "steelblue", alpha = 0.2) +
    geom_line(data = stat_s, aes(x = x, y = xbar), linewidth = 0.5) +
    geom_point(data = stat_s, aes(x = x, y = xbar), size = 2) +
    geom_label(data = labels, aes(x = x, y = value, label = text),
               hjust = 1, alpha = 0.5) +
    labs(x = xlab, y = ylab, subtitle = "Average Chart")
}

ggs <- function(x,y, xlab = "Time (Subgroups)", ylab = "Standard Deviation"){
  data  <- tibble(x=x, y=y)
  stat_s <- limits_s(x = data$x, y = data$y)
  stat_t <- get_stat_t(x = x, y = y)
  
  labels <- stat_s %>%
    reframe(
      x    = c(max(x), max(x), max(x)),
      type = c("sbar", "upper", "lower"),
      name = c("sbar", "+3 s", "-3 s"),
      value= c(mean(sbar), max(upper), min(lower))
    ) %>%
    mutate(value = round(value, 2),
           text  = paste0(name, " = ", value))
  
  ggplot() +
    geom_hline(data = stat_t, aes(yintercept = sbar), color = "lightgrey") +
    geom_ribbon(data = stat_s, aes(x = x, ymin = lower, ymax = upper),
                fill = "steelblue", alpha = 0.2) +
    geom_line(data = stat_s, aes(x = x, y = s), linewidth = 0.5) +
    geom_point(data = stat_s, aes(x = x, y = s), size = 3) +
    geom_label(data = labels, aes(x = x, y = value, label = text),
               hjust = 1, alpha = 0.5) +
    labs(x = xlab, y = ylab, subtitle = "Standard Deviation Chart")
}

ggr <- function(x,y, xlab = "Time (Subgroups)", ylab = "Range"){
  data  <- tibble(x=x, y=y)
  stat_s <- limits_r(x = data$x, y = data$y)
  stat_t <- get_stat_t(x = x, y = y)
  
  labels <- stat_s %>%
    reframe(
      x    = c(max(x), max(x), max(x)),
      type = c("rbar", "upper", "lower"),
      name = c("rbar", "+3 s", "-3 s"),
      value= c(mean(rbar), max(upper), min(lower))
    ) %>%
    mutate(value = round(value, 2),
           text  = paste0(name, " = ", value))
  
  ggplot() +
    geom_hline(data = stat_t, aes(yintercept = rbar), color = "lightgrey") +
    geom_ribbon(data = stat_s, aes(x = x, ymin = lower, ymax = upper),
                fill = "steelblue", alpha = 0.2) +
    geom_line(data = stat_s, aes(x = x, y = r), linewidth = 1) +
    geom_point(data = stat_s, aes(x = x, y = r), size = 5) +
    geom_label(data = labels, aes(x = x, y = value, label = text), hjust = 1) +
    labs(x = xlab, y = ylab, subtitle = "Range Chart")
}

ggmr <- function(x,y, xlab = "Time (Subgroups)", ylab = "Moving Range"){
  data  <- tibble(x=x, y=y)
  data2 <- data %>% reframe(x = x[-1], mr = abs(diff(y)))
  stat_s <- limits_mr(x = data$x, y = data$y)
  
  labels <- stat_s %>%
    reframe(
      x    = c(max(x), max(x), max(x)),
      type = c("mrbar", "upper", "lower"),
      name = c("mrbar", "+3 s", "-3 s"),
      value= c(mean(mrbar), max(upper), min(lower))
    ) %>%
    mutate(value = round(value, 2),
           text  = paste0(name, " = ", value))
  
  stat_t <- stat_s %>% summarize(mrbar = mean(mrbar))
  
  ggplot() +
    geom_hline(data = stat_t, aes(yintercept = mrbar), color = "lightgrey") +
    geom_ribbon(data = stat_s, aes(x = x, ymin = lower, ymax = upper),
                fill = "steelblue", alpha = 0.2) +
    geom_line(data = data2, aes(x = x, y = mr), linewidth = 1) +
    geom_point(data = data2, aes(x = x, y = mr), size = 2) +
    geom_label(data = labels, aes(x = x, y = value, label = text), hjust = 1) +
    labs(x = xlab, y = ylab, subtitle = "Moving Range Chart")
}

# ---- Individuals–MR (single asset) -----------------------------------
imr_stats <- function(metric_vec) {
  stopifnot(is.numeric(metric_vec))
  x  <- as.numeric(metric_vec)
  n  <- length(x)
  if (n < 3) stop("I–MR needs at least 3 points.")
  mr <- abs(diff(x))
  mrbar <- mean(mr, na.rm = TRUE)
  
  # Constants for MR of length 2
  d2 <- 1.128
  D3 <- 0.000
  D4 <- 3.267
  
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
                       xlab = "Time (t)", ylab_i = "Individuals", ylab_mr = "Moving Range") {
  stopifnot(length(unique(df$asset_id)) == 1)
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

ggi_mr <- function(df, asset_id, metric_col = "asset_power_w") {
  d <- df %>%
    filter(.data$asset_id == asset_id) %>%
    mutate(t_idx = as.integer(sub("^t_", "", .data$ts))) %>%
    arrange(t_idx)
  ggi_mr_one(d, metric_col = metric_col, t_col = "t_idx",
             ylab_i = paste0("Individuals (", metric_col, ")"),
             ylab_mr = paste0("Moving Range (", metric_col, ")"))
}

# ---- X̄–S convenience wrappers ---------------------------------------
spc_xbar_s <- function(df, metric_col = "asset_power_w",
                       subgroup_col = "t_idx",
                       xlab = "Minute (t)", ylab_mean = "Average", ylab_sd = "SD") {
  stopifnot(all(c(metric_col, subgroup_col) %in% names(df)))
  x <- df[[subgroup_col]]
  y <- df[[metric_col]]
  
  p_xbar <- ggxbar(x = x, y = y, xlab = xlab,
                   ylab = paste0(ylab_mean, " (", metric_col, ")"))
  p_s    <- ggs   (x = x, y = y, xlab = xlab,
                   ylab = paste0(ylab_sd,   " (", metric_col, ")"))
  ggarrange(p_xbar, p_s, ncol = 1, heights = c(2,1))
}

spc_room <- function(df, room_id, metric_col = "asset_power_w") {
  d <- df %>%
    filter(.data$room_id == room_id) %>%
    mutate(t_idx = as.integer(sub("^t_", "", .data$ts)))
  spc_xbar_s(d, metric_col = metric_col)
}

spc_rack <- function(df, rack_id, metric_col = "asset_power_w") {
  d <- df %>%
    filter(.data$rack_id == rack_id) %>%
    mutate(t_idx = as.integer(sub("^t_", "", .data$ts)))
  spc_xbar_s(d, metric_col = metric_col)
}

spc_server <- function(df, server_id, metric_col = "asset_power_w") {
  d <- df %>%
    filter(.data$server_id == server_id) %>%
    mutate(t_idx = as.integer(sub("^t_", "", .data$ts)))
  spc_xbar_s(d, metric_col = metric_col)
}

# =======================================================================
# Usage (matches your calls)
# =======================================================================

set_theme()

df <- read_csv("datacenter_assets_timeseries_8h_GPU_DC-Room-1.csv") %>%
  mutate(t_idx = as.integer(sub("^t_", "", ts)))

# 1) Room-level Xbar–S for GPU power
spc_room(df, room_id = "DC-Room-1", metric_col = "asset_power_w")

# 2) Rack-level Xbar–S (e.g., rack A01)
spc_rack(df, rack_id = "A01", metric_col = "asset_power_w")

# 3) Server-level Xbar–S (e.g., srv-nyc1-025)
spc_server(df, server_id = "srv-nyc1-025", metric_col = "server_temp_c")

# 4) Individuals–MR for a single GPU’s power
ggi_mr(df, asset_id = "GPU-srv-nyc1-025-0", metric_col = "asset_power_w")
