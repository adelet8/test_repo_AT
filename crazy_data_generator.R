# --- deps ---
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

set.seed(42)

# -------- knobs --------
n_racks          <- 6
servers_per_rack <- 5
gpus_per_server  <- 3
step_min         <- 5
duration_hours   <- 8
n_points         <- duration_hours * 60 / step_min  # 96
room_label       <- "Room-1"
start_clock      <- hms::as_hms("00:00:00")         # start at 00:00:00
start_date       <- as.Date("2025-01-01")          # arbitrary date (change if you want)

# simple smooth random walk (AR(1)) so each new temp depends on previous
ar1 <- function(n, base, phi = 0.93, sd = 0.35, start = NULL){
  x <- numeric(n)
  x[1] <- if (is.null(start)) rnorm(1, base, sd) else start
  for(i in 2:n){
    x[i] <- base*(1-phi) + phi*x[i-1] + rnorm(1, 0, sd)
  }
  x
}

# asset grid
assets <- expand_grid(
  rack_ix   = 1:n_racks,
  server_ix = 1:servers_per_rack,
  gpu_ix    = 1:gpus_per_server
) %>%
  mutate(
    rack_id   = sprintf("R%02d", rack_ix),
    server_id = sprintf("%s-S%02d", rack_id, server_ix),
    asset_id  = sprintf("GPU-%s-G%02d", server_id, gpu_ix),
    room_id   = room_label,
    # small offsets so each GPU has a slightly different baseline
    rack_off   = rnorm(n(),  0.5, 0.2),
    server_off = rnorm(n(),  2.0, 0.5),
    gpu_off    = rnorm(n(), 10.0, 1.0),
    #base_temp  = 55 + rack_off + server_off + gpu_off  # ~67°C average baseline
    base_temp  = rnorm(n(), mean = 65, sd = 12) + rack_off + server_off + gpu_off
  )

# time columns: index + real time
ts_vec   <- sprintf("t_%03d", 1:n_points)
time_vec <- start_date + start_clock + minutes(0:(n_points-1) * step_min)

# simulate per GPU
df <- lapply(seq_len(nrow(assets)), function(i){
  a <- assets[i, ]
  # asset temp evolves off its previous value (AR1)
  asset <- ar1(n_points, base = a$base_temp, phi = 0.60, sd = 0.8)
  asset <- pmin(pmax(asset, 10), 125)  # clamp to an unreasonable range
  
  tibble(
    ts                 = ts_vec,
    time               = time_vec,     # real clock time
    asset_id           = a$asset_id,
    server_id          = a$server_id,
    rack_id            = a$rack_id,
    room_id            = a$room_id,
    asset_temp_inlet_c = round(asset, 2),
    rack_inlet_c       = round(asset, 2),  # exact copies per your request
    room_temp_c        = round(asset, 2),
    server_temp_c      = round(asset, 2)
  )
}) %>% bind_rows()

# ensure column order
df <- df %>%
  select(ts, time, asset_id, server_id, rack_id, room_id,
         asset_temp_inlet_c, rack_inlet_c, room_temp_c, server_temp_c)

# write it
write_csv(df, "spc_dataset.csv")

# quick peek
cat("Rows:", nrow(df), " | Cols:", ncol(df), "\n")
print(head(df, 5))
