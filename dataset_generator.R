library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

#same random results every time
set.seed(42)

# set constants
#set number of racks,servers,gpus for this datacenter just for testing purposes
n_racks          <- 6 
servers_per_rack <- 5
gpus_per_server  <- 3
step_min         <- 5
duration_hours   <- 8
n_points         <- duration_hours * 60 / step_min  # 96
room_label       <- "Room-1"
start_clock      <- hms::as_hms("00:00:00")  # start at 00:00:00
start_date       <- as.Date("2025-01-01") # arbitrary date

# simple smooth random walk (AR(1)) so each new temp depends on previous
ar1 <- function(n, base, phi = 0.93, sd = 0.35, start = NULL){
  
  #vector of length n
  x <- numeric(n) 
  
  #make sure initial value is near  or given starter value
  x[1] <- if (is.null(start)) rnorm(1, base, sd) else start
  
  
  for(i in 2:n){
    #phi is close to 1 meaning smooth ish series
    #sd is small so step by step is small realistic changes in temperatures
    #AR(1) walk :
    x[i] <- base*(1-phi) + phi*x[i-1] + rnorm(1, 0, sd)
  }
  x
}

# asset grid
assets <- expand_grid(
  rack_ix   = 1:n_racks, #all rack indices
  server_ix = 1:servers_per_rack, #all server indices in each rack
  gpu_ix    = 1:gpus_per_server #all GPU indices in each server
) %>%
  mutate(
    rack_id   = sprintf("R%02d", rack_ix),
    server_id = sprintf("%s-S%02d", rack_id, server_ix),
    asset_id  = sprintf("GPU-%s-G%02d", server_id, gpu_ix),   #
    room_id   = room_label,                       #same room for all 
    
  
    # small offsets so each GPU has different baseline
    rack_off   = rnorm(n(),  0.5, 0.2),
    server_off = rnorm(n(),  2.0, 0.5),
    gpu_off    = rnorm(n(), 10.0, 1.0),    
    
    #baseline temp ~67°C average  
    base_temp  = 55 + rack_off + server_off + gpu_off 
  )

# time columns: index + real time
ts_vec   <- sprintf("t_%03d", 1:n_points)

#real timestamp for 96 steps
time_vec <- start_date + start_clock + minutes(0:(n_points-1) * step_min)

# simulate per GPU
df <- lapply(seq_len(nrow(assets)), function(i){
  a <- assets[i, ] # pick  i-th GPU's row
  
  # asset temp changes based off its previous value (AR1)
  asset <- ar1(n_points, base = a$base_temp, phi = 0.93, sd = 0.35)
  
  # clamp to a reasonable range
  asset <- pmin(pmax(asset, 30), 95) 
  
  tibble(
    ts                 = ts_vec,       #time fram from 0 to 96 of ith gpu
    time               = time_vec,     # real clock time
    asset_id           = a$asset_id,   #asset id of ith gpu
    server_id          = a$server_id,  #server id of ith gpu
    rack_id            = a$rack_id,    #rack id of ith gpu
    room_id            = a$room_id,
    asset_temp_inlet_c = round(asset, 2),  #generated temp of ith gpu
    rack_inlet_c       = round(asset, 2),  #copy of asset temp 
    room_temp_c        = round(asset, 2),  #copy of asset temp 
    server_temp_c      = round(asset, 2)  #copy of asset temp 
  )
}) %>% bind_rows()

# ensure column order
df <- df %>%
  select(ts, time, asset_id, server_id, rack_id, room_id,
         asset_temp_inlet_c, rack_inlet_c, room_temp_c, server_temp_c)

# save as csv
write_csv(df, "spc_dataset.csv")







