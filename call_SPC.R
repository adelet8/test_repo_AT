library(readr); library(dplyr)
source("SPC.R")

df <- read_csv("datacenter_assets_timeseries_8h_GPU_DC-Room-1.csv") %>%
  mutate(t_idx = as.integer(sub("^t_", "", ts)))
df
# 1) Room-level Xbar–S for GPU power
spc_room(df, room_id = "DC-Room-1", metric_col = "rack_inlet_c")

# 2) Rack-level Xbar–S (e.g., rack A01)
spc_rack(df, rack_id = "A02", metric_col = "asset_temp_inlet_c")

# 3) Server-level Xbar–S (e.g., srv-nyc1-025)
spc_server(df, server_id = "srv-nyc1-025", metric_col = "server_temp_c")




