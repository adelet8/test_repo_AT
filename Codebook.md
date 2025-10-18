

# Datacenter Dataset Codebook

This dataset contains temperature and metadata from datacenter components, sampled every 5 minutes (12 samples/hour).

| Variable | Description | Type | Units | Example |
|-----------|-------------|------|--------|----------|
| **ts** | String time index label for the sample | string | – | `t_001` |
| **t_idx** | Integer time index (derived from `ts`) | integer | – | `1` |
| **time** | Timestamp the data were collected (UTC, ISO-8601) | datetime | – | `2025-01-01T00:00:00Z` |
| **room_id** | Unique identifier for a room in the data center | string | – | `ROOM-01` |
| **rack_id** | Unique identifier for a rack within a room | string | – | `R01` |
| **server_id** | Unique identifier for a server within a rack | string | – | `R01-S01` |
| **asset_id** | Unique identifier for an asset within a server | string | – | `GPU-R01-S01-G01` |
| **asset_temp_inlet_c** | Asset inlet air temperature at sample time | double | °C | `24.8` |
| **server_temp_c** | Server sensor temperature at sample time | double | °C | `28.6` |
| **rack_inlet_c** | Rack inlet air temperature at sample time | double | °C | `25.0` |
| **room_temp_c** | Room ambient temperature at sample time | double | °C | `23.5` |
| **asset_type** *(optional)* | Asset class (e.g., GPU, CPU, ACC) | string | – | `GPU` |


**Hierarchy:** room_id → rack_id → server_id → asset_id  
**Primary key:** (time, asset_id)
