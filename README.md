# test_repo_AT
Test repo for Six Sigma class.

## Components
The dashboard will include a sidebar showing:

- **Number of rooms**
- **Number of racks**
- **Number of servers**
- **Number of GPUs per server**
- **Number of CPUs per server**

## Dataset (`dataset.csv`)
Asset-centric layout. Each row is one **asset** (GPU or CPU).

**Columns**
- `asset_id` — unique ID (e.g., `GPU-A100-001` or `CPU-64C-003`)
- `asset_type` — `GPU` or `CPU`
- `server_id` — server that hosts the asset (e.g., `srv-1B-01`)
- `rack_id` — rack label (e.g., `1B`)
- `room_id` — room label (e.g., `Room-01`)
- `asset_temp_C` — asset temperature (°C)
- `rack_temp_C` — rack temperature near the asset (°C)
- `humidity_pct` — ambient humidity (%)
- `status` — `OK`, `WARN`, or `FAIL`

**Example**
```csv
asset_id,asset_type,server_id,rack_id,room_id,asset_temp_C,rack_temp_C,humidity_pct,status
GPU-A100-001,GPU,srv-1B-01,1B,Room-01,70,23,42,OK
CPU-64C-001,CPU,srv-1B-02,1B,Room-01,61,23,41,OK
GPU-A100-008,GPU,srv-3C-01,3C,Room-02,88,25,46,FAIL


https://www.servermania.com/kb/articles/gpu-temperature-range-guide


https://shiny.posit.co/r/gallery/#user-showcase

good example :
https://shiny.posit.co/r/gallery/miscellaneous/lego-world/

https://www.servermania.com/kb/articles/gpu-temperature-range-guide

