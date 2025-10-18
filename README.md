# This project analyzes GPU failure rates across distributed data centers to improve uptime and energy efficiency. It integrates R-based SPC models and real-time monitoring dashboards for predictive maintenance. It integrates R-based Statistical Process Control (SPC) models with real-time monitoring dashboards to support predictive maintenance workflows.The purpose is to provide transparent, actionable information for every layer of the data center ecosystem. The dashboard allows anyone from on-site maintenance employees to higher-level stakeholders, to quickly identify and respond to GPU instability.

# Specifically, the system answers two key questions:
a) When and where are GPU failures occurring within the data center?
b) How can temperature and variance thresholds be adjusted to fit user-defined preferences?

# The project uses three datasets to model statistical process control (SPC) across data center components:

dataset.csv — system-level data capturing room, rack, and server temperature behavior. Used for baseline process control and hierarchical SPC models.

gpu_data(1).csv — component-level GPU telemetry for monitoring process variance, sigma-short metrics, and failure precursors.

crazy_data.csv — synthetic anomaly dataset used to validate SPC detection thresholds and identify instability under stress conditions.

# Each dataset represents a different layer of operational insight — from macro system health to micro component stability, allowing engineers to visualize and anticipate performance degradation in real-time.

# TEAM
Brandy
Juan
Adele
Michael 


# The Dashboard
The dashboard provides both high-level system views and component-level diagnostics, including a sidebar that displays:
# Number of rooms

# Number of racks

# Number of servers

# Number of GPUs per server

# Number of CPUs per server

These metrics give an at-a-glance summary of infrastructure health while enabling users to drill down into any rack or GPU for detailed SPC visualization.


# Dataset Structures

The first and largest dataset that we developed was the (dataset.csv). Each row represents a single processing asset—GPU or CPU—along with its physical and environmental context. The structure is designed for clarity and traceability, allowing us to move fluidly between system-wide behavior and individual component performance.

By linking every asset to its server, rack, and room, this dataset enables hierarchical analysis that connects local events (like a GPU overheating) to broader operational trends across the data center.
| Column         | Description                                                                                    |
| -------------- | ---------------------------------------------------------------------------------------------- |
| `asset_id`     | Unique identifier for each component (e.g., `GPU-A100-001`, `CPU-64C-003`).                    |
| `asset_type`   | Specifies whether the component is a GPU or CPU.                                               |
| `server_id`    | The server hosting the asset (e.g., `srv-1B-01`).                                              |
| `rack_id`      | Rack label identifying the asset’s physical location within a data center row (e.g., `1B`).    |
| `room_id`      | Room identifier (e.g., `Room-01`) used to map thermal and humidity conditions across clusters. |
| `asset_temp_C` | The current operating temperature of the asset (°C).                                           |
| `rack_temp_C`  | Ambient temperature near the rack that houses the asset (°C).                                  |
| `humidity_pct` | Relative humidity (%) inside the room.                                                         |
| `status`       | Operational state of the asset: `OK`, `WARN`, or `FAIL`.                                       |

#  GPU data

gpu_data(1).csv focuses specifically on GPU-level performance and thermal metrics.
While dataset.csv provides the larger system context, this file isolates the behavior of GPUs themselves — allowing for finer control, variance tracking, and process stability analysis.

Each record captures temperature and status data at the component level, forming the foundation for sigma-short calculations, reliability metrics, and control charts such as Individuals–Moving Range (I–MR) and X̄–S.
| Column       | Description                                                                                                              |
| ------------ | ------------------------------------------------------------------------------------------------------------------------ |
| `gpu_id`     | Unique identifier for each GPU unit under monitoring.                                                                    |
| `server_id`  | The server in which the GPU is installed, linking component data to its broader environment.                             |
| `rack_id`    | Rack identifier corresponding to the server location.                                                                    |
| `asset_type` | Component classification (e.g., `GPU`), used to filter assets for targeted SPC analysis.                                 |
| `timestamp`  | Time of observation, typically formatted as a sequential index or datetime string.                                       |
| `temp_c`     | Recorded GPU temperature (°C). Serves as the main metric for identifying process variation and potential failure events. |




