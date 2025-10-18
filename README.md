# Prompt: Analysis GPU Failure Rates within Data Centers 

 This project analyzes GPU failure rates across distributed data centers to improve uptime and energy efficiency. It integrates R-based SPC models and real-time monitoring dashboards for predictive maintenance. It integrates R-based Statistical Process Control (SPC) models with real-time monitoring dashboards to support predictive maintenance workflows. 

 The purpose is to provide transparent, actionable information for every layer of the data center ecosystem. The dashboard allows anyone from on-site maintenance employees to higher-level stakeholders, to quickly identify and respond to GPU instability.

## Specifically, the system answers two key questions:


a) When and where are GPU failures occurring within the data center?


b) How can temperature and variance thresholds be adjusted to fit user-defined preferences?


## The project uses three datasets to model statistical process control (SPC) across data center components:

dataset.csv - system-level data capturing room, rack, and server temperature behavior. Used for baseline process control and hierarchical SPC models.

gpu_data(1).csv — component-level GPU telemetry for monitoring process variance, sigma-short metrics, and failure precursors.

crazy_data.csv — synthetic anomaly dataset used to validate SPC detection thresholds and identify instability under stress conditions.


Each dataset represents a different layer of operational insight — from macro system health to micro component stability, allowing engineers to visualize and anticipate performance degradation in real-time.


## TEAM

Brandy Kinnunen, Juan Rodriguez, Adele Thompson, Michael Fizdale



# The Dashboard


The dashboard provides both high-level system views and component-level diagnostics, including a sidebar that displays:

 - Number of rooms

-  Number of racks

-  Number of servers

-  Number of GPUs per server

-  Number of CPUs per server


These metrics give an at-a-glance summary of infrastructure health while enabling users to drill down into any rack or GPU for detailed SPC visualization.


## Dataset Structures

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


##  GPU data

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

# Structure and Purpose

This dataset is designed for localized statistical monitoring.By isolating GPU data, it allows analysts to evaluate short-term process variation (σ-short), identify control limit breaches, and detect early signs of degradation in individual components.

The structure mirrors the broader system hierarchy but simplifies it to the variables most relevant to GPU performance. It’s intentionally compact — optimized for high-frequency sampling and SPC computation efficiency.

This structure allows for precise time-series tracking of each GPU, making it straightforward to visualize process trends, calculate control limits, and evaluate how performance shifts across time or environmental changes.

 crazy_data.csv is the dataset used for stress testing and anomaly detection.Unlike the other two datasets, this one intentionally includes irregular, unstable, or extreme values to test how well the SPC model responds when systems drift outside normal limits.


It’s designed to simulate unexpected operating conditions—rapid temperature spikes, inconsistent readings, or correlated rack-level faults—and is essential for validating that the dashboard’s control limits and sigma thresholds perform as intended.


| Column       | Description                                                                                                   |
| ------------ | ------------------------------------------------------------------------------------------------------------- |
| `timestamp`  | Sequential or datetime index marking each measurement event.                                                  |
| `rack_id`    | Rack identifier showing where the anomaly occurred.                                                           |
| `server_id`  | The server affected during the recorded event.                                                                |
| `gpu_id`     | GPU identifier associated with the anomaly instance.                                                          |
| `asset_type` | Classification of the component (`GPU`, sometimes `CPU`) for multi-component testing.                         |
| `temp_c`     | Recorded temperature (°C) under simulated stress or outlier conditions.                                       |
| `status`     | Observed system response (`OK`, `WARN`, `FAIL`), used to confirm correct alert behavior under extreme inputs. |


# Dataset Relationship Overview


The three datasets—dataset.csv, gpu_data(1).csv, and crazy_data.csv—are designed to work together as a hierarchical data system.
Each serves a distinct purpose within the SPC workflow, moving from macro-level operational context to micro-level component behavior and finally to controlled anomaly testing.


This structure mirrors the way a real data center operates: stable at the top level, variable at the component level, and unpredictable under failure or stress.
By modeling data at all three scales, the system can detect deviations early, trace their origin, and validate its own reliability under extreme conditions.


 | Level                 | Dataset           | Focus                     | Example Use                                                                                     |
| --------------------- | ----------------- | ------------------------- | ----------------------------------------------------------------------------------------------- |
| **System-Level**      | `dataset.csv`     | Rooms, racks, and servers | Analyze average rack temperatures or environmental drift across multiple rooms.                 |
| **Component-Level**   | `gpu_data(1).csv` | Individual GPUs           | Track process variation, temperature stability, and sigma-short behavior for each GPU.          |
| **Stress-Test Level** | `crazy_data.csv`  | Simulated anomalies       | Test alert thresholds and control limits under extreme temperature or performance fluctuations. |



## Why it Works

This layered approach ensures:

- Traceability: Every data point can be connected from component to environment.

- Scalability:  Models can expand to include additional sensors or performance metrics.

-  Validation:   System response can be tested against synthetic or real-world anomalies.


 In short, the dataset architecture is both practical and extensible — allowing engineers to evaluate, tune, and trust the SPC system before it’s deployed in live data center environments.

 ## How it Works

 A dashboard was created to visualize the temperature data for each of the components and layers in a Data Center - from the Data Center Rooms, to the racks, the servers, and the GPUs themselves. In order to run the dashboard, the user will need to have the "SPC.R" functions saved in the same directory as the "app.r" file. These functions were adapted to the model from Dr. Timothy Fraser's statistical process control functions for r-code. These functions were used to generate the Average and Standard Deviation plots, as well as the control process indices. 
 
  Additionally, the user will need to download the .csv files with the temperature data, as described above. These files do not need to be saved in the same directory as the "app.r" file because the user will be able to upload them directly into the dashboard. This feature was provided to allow users (Data Center workers or management) to analyze their own datasets, as long as the required data columns are present.
  
  Another input the user needs to be aware of are the Specification Limits. These are enabled through a check-box that the user can toggle. When checked-on, the specification limits are plotted on the temperature-time graph, as well as the data histogram. Since specification limits are user defined (i.e. the data center who is operating the racks, servers, and GPUs), the limits may vary between different customers, based on the quality of equipment they are using and the amount of risk they are willing to assume. As a result, sliding bars were utilized to allow the user to define their specification limits. When temperature values violate these thresholds (both upper and lower), the GPUs are considered to have failed. These failures are indicated by highlighting the data points "red" (high out of spec) or "blue" (low out of spec).

  ## Conclusion

  The dashboard will allow users to gain a quick and visual means for identifying failed GPUs and tracking trends that could indicate elevated risk of concern.



  
