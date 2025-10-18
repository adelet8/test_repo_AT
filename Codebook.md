Variable	Description	Units	Example
ts	String time index label for the sample	string (unitless)	t_001
t_idx	Integer time index (derived from ts)	integer	1
time	Timestamp the data were collected (UTC)	datetime (ISO-8601)	2025-01-01T00:00:00Z
room_id	Unique identifier for a room in the data center	string (unitless)	ROOM-01
rack_id	Unique identifier for a rack within a room	string (unitless)	R01
server_id	Unique identifier for a server within a rack	string (unitless)	R01-S01
asset_id	Unique identifier for an asset within a server	string (unitless)	GPU-R01-S01-G01
asset_temp_inlet_c	Asset inlet air temperature at sample time in degrees celcius	double	24.8
server_temp_c	Server sensor temperature at sample time  in degrees celcius	double	28.6
rack_inlet_c	Rack inlet air temperature at sample time in degrees celcius	double	25
room_temp_c	Room ambient temperature at sample time in degrees celcius	double	23.5
