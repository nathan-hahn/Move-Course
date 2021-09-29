This annotated dataset comes from the Environmental Data Automated Track Annotation System (Env-DATA) on Movebank (movebank.org). The environmental data attributes are created and distributed by government and research organizations. For general information on the Env-DATA System, see Dodge et al. (2013) and movebank.org/node/6607.

Terms of Use: Verify the terms of use for relevant tracking data and environmental datasets prior to presenting or publishing these data. Terms of use for animal movement data in Movebank are defined by the study owners in the License Terms for the study. Terms of use for environmental datasets vary by provider; see below for details. When using these results in presentations or publications, acknowledge the use of Env-DATA and Movebank and cite Dodge et al. (2013). Sample acknowledgement: "[source product/variable] values were annotated using the Env-DATA System on Movebank (movebank.org)." Please send copies of published work to support@movebank.org.

Contact: support@movebank.org. Include the access key below with questions about this request.

---------------------------

Annotated data for the following Movebank entities are contained in this file:
Movebank study name: LifeTrack White Stork SW Germany
Annotated Animal IDs: Linus-A + / DER A2L84 (eobs 6592)
Requested on Tue Sep 14 19:54:15 CEST 2021
Access key: 1681437255480774320
Requested by: Hanna McCaslin

---------------------------

File attributes

Attributes from the Movebank database (see the Movebank Attribute Dictionary at http://www.movebank.org/node/2381):
Location Lat: latitude in decimal degrees, WGS84 reference system
Location Long: longitude in decimal degrees, WGS84 reference system
Timestamp: the time of the animal location estimates, in UTC
Height Above Ellipsoid: the height above the ellipsoid returned by the GPS unit
Eobs Key Bin Checksum
Eobs Start Timestamp
Eobs Type Of Fix
Eobs Status
Eobs Used Time To Get Fix
Eobs Battery Voltage
Eobs Fix Battery Voltage
Eobs Temperature
Ground Speed
Heading
Eobs Speed Accuracy Estimate
Eobs Horizontal Accuracy Estimate
Eobs Activity
Eobs Activity Samples
Manually Marked Outlier
Update Ts
Import Marked Outlier
GPS DOP
GPS Satellite Count
Barometric Pressure
Data Decoding Software
Orientation Quaternion Raw W
Orientation Quaternion Raw X
Orientation Quaternion Raw Y
Orientation Quaternion Raw Z
Magnetic Field Raw X
Magnetic Field Raw Y
Magnetic Field Raw Z

Locations are the the geographic coordinates of locations along an animal track as estimated by the processed sensor data.


---------------------------

Attributes from annotated environmental data:
Name: ECMWF ERA5 PL U Wind
Description: Velocity of the east-west (zonal) component of wind. Positive values indicate west to east flow.
Unit: m/s
No data values: NaN (interpolated)
Interpolation: bilinear
Z-Dimension: interpolated to animal height

Name: ECMWF ERA5 PL V Wind
Description: Velocity of the north-south (meridional) component of wind. Positive values indicate south to north flow.
Unit: m/s
No data values: NaN (interpolated)
Interpolation: bilinear
Z-Dimension: interpolated to animal height

Name: ASTER ASTGTM2 Elevation
Description: Ground elevation above mean sea level
Unit: m
No data values: -9999 (provider), NaN (interpolated)
Interpolation: bilinear

Name: ECMWF ERA5 PL Temperature
Description: Air temperature.
Unit: K
No data values: NaN (interpolated)
Interpolation: bilinear
Z-Dimension: interpolated to animal height

---------------------------

Environmental data services

Service: ASTER ASTGTM2 Global 30-m DEM
Provider: NASA Land Processes Distributed Active Archive Center
Datum: N/A
Projection: N/A
Spatial granularity: 1 arc-second
Spatial range (long x lat): E: 180.0    W: -180.0 x N: 83.0    S: -83.0
Temporal granularity: N/A
Temporal range: N/A
Source link: https://doi.org/10.5067/ASTER/ASTGTM.002
Terms of use: https://lpdaac.usgs.gov/citing_our_data
Related websites:
https://lpdaac.usgs.gov/products/astgtmv002/
https://doi.org/10.5067/ASTER/ASTGTM.002

Service: ECMWF Global Atmospheric Reanalysis/ERA5 Pressure Levels
Provider: European Centre for Medium-Range Weather Forecasts
Datum: N/A
Projection: N/A
Spatial granularity: 0.25 degrees
Spatial range (long x lat): E: 180.0    W: -180.0 x N: 89.463    S: -89.463
Temporal granularity: hourly
Temporal range: 1979-01-01 to present
Source link: https://doi.org/10.24381/cds.bd0915c6
Terms of use: https://confluence.ecmwf.int/display/CKB/How+to+acknowledge%2C+cite+and+reference+data+published+on+the+Climate+Data+Store
Related websites:
https://doi.org/10.24381/cds.bd0915c6

---------------------------

Dodge S, Bohrer G, Weinzierl R, Davidson SC, Kays R, Douglas D, Cruz S, Han J, Brandes D, Wikelski M (2013) The Environmental-Data Automated Track Annotation (Env-DATA) System: linking animal tracks with environmental data. Movement Ecology 1:3. doi:10.1186/2051-3933-1-3

Development and maintenance of Env-DATA is funded by the Max Planck Society, and has been supported by US National Science Foundation Biological Infrastructure award 1564380, NASA ABoVE project NNX15AT91A, and NASA Earth Science Division, Ecological Forecasting Program Project NNX11AP61G.