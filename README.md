# EastWoods-MonitoringPlots
Code for various aspects of management for ecological monitoring in the East Woods
********
## Overview:
This is designed to be the first step a multi-purpose monitoring plan.  In 2017, we will start small with intensive surveys in 4 plots.  Measurements in these plots will include (but not limited to) micrometeorology, vegetation inventories, phenology, and tree growth analysis via dendrometer bands.  Intended uses of this data include:
Comparative phenological analysis among plant functional types (forbs, shrubs, trees) over multiple years and the effects of phenology on micrometerology
Comparative phenological analysis between accessioned, open-grown oaks of the Living Collections and native oaks in their natural habitats.
Pilot data collection & analysis for 2018 re-inventory of the East Woods and development of monitoring protocol to aid in data-backed adaptive management of the East Woods.  2017 data collection will focus on the effects of fire on vegetation communities and oak regeneration.
********
## Development:
*We welcome contributions from any individual, whether code, documentation, or issue tracking.  
All participants are expected to follow the [code of conduct](https://github.com/PalEON-Project/modeling_met_ensemble/blob/master/code_of_conduct.md) for this project.*

- **Lead: Christy Rollinson, Forest Ecologist, Morton Arboretum, crollinson@mortonarb.org**
- Collected and/or lab analysis by: 
	* Drew Duckett
	* Bethany Zumwalde
	* Note: To add in non-RA personnel
	
- Contact for data: Christy Rollinson, Forest Ecologist, Morton Arboretum, crollinson@mortonarb.org

- Date of data collection
2017- present

- Information about geographic location of data collection
The Morton Arboretum East Woods, Lisle, IL

********
## Plot & Sampling Design:
### Plot Selection
- 1 plot in each of 4 stands with no records of harvest in general GIS files:
 
- 2 stands in black have no records of fire or harvest (although there has likely been both)

### Plot Layout
Nested, hierarchical design
Big Plot (20 x 20 m): Trees >=10 cm DBH
-  Micrometeorology station
	* PAR
	* Air Temperature
	* Air Humidity
	* Soil moisture
	* Soil temperature
- Trees >= 10 cm DBH
	* Tag; record species
	* DBH
	* Location in plot grid
	* Dendroband
	* Core
	* phenology(?)
- Litter traps (x 4) --  to be installed late summer/fall 2017

Sub Plot (5 x 5 m, x3?)
- Saplings <10 cm DBH; > 1.3 m tall
	* DBH, species
	* Map location
	* Height
	* Dendrometer band?
	* Phenology (?)
- Shrubs
	* Tag individuals, species
	* % cover
	* Phenology(?)
	
Sub-Sub Plot (1 m x 1 m, x3?)
- Forbs/Grasses
	* % cover by species
- Seedlings
	* Count -- new, >1yr old


********
## Available Data
The following variables are available in our data (code, description, units):
### Meteorology Data
| Variable | Description | Units |
| -------- | ----------- | ----- |
| Time | Time at which met station is recorded data point (GMT-05:00)  |  Date: MM/DD/YYYY  H:M:S AM/PM |
| Plot | the plot where the met station is recording data (e.g. N115, HH115)  |  °F |
| Soil_temp | soil temperature |  °F |
| Air_temp | air temperature  | character string |
| Water_content | soil moisture |  m<sup>3</sup>/ m<sup>3</sup> |
| Relative_humidity | relative humidity of air | % |
| PAR | Photosynthetically Active Radiation | uE |

### Dendrometer Bands
| Variable | Description | Units |
| -------- | ----------- | ----- |
| site | observation site; This should correspond to the name of a Living Collection (e.g. Oak Collection) or designated site as part of the Morton Arboretum group (e.g. King's Woods) | character string |
| plot | the plot where the dendrometer bands were measured (e.g. N115, HH115)  | character string |
| date_observed | date of observation in field  | Date: YYYY-MM-DD |
| genus | individual genus (e.g. Quercus)  | character string |
| species | individual specific epithet (species; e.g. rubra) | character string |
| id | identification in local & NPN; if accessioned, use accession ID | character string |
| observer | last name of observer; if necessary add first/middle initial | character string |
| date_entered | date data entered into local database | Date: YYYY-MM-DD |
| data_entry | name of person entering data into local database | character string |
| dist_from_collar | distance from the edge of the dendroband collar to the initial mark measured by calipers | mm |

### Leaf Litter
| Variable | Description | Units |
| -------- | ----------- | ----- |
| sorter | last name of person sorting samples; if necessary add first/middle initial | character string |
| date_collection | date of sample collection in field  | Date: YYYY-MM-DD |
| plot | the plot where the leaf litter was collected (e.g. HH115, B127)  | character string |
| trap_ID | the trap where the leaf litter was collected (e.g. N, W)  | character string |
| genus | individual genus (e.g. Quercus)  | character string |
| species | individual specific epithet (species; e.g. rubra) | character string |
| oak_group | If unable identify to species rank, denotes if belonging to white or red oak groups | character string |
| tissue | type of tissue being weighed (e.g. leaf, twig, flower, fruit, etc.) | character string |
| num_fruit | number of fruit | numeric |
| num_immature_fruit | num_fruit that is immature | numeric |
| num_mature_fruit | num_fruit that is mature | numeric |
| weigher | last name of person weighing samples | character string |
| date_weighed | date data entered into local database | Date: YYYY-MM-DD |
| mass | mass of sample weighed | grams (g) |
| data_entry | last name of person entering data from printed data sheet; if necessary add first/middle initial | character string |
| notes | any additional notes | character string |

### Tree Survey
Note: To add in
### Plot Survey



********
## Workflow (how you should use this repository):
All scripts to be executed are in the "scripts/" directory. 
Scripts are generally R-based and should generate the dependent file structures as you go
If you clone/fork/branch this code for a different system, you will want to adjust these scripts for your particular system. 
Many of the numbered R scripts will call generalized functions (scripts without numbers). 
If you're looking for the nuts & bolts of how each step is done, these are the scripts to look at.

### Description of Workflow Scripts/Steps

#### Meteorology
1. **MetData_ConsolidatingRawData.R** 

2. **Met_QAQC.R**
#### Dendrometer
1. **dendroband_plotting_BZ.R**

2. **maps_dendrometer_bands_eastwoods.R**
#### Leaf Litter
1. **LeafLitter.R**
This script extracts data from [Google Sheets document](https://docs.google.com/spreadsheets/d/1d7Py4ehN2PmrmKmyv2hDUkX4fWa95xdlQlGVBN9x20g/edit#gid=0) and runs initial QAQC on data.

