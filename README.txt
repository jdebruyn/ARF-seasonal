This folder contains the R code and data files associated with the publication: 
Lois S Taylor, Allison R Mason, Hannah L Noel, Michael E Essington, Mary C Davis, Veronica A Brown, Dawnie W Steadman, Jennifer M DeBruyn, Transient hypoxia drives soil microbial community dynamics and biogeochemistry during human decomposition, FEMS Microbiology Ecology, Volume 100, Issue 10, October 2024, fiae119, https://doi.org/10.1093/femsec/fiae119


TEMPERATURE, MOISTURE, ELECTRICAL CONDUCTIVITY SENSOR DATA
The following files are required:
decagon_SP.csv
decagon_WIN.csv

This pair of files is used to generate all temperature, moisture, and conductivity (sensor-based) figures and table data. 
Temperature data falling below the 0C threshold was corrected to 0C. 
In instances where sensors failed, notably during study day 282 for internal temperature values, ambient mean temperatures were substituted for internal means. 
In instances where sensors skipped an hourly reading (notably during time or battery changes), adjacent values were substituted for the missing data.

Sample metadata and variables:
Column A - Day: cumulative study day in hour fractions
Column B - Measurement time: Decagon sensor output for date and time of data recording in hourly increments
Column C - SP1/WIN1 internal temperature: Hourly temperature recorded from Decagon RT-1 sensor placed posteriorly inside donor SP1/WIN1
Column D - SP2/WIN2 internal temperature: Hourly temperature recorded from Decagon RT-1 sensor placed posteriorly inside donor SP2/WIN2
Column E - SP3/WIN3 internal temperature: Hourly temperature recorded from Decagon RT-1 sensor placed posteriorly inside donor SP3/WIN3
Column F - ambient1 temperature: Hourly temperature recorded from Decagon RT-1 sensor placed aboved ground and open to ambient air
Column G - ambient2 temperature: Hourly temperature recorded from Decagon RT-1 sensor placed aboved ground and open to ambient air
Column H - Ambient means with 0C threshold included: calculated means for hourly ambient temperature measurements. Values falling below 0C were corrected to 0C.
Column I - ADH ambient (with 0C threshold): cumulative hourly ambient temperature means.
Column J - Internal means with 0C threshold included: calculated means for hourly internal temperature measurements. Values falling below 0C were corrected to 0C.
           In instances where sensors failed, notably during study day 282 for internal temperature values, ambient mean temperatures were substituted for internal means. 
Column K - ADH internal (with 0C threshold): cumulative hourly internal temperature means.
Column L - Measurement time: Decagon sensor output for date and time of data recording
Column M - SP1?WIN1 water %VWC: Percent soil moisture recorded from Decagon GS3 sensor placed in soil underneath donor SP1/WIN1
Column N - SP2/WIN2 water %VWC: Percent soil moisture recorded from Decagon GS3 sensor placed in soil underneath donor SP2/WIN2
Column O - SP3/WIN3 water %VWC: Percent soil moisture recorded from Decagon GS3 sensor placed in soil underneath donor SP3/WIN3
Column P - con1 water %VWC: Percent soil moisture recorded from Decagon GS3 sensor placed in soil upslope from donors
Column Q - con2 water %VWC: Percent soil moisture recorded from Decagon GS3 sensor placed in soil upslope from donors
Column R - SP1/WIN1 temperature: Hourly soil temperature (degrees C) recorded from Decagon GS3 sensor placed in soil underneath donor SP1/WIN1
Column S - SP2/WIN2 temperature: Hourly soil temperature (degrees C) recorded from Decagon GS3 sensor placed in soil underneath donor SP2/WIN2
Column T - SP3/WIN3 temperature: Hourly soil temperature (degrees C) recorded from Decagon GS3 sensor placed in soil underneath donor SP3/WIN3
Column U - con1 temperature: Hourly soil temperature (degrees C) recorded from Decagon GS3 sensor placed in soil upslope from donors
Column V - con2 temperature: Hourly soil temperature (degrees C) recorded from Decagon GS3 sensor placed in soil upslope from donors
Column W - SP1/WIN1ec: Hourly soil electrical conductivity (mS/cm) recorded from Decagon GS3 sensor placed in soil underneath donor SP1/WIN1
Column X - SP2/WIN2ec: Hourly soil electrical conductivity (mS/cm) recorded from Decagon GS3 sensor placed in soil underneath donor SP2/WIN2
Column Y - SP3/WIN3ec: Hourly soil electrical conductivity (mS/cm) recorded from Decagon GS3 sensor placed in soil underneath donor SP3/WIN3
Column Z - con1ec: Hourly soil electrical conductivity (mS/cm) recorded from Decagon GS3 sensor placed in soil upslope from donors
Column AA - con2ec: Hourly soil electrical conductivity (mS/cm) recorded from Decagon GS3 sensor placed in soil upslope from donors
Column AB - Moisture means: calculated means for hourly soil moisture measurements 
Column AC - Moisture_conmeans: calculated means for hourly soil moisture control measurements
Column AD - Soiltemp_means: calculated means for hourly soil temperature measurements
Column AE - Soiltemp_conmeans: calculated means for hourly soil temperature control measurements
Column AF - ADH_soil: cumulative hourly temperature calculated from soil temperature 
Column AG - ADH_consoil: cumulative hourly temperature calculated from soil temperature controls 
Column AH - EC_means: calculated means for hourly soil conductivity measurements
Column AI - EC_conmeans: calculated means for hourly soil conductivity control measurements


BIOGEOCHEMICAL DATA, GRAPHS, STATISTICAL TESTS
The following data files are required:
pH_EC_etc_SP.csv
pH_EC_etc_WIN.csv

pH_EC_etc_SP.csv and pH_EC_etc_WIN.csv are the Spring and Winter seasonal biogeochemical datasets used to create biogeochemical graphs and run the statistical tests.  
Sample metadata and variables are as follows:

Location: Where the sample originated - conint (control interface), con15 (control core), grint (grave interface), gr15 (grave core)
Sample: Sample name. A total of 12 samples taken at each daily time point, three each grave interface and core, and their respective controls.
Date: Study date in 5-digit numeric format.
Study-day: Study day begins with day 0 on the first sampling date
ADH_ambient: Accumulated degree hours calculated from the sum of mean hourly ambient temperature data, with a baseline of 0 degrees C.
ADH_internal: Accumulated degree hours calculated from the sum of mean hourly temperatures measured from internal donor probes.
ADH_soil: Accumulated degree hours calculated from the sum of mean hourly temperature measured from soil probes located beneath donors.
Stage: decomposition stage
Gravimetric moisture: soil moisture calculated by oven-dried soil at 105C.
pH: pH of each sample.
EC: electrical conductivity of each sample (µS/cm).
DO: dissolved oxygen in the soil, measred in percent.
CO2: umol gdw soil-1 day-1: respired CO2 in micromoles per gram dry weight soil per day
CO2: ug C gdw soil-1 day-1: respired C per gram dry weight soil per day
NH4: mg per gdw: ammonium expressed in mg per gram dry weight soil
NH4: ug per gdw: ammonium expressed in ug per gram dry weight soil
NO3: mg per gdw: nitrate expressed in mg per gram dry weight soil
NO3: ug per gdw: nitrate expressed in ug per gram dry weight soil
TBS: total body score 
Sorting: sorting field for calculations


BIOGEOCHEMICAL DATA, GRAPHS, STATISTICAL TESTS
The following data file is required:
CN_updatable.csv

CN_updatable.csv contains the Spring and Winter seasonal C and N data used to create biogeochemical graphs and run the statistical tests.  
Sample metadata and variables are as follows:

Season: spring or winter study
Location: conint=control interface, grint=grave interface, con15=conrol core, gr15=grave core 
Study day: Study day begins with day 0 on the first sampling date
Name: Sorting field
Weight mg: weight of sample
N percent: Total N in sample (TN) in percentage of sample
C percent: Total C in sample (TC) in percentage of sample
CN ratio: the ratio of C and N in the sample


TBS GRAPH
The following data file is required:
TBS_ADD.csv

Sample metadata and variables are as follows:

Study day: Date of the study beginning from 0.
ADD: Accumulated Degree Days calculated from daily average ambient temperatures
SP1(2,3)_WIN1(2,3)_TBS: Total Body Scores per Megyesi et al.,(2005) associated with the study day listed.
Study: Spring or winter data.
LogADD: Log of values in the ADD column.


MOTHUR OUTPUT AND METADATA FILES FOR PHYLOSEQ PIPELINE
Spring study ITS Mothur output and metadata files:
NIJARFSP.trim.contigs.pcr.good.unique.precluster.pick.agc.0.05.cons.taxonomy
NIJARFSP.trim.contigs.pcr.good.unique.precluster.pick.agc.shared
SP_ITS_metadata.csv

Spring study 16S Mothur output and metadata files:
NIJARFSP16S.shared
NIJARFSP16S.taxonomy
SP_16S_metadata.csv

Winter study ITS Mothur output and metadata files:
NIJARFWIN.trim.contigs.pcr.good.unique.precluster.pick.agc.0.05.cons.taxonomy
NIJARFWIN.trim.contigs.pcr.good.unique.precluster.pick.agc.shared
WIN_ITS_metadata.csv

Winter study 16S Mothur output and metadata files:
NIJARFWIN16S.shared
NIJARFWIN16S.taxonomy
WIN_16S_metadata.csv

SP_ITS_metadata.csv, SP_16S_metadata.csv, WIN_ITS_metadata.csv, and WIN_16S_metadata.csv are the Spring and Winter seasonal ITS and 16S metadata sets
used for the sequencing data.
Sample metadata and variables are as follows:

Sample: This is the sample name associated with individual donors. Individual donor soils are single samples, and controls are pooled.
Sample_name: This is used by Phyloseq to track samples. The name combines the Sample column, study day, and an alphabetical sorting code.
Type: Where the sample originated - conint (control interface), con15 (control core), grint (grave interface), gr15 (grave core)
Location: Sorting field by grave or control.
Depth: Sorting field by depth: core(1-16 cm) or interface (0-1 cm).
Study_day: Date of the study beginning from 0.
Sort1: optional data sorting field by study day.
Sort2: optional data sorting field by study day.
Stage: optional data sorting field by morphological decomposition stage.
Stage1: optional data sorting field by morphological decomposition stage.
Moisture: soil moisture calculated by oven-dried soil at 105C.
pH: pH of each sample.
EC: electrical conductivity of each sample (µS/cm).
DO: dissolved oxygen in the soil, measred in percent.
CO2: umol gdw soil-1 day-1: respired CO2 in micromoles per gram dry weight soil per day.
NH4: ug per gdw: ammonium expressed in ug per gram dry weight soil.
NO3: ug per gdw: nitrate expressed in ug per gram dry weight soil.
TN: Total N in sample (TN) in percentage of sample.
TC: Total C in sample (TC) in percentage of sample.
CN ratio: the ratio of C and N in the sample.
Copies/gdw: gene copies per gram dry weight of soil (16S or ITS depending on file name).
log copies/gdw: log of gene copy value (16S or ITS depending on file name).
fungal:bacterial ratio: ratio of fungal gene copies to bacterial gene copies (ITS/16S).
log f:b ratio: log of the fungal to bacterial ratio.
Chao1: diversity data generated by Phyloseq.
Shannon: diversity data generated by Phyloseq.
InvSimpson: diversity data generated by Phyloseq.

ALPHA DIVERSITY AND qPCR DATA, GRAPHS, STATISTICAL TESTS
The following data files are required:
SP_16S_alpha_qPCR.csv
SP_16S_alpha_qPCR_mo.csv
SP_ITS_alpha_qPCR.csv
SP_ITS_alpha_qPCR_mo.csv
WIN_16S_alpha_qPCR.csv
WIN_ITS_alpha_qPCR.csv

These files are the Spring and Winter seasonal ITS and 16S data sets generated by Phyloseq for alpha diversity, and the qPCR datasets.
Files ending with the _mo extension (SP series only) have three samples removed from their datasets that correspond to those with f:b ratios>20.
Sample metadata and variables are as follows:

Season: Spring (SP) or winter (WIN) study
Gene: 16S (bacterial) or ITS (fungal)
ADH_ambient: Accumulated degree hours calculated from the sum of mean hourly ambient temperature data, with a baseline of 0 degrees C.
ADH_internal: Accumulated degree hours calculated from the sum of mean hourly temperatures measured from internal donor probes.
ADH_soil: Accumulated degree hours calculated from the sum of mean hourly temperature measured from soil probes located beneath donors.
Location: Where the sample originated - conint (control interface), con15 (control core), grint (grave interface), gr15 (grave core)
Sample: Sample name. A total of 12 samples taken at each daily time point, three each grave interface and core, and their respective controls.
Date: Study date in 5-digit numeric format.
Study-day: Study day begins with day 0 on the first sampling date
Copies/gdw: gene copies per gram dry weight of soil (16S or ITS depending on file name).
log copies/gdw: log of gene copy value (16S or ITS depending on file name).
fungal:bacterial ratio: ratio of fungal gene copies to bacterial gene copies (ITS/16S).
log f:b ratio: log of the fungal to bacterial ratio.
Chao1: diversity data generated by Phyloseq.
Shannon: diversity data generated by Phyloseq.
InvSimpson: diversity data generated by Phyloseq.

