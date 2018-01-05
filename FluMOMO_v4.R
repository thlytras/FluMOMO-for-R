### Installing required packages #############################################
#required_packages <- c("foreign", "doBy", "ISOweek", "readstata13","ggplot2")
#install.packages(required_packages)
#rm(required_packages)
##############################################################################

### Clear enviromnet i.e. delete all data and variabels ###
rm(list = ls())
### Clear console ###
cat("\014")

### Country ###
country = "Denmark"

### Work directory - directory where the Stata programs are placed
wdir="."

### Period: Start and end (both included) ###
start_year = 2012
start_week = 27
end_year = 2017
end_week = 46

### Deaths data from A-MOMO
# 1 = A-MOMO complete file, renamed to A-MOMO data.txt
# 0 = you provide a Stata data file: deaths.dta, containing the variable: agegrp, year, week, deaths
A_MOMO = 1

### Population data
# 0 = no, 1 = yes
population = 1

### Restrict IA to only positive ###
# 0 = no, 1 = yes
IArest = 1

### Number of IA lags
# 0 = no lag, 1 = one week lag, ...(max=9)
IAlags = 2
### Number of OF lags
# 0 = no lag, 1 = one week lag, ...(max=9)
ETlags = 2

### Direcory setup #######################################################################

### Create general output dir
if (!dir.exists(paste0(wdir,"/FluMOMO_",end_year,"w",end_week))) { dir.create(paste0(wdir,"/FluMOMO_",end_year,"w",end_week)) }

### Copy data directory - directory where input data are stored
indir=paste0(wdir,"/FluMOMO_",end_year,"w",end_week,"/data")
if (!dir.exists(indir)) { dir.create(indir) }
file.copy(from = list.files(paste0(wdir,"/data"), all.files = T, full.names = T, no.. = T),
          to = indir, overwrite = T, recursive = F, copy.mode = T)
#file.remove(list.files(paste0(wdir,"/data"), all.files = T, full.names = T, no.. = T))

### Create output directory - directory where output are created
outdir=paste0(wdir,"/FluMOMO_",end_year,"w",end_week,"/output")
if (!dir.exists(outdir)) { dir.create(outdir) }
#########################################################################################

source(paste0(wdir,"/Estimation_v4.R"), echo = F)
# Output: csv-files
source(paste0(wdir,"/Output_csv_v4.R"))
# Output: graphs IA and ET
source(paste0(wdir,"/output_IA_ET_v4.R"))
# Output: graphs over calendar time
source(paste0(wdir,"/Output_calendar_v4.R"))
# Output: graphs cumulated IA
source(paste0(wdir,"/Output_cumulated_v4.R"))
