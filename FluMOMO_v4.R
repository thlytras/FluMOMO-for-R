# Install required packages (if not already installed)
# install.packages(c("readstata13","ggplot2"))


# Clear enviromnet i.e. delete all data and variabels
rm(list = ls())
# Clear console
cat("\014")

# Country
country <- "Denmark"

# Work directory - directory where the Stata programs are placed
wdir <- "."

# Period: Start and end (both included)
start_year <- 2012
start_week <- 27
end_year <- 2017
end_week <- 46

# Deaths data from A-MOMO
# 1 = A-MOMO complete file, renamed to A-MOMO data.txt
# 0 = you provide a Stata data file: deaths.dta, containing the variable: agegrp, year, week, deaths
A_MOMO <- 1

# Population data
population <- TRUE

# Restrict IA to only positive
IArest <- TRUE

# Number of IA lags
# 0 = no lag, 1 = one week lag, ...(max=9)
IAlags <- 2

# Number of OF lags
# 0 = no lag, 1 = one week lag, ...(max=9)
ETlags <- 2


# ## Direcory setup #######################################################################

# ## Create general output dir
if (!dir.exists(paste0(wdir,"/FluMOMO_",end_year,"w",end_week))) { dir.create(paste0(wdir,"/FluMOMO_",end_year,"w",end_week)) }

# ## Copy data directory - directory where input data are stored
indir <- paste0(wdir,"/FluMOMO_",end_year,"w",end_week,"/data")
if (!dir.exists(indir)) { dir.create(indir) }
file.copy(from = list.files(paste0(wdir,"/data"), all.files = TRUE, full.names = TRUE, no.. = TRUE),
          to = indir, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
#file.remove(list.files(paste0(wdir,"/data"), all.files = TRUE, full.names = TRUE, no.. = TRUE))

# ## Create output directory - directory where output are created
outdir <- paste0(wdir,"/FluMOMO_",end_year,"w",end_week,"/output")
if (!dir.exists(outdir)) { dir.create(outdir) }
# ########################################################################################

source(paste0(wdir,"/Estimation_v4.R"), echo = FALSE)
# Output: csv-files
source(paste0(wdir,"/Output_csv_v4.R"))
# Output: graphs IA and ET
source(paste0(wdir,"/Output_IA_ET_v4.R"))
# Output: graphs over calendar time
source(paste0(wdir,"/Output_calendar_v4.R"))
# Output: graphs cumulated IA
source(paste0(wdir,"/Output_cumulated_v4.R"))

