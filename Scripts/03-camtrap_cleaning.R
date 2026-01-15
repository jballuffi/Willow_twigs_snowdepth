
#script to clean camera trap data and prepare for future merges

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# Read in data ------------------------------------------------------------

#list and fread all cam trap data from 2022-2023
camfiles <- dir("Input/Camera_traps/", recursive = TRUE, full.names = TRUE)
camdat <- lapply(camfiles, fread)

#function to clean up data frames
cleandat <- function(X){
  colnames(X) <- as.character(X[2,])
  x <- tail(X, -2)
  return(x)
}

#clean and rbind data
camdat <- lapply(camdat, cleandat)
cams <- rbindlist(camdat, use.names = FALSE, idcol = "winter")

#re-assign the winter column based on directory name
cams[, winter := factor(winter, labels = dirname(camfiles))]
cams[, winter := tstrsplit(winter, "/", keep = 3)]



# clean data  -------------------------------------------------------------

#take only timer photos
cams <- cams[Trigger == "T"]

#what are the locations
cams[, unique(Location)]

#fix issue with names of locations, replace space with underscores
cams[, Location := gsub(" ", "_", Location)]

#create dates
cams[, Date := tstrsplit(`Image Name`, " ", keep = 1)]
cams[, Date := ymd(Date)]

#check dates
cams[, min(Date), by = .(Location, winter)]

#fix issue with date on one camera
cams[Location == "KL_48" & winter == "2022-2023", Date := Date + 365]

#rename cols
setnames(cams, "Moon Phase", "Moon")

#make snowdepth integer
cams[, Snow := as.integer(`4_snow`)]


# convert moon phases to levels of illumination ---------------------------

#convert to a proportion of illumination
cams[grep("Quarter", Moon), Moon := .5]
cams[grep("New", Moon), Moon := 0]
cams[grep("Full", Moon), Moon := 1]
cams[grep("Crescent", Moon), Moon := .25]
cams[grep("Gibbous", Moon), Moon := .75]

#set as numeric, capitalize 
cams[, Moon := as.numeric(Moon)]



# subset to key cols ------------------------------------------------------

camtwigs <- cams[, .(Location, winter, Date, Time, Moon, Temp, Snow, `1_orange`, `2_yellow`, `3_pink`)]



# save --------------------------------------------------------------------

saveRDS(camtwigs, "Output/Data/03_camtraps.rds")

