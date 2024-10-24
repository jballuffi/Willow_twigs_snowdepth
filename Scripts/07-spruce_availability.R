

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#load data for starting biomass and camera traps
biomass <- readRDS("Output/Data/starting_biomass.rds") #WHY IS MEDIAN ZERO
willow <- readRDS("Output/Data/willow_avail_bysite.rds")



# clean/prep data -------------------------------------------------------------

#take only spruce from biomass data
spruce <- biomass[species == "spruce"]

#reorder willow data by date
willow <- willow[order(Location, Date)]



# starting guide for calculating available spruce based on snow depth -------------------------

#hares can reach 50 cm above snow
hreach <- 50

#limits of each height class

#high branches start at 100 cm up to 200 cm
highmin <- 100
highmax <- 200

#medium branches are between 50 and 100 cm
medmin <- 50
medmax <- 100

#low branches are anything below 50 cm, but...
lowmin <- 25 #realistically no growth below 25 cm
lowmax <- 50

#create snow depths from 0 cm to 100 cm
reach <- data.table(snow = 0:200) 

#calculate how high a hare can reach based on snow depth
reach[, reach := snow + hreach]



# calculate spruce availability based on snow depth -----------------------

#calculate what proportion low branches are covered by snow, 
#there is no issue of reaching these branches
reach[snow > lowmin, low_covered := (snow - lowmin)/(lowmax - lowmin)]
#any value above 1 becomes 1, and any NA becomes a zero
reach[low_covered > 1, low_covered := 1.00][is.na(low_covered), low_covered := 0]


#medium branches are only available once snow starts to accumulate
reach[, medium_reach := (reach - medmin)/(medmax - medmin)]
#any value about 1 becomes 1
reach[medium_reach > 1, medium_reach := 1]
#once snow reaches 50 cm it begins to cover snow
reach[snow >= medmin, medium_covered := (snow - medmin)/(medmax - medmin)]
#value over 1 becomes 1 and any NAs become zero
reach[medium_covered > 1, medium_covered := 1][is.na(medium_covered), medium_covered := 0]


#high branches are only available once snow reaches 50 cm
reach[reach >= highmin, high_reach := (reach - highmin)/(highmax - highmin)]
#once snow reaches 100 cm it will cover branches 
reach[snow >= highmin, high_covered := (snow - highmin)/(highmax - highmin)]
#any NAs will be zeros
reach[is.na(high_reach), high_reach := 0][is.na(high_covered), high_covered := 0][high_reach > 1, high_reach := 1]


#final step: calculate total proportion available for each height class
reach[, low := 1 - low_covered] #inverse of what low branches are covered
reach[, medium := medium_reach - medium_covered] #what hares can reach minus what is covered
reach[, high := high_reach - high_covered] #what hares can reach minus what is covered
#reach[, allheights := (low + medium + high)/3] #average to get all heights

#take just final estimates of availabilty for each height class
reach2 <- reach[, .(snow, low, medium, high)]

#melt twig availability by height class
spruce <- data.table::melt(reach2, measure.vars = c("low", "medium", "high"), variable.name = "height", value.name = "propavail_spruce")
setnames(spruce, "snow", "Snow")



# Figure to show calculation ----------------------------------------------

(spruceavail <- 
  ggplot(spruce)+
  geom_path(aes(x = Snow, y = propavail_spruce), linewidth = 2, alpha = 0.5)+
  labs(x = "Snow depth (cm)", y = "Proportion of spruce available")+
  theme_minimal()+
  facet_wrap(~height))



# merge with willow -------------------------------

#merge with willow by snow depth and height class
avail <- merge(willow, spruce, by = c("height", "Snow"), all.x = TRUE)

#reorder final data set
avail <- avail[order(Location, Date)]



#save
saveRDS(spruce, "Output/Data/spruce_avail_prediction.rds")
saveRDS(avail, "Output/Data/proportion_available.rds")
ggsave("Output/Figures/Spruce_avail_calc.jpeg", spruceavail, width = 9, height = 4, unit = "in")
