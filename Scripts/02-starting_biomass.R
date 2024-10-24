
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


source("Scripts/01-allometric_eqn.R")
biomass <- fread("Input/transects.csv")

#number of transects done
biomass[, length(unique(Loc)), by = Grid]

#use only willow
biomass <- dat[Species == "willow"]


# convert to biomass ------------------------------------------------------

#take just slope
biomass[, Slope := eqn$Slope]

#calculate biomass from branch BD for each height class
biomass[, Mass_low := (Slope*BD)*(Low/100)]
biomass[, Mass_med := (Slope*BD)*(Med/100)]
biomass[, Mass_high := (Slope*BD)*(High/100)]

#calculate biomass from branch BD for all height classes combined
biomass[, Mass_total := Slope*BD]

#replace is.na with zeros
biomass[is.na(Mass_low), Mass_low := 0]
biomass[is.na(Mass_med), Mass_med := 0]
biomass[is.na(Mass_high), Mass_high := 0]
biomass[is.na(Mass_total), Mass_total := 0]

#sum biomass per transect then divide by size of transect (15 m2)
#final unit is g/m2
sums <- biomass[, .(low = sum(Mass_low, na.rm = TRUE)/15, 
                    med = sum(Mass_med, na.rm = TRUE)/15, 
                    high = sum(Mass_high, na.rm = TRUE)/15, 
                    total = sum(Mass_total, na.rm = TRUE)/15), 
                by = .(Grid, Loc)]



# fill in cases where there was no biomass of a species in a transect --------


#create empty sheet of transects and biomass for cases where there is zero biomass of a species
emptywillow <- biomass[, .(Loc = unique(Loc)), by = Grid]

#merge empty sheet with sums sheet and NAs now appear occassionally in the biomass col
sums <- merge(emptywillow, sums, by = c("Grid", "Loc"), all.x = TRUE)

#convert NAs to zeros.
sums[is.na(low), low := 0][is.na(med), med := 0][is.na(high), high := 0][is.na(total), total := 0]



# get average biomass by grid -------------------------

#make data long form to work by height class
heights <- melt.data.table(sums, measure.vars = c("low", "med", "high"),
                variable.name = "Height",
                value.name = "Biomass")

heights[Height == "med", Height := "medium"]

#take average biomass by species, height, and grid
# this will be exported to later be merged with nutrition and twig availability data
avg <- heights[, .(biomass_mean = mean(Biomass),
                   biomass_median = median(Biomass),
                   biomass_sd = sd(Biomass)), by = .(Height, Grid)]
names(avg) <- c("height", "grid", "biomass_mean", "biomass_median", "biomass_sd")

#get avg biomass by species and height (not grid)
#this will be exported to later be merged with twig availability predictive data sets
#that don't incorporate grid
avg_nogrid <- heights[, .(biomass_mean = mean(Biomass),
                   biomass_median = median(Biomass),
                   biomass_sd = sd(Biomass)), by = .(Height)]
names(avg_nogrid) <- c("height", "biomass_mean", "biomass_median", "biomass_sd")


# Figures for biomass -----------------------------------------------------

#set height to leveled factor
heights[, Height := factor(Height, levels = c("low", "medium", "high"))]

#summary figure showing 
(summary <- ggplot(heights)+
  geom_boxplot(aes(x = Height, y = Biomass, fill = Grid), alpha = 0.5, color = "grey30")+
  labs(x = "Height class", y = "Available forage (dry g/m2)")+
  theme_minimal())


# save outputs ------------------------------------------------------------

saveRDS(avg_nogrid, "Output/Data/starting_biomass_nogrid.rds")
saveRDS(avg, "Output/Data/starting_biomass.rds")
ggsave("Output/Figures/sum_starting_biomass.jpeg", summary, width = 7, height = 5, unit = "in")
