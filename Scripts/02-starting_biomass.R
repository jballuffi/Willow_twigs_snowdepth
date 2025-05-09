
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


source("Scripts/00-allometric_eqn.R")
biomass <- fread("Input/transects.csv")

#number of transects done
biomass[, length(unique(Loc)), Grid]

#number of lines done
biomass[, line := tstrsplit(Loc, "_", keep = 1)]
biomass[, length(unique(line)), Grid]

#use only willow
biomass <- biomass[Species == "willow"]


# convert to biomass ------------------------------------------------------

#take just slope
biomass[, Slope := eqn$Slope]

#calculate biomass from branch BD for each height class
biomass[, Mass_low := (Slope*BD)*(Low/100)]
biomass[, Mass_med := (Slope*BD)*(Med/100)]
biomass[, Mass_high := (Slope*BD)*(High/100)]

#replace is.na with zeros
biomass[is.na(Mass_low), Mass_low := 0]
biomass[is.na(Mass_med), Mass_med := 0]
biomass[is.na(Mass_high), Mass_high := 0]

#calculate biomass from branch BD for all height classes combined
biomass[, Mass_total := Mass_low + Mass_med + Mass_high]

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

#set height to leveled factor
heights[, height := factor(Height, levels = c("low", "medium", "high"))]


#get avg biomass by species and height (not grid)
#this will be exported to later be merged with twig availability predictive data sets
#that don't incorporate grid
avg_nogrid <- heights[, .(biomass_mean = mean(Biomass),
                   biomass_median = median(Biomass),
                   biomass_sd = sd(Biomass)), by = .(height)]




# Figures for biomass -----------------------------------------------------

#summary figure showing 
(summary <- ggplot(heights)+
  geom_boxplot(aes(x = height, y = Biomass, fill = height), outlier.shape = NA, alpha = 0.5, color = "grey30")+
  scale_fill_manual(values = heightcols, guide = NULL)+
  labs(x = "Height class", y = "Biomass (dry g/m2)")+
  ylim(0, 45)+
  themethesistop)



# save outputs ------------------------------------------------------------

saveRDS(heights, "Output/Data/02_cleaned_biomass.rds")
saveRDS(avg_nogrid, "Output/Data/02_starting_biomass_means.rds")
ggsave("Output/Figures/sum_starting_biomass.jpeg", summary, width = 5, height = 4, unit = "in")
