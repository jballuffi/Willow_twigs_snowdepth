
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

#data
startingbiomass <- readRDS("Output/Data/starting_biomass_nogrid.rds")
startingnuts <- readRDS("Output/Data/starting_nutrition.rds")

#change col names in starting nutrients
setnames(startingnuts, "Height", "height")

#predictive data
willow <- readRDS("Output/Data/willow_avail_prediction.rds")



# Combine spruce and willow into one data set -------------------------------

#cut to just up to 100 cm snow depth because prediction after that is incorrect
willow <- willow[Snow < 101]


# merge with biomass and quality data  ------------------------------------------------------

snowpred <- merge(willow, startingbiomass[, .(height, biomass_mean)], 
                 by = "height", all.x = TRUE)

#merge in quality to biomass and proportion available
snowpred <- merge(snowpred, startingnuts[, .(height, CP_mean)], 
               by = "height", all.x = TRUE)



# get total available biomass and CP for each row (i.e., height) -------------------------------------

#get total available biomass for each height and snow depth
snowpred[, biomassavail := biomass_mean*pred]

#calculate the grams of CP in each height class (Biomass x avg CP composition)
snowpred[, CPavail_grams := biomassavail*CP_mean]



# sum up total biomass and CP for each snow depth and species -------------

#new data set that sums biomass and CP grams for all heights
pred <- snowpred[, .(biomassavail = sum(biomassavail),
                            biomassstart = sum(biomass_mean),
                            CPavail_grams = sum(CPavail_grams)),
                        by = Snow]


#calculate the avg CP composition taking into account all heights
pred[, CPavail_comp := CPavail_grams/biomassavail*100]

#get total proportion of biomass available by day location and species
pred[, propavail := biomassavail/biomassstart]

pred <- pred[order(Snow)]

(biomassplot <- 
  ggplot(pred)+
  geom_path(aes(x = Snow, y = biomassavail))+
  labs(x = "", y = "Available biomass (g/m2)")+
  theme_minimal())

(CPplot<- 
  ggplot(pred)+
  geom_path(aes(x = Snow, y = CPavail_comp))+
  labs(x = "Snow depth (cm)", y = "Available CP (%)")+
  theme_minimal())

fullplot <- ggarrange(biomassplot, CPplot, ncol = 1, nrow = 2)


# save  -------------------------------------------------------------------

saveRDS(pred, "Output/Data/snowdepth_predictions.rds")
ggsave("Output/Figures/total_snow_pred.jpeg", width = 8, height = 7, unit = "in")
