

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

willow <- readRDS("Output/Data/willow_avail_noduplicates.rds")
startingbiomass <- readRDS("Output/Data/starting_biomass_nogrid.rds")
startingnuts <- readRDS("Output/Data/starting_nutrition_wide.rds")



# model willow proportion availability for each height -------------------------------

#write gam for low height class
lowgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "low"])
summary(lowgam)

#gam for medium height class
medgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "medium"])
summary(medgam)

#gam for high height class
highgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "high"])
summary(highgam)

#list the models
mods <- list(lowgam, medgam, highgam)

#name the models
modnames <- c("low", "medium", "high")

#use lapply to apply the getgam function (R/ folder) to these models
#getgam function makes a predictive dataset for snow depths 0 - 100 cm for each gam
modoutlist <- lapply(mods, getgam)

#rbind all the GAM predictions into one data table
modout <- rbindlist(modoutlist, idcol = "height")

#make height a factor with order
modout[, height := factor(height, labels = modnames)]



# Merge with biomass and quality ------------------------------------------

#merge model prediction with wide nutrient summary by height
pred <- merge(modout, startingnuts, by = "height", all.x = TRUE)

#merge model prediction with biomass summary by height
pred <- merge(pred, startingbiomass, by = "height", all.x = TRUE)

#change col names
setnames(pred, c("pred", "lower", "upper"), c("prop", "prop_lower", "prop_upper"))

pred[, height := factor(height, levels = c("low", "medium", "high"))]



# calculate available biomass and nutrients -------------------------------

#NA for %cp when biomass is gone


#get total available biomass for each height and snow depth
pred[, biomassavail := biomass_mean*prop]

#calculate the grams of CP in each height class (Biomass x avg CP composition)
pred[, CPavail_grams := biomassavail*mean_cp]

#calc grams of carbs avail
pred[, carbavail_grams := biomassavail*mean_carb]

#new data set that sums biomass and CP grams for all heights
food_pred <- pred[, .(biomassavail = sum(biomassavail),
                     biomassstart = sum(biomass_mean),
                     CPavail_grams = sum(CPavail_grams),
                     carbavail_grams = sum(carbavail_grams)),
                 by = Snow]


#calculate the avg CP composition taking into account all heights
food_pred[, CPavail_comp := CPavail_grams/biomassavail*100]

#calc the avg carb composition
food_pred[, carbavail_comp := carbavail_grams/biomassavail*100]

#get total proportion of biomass available by day location and species
food_pred[, propavail := biomassavail/biomassstart]

food_pred <- food_pred[order(Snow)]



# figures -----------------------------------------------------------------

#plot the real data that went into GAMs
(willow_gam <- 
   ggplot(willow)+
   geom_point(aes(x = Snow, y = propavail_willow), alpha = 0.5, color = "grey50")+
   geom_smooth(aes(x = Snow, y = propavail_willow, color = height, fill = height), method = "gam")+
   scale_color_manual(values = heightcols, guide = NULL)+
   scale_fill_manual(values = heightcols, guide = NULL)+
   labs(y = "Proportion of twigs available", x = "Snow depth (cm)")+
   facet_wrap(~ height)+
   theme_minimal())

#plot just the gam prediction for snow 0 - 100 cm
(willow_pred <-
  ggplot(pred)+
  geom_ribbon(aes(x = Snow, ymin = prop_lower, ymax = prop_upper), alpha = 0.5, fill = "grey70")+
  geom_path(aes(x = Snow, y = prop, color = height), linewidth = .75)+
  scale_color_manual(values = heightcols, guide = NULL)+
  labs(x = "Snow depth (cm)", y = "Predicted twig availablity")+
  facet_wrap(~height)+
  theme_minimal(base_size = 16))

(biomassplot <- 
    ggplot(food_pred)+
    geom_path(aes(x = Snow, y = biomassavail))+
    labs(x = "", y = "Available biomass (g/m2)")+
    theme_minimal())

(CPplot<- 
    ggplot(food_pred)+
    geom_path(aes(x = Snow, y = CPavail_comp))+
    labs(x = "Snow depth (cm)", y = "Available CP (%)")+
    theme_minimal())

(Carbplot<- 
    ggplot(food_pred)+
    geom_path(aes(x = Snow, y = carbavail_comp))+
    labs(x = "Snow depth (cm)", y = "Available Carbohydrate (%)")+
    theme_minimal())


fullplot <- ggarrange(biomassplot, CPplot, Carbplot, ncol = 1, nrow = 3)



# save predictions --------------------------------------------------------

saveRDS(modout, "Output/Data/willow_avail_prediction.rds")
ggsave("Output/Figures/Willow_avail_gam.jpeg", willow_gam, width = 9, height = 5, unit = "in")
ggsave("Output/Figures/Willow_avail_pred.jpeg", willow_pred, width = 9, height = 4, unit = "in")
ggsave("Output/Figures/Total_food_avail.jpeg", fullplot, width = 5, height = 10, unit = "in")
