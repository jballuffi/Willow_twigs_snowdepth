

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

willow <- readRDS("Output/Data/willow_avail_noduplicates.rds")
startingbiomass <- readRDS("Output/Data/starting_biomass_means.rds")
startingnuts <- readRDS("Output/Data/starting_nutrition_wide.rds")



# make models for each height and extract outputs -------------------------------

#write gam for low height class
lowgam <- gam(propavail_willow ~ s(Snow) + 1, data = willow[height == "low"])
sumlow <- summary(lowgam)

#make s-table, and indicate model, this is for variable stuff
sumslow <- as.data.table(round((sumlow$s.table), 4))
sumslow[, Model := "Low"]
sumslow[, `Dev. Explained` := round(sumlow$dev.expl, 1)]


#gam for medium height class
medgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "medium"])
summed <- summary(medgam)

#make s-table, and indicate model, this is for variable stuff
sumsmed <- as.data.table(round((summed$s.table), 4))
sumsmed[, Model := "Medium"]
sumsmed[, `Dev. Explained` := round(summed$dev.expl, 1)]


#gam for high height class
highgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "high"])
sumhigh <- summary(highgam)

#make s-table, and indicate model, this is for variable stuff
sumshigh <- as.data.table(round((sumhigh$s.table), 4))
sumshigh[, Model := "High"]
sumshigh[, `Dev. Explained` := round(sumhigh$dev.expl, 1)]



# Merge outputs into one table -----------------------------------------------------

#bind all s tables
allsums <- rbind(sumslow, sumsmed, sumshigh)

setcolorder(allsums, c("Model", "edf", "Ref.df", "F", "p-value"))

#round the table to 2 decimal places
summarytable <- allsums %>% mutate_if(is.numeric, round, digits = 2)



# build predictive datasheet ----------------------------------------------

#list the models
mods <- list(lowgam, medgam, highgam)

#name the models
modnames <- c("low", "medium", "high")

#use lapply to apply the getgam function (R/ folder) to these models
#getgam function makes a predictive dataset for snow depths 0 - 90 cm for each gam
modoutlist <- lapply(mods, predsnowgam)

#rbind all the GAM predictions into one data table
modout <- rbindlist(modoutlist, idcol = "height")

#convert height column to a factor with the same name as models
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
pred[, biomassavail_lower := biomass_mean*prop_lower]
pred[, biomassavail_upper := biomass_mean*prop_upper]

#calculate the grams of CP in each height class (Biomass x avg CP composition)
pred[, CPavail_grams := biomassavail*mean_cp]
pred[, CPavail_grams_lower := biomassavail_lower*mean_cp]
pred[, CPavail_grams_upper := biomassavail_upper*mean_cp]

#calc grams of carbs avail
pred[, carbavail_grams := biomassavail*mean_carb]
pred[, carbavail_grams_lower := biomassavail_lower*mean_carb]
pred[, carbavail_grams_upper := biomassavail_upper*mean_carb]

#new data set that sums biomass and CP grams for all heights
food_pred <- pred[, .(biomassavail = sum(biomassavail),
                      biomassavail_lower = sum(biomassavail_lower),
                      biomassavail_upper = sum(biomassavail_upper),
                      biomassstart = sum(biomass_mean),
                      CPavail_grams = sum(CPavail_grams),
                      CPavail_grams_lower = sum(CPavail_grams_lower),
                      CPavail_grams_upper = sum(CPavail_grams_upper),
                      carbavail_grams = sum(carbavail_grams),
                      carbavail_grams_lower = sum(carbavail_grams_lower),
                      carbavail_grams_upper = sum(carbavail_grams_upper)),
                 by = Snow]


#calculate the avg CP composition taking into account all heights
food_pred[, CPavail_comp := CPavail_grams/biomassavail*100]
food_pred[, CPavail_comp_lower := CPavail_grams_lower/biomassavail_lower*100]
food_pred[, CPavail_comp_upper := CPavail_grams_upper/biomassavail_upper*100]

#calc the avg carb composition
food_pred[, carbavail_comp := carbavail_grams/biomassavail*100]
food_pred[, carbavail_comp_lower := carbavail_grams_lower/biomassavail_lower*100]
food_pred[, carbavail_comp_upper := carbavail_grams_upper/biomassavail_upper*100]

food_pred <- food_pred[order(Snow)]



# figures -----------------------------------------------------------------

#plot just the gam prediction and original data for snow 0 - 90 cm
(willow_pred <-
  ggplot()+
  geom_point(aes(x = Snow, y = propavail_willow), alpha = 0.5, color = "grey50", data = willow)+
  geom_ribbon(aes(x = Snow, ymin = prop_lower, ymax = prop_upper), alpha = 0.5, fill = "grey70", data = pred)+
  geom_path(aes(x = Snow, y = prop, color = height), linewidth = .75, data = pred)+
  scale_color_manual(values = heightcols, guide = NULL)+
  labs(x = "Snow depth (cm)", y = "Proportion of twigs available")+
  facet_wrap(~height, dir = "v")+
  themepoints)

(biomassplot <- 
    ggplot(food_pred)+
    geom_ribbon(aes(x = Snow, ymin = biomassavail_lower, ymax = biomassavail_upper), alpha = 0.3, color = "grey")+
    geom_line(aes(x = Snow, y = biomassavail))+
    labs(x = "", y = "Available biomass (g/m2)")+
    themepoints)

(CPplot<- 
    ggplot(food_pred)+
    geom_ribbon(aes(x = Snow, ymin = CPavail_comp_lower, ymax = CPavail_comp_upper), alpha = 0.3, color = "grey")+
    geom_line(aes(x = Snow, y = CPavail_comp))+
    labs(x = "Snow depth (cm)", y = "Available CP (%)")+
    themepoints)

(Carbplot<- 
    ggplot(food_pred)+
    geom_ribbon(aes(x = Snow, ymin = carbavail_comp_lower, ymax = carbavail_comp_upper), alpha = 0.3, color = "grey")+
    geom_path(aes(x = Snow, y = carbavail_comp))+
    labs(x = "Snow depth (cm)", y = "Available Carbohydrate (%)")+
    themepoints)


fullplot <- ggarrange(biomassplot, CPplot, Carbplot, ncol = 1, nrow = 3)



# save predictions --------------------------------------------------------

saveRDS(modout, "Output/Data/willow_avail_prediction.rds")
write.csv(summarytable, "Output/Tables/GAM_output_table.rds")
ggsave("Output/Figures/Willow_avail_pred.jpeg", willow_pred, width = 5, height = 10, unit = "in")
ggsave("Output/Figures/Total_food_avail.jpeg", fullplot, width = 5, height = 10, unit = "in")
