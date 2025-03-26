

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

willow <- readRDS("Output/Data/04_willow_avail_noduplicates.rds")
startingbiomass <- readRDS("Output/Data/02_starting_biomass_means.rds")
startingnuts <- readRDS("Output/Data/01_starting_nutrition_wide.rds")



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

#calc grams of carbs avail
pred[, NDSavail_grams := biomassavail*mean_nds]
pred[, NDSavail_grams_lower := biomassavail_lower*mean_nds]
pred[, NDSavail_grams_upper := biomassavail_upper*mean_nds]

#new data set that sums biomass and CP grams for all heights
food_pred <- pred[, .(biomassavail = sum(biomassavail),
                      biomassavail_lower = sum(biomassavail_lower),
                      biomassavail_upper = sum(biomassavail_upper),
                      biomassstart = sum(biomass_mean),
                      NDSavail_grams = sum(NDSavail_grams),
                      NDSavail_grams_lower = sum(NDSavail_grams_lower),
                      NDSavail_grams_upper = sum(NDSavail_grams_upper)),
                 by = Snow]


#calculate the avg CP composition taking into account all heights
food_pred[, NDSavail_comp := NDSavail_grams/biomassavail*100]
food_pred[, NDSavail_comp_lower := NDSavail_grams_lower/biomassavail_lower*100]
food_pred[, NDSavail_comp_upper := NDSavail_grams_upper/biomassavail_upper*100]

food_pred <- food_pred[order(Snow)]

#make linear prediction based on the values at 0 and 75 cm
val0 <- food_pred[Snow == 0, biomassavail]
val75 <- food_pred[Snow == 75, biomassavail]

linearslope <- (val0 - val75)/(0 - 75)



# figures -----------------------------------------------------------------

#set order of heights
pred[, height := factor(height, levels = c("high", "medium", "low"))]
willow[, height := factor(height, levels = c("high", "medium", "low"))]

#plot just the gam prediction and original data for snow 0 - 90 cm
(willow_pred <-
    ggplot()+
    geom_point(aes(x = Snow, y = propavail_willow), alpha = 0.3, color = "grey50", data = willow)+
    geom_ribbon(aes(x = Snow, ymin = prop_lower, ymax = prop_upper), alpha = 0.5, fill = "grey70", data = pred)+
    geom_path(aes(x = Snow, y = prop, color = height), linewidth = .75, data = pred)+
    scale_color_manual(values = heightcols, guide = NULL)+
    labs(x = "Snow depth (cm)", y = "Proportion of twigs available (PTA)")+
    facet_wrap(~height, dir = "v")+
    themepoints)



# make final predictions summing all height classes --------------------------------------------------

#total biomass 
(biomassplot <- 
    ggplot(food_pred)+
    geom_ribbon(aes(x = Snow, ymin = biomassavail_lower, ymax = biomassavail_upper), alpha = 0.3, color = "grey")+
    geom_line(aes(x = Snow, y = biomassavail))+
    geom_abline(aes(intercept = val0, slope = linearslope), linetype = 3)+
    labs(x = " " , y = "Total biomass (g/m2)")+
    themepoints)

#solubility %
(NDSplot<- 
    ggplot(food_pred)+
    geom_ribbon(aes(x = Snow, ymin = NDSavail_comp_lower, ymax = NDSavail_comp_upper), alpha = 0.3, color = "grey")+
    geom_path(aes(x = Snow, y = NDSavail_comp))+
    labs(x = " ", y = "Solubility (NDS; %)")+
    ylim(min(food_pred$NDSavail_comp - 0.5), max(food_pred$NDSavail_comp + 0.5))+
    themepoints)

#soluble biomass
(NDSmassplot <- 
    ggplot(food_pred)+
    geom_ribbon(aes(x = Snow, ymin = NDSavail_grams_lower, ymax = NDSavail_grams_upper), alpha = 0.3, color = "grey")+
    geom_line(aes(x = Snow, y = NDSavail_grams))+
    labs(x = "Snow depth (cm)", y = "Soluble biomass (NDS; g/m2)")+
    themepoints)

fullplot <- ggarrange(biomassplot, NDSplot, NDSmassplot, ncol = 1, nrow = 3)



# figure for conceptual diagram -------------------------------------------

#total biomass 
(biomassplotconceptual <- 
   ggplot(food_pred)+
   geom_ribbon(aes(x = Snow, ymin = biomassavail_lower, ymax = biomassavail_upper), alpha = 0.3, color = "grey")+
   geom_line(aes(x = Snow, y = biomassavail))+
   labs(x = "Snow depth (cm)" , y = "Available willow (g/m2)")+
   theme_pubr(base_size = 16))




# save predictions --------------------------------------------------------

saveRDS(food_pred, "Output/Data/05_willow_prediction.rds")
saveRDS(modout, "Output/Data/05_willow_avail_prediction.rds")

write.csv(summarytable, "Output/Tables/GAM_output_table.rds")

ggsave("Output/Figures/Willow_avail_pred.jpeg", willow_pred, width = 4, height = 10, unit = "in")
ggsave("Output/Figures/Total_food_avail.jpeg", fullplot, width = 4, height = 10, unit = "in")

ggsave("Output/Figures/conceptual_biomass.jpeg", biomassplotconceptual, width = 6, height = 5, unit = "in")
