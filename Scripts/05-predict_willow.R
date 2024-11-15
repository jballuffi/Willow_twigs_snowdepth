

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

willow <- readRDS("Output/Data/04_willow_avail_noduplicates.rds")
startingbiomass <- readRDS("Output/Data/02_starting_biomass_means.rds")



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

#merge model prediction with biomass summary by height
pred <- merge(modout, startingbiomass, by = "height", all.x = TRUE)

#change col names
setnames(pred, c("pred", "lower", "upper"), c("prop", "prop_lower", "prop_upper"))



# calculate available biomass and nutrients -------------------------------

#get total available biomass for each height and snow depth
pred[, biomassavail := biomass_mean*prop]
pred[, biomassavail_lower := biomass_mean*prop_lower]
pred[, biomassavail_upper := biomass_mean*prop_upper]

#new data set that sums biomass and CP grams for all heights
food_pred <- pred[, .(biomassavail = sum(biomassavail),
                      biomassavail_lower = sum(biomassavail_lower),
                      biomassavail_upper = sum(biomassavail_upper),
                      biomassstart = sum(biomass_mean)),
                 by = Snow]

food_pred <- food_pred[order(Snow)]



# figures -----------------------------------------------------------------

#set order of heights
pred[, height := factor(height, levels = c("high", "medium", "low"))]
willow[, height := factor(height, levels = c("high", "medium", "low"))]


#plot just the gam prediction and original data for snow 0 - 90 cm
(willow_pred <-
  ggplot()+
  geom_point(aes(x = Snow, y = propavail_willow), alpha = 0.5, color = "grey50", data = willow)+
  geom_ribbon(aes(x = Snow, ymin = prop_lower, ymax = prop_upper), alpha = 0.5, fill = "grey70", data = pred)+
  geom_path(aes(x = Snow, y = prop, color = height), linewidth = .75, data = pred)+
  scale_color_manual(values = heightcols, guide = NULL)+
  labs(x = "Snow depth (cm)", y = "Proportion of twigs available (PTA)")+
  facet_wrap(~height, dir = "v")+
  themepoints)

(biomassplot <- 
    ggplot(food_pred)+
    geom_ribbon(aes(x = Snow, ymin = biomassavail_lower, ymax = biomassavail_upper), alpha = 0.3, color = "grey")+
    geom_line(aes(x = Snow, y = biomassavail))+
    labs(x = "Snow depth (cm)", y = "Available biomass (g/m2)")+
    themepoints)



# save predictions --------------------------------------------------------

saveRDS(modout, "Output/Data/05_willow_avail_prediction.rds")
write.csv(summarytable, "Output/Tables/GAM_output_table.rds")
ggsave("Output/Figures/Willow_avail_pred.jpeg", willow_pred, width = 5, height = 10, unit = "in")
ggsave("Output/Figures/Total_biomass.jpeg", biomassplot, width = 5, height = 5, unit = "in")
