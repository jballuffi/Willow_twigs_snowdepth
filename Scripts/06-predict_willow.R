

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in data ------------------------------------------------------------

willow <- readRDS("Output/Data/willow_avail_noduplicates.rds")



# model willow availability for each height -------------------------------

lowgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "low"])
summary(lowgam)

medgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "medium"])
summary(medgam)

highgam <- gam(propavail_willow ~ s(Snow), data = willow[height == "high"])
summary(highgam)

mods <- list(lowgam, medgam, highgam)
modnames <- c("low", "medium", "high")

modoutlist <- lapply(mods, getgam)

modout <- rbindlist(modoutlist, idcol = "height")
modout[, height := factor(height, labels = modnames)]



# figures -----------------------------------------------------------------

(willow_pred <-
  ggplot(modout)+
  geom_ribbon(aes(x = Snow, ymin = lower, ymax = upper), alpha = 0.5, fill = "grey70")+
  geom_path(aes(x = Snow, y = pred, color = height), linewidth = .75)+
  scale_color_manual(values = heightcols, guide = NULL)+
  labs(x = "Snow depth (cm)", y = "Predicted twig availablity")+
  facet_wrap(~height)+
  theme_minimal(base_size = 16))


#plot using geom smooth GAM
(willow_gam <- 
  ggplot(willow)+
  geom_point(aes(x = Snow, y = propavail_willow), alpha = 0.5, color = "grey50")+
  geom_smooth(aes(x = Snow, y = propavail_willow, color = height, fill = height), method = "gam")+
  scale_color_manual(values = heightcols, guide = NULL)+
  scale_fill_manual(values = heightcols, guide = NULL)+
  labs(y = "Proportion of twigs available", x = "Snow depth (cm)")+
  facet_wrap(~ height)+
  theme_minimal())



# save predictions --------------------------------------------------------

saveRDS(modout, "Output/Data/willow_avail_prediction.rds")
ggsave("Output/Figures/Willow_avail_gam.jpeg", willow_gam, width = 9, height = 5, unit = "in")
ggsave("Output/Figures/Willow_avail_pred.jpeg", willow_pred, width = 9, height = 4, unit = "in")
