
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in willow data
allo <- fread("Input/Allometric_willow_twig.csv")

#calc water content (proportion)
allo[, water := (Wet_sample - Dry_sample)/Wet_sample]

#remove one case where water content is neg
allo[water < 0, water := NA]

#fill in NA water content
allo[, watermean := mean(water, na.rm = TRUE)]
allo[is.na(water), water := watermean]

#calculate total dry
allo[, Dry_total := (1- water)*Wet_total]

#run makemod function by species and rename table output
eqn <- allo[, modout_zero(yvar = Dry_total, xvar1 = BD)]


#test plot for relationship between BD and Dry biomass
alloplot <- ggplot(allo)+
  geom_point(aes(x = BD, y = Dry_total), shape= 1)+
  xlim(0, 56)+
  geom_abline(aes(intercept = 0, slope = eqn[,return(Slope)]), alpha = 0.8)+
  labs(y = "Dry biomass (g)", x = "Basal diameter (mm)")+
  theme_minimal()


ggsave("Output/Figures/allometric_equations.jpeg", alloplot, width = 5, height = 4, unit = "in")
