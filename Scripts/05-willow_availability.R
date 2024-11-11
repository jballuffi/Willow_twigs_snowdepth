
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#read in initial flag counts for cameras
#this data is from the first count of all flags in the camera trap image
#it is not from my field book where I logged how many twigs we flagged
flags <- fread("Input/starting_flag_count.csv")
cams <- readRDS("Output/Data/camtraps.rds")



# get twig availability by height ---------------------------------------------------

#remove extra underscore from loc
flags[, loc := gsub("_", "", loc)]

#make new full loc column
flags[, Location := paste0(grid, "_", loc)]

#merge cam data with starting flag counts
twigs <- merge(cams, flags, by = c("Location", "winter"), all.x = TRUE)

twigs[, orangeC := as.integer(`1_orange`)][, yellowC := as.integer(`2_yellow`)][, pinkC := as.integer(`3_pink`)]

#calculate the proportion of twig colors available according to photos
#height according to color
twigs[, low := orangeC/orange]
twigs[, medium := yellowC/yellow]
twigs[, high := pinkC/pink]

#convert farenheit to celcius
twigs[, Temp := as.integer(Temp)]
twigs[, temp := (Temp-32)/1.8]

#subset camera trap data to just proportions and info 
twigs2 <- twigs[, .(Location, winter, Date, Snow, temp, Moon, grid, loc, low, medium, high)]

#get rid of duplicated data for later GAMs
twigs2 <- twigs2[order(winter, Location, Date)]



# prep data for time figures ----------------------------------------------

#this data includes all days

#melt twig availability by height class
willow <- data.table::melt(twigs2, measure.vars = c("low", "medium", "high"), variable.name = "height", value.name = "propavail_willow")

#any proportion greater than 1 gets a 1.
willow[propavail_willow > 1, propavail_willow := 1]



# prep data for models ----------------------------------------------------

#this data cuts out replicated sequential days
#i.e., if the same snow height and twig availability occurs across multiple days
#we only take the first day

#round proportion data
twigs2[, c("low", "medium", "high") := .(round(low, 3), round(medium, 3), round(high, 3))]

#past snow and proportions into one data column
twigs2[, testcol := paste0(Snow, "_", low, "_", medium, "_", high)]

#use seqduplicate fundtion to determine which days are just duplicates of each other
twigs2[, rep := divDyn::seqduplicated(testcol), .(Location, winter)]

#take only non-replicated data
formod <- twigs2[rep == FALSE]

#remove rep and test col for final dataset
formod[, rep := NULL][, testcol := NULL]

#melt twig availability by height class
formod <- data.table::melt(formod, measure.vars = c("low", "medium", "high"), variable.name = "height", value.name = "propavail_willow")

#any proportion greater than 1 gets a 1.
formod[propavail_willow > 1, propavail_willow := 1]



# Show availability over time -----------------------------------------------------------------

#get mean proportion available and snow depth by date and height class
means <- willow[, .(propmean = mean(propavail_willow, na.rm = TRUE),
                    propsd = sd(propavail_willow, na.rm = TRUE),
                    snowmean = mean(Snow, na.rm = TRUE),
                    snowsd = sd(Snow, na.rm = TRUE)),
                by = .(winter, Date, height)]

#order by date for path plots
means <- means[order(Date)]

#proportion available for each height over time
(proptrend <- 
  ggplot(means)+
    geom_ribbon(aes(x = Date, ymax = propmean + propsd, ymin = propmean - propsd, fill = height), alpha = 0.2)+
    geom_path(aes(x = Date, y = propmean, group = height, color = height), linewidth = 1)+
    labs(y = "Proportion of twigs available", x = "Date")+
    scale_color_manual(values = heightcols)+
    scale_fill_manual(values = heightcols)+
    facet_wrap(~winter, scales = "free")+
    theme_minimal())

#snow depth over time
(snowtrend <- 
  ggplot(means)+
  geom_ribbon(aes(x = Date, ymin = snowmean - snowsd, ymax = snowmean + snowsd), alpha = 0.2)+
  geom_path(aes(x = Date, y = snowmean), linewidth = 1)+
  labs(y = "Snow depth (cm)", x = "Date")+
  facet_wrap(~winter, scales = "free")+
  theme_minimal())

#Combine proportion available and snow depth into one figure
timetrend <- ggarrange(snowtrend, proptrend, ncol = 1, nrow = 2)



# Show availability with snow depth ---------------------------------------

#that figure is tough to follow, try using a boxplot
#bin snow data
willow[, snow_cat := cut(Snow, breaks = 6, include.lowest = TRUE)]
willow[is.na(snow_cat)]

#boxplot of proportion available in response to snow
(propandsnowbox <- 
  ggplot(willow)+
  geom_boxplot(aes(x = snow_cat, y = propavail_willow, fill = height), color = "grey20", alpha = 0.5, outlier.shape = NA)+
  scale_fill_manual(values = heightcols)+
  labs(y = "Proportion of twigs available", x = "Snow depth bins (cm)")+
  theme_minimal())



# save output data --------------------------------------------------------

#save the daily measures, with replicates removed of availability by camera trap site
saveRDS(formod, "Output/Data/willow_avail_noduplicates.rds")

#save daily means for twig and snow data that was used for time figure
saveRDS(means, "Output/Data/daily_meanavail_allgrids.rds")

ggsave("Output/Figures/Willowavail_snow_time.jpeg", timetrend, width = 8, height = 8, unit = "in" )
ggsave("Output/Figures/Willowavail_over_snow_boxplot.jpeg", propandsnowbox, width = 8, height = 6, unit = "in" )
