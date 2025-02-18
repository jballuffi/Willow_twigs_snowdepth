
# script for exploring nutritional compositions of twigs by species and height 

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#these are the results for plant samples taken before winter 2022-2023
nuts <- fread("Input/Plant_nutrition.csv")



# prep variable names  -------------------------------------------

#pull just willow
willow <- nuts[grep("W", Sample)]

#create height column with sample id
willow[grep("L", Sample), Height := "low"]
willow[grep("M", Sample), Height := "medium"]
willow[grep("H", Sample), Height := "high"]

#subset to main variables
wide <- willow[, .(Height, Grid, Loc, CP_F, NDF_F, ADF_F, ADL_F)]

#calculate Neutral deterfent soluble yield 
wide[, NDS_F := 100 - NDF_F]

#change name
setnames(wide, "Height", "height")

#make height class a leveled factor
wide[, height := factor(height, levels = c("low", "medium", "high"), ordered = TRUE)]



# make data long ----------------------------

#create a melted version of nutrient data, melted by nutrient
long <- melt.data.table(wide, measure.vars = c("CP_F", "NDF_F", "NDS_F"), variable.name = "Nutrient", value.name = "Percent")

#rename nutrients for figures
long[, Nutrient := gsub("_F", "", Nutrient)]

#remove NAs
long <- long[!is.na(Percent)]

#make proportion
long[, Composition := Percent/100]



# Get final summary by height class ---------------------------------------

#create data table of means, medians, and standard deviations for %CP by species and height
meanslong <- long[, .(mean = round(mean(Composition), 3),
                      median = round(median(Composition), 3),
                      sd = round(sd(Composition), 3),
                      upper_quant = round(quantile(Composition, prob = .75), 3),
                      lower_quant = round(quantile(Composition, prob = 0.25), 3)), 
                  by = .(height, Nutrient)]


meanswide <- wide[, .(mean_cp = round(mean(CP_F/100, na.rm = TRUE), 2),
                      mean_nds = round(mean(NDS_F/100, na.rm = TRUE), 2),
                      mean_ndf = round(mean(NDF_F/100, na.rm = TRUE), 2)),
                  height]



# Look at nutrient trends by height ------------------------------

#figure to look at difference between height classes
(allnuts <- 
    ggplot(long)+
    geom_boxplot(aes(x = height, y = Percent, fill = height), alpha = 0.4, width = .5)+
    labs(y = "Composition (%)", x = "Browse height")+
    scale_fill_manual(values = heightcols, guide = NULL)+
    themepoints+
    facet_wrap(~ Nutrient, scales = "free", dir = "h"))

#min and max of plant compositions
long[, .(min = min(Percent), max = max(Percent)), by = Nutrient]

#density for NDS
ggplot()+
  geom_density(aes(x = Composition, fill = height, color = height), alpha = .3, data = long[Nutrient == "NDS"])+
  scale_fill_manual(values = heightcols)+
  scale_color_manual(values = heightcols)+
  labs(x = "NDS composition (%)")+
  themepoints





# save outputs ------------------------------------------------------------

saveRDS(meanslong, "Output/Data/01_starting_nutrition_long.rds")
saveRDS(meanswide, "Output/Data/01_starting_nutrition_wide.rds")
saveRDS(long, "Output/Data/01_cleaned_compositions.rds")
ggsave("Output/Figures/composition_by_height.jpeg", allnuts, width = 8, height = 5, unit = "in")

