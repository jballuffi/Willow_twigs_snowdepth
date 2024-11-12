
# script for exploring nutritional compositions of twigs by species and height 

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#these are the results for plant samples taken before winter 2022-2023
nuts <- fread("Input/Plant_nutrition.csv")



# prep variable names  -------------------------------------------

#create species column with sample id
nuts[grep("S", Sample), Species := "spruce"]
nuts[grep("W", Sample), Species := "willow"]

#create height column with sample id
nuts[grep("L", Sample), Height := "low"]
nuts[grep("M", Sample), Height := "medium"]
nuts[grep("H", Sample), Height := "high"]

#take only willow
willow <- nuts[Species == "willow"]

#subset to main variables
wide <- willow[, .(Height, Grid, Loc, CP_F, NDF_F, ADF_F, ADL_F)]

#What is not protein or fibre should just be carb
wide[, Carb_F := 100 - (NDF_F + CP_F)]

#change name
setnames(wide, "Height", "height")

#make height class a leveled factor
wide[, Height := factor(height, levels = c("low", "medium", "high"), ordered = TRUE)]



# make data long ----------------------------

#create a melted version of nutrient data, melted by nutrient
long <- melt.data.table(wide, measure.vars = c("NDF_F", "ADF_F", "ADL_F", "CP_F", "Carb_F"), variable.name = "Nutrient", value.name = "Percent")

#rename nutrients for figures
long[, Nutrient := gsub("_F", "", Nutrient)]

#remove NAs
long <- long[!is.na(Percent)]

#make proportion
long[, Composition := Percent/100]



# Summarize data  ------------------------------

#figure to look at difference between height classes
(allnuts <- 
    ggplot(long)+
    geom_boxplot(aes(x = height, y = Percent, fill = height), alpha = 0.4)+
    labs(y = "Composition (%)", x = "Browse height")+
    scale_fill_manual(values = heightcols, guide = NULL)+
    themepoints+
    facet_wrap(~ Nutrient, scales = "free", dir = "v"))

#min and max of plant compositions
long[, .(min = min(Percent), max = max(Percent)), by = Nutrient]



# Get final summary by height class ---------------------------------------

#create data table of means, medians, and standard deviations for %CP by species and height
meanslong <- long[, .(mean = mean(Composition),
                      median = median(Composition),
                      sd = sd(Composition)), 
                 by = .(height, Nutrient)]


meanswide <- wide[, .(mean_cp = mean(CP_F/100, na.rm = TRUE),
                      mean_carb = mean(Carb_F/100, na.rm = TRUE),
                      mean_ndf = mean(NDF_F/100, na.rm = TRUE)),
                  height]

# save outputs ------------------------------------------------------------

saveRDS(meanslong, "Output/Data/starting_nutrition_long.rds")
saveRDS(meanswide, "Output/Data/starting_nutrition_wide.rds")
saveRDS(long, "Output/Data/cleaned_compositions.rds")
ggsave("Output/Figures/composition_by_height.jpeg", allnuts, width = 6, height = 10, unit = "in")

