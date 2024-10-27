
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
justnuts <- willow[, .(Height, Grid, Loc, CP_F, NDF_F, ADF_F, ADL_F)]

#What is not protein or fibre should just be carb
justnuts[, Carb_F := 100 - (NDF_F + CP_F)]



# make data long ----------------------------

#create a melted version of nutrient data, melted by nutrient
justnuts <- melt.data.table(justnuts, measure.vars = c("NDF_F", "ADF_F", "ADL_F", "CP_F", "Carb_F"), variable.name = "Nutrient", value.name = "Percent")

#make height class a leveled factor
justnuts[, Height := factor(Height, levels = c("low", "medium", "high"), ordered = TRUE)]

#rename nutrients for figures
justnuts[, Nutrient := gsub("_F", "", Nutrient)]

#remove NAs
justnuts <- justnuts[!is.na(Percent)]

#make proportion
justnuts[, Composition := Percent/100]

#change name
setnames(justnuts, "Height", "height")


# Summarize data  ------------------------------

#figure to look at difference between height classes
(allnuts <- 
    ggplot(justnuts)+
    geom_boxplot(aes(x = height, y = Percent, fill = height), alpha = 0.4)+
    labs(y = "Composition (%)", x = "Browse height")+
    scale_fill_manual(values = heightcols, guide = NULL)+
    theme_minimal()+
    facet_wrap(~ Nutrient, scales = "free"))


#look at significant differences between nutritional compositions of different heights
carbmod <- lm(Percent ~ height, data = justnuts[Nutrient == "Carb"])
cpmod <- lm(Percent ~ height, data = justnuts[Nutrient == "CP"])

#min and max of plant compositions
justnuts[, .(min = min(Percent), max = max(Percent)), by = Nutrient]



# Get final summary by height class ---------------------------------------

#create data table of means, medians, and standard deviations for %CP by species and height
means <- justnuts[, .(mean = mean(Composition),
                      median = median(Composition),
                      sd = sd(Composition)), 
                 by = .(height, Nutrient)]


# save outputs ------------------------------------------------------------

saveRDS(means, "Output/Data/starting_nutrition.rds")
saveRDS(justnuts, "Output/Data/cleaned_compositions.rds")
ggsave("Output/Figures/composition_by_height.jpeg", allnuts, width = 10, height = 6, unit = "in")

