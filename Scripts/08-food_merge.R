
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# read in all cleaned and prepped data ------------------------------------

#data
startingbiomass <- readRDS("Output/Data/starting_biomass.rds")
startingnuts <- readRDS("Output/Data/starting_nutrition.rds")
prop <- readRDS("Output/Data/willow_avail_daily.rds")

#change col names in starting nutrients
setnames(startingnuts, "Height", "height")



# merge in biomass and quality --------------------------------------------------------

#merge in biomass means and proportion available
biomass <- merge(prop, startingbiomass[, .(height, grid, biomass_mean)], 
                 by = c("grid", "height"), all.x = TRUE)

#merge in quality to biomass and proportion available
daily <- merge(biomass, startingnuts[, .(height, CP_mean)], 
               by = c("height"), all.x = TRUE)

#reorder daily values
daily <- daily[order(Location, Date)]

#rename
setnames(daily, "propavail_willow", "propavail")

#cases where proportion available was greater than one
daily[propavail > 1, propavail := 1]

#change biomass col name
setnames(daily, "biomass_mean", "biomassstart")

# set your by's, what factors do you want to calculate by
bys <- c("winter", "grid", "loc", "Date") 


# get total available biomass  -------------------------------------

#calculate total starting biomass
daily[, biomassstart_total := sum(biomassstart), by = bys]

#calculate available biomass based on starting biomass and proportion of twigs available
daily[, biomassavail := biomassstart*propavail]

#sum all biomass available across all three heights and both species by day and location
daily[, biomassavail_total := sum(biomassavail), by = bys]

#get total proportion of biomass available by day location and species
daily[, propavail_total := biomassavail_total/biomassstart_total]



# get avg CP composition available  ---------------------------

#calculate the grams of CP in each height class (Biomass x avg CP composition)
daily[, CPavail_grams := biomassavail*CP_mean]

#sum all available CP across all heights and both species
daily[, CPavail_grams_total := sum(CPavail_grams), by = bys]

#calculate the avg CP composition taking into account all heights
daily[, CPavail_comp_total := CPavail_grams_total/biomassavail_total]



# trim down to main information -------------------------------------------

#grab values by same factors as calculation. Repeated values per by, 
#means will supply mode
dt <- daily[, .(temp = mean(temp), 
               snow = mean(Snow),
               proportion = mean(propavail_total),
               biomass = mean(biomassavail_total), 
               CP_grams = mean(CPavail_grams_total), 
               CP_comp = mean(CPavail_comp_total)), 
            by = bys]

#change date name
setnames(dt, "Date", "idate")



# save  -------------------------------------------------------------------

saveRDS(dt, "Output/Data/Total_daily_food_availability.rds")

#some sort of diversity index? At super high snow depth its no more willow
