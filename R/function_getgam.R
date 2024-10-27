
#function to build predictions from GAMs 
#feed it one GAM object, create a predction for snow depth from 0 - 100 cm
#uses predict() fnction, output is predicted value and lower and upper limits

getgam <- function(mod){
  
  #create new dataframe for prediction
  newdat <- data.table(Snow = seq(0, 100))
  
  #use predict function to create prediction for data based on gam
  pred <- predict(mod, newdat, type = 'response', se.fit = TRUE) 
  
  #add column in new data for predicted value
  newdat$pred <- pred$fit

  #add column in new data for lower and upper confidence band
  newdat$lower <- pred$fit - 1.96 * pred$se.fit
  newdat$upper <- pred$fit + 1.96 * pred$se.fit
  
  #newdat$height <- print(substitute(mod))
  
  #return data
  return(newdat)
}

