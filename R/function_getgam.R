
#function to make gam and build predictions
#But doesn't work right now

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

