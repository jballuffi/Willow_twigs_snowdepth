
#functin that runs a linear model and returns the slope and R2
#returns as a table
#can be run in data table
#use for any basic linear model with one x variable

modout <- function(yvar, xvar1) {
  # Make the model
  model <- lm(yvar ~ xvar1)
  # Transpose the coef of the model and cast as data.table
  coefOut <- data.table(t(coef(model)))
  # extract r-squared from model
  rsqOut <- data.table(rsq(model))
  # Return combined columns
  out <- data.table(coefOut, rsqOut)
  names(out) <- c("Intercept", "Slope", "R2")
  return(out)
}
