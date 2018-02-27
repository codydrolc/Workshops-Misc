#-------------------------------------------------------------------------------
#---- Optimal scaling using the ALSOS algorithm

# This example draws on data from the 2016 American National Election 
# Study (ANES). The data are clean, missing values are eliminated, and 
# the data are hosted in a tab-delimited text file on my github. Keep in 
# mind that the goal of this example is to demonstrate the utility of 
# relaxing measurment assumptions. As such, the estimated model is 
# missing obvious covariates. 

#---- Data description
# DV: relative - The difference between feeling thermometer ratings of 
#       Hillary Clinton and Donald Trump. Higher values indicate a more
#       positive rating for Clinton. (-100, +100)
# IV: partyid -- Standard 7-point party identification scale. 
#       1 for "strong Democrat", 7 for "strong Republican"
#     ideo -- 7-point self ideological placement. 
#       1 for "extremely liberal", 7 for "extremely conservative"
#     econ -- Judgement of the economy in the past year. 
#       1 for "much better", 5 for "much better"
#     bill -- Feeling thermometer rating of Bill Clinton. (0, 100)

# Install packages
install.packages(c("optiscale", "ggplot2"), dependencies = T)

# Load packages
library(optiscale); library(ggplot2)

# Read data
url <- "https://raw.githubusercontent.com/codydrolc/Workshops-Misc/master/Measurement_Theory/2016anes.txt"
anes <- read.table(url, header = TRUE, sep = "\t", check.names = FALSE)

# Basic regression estimates
basic <- lm(relative ~ partyid + ideo + econ + bill, data = anes)
summary(basic) # Pretty much what you would expect

#---- Set up alternating least squares optimal scaling (ALSOS) procedure
# Set starting values equal to the vectors in original data
# Under the ALSOS procedure, all variables are assumed to be ordinal
relative.os <- anes$relative
partyid.os <- anes$partyid
ideo.os <- anes$ideo
econ.os <- anes$econ
bill.os <- anes$bill

# Starting values for R-squared, loop number, and difference in R-squared values
pr.r2 <- 0
iter <- 0
r2.diff <- 1
record <- c() # matrix to record iterations

#---- Run ALSOS procedure
# Run loop as long as the difference in R-squared values is greater than 0.00001
# and the number of iterations is less than 50. This level of precision is 
# probably not necessary but will yield more iterations to examine. 
while (r2.diff > 0.00001 && iter <= 50) {
  # Housekeeping
  iter <- iter + 1 # Add 1 to iteration
  # Regression with optimally-scaled variables
  reg.os <- lm(relative.os ~ partyid.os + ideo.os + econ.os + bill.os)
  
  # Difference in R-squared values
  r2.diff <- summary(reg.os)$r.squared - pr.r2
  pr.r2 <- summary(reg.os)$r.squared # Save R-squared for next iteration
  record <- c(record, iter, summary(reg.os)$r.squared, r2.diff) # Record results
  
  # If the R-squared values improved, move to the next iteration. If not, stop.
  if (r2.diff > 0.00001) {
    # Create predicted values of the dependent variable ("relative") and 
    # perform the optimal scaling routine on the predicted values.
    dvar.pred <- predict(reg.os) # Predicted values
    opscaled.dvar <- opscale(anes$relative, dvar.pred, level = 2, process = 1)
    relative.os <- opscaled.dvar$os # Extract and save optimally-scaled values
    
    # Create predicted values of the independent variables based on the 
    # previously estimated regression and perform the optimal scaling routine
    # on the predicted values. OS objects are updated at each iteration.
    
    # Party identification
    partyid.pred <- (relative.os - (reg.os$coefficients[1] + (reg.os$coefficients[3]*ideo.os) +
      (reg.os$coefficients[4]*econ.os) + (reg.os$coefficients[5]*bill.os))) * (1/reg.os$coefficients[2])
    opscaled.partyid <- opscale(anes$partyid, partyid.pred, level = 2, process = 1)
    partyid.os <- opscaled.partyid$os
    
    # Ideology
    ideo.pred <- (relative.os - (reg.os$coefficients[1] + (reg.os$coefficients[2]*partyid.os) +
      (reg.os$coefficients[4]*econ.os) + (reg.os$coefficients[5]*bill.os))) * (1/reg.os$coefficients[3])
    opscaled.ideo <- opscale(anes$ideo, ideo.pred, level = 2, process = 1)
    ideo.os <- opscaled.ideo$os
    
    # Economic perceptions
    econ.pred <- (relative.os - (reg.os$coefficients[1] + (reg.os$coefficients[2]*partyid.os) +
      (reg.os$coefficients[3]*ideo.os) + (reg.os$coefficients[5]*bill.os))) * (1/reg.os$coefficients[4])
    opscaled.econ <- opscale(anes$econ, econ.pred, level = 2, process = 1)
    econ.os <- opscaled.econ$os
    
    # Bill Clinton feeling termometer
    bill.pred <- (relative.os - (reg.os$coefficients[1] + (reg.os$coefficients[2]*partyid.os) +
      (reg.os$coefficients[3]*ideo.os) + (reg.os$coefficients[4]*econ.os))) * (1/reg.os$coefficients[5])
    opscaled.bill <- opscale(anes$bill, bill.pred, level = 2, process = 1)
    bill.os <- opscaled.bill$os
  }
}

#----- Finishing up
# Examine iterations
record <- matrix(data = record, ncol = 3, byrow = T)
colnames(record) <- c("Iteration", "R-squared", "Improvement")
record # Print object

# View summary of final regression. What changed?
summary(reg.os)

# Difference in coefficients
basic$coefficients - reg.os$coefficients

#----- Graphs
# Plot the optimally-scaled values versus the original coding using the built-in
# plotting function for optiscale. The optiscale object forces R to invoke
# lattice graphics and extract the orginal and OS values. 
plot(opscaled.dvar)
plot(opscaled.partyid)
plot(opscaled.ideo)
plot(opscaled.econ)
plot(opscaled.bill)

# The built-in graphing function is okay, but we can do better!
# Combine optimally scaled values with original values
os.orig <- as.data.frame(cbind(relative.os, anes$relative, partyid.os, 
  anes$partyid, ideo.os, anes$ideo, econ.os, anes$econ, bill.os, anes$bill))
colnames(os.orig) <- c("relative_os", "relative", "partyid_os", "partyid", 
  "ideo_os", "ideo", "econ_os", "econ", "bill_os", "bill") # Name columns

# Create plotting function so we do not have to repeat chunks of code
plot.os <- function(obj) {
  df <- as.data.frame(cbind(obj$qual, obj$os)) # extract elements to data frame
  ggplot(df, aes(x = V1, y = V2)) + geom_point() + geom_line() + 
  coord_fixed() + theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(sec.axis = dup_axis(name = element_blank())) +
  scale_x_continuous(sec.axis = dup_axis(name = element_blank())) +
  ylab("Optimally Scaled Values") + xlab("Original Values")
}

# Pass optiscale object to function  
plot.os(opscaled.dvar)
plot.os(opscaled.partyid)
plot.os(opscaled.ideo)
plot.os(opscaled.econ)
plot.os(opscaled.bill)
