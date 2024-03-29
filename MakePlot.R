# run lines 4-13 to plot a single simulated set of points
slopes = as.array(1:trials)
for(z in 1:trials) {
  xrands = as.array(runif(points,0,1))
  yrands = as.array(runif(points,0,1))
  xnums = as.array(1:points)
  ynums = as.array(1:points)
  for(x in 1:points) {
     xnums[x] <- round(qnorm(xrands[x],meanx,stdevx,TRUE,FALSE))
     ynums[x] <- round(qnorm(yrands[x],meany,stdevy,TRUE,FALSE))
  }
  #plot(xnums,ynums) # draws the graph for this current simulation
  #abline(lm(ynums~xnums)) # draws the regression line
  slopes[z] <- summary(lm(ynums~xnums))$coefficients["xnums", "Estimate"]
}
counter = 0
for(number in slopes) {
  if(number > observedslope)  { 
    counter = counter+1
  }
}
print(counter)
#plot(1:trials, slopes) # makes a plot of all generated slopes
#abline(h=observedslope, col = "red", lty = "dashed") # draws your observed slope as a dotted line through the generated slopes
