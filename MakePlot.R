slopes = as.array(1:1000) 
for(z in 1:1000) {
  xrands = as.array(runif(16,0,1))
  yrands = as.array(runif(16,0,1))
  xnums = as.array(1:16)
  ynums = as.array(1:16)
  for(x in 1:16) {
     xnums[x] <- round(qnorm(xrands[x],308,128,TRUE,FALSE))
     ynums[x] <- round(qnorm(yrands[x],2.588,1.278,TRUE,FALSE))
  }
  plot(xnums,ynums)
  abline(lm(ynums~xnums))
  slopes[z] <- summary(lm(ynums~xnums))$coefficients["xnums", "Estimate"]
}
counter = 0
for(number in slopes) {
  if(number > 0.00224)  {
    counter = counter+1
  }
}
print(counter)