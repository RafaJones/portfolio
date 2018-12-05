#
set.seed(4)

year <- 1:20
netbenefit <- rep(NA,20)
MC.simulation <- data.frame(year,netbenefit)
MC.superset <- rep(list(MC.simulation),100)

# go to each dataframe in the superset 
# create a temporary dataframe 
# simulate net benefits for each year in that dataframe
# return it to the superset 
for(i in 1:length(MC.superset)){ 
  temp. <- MC.superset[[i]]
  for(j in temp.$year){ 
    temp.$netbenefit[j] <- simulation(year = j)/1000000 # put it into millions scale 
  }
  MC.superset[[i]] <- temp.
}

#combine it into a single dataframe for plotting 
MC.dataset <- do.call(rbind,MC.superset)
g <- ggplot(data = MC.dataset, aes(x = year, y = netbenefit)) + geom_point(alpha = 0.25) + geom_smooth(method = "loess")
g + labs(title = "Monte Carlo Simulations of Decriminalization", x = "Year", y = "Net Benefit ($ Millions)") + coord_cartesian(ylim = c(15000,30000))


################### Function below 



# wrap entire thing in a function 
simulation <- function(year, parole.cost = 4512, 
                       crim.record.penalty.percent = .12,
                       expungement.fee = 60, 
                       first.year.arrests = 600000,
                       arrest.decay = 0.02,
                       arrest.cost = 1570,
                       adjudication.cost = 801,
                       officeryear.cost = 18500,
                       murders.saved.per.officeryear = .11, 
                       eligible.not.in.prison = 1407718,
                       median.years.incarcerated = 1.3, 
                       VSL = 2000000){

#### STOCHASTIC

currently.in.prison <- runif(n = 1, min = 400, max = 2000)
incarceration.cost <- runif(n = 1, min = 32136, max = 57522)
crim.record.penalty.annual <- runif(n = 1, min = 5437.5, max = 5571) * crim.record.penalty.percent/.12
relevant.arrests.ratio <- runif(n = 1, min = .8, max = .9)


#### Net Benefits Analysis

  ## Currently Incarcerated

#benefits
incarc.costs.saved.CI <- incarceration.cost*median.years.incarcerated*currently.in.prison
removal.of.crim.penalty.CI <- crim.record.penalty.annual*currently.in.prison

#costs
total.parole.CI <- parole.cost*currently.in.prison
total.expunge.CI <- expungement.fee*currently.in.prison  

  ## Eligible, not currently in prison 

#benefits
removal.of.crim.penalty.ENCI <- crim.record.penalty.annual*eligible.not.in.prison

#costs
total.expunge.ENCI <- expungement.fee*eligible.not.in.prison


  ## Eligible, Arrested in Status Quo:: 

# relevant arrests decays over time, to follow the current pattern of declining arrests 
relevant.arrests.count <- relevant.arrests.ratio*first.year.arrests*(1-arrest.decay)^ifelse(year == 1, 0, year)

#benefits
arrest.adjuct.savings <- relevant.arrests.count*(arrest.cost + adjudication.cost)
removal.of.crim.penalty.EASQ <- crim.record.penalty.annual*relevant.arrests.count
rare.incarceration.avoided <- .0046*relevant.arrests.count*median.years.incarcerated*incarceration.cost

      #not in dollars
officeryears.budgeted <- (arrest.adjuct.savings + rare.incarceration.avoided)/officeryear.cost

officeryears.benefit <- officeryears.budgeted*murders.saved.per.officeryear*VSL

#costs
officeryears.totalcost <- officeryears.budgeted*officeryear.cost

### Year 1 is differentiated 
allbenefits <- ifelse(year == 1, incarc.costs.saved.CI, 0) + removal.of.crim.penalty.CI + 
  removal.of.crim.penalty.ENCI + arrest.adjuct.savings + removal.of.crim.penalty.EASQ + 
  rare.incarceration.avoided + officeryears.benefit

allcosts <- ifelse(year == 1, total.parole.CI +total.expunge.CI + total.expunge.ENCI , 0) + officeryears.totalcost
  
return(allbenefits - allcosts)
} 
