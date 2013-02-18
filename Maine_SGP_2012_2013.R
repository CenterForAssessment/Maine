###########################################################
###
### Maine SGP Analysis for 2012_2013
###
###########################################################

### Load SGP Package

require(SGP)
options(error=recover)


### Load previous SGP object and 2012_2013 data

load("Data/Maine_SGP.Rdata")
load("Data/Maine_Data_LONG_2012_2013.Rdata")


### Update SGPs

Maine_SGP <- updateSGP(
		Maine_SGP,
		Maine_Data_LONG_2012_2013,
		sgPlot.demo.report=TRUE,
		parallel.config=list(BACKEND="PARALLEL", WORKERS=list(PERCENTILES=4, BASELINE_PERCENTILES=4, PROJECTIONS=4, LAGGED_PROJECTIONS=4, SUMMARY=4, GA_PLOTS=4, SG_PLOTS=1)))


### Save results

save(Maine_SGP, file="Data/Maine_SGP.Rdata")
