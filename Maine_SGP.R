#########################################################
###
### Code to produce 2011_2012 SGPs for Maine
###
#########################################################

### Load SGP package

require(SGP)
options(error=recover)

### Load data

load("Data/Maine_SGP.Rdata")
load("Data/Maine_Data_LONG_2011_2012.Rdata")


### Clean up duplicated cases in previous Maine_SGP

setkeyv(Maine_SGP@Data, c("YEAR", "CONTENT_AREA", "ID", "GRADE", "SCALE_SCORE"))
setkeyv(Maine_SGP@Data, c("YEAR", "CONTENT_AREA", "ID"))
Maine_SGP@Data[['VALID_CASE']][which(duplicated(Maine_SGP@Data) & Maine_SGP@Data[['VALID_CASE']]=="VALID_CASE")-1] <- "INVALID_CASE"


### Merge files and add in STATE, DISTRICT, and SCHOOL ENROLLMENT STATUS variables

Maine_SGP@Data <- as.data.table(rbind.fill(as.data.frame(Maine_SGP@Data), as.data.frame(Maine_Data_LONG_2011_2012)))
Maine_SGP@Data$STATE_ENROLLMENT_STATUS <- factor(1, levels=1:2, labels=c("Enrolled State: Yes", "Enrolled State: No"))
Maine_SGP@Data$DISTRICT_ENROLLMENT_STATUS <- factor(1, levels=1:2, labels=c("Enrolled District: Yes", "Enrolled District: No"))
Maine_SGP@Data$SCHOOL_ENROLLMENT_STATUS <- factor(1, levels=1:2, labels=c("Enrolled School: Yes", "Enrolled School: No"))


### abcSGP

Maine_SGP <- abcSGP(
		Maine_SGP,
		steps=c("prepareSGP", "analyzeSGP", "combineSGP", "visualizeSGP"),
		years="2011_2012",
		sgp.percentiles=TRUE,
		sgp.projections=TRUE,
		sgp.projections.lagged=FALSE,
		sgp.percentiles.baseline=FALSE,
		sgp.projections.baseline=FALSE,
		sgp.projections.lagged.baseline=FALSE,
		save.intermediate.results=TRUE,
		plot.types=c("growthAchievementPlot", "studentGrowthPlot"),
		parallel.config=list(BACKEND="MULTICORE", WORKERS=list(PERCENTILES=30, PROJECTIONS=24, GA_PLOTS=8, SG_PLOTS=30)))


### Save results

#save(Maine_SGP, file="Data/Maine_SGP.Rdata")
