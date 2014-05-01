##############################################################
###
### Script for creating Maine_Data_LONG_2013_2014
###
##############################################################

### Load SGP and data.table

require(SGP)
require(data.table)


### Load data

tmp.data <- read.csv("Data/Base_Files/NECAP1314FallStateStudentReleasedItem0308.csv")
load("Data/Maine_SGP.Rdata")
load("Data/Base_Files/School_District_Number_Table_2013_2014.Rdata")


##########################################################
### Clean up 2013_2014 data
##########################################################

names(tmp.data) <- toupper(names(tmp.data))
tmp.variables <- c("RPTSTUDID", "LNAME", "FNAME", "GRADE", "DISCODE", "SCHCODE", "SPRDISCODE", "SPRSCHCODE",
                   "GENDER", "ETHNIC", "LEP", "IEP", "SES", "REASCALEDSCORE", "REAAL", "MATSCALEDSCORE", "MATAL", "REATESTSTATUS", "MATTESTSTATUS")
tmp.data <- subset(tmp.data, select=tmp.variables)

attach(tmp.data)

read_1314 <- data.frame(ID=RPTSTUDID,
                        YEAR="2013_2014",
                        CONTENT_AREA="READING",
                        LAST_NAME=LNAME,
                        FIRST_NAME=FNAME,
                        GRADE=GRADE, 
                        DISTRICT_NUMBER=SPRDISCODE,
                        SCHOOL_NUMBER=SPRSCHCODE,
			DISTRICT_NUMBER_CURRENT=DISCODE,
			SCHOOL_NUMBER_CURRENT=SCHCODE,
                        GENDER=GENDER,
                        ETHNICITY=ETHNIC,
                        IEP_STATUS=IEP,
                        LEP_STATUS=LEP,
                        SES_STATUS=SES,
                        SCALE_SCORE=REASCALEDSCORE,
                        ACHIEVEMENT_LEVEL=REAAL,
			TEST_STATUS=REATESTSTATUS)

math_1314 <- data.frame(ID=RPTSTUDID,
                        YEAR="2013_2014",
                        CONTENT_AREA="MATHEMATICS",
                        LAST_NAME=LNAME,
                        FIRST_NAME=FNAME,
                        GRADE=GRADE, 
                        DISTRICT_NUMBER=SPRDISCODE,
                        SCHOOL_NUMBER=SPRSCHCODE,
			DISTRICT_NUMBER_CURRENT=DISCODE,
			SCHOOL_NUMBER_CURRENT=SCHCODE,
                        GENDER=GENDER,
                        ETHNICITY=ETHNIC,
                        IEP_STATUS=IEP,
                        LEP_STATUS=LEP,
                        SES_STATUS=SES,
                        SCALE_SCORE=MATSCALEDSCORE,
                        ACHIEVEMENT_LEVEL=MATAL,
			TEST_STATUS=MATTESTSTATUS)

detach(tmp.data)

Maine_Data_LONG_2013_2014 <- rbind(read_1314, math_1314)


#############################################################################
### Tidy up data
#############################################################################

# ID

Maine_Data_LONG_2013_2014$ID <- as.character(Maine_Data_LONG_2013_2014$ID)

# YEAR

Maine_Data_LONG_2013_2014$YEAR <- as.character(Maine_Data_LONG_2013_2014$YEAR)

# CONTENT_AREA

Maine_Data_LONG_2013_2014$CONTENT_AREA <- as.character(Maine_Data_LONG_2013_2014$CONTENT_AREA)

# GRADE

Maine_Data_LONG_2013_2014$GRADE <- as.character(as.numeric(as.character(Maine_Data_LONG_2013_2014$GRADE)))

# GENDER

levels(Maine_Data_LONG_2013_2014$GENDER) <- c("Female", "Male")

# ETHNICITY

Maine_Data_LONG_2013_2014$ETHNICITY <- factor(Maine_Data_LONG_2013_2014$ETHNICITY, levels=1:7, 
	labels=c("American Indian or Alaskan Native", "Asian", "African American", "Hispanic or Latino", "Native Hawaiian or Pacific Islander", "White", "Multiple Ethnicities Reported"))

# IEP_STATUS

Maine_Data_LONG_2013_2014$IEP_STATUS <- factor(Maine_Data_LONG_2013_2014$IEP_STATUS, levels=0:1, c("Students without Disabilities (Non-IEP)", "Students with Disabilities (IEP)"))

# LEP STATUS

Maine_Data_LONG_2013_2014$LEP_STATUS[Maine_Data_LONG_2013_2014$LEP_STATUS %in% 1:3] <- 1
Maine_Data_LONG_2013_2014$LEP_STATUS <- factor(Maine_Data_LONG_2013_2014$LEP_STATUS, levels=0:1, labels=c("Non-English Language Learners (LEP)", "English Language Learners (LEP)"))

# SES_STATUS

Maine_Data_LONG_2013_2014$SES_STATUS <- factor(Maine_Data_LONG_2013_2014$SES_STATUS, levels=0:1, c("Not Economically Disadvanted", "Economically Disadvantaged"))

# ACHIEVEMENT_LEVEL

Maine_Data_LONG_2013_2014$ACHIEVEMENT_LEVEL[Maine_Data_LONG_2013_2014$ACHIEVEMENT_LEVEL %in%
        c("A", "E", "N", "S", "W", "L")] <- NA
Maine_Data_LONG_2013_2014$ACHIEVEMENT_LEVEL <- droplevels(Maine_Data_LONG_2013_2014$ACHIEVEMENT_LEVEL)
levels(Maine_Data_LONG_2013_2014$ACHIEVEMENT_LEVEL) <-
	        c("Substantially Below Proficient", "Partially Proficient", "Proficient", "Proficient with Distinction")

# VALID_CASE

Maine_Data_LONG_2013_2014$VALID_CASE <- "VALID_CASE"

# ENROLLMENT_STATUS

Maine_Data_LONG_2013_2014$STATE_ENROLLMENT_STATUS <- factor(2, levels=1:2, labels=c("Enrolled State: No", "Enrolled State: Yes"))
Maine_Data_LONG_2013_2014$DISTRICT_ENROLLMENT_STATUS <- factor(2, levels=1:2, labels=c("Enrolled District: No", "Enrolled District: Yes"))
Maine_Data_LONG_2013_2014$SCHOOL_ENROLLMENT_STATUS <- factor(2, levels=1:2, labels=c("Enrolled School: No", "Enrolled School: Yes"))

### VALIDATE cases

Maine_Data_LONG_2013_2014$VALID_CASE[Maine_Data_LONG_2013_2014$TEST_STATUS!="A"] <- "INVALID_CASE"


#############################################################################
###
### Create Long file and save result
###
#############################################################################


### Fix up Maine District and School Names for 2013_2014

Maine_Data_LONG_2013_2014 <- as.data.table(Maine_Data_LONG_2013_2014)
setkeyv(School_District_Number_Table_2013_2014, c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))
setkeyv(Maine_Data_LONG_2013_2014, c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))
Maine_Data_LONG_2013_2014 <- School_District_Number_Table_2013_2014[Maine_Data_LONG_2013_2014, mult="all"]

## Identify duplicates

setkey(Maine_Data_LONG_2013_2014, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, SCALE_SCORE)
setkey(Maine_Data_LONG_2013_2014, VALID_CASE, CONTENT_AREA, YEAR, ID)
Maine_Data_LONG_2013_2014[which(duplicated(Maine_Data_LONG_2013_2014))-1, VALID_CASE := "INVALID_CASE"]
setkey(Maine_Data_LONG_2013_2014, VALID_CASE, CONTENT_AREA, YEAR, ID)
dups <- Maine_Data_LONG_2013_2014[Maine_Data_LONG_2013_2014[J("VALID_CASE")][duplicated(Maine_Data_LONG_2013_2014[J("VALID_CASE")])][,list(VALID_CASE, YEAR, CONTENT_AREA, ID)]]

### Convert to data.frame and save results

Maine_Data_LONG_2013_2014 <- as.data.frame(Maine_Data_LONG_2013_2014)
Maine_Data_LONG_2013_2014 <- Maine_Data_LONG_2013_2014[,names(sort(sapply(names(Maine_Data_LONG_2013_2014), function(x) match(x, names(Maine_SGP@Data)))))]
Maine_Data_LONG_2013_2014 <- as.data.table(Maine_Data_LONG_2013_2014)
save(Maine_Data_LONG_2013_2014, file="Data/Maine_Data_LONG_2013_2014.Rdata")
