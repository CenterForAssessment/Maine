##############################################################
###
### Script for creating Maine_Data_LONG_2012_2013
###
##############################################################

### Load SGP and data.table

require(SGP)
require(data.table)


### Load data

tmp.data <- read.csv("Data/Base_Files/NECAP2012-13_Final.csv")
load("Data/Maine_SGP.Rdata")
load("Data/Base_Files/Maine_District_and_School_Names.Rdata")


##########################################################
### Clean up 2012 2013 data
##########################################################

names(tmp.data) <- toupper(names(tmp.data))
tmp.variables <- c("RPTSTUDID", "LNAME", "FNAME", "GRADE", "DISCODE", "SCHCODE", "SPRDISCODE", "SPRSCHCODE",
                   "GENDER", "ETHNIC", "LEP", "IEP", "SES", "REASCALEDSCORE", "REAAL", "MATSCALEDSCORE", "MATAL", "REATESTSTATUS", "MATTESTSTATUS")
tmp.data <- subset(tmp.data, select=tmp.variables)

attach(tmp.data)

read_1213 <- data.frame(ID=RPTSTUDID,
                        YEAR="2012_2013",
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

math_1213 <- data.frame(ID=RPTSTUDID,
                        YEAR="2012_2013",
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

Maine_Data_LONG_2012_2013 <- rbind(read_1213, math_1213)


#############################################################################
### Tidy up data
#############################################################################

# ID

Maine_Data_LONG_2012_2013$ID <- as.character(Maine_Data_LONG_2012_2013$ID)

# YEAR

Maine_Data_LONG_2012_2013$YEAR <- as.character(Maine_Data_LONG_2012_2013$YEAR)

# CONTENT_AREA

Maine_Data_LONG_2012_2013$CONTENT_AREA <- as.character(Maine_Data_LONG_2012_2013$CONTENT_AREA)

# GRADE

Maine_Data_LONG_2012_2013$GRADE <- as.character(as.numeric(as.character(Maine_Data_LONG_2012_2013$GRADE)))

# GENDER

levels(Maine_Data_LONG_2012_2013$GENDER) <- c("Female", "Male")

# ETHNICITY

Maine_Data_LONG_2012_2013$ETHNICITY <- factor(Maine_Data_LONG_2012_2013$ETHNICITY, levels=1:7, 
	labels=c("American Indian or Alaskan Native", "Asian", "African American", "Hispanic or Latino", "Native Hawaiian or Pacific Islander", "White", "Multiple Ethnicities Reported"))

# IEP_STATUS

Maine_Data_LONG_2012_2013$IEP_STATUS <- factor(Maine_Data_LONG_2012_2013$IEP_STATUS, levels=0:1, c("Students without Disabilities (Non-IEP)", "Students with Disabilities (IEP)"))
levels(Maine_SGP@Data$IEP_STATUS) <- c("Students without Disabilities (Non-IEP)", "Students with Disabilities (IEP)")

# LEP STATUS

Maine_Data_LONG_2012_2013$LEP_STATUS[Maine_Data_LONG_2012_2013$LEP_STATUS %in% 1:3] <- 1
Maine_Data_LONG_2012_2013$LEP_STATUS <- factor(Maine_Data_LONG_2012_2013$LEP_STATUS, levels=0:1, labels=c("Non-English Language Learners (LEP)", "English Language Learners (LEP)"))
levels(Maine_SGP@Data$LEP_STATUS) <- c("Non-English Language Learners (LEP)", "English Language Learners (LEP)")

# SES_STATUS

Maine_Data_LONG_2012_2013$SES_STATUS <- factor(Maine_Data_LONG_2012_2013$SES_STATUS, levels=0:1, c("Not Economically Disadvanted", "Economically Disadvantaged"))
levels(Maine_SGP@Data$SES_STATUS) <- c("Not Economically Disadvanted", "Economically Disadvantaged")

# ACHIEVEMENT_LEVEL

Maine_Data_LONG_2012_2013$ACHIEVEMENT_LEVEL[Maine_Data_LONG_2012_2013$ACHIEVEMENT_LEVEL %in%
        c("A", "E", "N", "S", "W", "L")] <- NA
Maine_Data_LONG_2012_2013$ACHIEVEMENT_LEVEL <- droplevels(Maine_Data_LONG_2012_2013$ACHIEVEMENT_LEVEL)
levels(Maine_Data_LONG_2012_2013$ACHIEVEMENT_LEVEL) <-
	        c("Substantially Below Proficient", "Partially Proficient", "Proficient", "Proficient with Distinction")

# VALID_CASE

Maine_Data_LONG_2012_2013$VALID_CASE <- "VALID_CASE"

# ENROLLMENT_STATUS

Maine_Data_LONG_2012_2013$STATE_ENROLLMENT_STATUS <- factor(2, levels=1:2, labels=c("Enrolled State: No", "Enrolled State: Yes"))
Maine_Data_LONG_2012_2013$DISTRICT_ENROLLMENT_STATUS <- factor(2, levels=1:2, labels=c("Enrolled District: No", "Enrolled District: Yes"))
Maine_Data_LONG_2012_2013$SCHOOL_ENROLLMENT_STATUS <- factor(2, levels=1:2, labels=c("Enrolled School: No", "Enrolled School: Yes"))

### VALIDATE cases

Maine_Data_LONG_2012_2013$VALID_CASE[Maine_Data_LONG_2012_2013$TEST_STATUS!="A"] <- "INVALID_CASE"


#############################################################################
###
### Create Long file and save result
###
#############################################################################


### Fix up Maine District and School Names for 2012_2013

Maine_Data_LONG_2012_2013 <- as.data.table(Maine_Data_LONG_2012_2013)
Maine_District_and_School_Names <- as.data.table(Maine_District_and_School_Names)
Maine_District_and_School_Names$DISTRICT_NAME <- factor(Maine_District_and_School_Names$DISTRICT_NAME)
Maine_District_and_School_Names$SCHOOL_NAME <- factor(Maine_District_and_School_Names$SCHOOL_NAME)
setkeyv(Maine_District_and_School_Names, c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))
setkeyv(Maine_Data_LONG_2012_2013, c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))
Maine_Data_LONG_2012_2013 <- Maine_District_and_School_Names[Maine_Data_LONG_2012_2013, mult="all"]

## Identify duplicates

#setkey(Maine_Data_LONG_2012_2013, VALID_CASE, CONTENT_AREA, YEAR, ID)
#dups <- Maine_Data_LONG_2012_2013[Maine_Data_LONG_2012_2013[J("VALID_CASE")][duplicated(Maine_Data_LONG_2012_2013[J("VALID_CASE")])][,list(VALID_CASE, YEAR, CONTENT_AREA, ID)]]
setkey(Maine_Data_LONG_2012_2013, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, SCALE_SCORE)
setkey(Maine_Data_LONG_2012_2013, VALID_CASE, CONTENT_AREA, YEAR, ID)
Maine_Data_LONG_2012_2013[which(duplicated(Maine_Data_LONG_2012_2013))-1, VALID_CASE := "INVALID_CASE"]
setkey(Maine_Data_LONG_2012_2013, VALID_CASE, CONTENT_AREA, YEAR, ID)
dups <- Maine_Data_LONG_2012_2013[Maine_Data_LONG_2012_2013[J("VALID_CASE")][duplicated(Maine_Data_LONG_2012_2013[J("VALID_CASE")])][,list(VALID_CASE, YEAR, CONTENT_AREA, ID)]]

### Convert to data.frame and save results

Maine_Data_LONG_2012_2013 <- as.data.frame(Maine_Data_LONG_2012_2013)
Maine_Data_LONG_2012_2013 <- Maine_Data_LONG_2012_2013[,names(sort(sapply(names(Maine_Data_LONG_2012_2013), function(x) match(x, names(Maine_SGP@Data)))))]
Maine_Data_LONG_2012_2013 <- as.data.table(Maine_Data_LONG_2012_2013)
save(Maine_SGP, file="Data/Maine_SGP.Rdata")
save(Maine_Data_LONG_2012_2013, file="Data/Maine_Data_LONG_2012_2013.Rdata")
