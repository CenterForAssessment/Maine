#########################################################################################
###
### Create LONG file from previous long file and new csv data for Maine student reports
###
#########################################################################################

### Load SGP Package

require(SGP)
require(car) ### for recode


### Load data

load("Data/Base_Files/Maine_Data.Rdata")
data_1011 <- read.csv("Data/Base_Files/NECAP1011FallStateStudentReleasedItem0308.csv")
load("Data/Base_Files/Maine_District_and_School_Names.Rdata")

############################################################################
### Create new long data from previous long data and clean up
############################################################################

Maine_Data_LONG <- Maine_Data$Student
Maine_Data_LONG$YEAR_INTEGER <- NULL
Maine_Data_LONG$TRANSFORMED_SCALE_SCORE <- NULL

Maine_Data_LONG$LAST_NAME <- as.factor(Maine_Data_LONG$LAST_NAME)
Maine_Data_LONG$FIRST_NAME <- as.factor(Maine_Data_LONG$FIRST_NAME)
Maine_Data_LONG$SGP <- as.integer(Maine_Data_LONG$SGP)

Maine_Data_LONG$GENDER[Maine_Data_LONG$GENDER==""] <- NA
Maine_Data_LONG$GENDER <- factor(Maine_Data_LONG$GENDER)

names(Maine_Data_LONG)[11:16] <- c("IEP_STATUS", "LEP_STATUS", "SES_STATUS", "GIFTED_AND_TALENTED_PROGRAM_STATUS", "TITLE_1_STATUS", "SPECIAL_EDUCATION_STATUS")
levels(Maine_Data_LONG$IEP_STATUS) <- c("IEP: No", "IEP: Yes")
levels(Maine_Data_LONG$SES_STATUS) <- c("SES: No", "SES: Yes")
levels(Maine_Data_LONG$GIFTED_AND_TALENTED_PROGRAM_STATUS) <- c("Gifted and Talented Program: No", "Gifted and Talented Program: Yes")
levels(Maine_Data_LONG$TITLE_1_STATUS) <- c("Title 1: No", "Title 1: Yes")
levels(Maine_Data_LONG$SPECIAL_EDUCATION_STATUS) <- c("Special Education: No", "Special Education: Yes")
levels(Maine_Data_LONG$ACHIEVEMENT_LEVEL)[c(1,6,2)] <- c("Substantially Below Proficient", "Partially Proficient", "Proficient with Distinction")

Maine_Data_LONG <- as.data.table(Maine_Data_LONG)
key(Maine_Data_LONG) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")

###############################################################
### Clean up 2010-2011 data
###############################################################

names(data_1011) <- toupper(names(data_1011))
tmp.variables <- c("RPTSTUDID", "LNAME", "FNAME", "GRADE", "SPRDISCODE", "SPRSCHCODE", 
                   "GENDER", "ETHNIC", "LEP", "IEP", "SES", "REASCALEDSCORE", "REAAL", "MATSCALEDSCORE", "MATAL")
data_1011 <- subset(data_1011, select=tmp.variables)

attach(data_1011)
read_1011 <- data.frame(ID=RPTSTUDID,
                        YEAR="2010_2011",
                        CONTENT_AREA="READING",
                        LAST_NAME=LNAME,
                        FIRST_NAME=FNAME,
                        GRADE=GRADE, 
                        DISTRICT_NUMBER=SPRDISCODE,
                        SCHOOL_NUMBER=SPRSCHCODE,
                        GENDER=GENDER,
                        ETHNICITY=ETHNIC,
                        IEP_STATUS=IEP,
                        LEP_STATUS=LEP,
                        SES_STATUS=SES,
                        GIFTED_AND_TALENTED_PROGRAM_STATUS=NA,
                        TITLE_1_STATUS=NA,
                        SPECIAL_EDUCATION_STATUS=NA,
                        SCALE_SCORE=REASCALEDSCORE,
                        ACHIEVEMENT_LEVEL=REAAL)

math_1011 <- data.frame(ID=RPTSTUDID,
                        YEAR="2010_2011",
                        CONTENT_AREA="MATHEMATICS",
                        LAST_NAME=LNAME,
                        FIRST_NAME=FNAME,
                        GRADE=GRADE, 
                        DISTRICT_NUMBER=SPRDISCODE,
                        SCHOOL_NUMBER=SPRSCHCODE,
                        GENDER=GENDER,
                        ETHNICITY=ETHNIC,
                        IEP_STATUS=IEP,
                        LEP_STATUS=LEP,
                        SES_STATUS=SES,
                        GIFTED_AND_TALENTED_PROGRAM_STATUS=NA,
                        TITLE_1_STATUS=NA,
                        SPECIAL_EDUCATION_STATUS=NA,
                        SCALE_SCORE=MATSCALEDSCORE,
                        ACHIEVEMENT_LEVEL=MATAL)

detach(data_1011)

data_1011 <- rbind(read_1011, math_1011)
data_1011 <- subset(data_1011, !is.na(ID))

data_1011$GENDER[data_1011$GENDER=="X"] <- NA
data_1011$GENDER <- factor(data_1011$GENDER)

data_1011$ETHNICITY <- factor(data_1011$ETHNICITY, levels=1:7, 
	labels=c("American Indian or Alaskan Native", "Asian", "African American", "Hispanic or Latino", "Native Hawaiian or Pacific Islander", "White", "Multiple Ethnicities Reported"))

data_1011$IEP_STATUS <- factor(data_1011$IEP_STATUS, levels=0:1, labels=c("IEP: No", "IEP: Yes"))
data_1011$SES_STATUS <- factor(data_1011$SES_STATUS, levels=0:1, labels=c("SES: No", "SES: Yes"))
data_1011$GIFTED_AND_TALENTED_PROGRAM_STATUS <- factor(data_1011$GIFTED_AND_TALENTED_PROGRAM_STATUS)
data_1011$TITLE_1_STATUS <- factor(data_1011$TITLE_1_STATUS)
data_1011$SPECIAL_EDUCATION_STATUS <- factor(data_1011$SPECIAL_EDUCATION_STATUS)

data_1011$ACHIEVEMENT_LEVEL <- recode(data_1011$ACHIEVEMENT_LEVEL, 
                                               "'1' = 'Substantially Below Proficient'; 
                                                '2' = 'Partially Proficient';
                                                '3' = 'Proficient';
                                                '4' = 'Proficient with Distinction';
                                                'A' = NA;
                                                'L' = NA;
                                                'N' = NA;
                                                'S' = NA;
						'' = NA")

data_1011$VALID_CASE <- factor(2, levels=1:2, labels=c("INVALID_CASE", "VALID_CASE"))

data_1011 <- as.data.table(data_1011)
key(data_1011) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")



#############################################################################
###
### Create Long file and save result
###
#############################################################################

Maine_Data_LONG <- rbind.fill(Maine_Data_LONG, data_1011)
Maine_Data_LONG <- as.data.table(Maine_Data_LONG)
Maine_Data_LONG$SGP <- NULL

### Fix up Maine District and School Names

Maine_District_and_School_Names <- as.data.table(Maine_District_and_School_Names)
Maine_District_and_School_Names$DISTRICT_NAME <- factor(Maine_District_and_School_Names$DISTRICT_NAME)
Maine_District_and_School_Names$SCHOOL_NAME <- factor(Maine_District_and_School_Names$SCHOOL_NAME)
key(Maine_District_and_School_Names) <- c("DISTRICT_NUMBER", "SCHOOL_NUMBER")
key(Maine_Data_LONG) <- c("DISTRICT_NUMBER", "SCHOOL_NUMBER")
Maine_Data_LONG <- merge(Maine_District_and_School_Names, Maine_Data_LONG, all.y=TRUE)
Maine_Data_LONG <- Maine_Data_LONG[,c(5:9,1:4,10:21), with=FALSE]


### Save results

save(Maine_Data_LONG, file="Data/Maine_Data_LONG.Rdata")




