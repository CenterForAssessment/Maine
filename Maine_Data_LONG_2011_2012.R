#########################################################################################
###
### Create LONG file from previous long file and new csv data for Maine student reports
###
#########################################################################################

### Load SGP Package

require(SGP)
require(car) ### for recode


### Utility functions

capwords <- function(x) {
      special.words <- c("ELA", "EMH", "II", "III", "IV")
      if (x %in% special.words) return(x)
      s <- sub("_", " ", x)
      s <- strsplit(s, split=" ")[[1]]
      s <- paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)), sep="", collapse=" ")
      s <- strsplit(s, split="-")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse="-")
}


### Load data

Maine_Data_LONG_2011_2012 <- read.csv("Data/Base_Files/NECAP1112FallStateStudentReleasedItem0308-Final.csv")
load("Data/Base_Files/School_District_Number_Table_2011_2012.Rdata")
Maine_District_and_School_Names <- School_District_Number_Table_2011_2012


###############################################################
### Clean up 2011-2012 data
###############################################################

names(Maine_Data_LONG_2011_2012) <- toupper(names(Maine_Data_LONG_2011_2012))
tmp.variables <- c("RPTSTUDID", "LNAME", "FNAME", "GRADE", "SPRDISCODE", "SPRSCHCODE", 
                   "GENDER", "ETHNIC", "LEP", "IEP", "SES", "REASCALEDSCORE", "REAAL", "MATSCALEDSCORE", "MATAL")
Maine_Data_LONG_2011_2012 <- subset(Maine_Data_LONG_2011_2012, select=tmp.variables)

attach(Maine_Data_LONG_2011_2012)
read_1112 <- data.frame(ID=RPTSTUDID,
                        YEAR="2011_2012",
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

math_1112 <- data.frame(ID=RPTSTUDID,
                        YEAR="2011_2012",
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

detach(Maine_Data_LONG_2011_2012)

Maine_Data_LONG_2011_2012 <- rbind(read_1112, math_1112)
Maine_Data_LONG_2011_2012 <- subset(Maine_Data_LONG_2011_2012, !is.na(ID))

levels(Maine_Data_LONG_2011_2012$GENDER) <- c("Female", "Male")

Maine_Data_LONG_2011_2012$ETHNICITY <- factor(Maine_Data_LONG_2011_2012$ETHNICITY, levels=1:7, 
	labels=c("American Indian or Alaskan Native", "Asian", "African American", "Hispanic or Latino", "Native Hawaiian or Pacific Islander", "White", "Multiple Ethnicities Reported"))

Maine_Data_LONG_2011_2012$IEP_STATUS <- factor(Maine_Data_LONG_2011_2012$IEP_STATUS, levels=0:1, labels=c("IEP: No", "IEP: Yes"))
Maine_Data_LONG_2011_2012$SES_STATUS <- factor(Maine_Data_LONG_2011_2012$SES_STATUS, levels=0:1, labels=c("SES: No", "SES: Yes"))
Maine_Data_LONG_2011_2012$GIFTED_AND_TALENTED_PROGRAM_STATUS <- factor(Maine_Data_LONG_2011_2012$GIFTED_AND_TALENTED_PROGRAM_STATUS)
Maine_Data_LONG_2011_2012$TITLE_1_STATUS <- factor(Maine_Data_LONG_2011_2012$TITLE_1_STATUS)
Maine_Data_LONG_2011_2012$SPECIAL_EDUCATION_STATUS <- factor(Maine_Data_LONG_2011_2012$SPECIAL_EDUCATION_STATUS)
Maine_Data_LONG_2011_2012$SCALE_SCORE <- as.numeric(Maine_Data_LONG_2011_2012$SCALE_SCORE)
Maine_Data_LONG_2011_2012$LEP_STATUS[Maine_Data_LONG_2011_2012$LEP_STATUS %in% 1:3] <- 1
Maine_Data_LONG_2011_2012$LEP_STATUS <- factor(Maine_Data_LONG_2011_2012$LEP_STATUS, levels=0:1, labels=c("LEP Status: No", "LEP Status: Yes"))

Maine_Data_LONG_2011_2012$ACHIEVEMENT_LEVEL <- recode(Maine_Data_LONG_2011_2012$ACHIEVEMENT_LEVEL, 
                                               "'1' = 'Substantially Below Proficient'; 
                                                '2' = 'Partially Proficient';
                                                '3' = 'Proficient';
                                                '4' = 'Proficient with Distinction';
                                                'A' = NA;
                                                'L' = NA;
                                                'N' = NA;
                                                'S' = NA;
						'' = NA")

Maine_Data_LONG_2011_2012$VALID_CASE <- factor(2, levels=1:2, labels=c("INVALID_CASE", "VALID_CASE"))

Maine_Data_LONG_2011_2012 <- as.data.table(Maine_Data_LONG_2011_2012)
setkeyv(Maine_Data_LONG_2011_2012, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))



#############################################################################
###
### Create Long file and save result
###
#############################################################################


### Fix up Maine District and School Names

Maine_District_and_School_Names <- as.data.table(Maine_District_and_School_Names)
Maine_District_and_School_Names$DISTRICT_NAME <- factor(Maine_District_and_School_Names$DISTRICT_NAME)
Maine_District_and_School_Names$SCHOOL_NAME <- factor(Maine_District_and_School_Names$SCHOOL_NAME)
setkeyv(Maine_District_and_School_Names, c("SCHOOL_NUMBER"))
setkeyv(Maine_Data_LONG_2011_2012, c("SCHOOL_NUMBER"))
Maine_Data_LONG_2011_2012 <- Maine_District_and_School_Names[Maine_Data_LONG_2011_2012]
Maine_Data_LONG_2011_2012 <- as.data.frame(Maine_Data_LONG_2011_2012)

### Save results

#save(Maine_Data_LONG_2011_2012, file="Data/Maine_Data_LONG_2011_2012.Rdata")




