######################################################################################
######################################################################################
######################################################################################
### Social Network Analysis
### Lab 1
######################################################################################
######################################################################################
######################################################################################

### Multi-City Inequality Study
### Codebook: http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/2535/version/3

setwd("~/Desktop/SOCIAL NETWORK ANALYSIS/Data/Multi-City Inequality Study")
require(foreign)
require(arm)
data <- read.dta(file="Inequality.dta", convert.dates = TRUE, convert.factors = TRUE)
dim(data)
names(data)

table(data$city)
#     detroit     atlanta    los angeles      boston 
#      1543        1528            4025        1820 

table(data$intvsex) # sex of interviewer
# male female 
# 4661   4255 

table(data$intvrace) # race of interviewer
# white    black hispanic    asian 
# 3098     3029     1292     1497 

# To help us understand your living situation, I would like to make a list of persons 
# who usually live here. Please include the adults as well as the children. 
# Let's start with you, then continue with the other adults, then the children. 
# It will help if we identify them by name, but that is not necessary. 
# What I need to know is their sex, their age on their last birthday, 
# and their relationship to you.
table(data$cp01sex)
table(data$cp01age)
table(data$cp01rel)
# .....
table(data$cp12sex)
table(data$cp12age)
table(data$cp12rel)

# How many of your children under 18 are living here with you?
table(data$cchilhom)
#    0    1    2    3    4    5    6    7    8   99 
# 5541 1319 1174  592  202   61   15    5    3    4 

# Now, I would like to know a little about your brothers and sisters. 
# How many brothers and sisters do you have? 
# Please count your full, half, adopted or step brothers and sisters, 
# whether or not they are still living.
table(data$enumsibs)

# Starting with the oldest, could you tell me each one's age on their 
# last birthday, and the grade of school or year of college each has completed?
table(data$esb01sex)
table(data$esb01age)
table(data$esb01yrd)
table(data$esb01edu)
# .................
table(data$esb15sex)
table(data$esb15age)
table(data$esb15yrd)
table(data$esb15edu)

# Now I am going to read you some statements that describe some ways a person 
# might feel about (himself/herself). After each statement is read please 
# tell me how you: if you strongly agree, somewhat agree, somewhat disagree, 
# or strongly disagree with the following:
          # 0 Logical Skip---4, 5 in F58 (LA only)
          # 1 Strongly Agree
          # 2 Somewhat Agree
          # 3 Moderately Disagree
          # 4 Strongly Disagree
          # 7 Refused 
          # 8 Don't Know
          # 9 Missing

### I feel I do not have much to be proud of.
table(data$fsestem1)
### On the whole, I am satisfied with myself.
table(data$fsestem2)
### All in all, I am inclined to feel that I am a failure.
table(data$fsestem3)
### I take a positive attitude towards myself.
table(data$fsestem4)

table(data$gicrace) # respondent's race

########################################
table(data$gbneiat1) # would you move to a black neighborhood
table(data$ghneiat1) # would you move to a hispanic neighborhood
table(data$ganeiat1) # would you move to a asian neighborhood
########################################

# Now I'd like you to imagine an ideal neighborhood that had the ethnic and 
# racial mix you personally would feel most comfortable in. 
# Here is a blank neighborhood card like those we have been using. 
# Using the letters A for Asian, B for Black, H for Hispanic, and W for White, 
# please put a letter in each of these houses to represent you ideal neighborhood 
# where you would most like to live. Please be sure to fill in all the houses.
            # 0 Logical Skip---1 in G40 0
            # 1 Asian 
            # 2 Black
            # 3 Hispanic 
            # 4 White 
            # 5 Other 
            # 7 Refused 
            # 8 Don't Know 
            # 9 Missing
table(data$gidlrac1)
# ......
table(data$gidlrc15)

# Now we have some questions about your social relationships and neighborhood activities. 
# From time to time, most people discuss important matters with other people. 
# Looking back over the last six months-- who are the people, other than people 
# living in your household, with whom you discussed matters important to you? 
# Please tell me the first name or initials of the people with whom you discussed 
# matters important to you.
table(data$hnumnetw)
# none     one     two   three skipped missing 
# 2052     963     998    2892     454      14 

### Sex of Person 1
table(data$hsexper1)
### What is person 1's relationship to you?
table(data$hrelper1)
### Is person 1 married?
table(data$hmarper1)
### Does person 1 live in your neighborhood?
table(data$hneiper1)
### In the past month, did you help person 1 do everyday things like giving 
# him/her a ride somewhere lending him/her a little money, or running errands?
table(data$hhlpper1)
### Is person 1 someone you could count on for help in a major crisis, 
# such as serious illness or if you needed a place to stay?
table(data$hcntper1)
### Does person 1 have a steady job?
table(data$hjobper1)
### Is person 1 receiving public aid or welfare?
table(data$hwelper1)
### What level of education has person 1 completed?
table(data$heduper1)
### What is person 1's race or ethnicity?
table(data$hracper1)

### NOTE: up to 3 friends

######################################################################################
######################################################################################
######################################################################################
### Recode my variables of interest
######################################################################################
######################################################################################
######################################################################################


############################################
### Independent Variable
### Self Esteem
############################################

# Now I am going to read you some statements that describe some ways a person 
# might feel about (himself/herself). After each statement is read please 
# tell me how you: if you strongly agree, somewhat agree, somewhat disagree, 
# or strongly disagree with the following:
            # 0 Logical Skip---4, 5 in F58 (LA only)
            # 1 Strongly Agree
            # 2 Somewhat Agree
            # 3 Moderately Disagree
            # 4 Strongly Disagree
            # 7 Refused 
            # 8 Don't Know
            # 9 Missing
### I feel I do not have much to be proud of.
table(data$fsestem1)
data$SelfEsteem1 <- NA
data$SelfEsteem1[data$fsestem1 == 1] <- 1
data$SelfEsteem1[data$fsestem1 == 2] <- 2
data$SelfEsteem1[data$fsestem1 == 3] <- 3
data$SelfEsteem1[data$fsestem1 == 4] <- 4
table(data$SelfEsteem1, useNA="always")
### On the whole, I am satisfied with myself.
table(data$fsestem2)
data$SelfEsteem2 <- NA
data$SelfEsteem2[data$fsestem2 == 4] <- 1
data$SelfEsteem2[data$fsestem2 == 3] <- 2
data$SelfEsteem2[data$fsestem2 == 2] <- 3
data$SelfEsteem2[data$fsestem2 == 1] <- 4
table(data$SelfEsteem2, useNA="always")
### All in all, I am inclined to feel that I am a failure.
table(data$fsestem3)
data$SelfEsteem3 <- NA
data$SelfEsteem3[data$fsestem3 == 1] <- 1
data$SelfEsteem3[data$fsestem3 == 2] <- 2
data$SelfEsteem3[data$fsestem3 == 3] <- 3
data$SelfEsteem3[data$fsestem3 == 4] <- 4
table(data$SelfEsteem3, useNA="always")
### I take a positive attitude towards myself.
table(data$fsestem4)
data$SelfEsteem4 <- NA
data$SelfEsteem4[data$fsestem4 == 4] <- 1
data$SelfEsteem4[data$fsestem4 == 3] <- 2
data$SelfEsteem4[data$fsestem4 == 2] <- 3
data$SelfEsteem4[data$fsestem4 == 1] <- 4
table(data$SelfEsteem4, useNA="always")

### INDEX SELF-ESTEEM VARIABLE
names(data[,687:690]) # [1] "SelfEsteem1" "SelfEsteem2" "SelfEsteem3" "SelfEsteem4"
data$SelfEsteem <- rowMeans(data[,689:690], na.rm=TRUE)
table(data$SelfEsteem)
#  1  1.5    2  2.5    3  3.5    4 
# 12   21   74  305  850 1123 3441 
class(data$SelfEsteem) # [1] "numeric"


############################################
### Independent Variable
### Average Age of Individuals in Household
############################################

# To help us understand your living situation, I would like to make a list of persons 
# who usually live here. Please include the adults as well as the children. 
# Let's start with you, then continue with the other adults, then the children. 
# It will help if we identify them by name, but that is not necessary. 
# What I need to know is their sex, their age on their last birthday, 
# and their relationship to you.
table(data$cp01age)
data$AgeHouse1 <- data$cp01age
data$AgeHouse1[data$cp01age == 0] <- NA
data$AgeHouse1[data$cp01age == 97] <- NA
data$AgeHouse1[data$cp01age == 98] <- NA
data$AgeHouse1[data$cp01age == 99] <- NA
data$AgeHouse1[data$cp01age == 107] <- NA
table(data$AgeHouse1, useNA= "always")
table(data$AgeHouse1)

table(data$cp02age)
data$AgeHouse2 <- data$cp02age
data$AgeHouse2[data$cp02age == 0] <- NA
data$AgeHouse2[data$cp02age == 97] <- NA
data$AgeHouse2[data$cp02age == 98] <- NA
data$AgeHouse2[data$cp02age == 99] <- NA
table(data$AgeHouse2, useNA= "always")
table(data$AgeHouse2)

table(data$cp03age)
data$AgeHouse3 <- data$cp03age
data$AgeHouse3[data$cp03age == 0] <- NA
data$AgeHouse3[data$cp03age == 97] <- NA
data$AgeHouse3[data$cp03age == 98] <- NA
data$AgeHouse3[data$cp03age == 99] <- NA
table(data$AgeHouse3, useNA= "always")
table(data$AgeHouse3)

table(data$cp04age)
data$AgeHouse4 <- data$cp04age
data$AgeHouse4[data$cp04age == 0] <- NA
data$AgeHouse4[data$cp04age == 99] <- NA
table(data$AgeHouse4, useNA= "always")
table(data$AgeHouse4)

table(data$cp05age)
data$AgeHouse5 <- data$cp05age
data$AgeHouse5[data$cp05age == 0] <- NA
data$AgeHouse5[data$cp05age == 99] <- NA
table(data$AgeHouse5, useNA= "always")
table(data$AgeHouse5)

table(data$cp06age)
data$AgeHouse6 <- data$cp06age
data$AgeHouse6[data$cp06age == 0] <- NA
data$AgeHouse6[data$cp06age == 99] <- NA
table(data$AgeHouse6, useNA= "always")
table(data$AgeHouse6)

table(data$cp07age)
data$AgeHouse7 <- data$cp07age
data$AgeHouse7[data$cp07age == 0] <- NA
data$AgeHouse7[data$cp07age == 99] <- NA
table(data$AgeHouse7, useNA= "always")
table(data$AgeHouse7)

table(data$cp08age)
data$AgeHouse8 <- data$cp08age
data$AgeHouse8[data$cp08age == 0] <- NA
data$AgeHouse8[data$cp08age == 97] <- NA
table(data$AgeHouse8, useNA= "always")
table(data$AgeHouse8)

table(data$cp09age)
data$AgeHouse9 <- data$cp09age
data$AgeHouse9[data$cp09age == 0] <- NA
table(data$AgeHouse9, useNA= "always")
table(data$AgeHouse9)

table(data$cp10age)
data$AgeHouse10 <- data$cp10age
data$AgeHouse10[data$cp10age == 0] <- NA
data$AgeHouse10[data$cp10age == 99] <- NA
table(data$AgeHouse10, useNA= "always")
table(data$AgeHouse10)

table(data$cp11age)
data$AgeHouse11 <- data$cp11age
data$AgeHouse11[data$cp11age == 0] <- NA
data$AgeHouse11[data$cp11age == 99] <- NA
table(data$AgeHouse11, useNA= "always")
table(data$AgeHouse11)

table(data$cp12age)
data$AgeHouse12 <- data$cp12age
data$AgeHouse12[data$cp12age == 0] <- NA
data$AgeHouse12[data$cp12age == 99] <- NA
table(data$AgeHouse12, useNA= "always")
table(data$AgeHouse12)

### AVERAGE AGE OF INDIVIDUALS IN HOUSEHOLD
names(data[,692:703]) 
#  [1] "AgeHouse1"  "AgeHouse2"  "AgeHouse3"  "AgeHouse4"  "AgeHouse5"  "AgeHouse6"  "AgeHouse7" 
#  [8] "AgeHouse8"  "AgeHouse9"  "AgeHouse10" "AgeHouse11" "AgeHouse12"
data$MeanHouseholdAge <- rowMeans(data[,692:703], na.rm=TRUE)
range(data$MeanHouseholdAge, na.rm=TRUE)
#  [1]  4 96
class(data$MeanHouseholdAge) # [1] "numeric"

#############################################
### Independent Variable
### Number children under 18 in the household
#############################################

# How many of your children under 18 are living here with you?
table(data$cchilhom)
#    0    1    2    3    4    5    6    7    8   99 
# 5541 1319 1174  592  202   61   15    5    3    4 
data$MinorsHousehold <- data$cchilhom
data$MinorsHousehold[data$cchilhom == 0] <- NA
data$MinorsHousehold[data$cchilhom == 99] <- NA
table(data$MinorsHousehold, useNA= "always")
table(data$MinorsHousehold)

############################################
### Independent Variable
### Number of siblings
############################################

# Now, I would like to know a little about your brothers and sisters. 
# How many brothers and sisters do you have? 
# Please count your full, half, adopted or step brothers and sisters, 
# whether or not they are still living.
table(data$enumsibs)
data$Siblings <- data$enumsibs
data$Siblings[data$enumsibs == 0] <- NA
data$Siblings[data$enumsibs == 90] <- NA
data$Siblings[data$enumsibs == 97] <- NA
data$Siblings[data$enumsibs == 99] <- NA
table(data$Siblings, useNA= "always")
table(data$Siblings)

############################################
### Independent Variable
### Marital Status
############################################

# This next set of questions is about your own background. 
# What is your present marital status? 
# Are you married, separated, divorced, widowed, have you never been married, 
# or are you living with a partner?
table(data$cmarrsta)
data$MaritalStatus <- data$cmarrsta
levels(data$MaritalStatus)[9] <- NA
levels(data$MaritalStatus)[8] <- NA
levels(data$MaritalStatus)[7] <- NA
levels(data$MaritalStatus)[6] <- 'LivingWithPartner'
levels(data$MaritalStatus)[5] <- 'NeverMarried'
levels(data$MaritalStatus)[4] <- 'Widowed'
levels(data$MaritalStatus)[3] <- 'Divorced'
levels(data$MaritalStatus)[2] <- 'Separated'
levels(data$MaritalStatus)[1] <- 'Married'
table(data$MaritalStatus, useNA= "always")
table(data$MaritalStatus)
class(data$MaritalStatus) # [1] "factor"

data$Married <- data$cmarrsta
levels(data$Married)[9] <- NA
levels(data$Married)[8] <- NA
levels(data$Married)[7] <- NA
levels(data$Married)[6] <- 'Not Married'
levels(data$Married)[5] <- 'Not Married'
levels(data$Married)[4] <- 'Not Married'
levels(data$Married)[3] <- 'Not Married'
levels(data$Married)[2] <- 'Not Married'
levels(data$Married)[1] <- 'Married'
table(data$Married, useNA= "always")
table(data$Married)
class(data$Married) # [1] "factor"

############################################
### Independent Variable
### Household Size
############################################

# Number of people in the household
table(data$chhsize)
data$HouseholdSize <- data$chhsize
table(data$HouseholdSize, useNA= "always")
table(data$HouseholdSize)

############################################
### Independent Variable
### Respondent's Race
############################################

# Do you think most people would treat you as a black, Hispanic, white or something else?
table(data$crace)
data$Race <- as.character(data$crace)
data$Race[data$crace == 'skipped'] <- NA
data$Race[data$crace == 'white'] <- 'White'
data$Race[data$crace == 'black/afr.american'] <- 'Black'
data$Race[data$crace == 'asian (specify)'] <- 'Asian'
data$Race[data$crace == 'american indian'] <- 'Other'
data$Race[data$crace == 'other (specify)'] <- 'Other'
data$Race[data$crace == 'missing'] <- NA
table(data$Race, useNA= "always")
table(data$Race)
class(data$Race) # [1] "character"
data$Race <- as.factor(data$Race)
class(data$Race) # [1] "factor"
data$Race <- factor(data$Race, levels = c("White","Black","Asian","Other"))

############################################
### Independent Variable
### Respondent's Home Language
############################################

# Is any language other than English used in your home now?
table(data$clangoth)
data$OtherLanguage <- as.character(data$clangoth)
data$OtherLanguage[data$clangoth == 'yes'] <- "Yes" 
data$OtherLanguage[data$clangoth == 'no'] <- "No" 
data$OtherLanguage[data$clangoth == 'refused'] <- NA
data$OtherLanguage[data$clangoth == "don't know"] <- NA
data$OtherLanguage[data$clangoth == 'missing'] <- NA
table(data$OtherLanguage, useNA= "always")
table(data$OtherLanguage)
class(data$OtherLanguage) # [1] "character"
data$OtherLanguage <- as.factor(data$OtherLanguage)
class(data$OtherLanguage) # [1] "factor"

############################################
### Dependent Variable
### Network Size
############################################

# Now we have some questions about your social relationships and neighborhood activities. 
# From time to time, most people discuss important matters with other people. 
# Looking back over the last six months-- who are the people, other than people 
# living in your household, with whom you discussed matters important to you? 
# Please tell me the first name or initials of the people with whom you discussed 
# matters important to you.
table(data$hnumnetw)
# none     one     two   three skipped missing 
# 2052     963     998    2892     454      14 
data$NetworkSize <- as.character(data$hnumnetw)
data$NetworkSize[data$hnumnetw == 'none'] <- 0
data$NetworkSize[data$hnumnetw == 'one'] <- 1
data$NetworkSize[data$hnumnetw == 'two'] <- 2
data$NetworkSize[data$hnumnetw == 'three'] <- 3
data$NetworkSize[data$hnumnetw == 'skipped'] <- NA
data$NetworkSize[data$hnumnetw == 'missing'] <- NA
table(data$NetworkSize, useNA= "always")
table(data$NetworkSize)
class(data$NetworkSize) # [1] "character"
data$NetworkSize <- as.numeric(data$NetworkSize)
class(data$NetworkSize) # [1] "numeric"


######################################################################################
######################################################################################
######################################################################################
### Linear Regression Models
######################################################################################
######################################################################################
######################################################################################

### Dependent variable: 
        # data$NetworkSize
summary(data$NetworkSize)
### Independent variables:
        # data$MeanHouseholdAge
        # data$MinorsHousehold
        # data$Siblings
        # data$SelfEsteem
        # data$MaritalStatus
        # data$Married
        # data$HouseholdSize
        # data$Race
        # data$OtherLanguage
summary(data$HouseholdSize)


### Running Models:

Model1 <- lm(formula=data$NetworkSize ~ data$Siblings)
display(Model1, detail=TRUE)

Model2 <- lm(formula=data$NetworkSize ~ data$Siblings + data$SelfEsteem)
display(Model2, detail=TRUE)

Model3 <- lm(formula=data$NetworkSize ~ data$Siblings + data$SelfEsteem + data$city)
display(Model3, detail=TRUE)

Model4 <- lm(formula=data$NetworkSize ~ data$Siblings + data$SelfEsteem + data$city +
               data$Married)
display(Model4, detail=TRUE)

Model5 <- lm(formula=data$NetworkSize ~ data$Siblings + data$SelfEsteem + data$city +
               data$HouseholdSize)
display(Model5, detail=TRUE)

Model6 <- lm(formula=data$NetworkSize ~ data$Siblings + data$SelfEsteem + data$city +
               data$HouseholdSize + data$Race)
display(Model6, detail=TRUE)

Model7 <- lm(formula=data$NetworkSize ~ data$Siblings + data$SelfEsteem + data$city +
               data$HouseholdSize + data$Race + data$OtherLanguage)
display(Model7, detail=TRUE)

require(coefplot)
multiplot(Model1, Model2, Model3, Model4, Model5, Model6, Model7)
