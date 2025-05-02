# Unions
Factors of being Pro Union

## Project:  FILL THIS OUT
# Located:   FILL THIS OUT
# File Name: FILL THIS OUT
# Date:      FILL THIS OUT
# Who:       FILL THIS OUT


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################


### Settings + Packages
#install.packages("dplyr")
#install.packages("psych")
library(dplyr)
library(psych)

### Load data 
GSS <- read.csv("GSS2022.csv")

####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################



############                     DEPENDENT VARIABLE                     ############
############                   WORKERS BELIEFS ON UNIONS                ############

## Steps of cleaning variables Clear vars
# Step 1: Examine variable and coding schema: Table() / summary() 
table(GSS$trdunion)

# Step 2: Recode (if necessary/warrented): mutate(), ifelse(), etc
GSS <- mutate(GSS, vprounion = ifelse(trdunion == 1, 1, 0))
GSS <- mutate(GSS, sprounion = ifelse(trdunion == 2, 1, 0))
GSS <- mutate(GSS, sdisunion = ifelse(trdunion == 3, 1, 0))
GSS <- mutate(GSS, vdisunion = ifelse(trdunion == 4, 1, 0))
GSS <- mutate(GSS, prounion = ifelse(trdunion == 1| trdunion == 2, 1, 0))
GSS <- mutate(GSS, disunion = ifelse(trdunion == 3| trdunion == 4, 1, 0))
# Step 3: Confirm: table() / summary()
table(GSS$trdunion, GSS$vprounion)
table(GSS$trdunion, GSS$sprounion)
table(GSS$trdunion, GSS$sdisunion)
table(GSS$trdunion, GSS$vdisunion)

table(GSS$trdunion, GSS$prounion)
table(GSS$trdunion, GSS$disunion)

############                  INDEPENDENT VARIABLE                    ############
############                    JOB OCCUPATION                ############

# STEP 1: Examine variable and coding schema 
table(GSS$wrkslf)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, selfemployed = ifelse(wrkslf == 1, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$wrkslf, GSS$selfemployed)

############                  INDEPENDENT VARIABLE                    ############
############                    JOB OCCUPATION                ############

# STEP 1: Examine variable and coding schema 


####################################################################################

# STEP 1: Examine variable and coding schema 
table(GSS$pandinc)

# STEP 2: Recode if necessary or justify if not neccesary

GSS <- mutate(GSS, increaselot = ifelse(pandinc == 1, 1, 0))
GSS <- mutate(GSS, increaselil = ifelse(pandinc == 2, 1, 0))
GSS <- mutate(GSS, staysame= ifelse(pandinc == 3, 1, 0))
GSS <- mutate(GSS, decreaselil = ifelse(pandinc == 4, 1, 0))
GSS <- mutate(GSS, decreaselot = ifelse(pandinc == 5, 1, 0))
GSS <- mutate(GSS, Increase = ifelse(pandinc == 1| pandinc == 2, 1, 0))
GSS <- mutate(GSS, Decrease = ifelse(pandinc == 4| pandinc == 5, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$pandinc, GSS$increaselot)
table(GSS$pandinc, GSS$increaselil)
table(GSS$pandinc, GSS$staysame)
table(GSS$pandinc, GSS$decreaselil)
table(GSS$pandinc, GSS$decreaselot)
####################################################################################

# STEP 1: Examine variable and coding schema 
table(GSS$union)

# STEP 2: Recode if necessary or justify if not neccesary

GSS <- mutate(GSS, respondent = ifelse(union == 1, 1, 0))
GSS <- mutate(GSS, spouse = ifelse(union == 2, 1, 0))
GSS <- mutate(GSS, both = ifelse(union == 3, 1, 0))
GSS <- mutate(GSS, neither_union = ifelse(union == 4, 1, 0))


# STEP 3: Confirm creation (if necessary)

table(GSS$union, GSS$respondent)
table(GSS$union, GSS$spouse)
table(GSS$union, GSS$both)
table(GSS$union, GSS$neither_union)

####################################################################################
# STEP 1: Examine variable and coding schema 
table(GSS$sex)

# STEP 2: Recode if necessary or justify if not neccesary

GSS <- mutate(GSS, male = ifelse(sex == 1, 1, 0))
GSS <- mutate(GSS, female = ifelse(sex == 2, 1, 0))

# STEP 3: Confirm creation (if necessary)

table(GSS$sex, GSS$male)
table(GSS$sex, GSS$female)


####################################################################################

# STEP 1: Examine variable and coding schema 
table(GSS$partyid)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS,  strongdemo= ifelse(partyid == 0, 1, 0))
GSS <- mutate(GSS,  semidemo= ifelse(partyid == 1, 1, 0))
GSS <- mutate(GSS,  closedemo= ifelse(partyid == 2, 1, 0))
GSS <- mutate(GSS,  independent= ifelse(partyid == 3, 1, 0))
GSS <- mutate(GSS,  closerep= ifelse(partyid == 4, 1, 0))
GSS <- mutate(GSS,  semirep= ifelse(partyid == 5, 1, 0))
GSS <- mutate(GSS,  strongrep= ifelse(partyid == 6, 1, 0))
GSS <- mutate(GSS,  neither= ifelse(partyid == 7, 1, 0))

GSS <- mutate(GSS, democrat = ifelse(partyid == 0| partyid == 1| partyid == 2, 1, 0))
GSS <- mutate(GSS, neither_party = ifelse(partyid == 3| partyid == 7, 1, 0))
GSS <- mutate(GSS, republican = ifelse(partyid == 4| partyid == 5| partyid == 6, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$partyid, GSS$democrat)
table(GSS$partyid, GSS$neither_party)
table(GSS$partyid, GSS$republican)

####################################################################################
####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep
my_varlist <- c("trdunion", "prounion", "disunion", 
                "wrkslf", "selfemployed", "staysame", "Increase", "Decrease", "respondent",
                "spouse", "both", "neither_union", "democrat", "neither_party", "republican", 
                "male", "female")
### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))
### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)


####################################################################################
############              PHASE 3: Descriptive Statistics     ############
####################################################################################
# TABLE 1: DESCRIPTIVE STATISTICS HERE
describe(my_dataset)

####################################################################################
############              PHASE 4: Correlation Matrix                  ############
####################################################################################

cor(my_dataset)

####################################################################################
############              PHASE 5: Regression                  ############
####################################################################################

model1 <- glm(prounion ~ selfemployed, data = my_dataset, family=binomial)
summary(model1)

model2 <- glm(prounion ~ Increase + Decrease, data = my_dataset, family=binomial)
summary(model2)

model3 <- glm(prounion ~  spouse + respondent + both, data = my_dataset, family=binomial)
summary(model3)

model4 <- glm(prounion ~  democrat + republican, data = my_dataset, family=binomial)
summary(model4)

model5 <- glm(prounion ~ female, data = my_dataset, family=binomial)
summary(model5)


model6  <-glm(prounion ~ selfemployed + Increase + Decrease + spouse + respondent + both + democrat + republican + female, data = my_dataset,
              family=binomial) 
summary (model6)

