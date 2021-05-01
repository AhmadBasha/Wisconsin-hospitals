library(DataExplorer)
# to use copy function
library(data.table)

my_data <- read.csv("/Users/ahmadbasha/Desktop/Internship/SL/R/Project/Projects\ for\ Submission/Healthcare/Healthcare/HospitalCosts.csv")

# make a copy of the data 
my_data2 <- copy(my_data)
#see the information about the data . Here we see having a missing value in total
introduce(my_data2)
# here to see in which column is the missing values , so in race column is the missing value 
plot_missing(my_data2)
# seeing the summary of the data and looking for something strange .
summary(my_data2)
# here i wanna see the type for each column and all are int values
str(my_data2)
# As what i have seen here is changing the FEMALE to Factor because is a Binary variabl " 1 / 0. Yes / No."
my_data2$FEMALE <- as.factor(my_data2$FEMALE)
# also change the race to factor 
my_data2$RACE <- as.factor(my_data2$RACE)
#check the result again to make sure it's work
str(my_data2)
# see the number of level to check there in no strange value 
my_data2$FEMALE
# beacuse we here has just one missing variable in our data set so it won't effect if i delete it or not 
# so , with the next line i delete the row that has missing value.
# the first line to give me how many rows and column and we have 500 rows and 6 columns then 
dim(my_data2)
# delete the NA values so it will be 499 rows and 6 column 
my_data2 <- na.omit(my_data2)
dim(my_data2)

################### Exploration ###################
#***********
#1
# *To record the patient statistics, the agency wants to find the age category of people who frequent the hospital and has the maximum expenditure.*
# using histogram to find the age category and as we can see the age from 0 to 1 are the largest number to visit the hospital 
hist(my_data2$AGE)
summary(as.factor(my_data2$AGE))
# here to divide the age and see the number of the people that come and see the hosptial 
# for this one to see each group how much have expenditure
aggregate(TOTCHG ~ AGE, sum , data = my_data2)
#assign it to var
ageMaxExpend <-aggregate(TOTCHG ~ AGE, sum , data = my_data2)
#The maximum expenditure are the people from age 0 to 1 which is 676962
max(ageMaxExpend)
ageMaxExpend[which.max(ageMaxExpend$TOTCHG),]


#***********
#2
# In order of severity of the diagnosis and treatments and to find out the expensive treatments, 
# the agency wants to find the diagnosis related group that has maximum hospitalization and expenditure.

#we can see the group with 640 has the maximum hospitalization
summary(as.factor(my_data2$APRDRG))
# here for this one to see each group how much have expenditure and assign it to var
groupMaxExpend <-aggregate(TOTCHG ~ APRDRG, sum , data = my_data2)
# here gitting the max for expanditure which is 436822 for group 640
max(groupMaxExpend)
groupMaxExpend[which.max(groupMaxExpend$TOTCHG),]

#***********
#3
# To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.
#On this one i can use ANOVA to chech the amount of hospitalization cost.
#as we can see here the number of race is 6 and most of them from race one. 
summary(as.factor(my_data2$RACE))
# do it with aov function 
raceAOV <- aov(TOTCHG ~ RACE , data = my_data2)
# we can see 484 for race one for clean data that has 499 observation(rows)
# so , there is no race of the patient to the hospitalization costs
summary(raceAOV)
# theis is the plot for all the races depend on the expanditure
plot(TOTCHG ~ RACE , data = my_data2)


#***********
#4
#To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for proper allocation of resources.
# so here we can check the cost by Multiple linear regression for age and gender
# The number of male and female are similar , the number of famile arer larger 255 while male is 244
summary(my_data2$FEMALE)
# make the linear regression and assign it to var
ageGenderLM <- lm(TOTCHG ~ AGE+FEMALE , data = my_data2)
#Here AGE has a significan effect because pValue is less than 0.05 and having three stars next to it
#as well gender in pValue is less than 0.05
# so , both of them having impact for the cost 
summary(ageGenderLM)

#***********
#5
#Since the length of stay is the crucial factor for inpatients, 
#the agency wants to find if the length of stay can be predicted from age, gender, and race.

# we can use linear regression as well on this .
stayLengthLM <- lm(LOS ~ AGE+FEMALE+RACE , data = my_data2)
# There is no relationship with this model because the pValue is larger than 0.5 on all races
# and there is no three stars next each element from the result .
# so the three variables that have been chosen can't predict the value of length of stay.
summary(stayLengthLM)


#***********
#6
#To perform a complete analysis, the agency wants to find the variable that mainly affects the hospital costs.
hospitalCost <- lm(TOTCHG ~ . , data = my_data2)
# what i can see here are AGE , LOD and APRDRG all have three stars and that means they have a relationship with the cost 
# so all three mentioned columns will effect the value of the cost 
summary(hospitalCost)







