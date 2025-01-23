## check working directory
getwd()

## set working directory
setwd("C:/Users/kanik/Desktop/Data Mgmt/")

##installing required packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidyr")



## loading libraries
library(dplyr) #for data manipulation
library(readxl)#for reading in excel files
library(tidyverse)
library(tidyr)

##reading in all tables
Customer <- read_xlsx("Data 1_Customer.xlsx")
Motor_Policies <- read_xlsx("Data 2_Motor Policies.xlsx")
Travel_Policies <- read_xlsx("Data 4_Travel Policies.xlsx")
Health_Policies <- read_xlsx("Data 3_Health Policies.xlsx")

## joining tables using join function
## Performing JOIN with Customer and Motor_policies data using MotorID

Customer_MotorPolicies <- left_join(Customer,Motor_Policies, by="MotorID")

## Performing JOIN with Customer and Travel_Policies data using TravelID
Customer_TravelPolicies <- left_join(Customer, Travel_Policies,by = "TravelID")

## Performing LEFT JOIN with Customer and Health_Policies data using HealthID
Customer_HealthPolicies <- left_join(Customer,Health_Policies, by = "HealthID")

## Performing FULL OUTER JOIN with Customer_MotorPolicies and Customer_Travelpolicies 
##data using CustomerID
Customer_MotorTravel <- full_join(Customer_MotorPolicies, Customer_TravelPolicies,by = "CustomerID")


## Performing FULL OUTER JOIN with Customer_MotorTravel and Customer_HealthPolicies 
##data using CustomerID
Customer_InsurancePolicies <- full_join(Customer_MotorTravel,Customer_HealthPolicies, by="CustomerID")

## Forming an ABT
## Saving only useful columns in the ABT

select(- Customer_InsurancePolicies$Title.x,-Customer_InsurancePolicies$GivenName.x,
        -Customer_InsurancePolicies$MiddleInitial.x,-Customer_InsurancePolicies$Surname.x,
       -Customer_InsurancePolicies$CardType.x,-Customer_InsurancePolicies$Occupation.x, 
       -Customer_InsurancePolicies$Gender.x, -Customer_InsurancePolicies$Age.x, -Customer_InsurancePolicies$Location.x, 
       -Customer_InsurancePolicies$ComChannel.x, -Customer_InsurancePolicies$MotorID.x, -Customer_InsurancePolicies$HealthID.x, 
       -Customer_InsurancePolicies$TravelID.x,-Customer_InsurancePolicies$Title.y,Customer_InsurancePolicies$GivenName.y,
       -Customer_InsurancePolicies$MiddleInitial.y,-Customer_InsurancePolicies$Surname.y, -Customer_InsurancePolicies$CardType.y,
      -Customer_InsurancePolicies$Occupation.y, -Customer_InsurancePolicies$Gender.y, -Customer_InsurancePolicies$Age.y,
      -Customer_InsurancePolicies$Location.y, -Customer_InsurancePolicies$ComChannel.y, -Customer_InsurancePolicies$MotorID.y,
      -Customer_InsurancePolicies$HealthID.y, -Customer_InsurancePolicies$TravelID.y)

## subsetting columns
ABT_Customer_InsurancePolicies <- Customer_InsurancePolicies[,-c(2:13)]
ABT_Customer_InsurancePolicies <- Customer_InsurancePolicies[,-c(2:13,14,25:37)]

## Rearraning the columns

ABT_Customer_InsurancePolicies <- select(Customer_InsurancePolicies, CustomerID, Title, GivenName, 
              MiddleInitial, Surname, CardType, Occupation, Gender, Age, Location, ComChannel, MotorID,
              MotorType, PolicyStart, PolicyEnd.x, veh_value, Exposure, clm, Numclaims, v_body, v_age,
              LastClaimDate, TravelID, policyStart.x, PolicyEnd.y, TravelType, HealthID, policyStart.y,
              policyEnd, HealthType, HealthDependentsAdults, DependentsKids)


## Renaming column names
ABT_Customer_InsurancePolicies <- ABT_Customer_InsurancePolicies %>%
rename("customer_motor_policyend" = PolicyEnd.x)%>%
  rename("customer_motor_policystart" = PolicyStart)%>%
  rename("customer_travel_policystart" = policyStart.x)%>%
  rename("customer_travel_policyend" = PolicyEnd.y)%>%
rename("customer_health_policystart" = policyStart.y)%>%
  rename("customer_health_policyend" = policyEnd)



View(ABT_Customer_InsurancePolicies)

## Data quality analysis of the abt



## summarising the data

summary(ABT_Customer_InsurancePolicies)

## Checking for null values in table

ABT_Customer_InsurancePolicies %>%
  select(everything()) %>%
  summarise_all(list(~sum(is.na(.))))

## Found errors in Age : Max is 210
## Vehicle Value; Max : 16
## DependentsKids ; Max : 40
## Num_Claims : Max : weird value
## Gender - f or female and m for male
## 0 in CardType
## Deal with NAs




View(Customer_InsurancePolicies)

summary(Customer_InsurancePolicies)

#rename_with(.fn = snakecase::to_snake_case)

boxplot(ABT_Customer_InsurancePolicies$Age)

## Checking for negative values in Age attribute
ABT_Customer_InsurancePolicies %>% 
  filter(Age <= 0)%>%
  View()

 
##identifying outlier in Age and removing outliers


  
outliers <- boxplot(ABT_Customer_InsurancePolicies$Age)$out

outliers

##convert these values into NA so that R treats them appropriately
ABT_Customer_InsurancePolicies <-ABT_Customer_InsurancePolicies%>% 
  mutate(Age = na_if(Age, y= -44),
         Age = na_if(Age, y = 180), Age = na_if(Age, y = 210))

boxplot(ABT_Customer_InsurancePolicies$Age)

summary(ABT_Customer_InsurancePolicies$Age)

## Replacing NAs with Mean

mean(ABT_Customer_InsurancePolicies$Age)

ABT_Customer_InsurancePolicies$Age[is.na(ABT_Customer_InsurancePolicies$Age)]<- 
  mean(ABT_Customer_InsurancePolicies$Age,na.rm = TRUE)

summary(ABT_Customer_InsurancePolicies$Age)

## Data cleaning for categorical variables

summary(ABT_Customer_InsurancePolicies$Gender)
ABT_Customer_InsurancePolicies$Gender <- as.factor(ABT_Customer_InsurancePolicies$Gender)
summary(ABT_Customer_InsurancePolicies$Gender)
plot(ABT_Customer_InsurancePolicies$Gender)
## Changing values to accurate values
## m to male and f to female in gender attribute

ABT_Customer_InsurancePolicies$Gender[ABT_Customer_InsurancePolicies$Gender == "f"] <- "female"
ABT_Customer_InsurancePolicies$Gender[ABT_Customer_InsurancePolicies$Gender == "m"] <- "male"
summary(ABT_Customer_InsurancePolicies$Gender)

ABT_Customer_InsurancePolicies$Gender<- droplevels(ABT_Customer_InsurancePolicies$Gender)
summary(ABT_Customer_InsurancePolicies$Gender)

## s to SMS , E to Email and P to Phone in ComChannel attribute
summary(ABT_Customer_InsurancePolicies$ComChannel)
ABT_Customer_InsurancePolicies$ComChannel <- as.factor(ABT_Customer_InsurancePolicies$ComChannel)
summary(ABT_Customer_InsurancePolicies$ComChannel)

ABT_Customer_InsurancePolicies$ComChannel[ABT_Customer_InsurancePolicies$ComChannel == "S"] <- "SMS"
ABT_Customer_InsurancePolicies$ComChannel[ABT_Customer_InsurancePolicies$ComChannel == "E"] <- "Email"
ABT_Customer_InsurancePolicies$ComChannel[ABT_Customer_InsurancePolicies$ComChannel == "P"] <- "Phone"
summary(ABT_Customer_InsurancePolicies$ComChannel)

ABT_Customer_InsurancePolicies$Gender<- droplevels(ABT_Customer_InsurancePolicies$ComChannel)
summary(ABT_Customer_InsurancePolicies$ComChannel)

## removing data quality issues with DependentKids

plot(ABT_Customer_InsurancePolicies$DependentsKids)
boxplot(ABT_Customer_InsurancePolicies$DependentsKids)
hist(ABT_Customer_InsurancePolicies$DependentsKids)

#Customer_InsurancePolicies$veh_value <- replace_na(Customer_InsurancePolicies$veh_value,0)

ABT_Customer_InsurancePolicies$DependentsKids[ABT_Customer_InsurancePolicies$DependentsKids >20] <- NA

boxplot(ABT_Customer_InsurancePolicies$DependentsKids)

summary(ABT_Customer_InsurancePolicies$DependentsKids)

#removing na values from Dependentkids
# replaced with 0 to be on a safer side 
Customer_InsurancePolicies$DependentsKids <- replace_na(Customer_InsurancePolicies$DependentsKids,0)
summary(Customer_InsurancePolicies$DependentsKids)
Customer_InsurancePolicies %>%
  select(everything()) %>%
  summarise_all(list(~sum(is.na(.))))%>%
  View()

summary(Customer_InsurancePolicies)


## Addressing 0 in CardType

summary(ABT_Customer_InsurancePolicies$CardType)
pie(ABT_Customer_InsurancePolicies$CardType)

ABT_Customer_InsurancePolicies$CardType

ABT_Customer_InsurancePolicies %>%
  mutate(CardType = replace(CardType,CardType == '0', 'None'))
 
ABT_Customer_InsurancePolicies$CardType[ABT_Customer_InsurancePolicies$CardType == "0"] <- "None"
Customer_InsurancePolicies$CardType


## Addressing null values

Null_Values <- ABT_Customer_InsurancePolicies %>%
  select(everything()) %>%
  summarise_all(list(~sum(is.na(.)))) 
  View()



