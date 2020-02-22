library(lubridate)
library(tidyr)
library(dplyr)
library(readr)

# List all the files in the directory
groupFiles <- list.files("C:/Users/dkalapati/Documents/GitHub/OpenSourceProgramming/Group Assignment")

groupFiles

groupFilesSAS <- list.files("C:/Users/dkalapati/Documents/GitHub/OpenSourceProgramming/Group Assignment",
                            pattern = "sas7bdat",full.names=TRUE)
groupFilesSAS

if(!require("haven")) install.packages("haven"); library("haven")

# Load all required data files into a list
data_files <- lapply(groupFilesSAS,read_sas)



# Extract these list elements to data frames
# name the data frames accordingly
# Get only the names of our datasets

# first: get only the last part of the path
groupFilesSAS_limit <- list.files("C:/Users/dkalapati/Documents/GitHub/OpenSourceProgramming/Group Assignment",
                                  pattern = "sas7bdat")
groupFilesSAS_limit

# second: delete the part .sasbdat
groupFilesSAS_limit <- gsub("\\..*","",list.files("C:/Users/dkalapati/Documents/GitHub/OpenSourceProgramming/Group Assignment",
                                                  pattern = "sas7bdat"))
groupFilesSAS_limit

# Extract the data frames with a for loop and the assign function
for(i in 1:length(data_files)){
  assign(groupFilesSAS_limit[i], data_files[[i]])
}

# delete the list
rm(data_files)


#Changing to Date type
RawDataIDemographics$RegDate<- ymd(RawDataIDemographics$RegDate)
RawDataIDemographics$FirstPay <- ymd(RawDataIDemographics$FirstPay)

#Selecting Required Columns
demographic <-  RawDataIDemographics%>%
  select(UserID,Country,Language,Gender,RegDate,FirstPay,ApplicationID)

AnalyticData <- AnalyticDataInternetGambling  %>%
  select(USERID,AGE) %>%
  rename(UserID = USERID, Age = AGE)

#reading csv 
Language <- read_csv("Language.csv")
Application <- read_csv("Application.csv")
Country <- read_csv("Country.csv")
Product <- read_csv("Product.csv")

#merging the above data
demographic <- merge(demographic,Country,by="Country")
demographic <- merge(demographic,Language,by="Language")
demographic <- merge(demographic,Application,by="ApplicationID")
demographic <- merge(x=demographic,y=AnalyticData,by="UserID",all.x = TRUE)

#Renaming and modifying columns
demographic <- demographic %>%
select(-c("ApplicationID","Language","Country")) %>%
  mutate(Gender = ifelse(Gender == 1,"Male","Female")) %>%
  rename(Application=`Application Description`,Country=`Country Name` ,Language=`Language Description`)

#changing to date type
RawDataIIUserDailyAggregation$Date <- ymd(RawDataIIUserDailyAggregation$Date)

#Creating a summary for user by product for analysis purpose
UserDailyAggregration <- as_tibble(RawDataIIUserDailyAggregation)

UserDailyAggregration <- merge(UserDailyAggregration,Product,by="ProductID")

UserDailyAggregration <- UserDailyAggregration %>%
  select(-ProductID) %>%
  rename(Product=`Product Description`)

UserAgg <- UserDailyAggregration %>% 
  group_by(UserID,Product) %>% 
  summarise(TotalStakes = sum(Stakes),
         TotalWinnings = sum(Winnings),
         TotalBets = sum(Bets), 
         FirstActivedate = min(Date), 
         LastActivedate = max(Date), 
         TotalActivedays = n_distinct(Date))


#Creating poker Summary for user.
Poker <- RawDataIIIPokerChipConversions[RawDataIIIPokerChipConversions$TransDateTime < as.Date('2005-09-30'),]

Poker <- Poker %>% 
  group_by(UserID) %>% 
  summarise(POTotalbuy = round(sum(TransAmount[TransType == 124]),2),
            POTotalsell = round(sum(TransAmount[TransType == 24]),2),
            PODaysSinceActive = round(as.numeric(difftime(as.Date('2005-09-30'),max(TransDateTime)))),
            POLastActivebuy = min(TransDateTime[TransType == 124]), 
            POLastActivesell = max(TransDateTime[TransType == 24]), 
            POTotalActivedays = n_distinct(TransDateTime))

#Merging with all the tables by UserID
Main <- merge(x=demographic,y=UserAgg,by="UserID",all.x = TRUE)
Main <- merge(x=Main,y=Poker,by="UserID",all.x = TRUE)

# the main can be used for visualization
write.csv(MainTable,"C:\\Users\\dkalapati\\Documents\\GitHub\\OpenSourceProgramming\\Group Assignment\\MainTable.csv", row.names = FALSE)



#Marketing Data mart

MarketingDatamart <- UserDailyAggregration %>% 
  group_by(UserID) %>% 
  summarise(FavProduct = first(Product),
            TotalStakes = sum(Stakes),
            TotalWinnings = sum(Winnings),
            FavProductBets = max(Bets),
            TotalBets = sum(Bets), 
            FirstActivedate = min(Date), 
            LastActivedate = max(Date), 
            DaysSinceActive = as.numeric(difftime(as.Date('2005-09-30'),LastActivedate),units = 'days'),
            TotalActivedays = n_distinct(Date)) %>%
            mutate(GGR = TotalStakes - TotalWinnings,ARPU = TotalStakes/TotalActivedays, DepositPerBet = TotalStakes/TotalBets, 
                   ChurnRate = (DaysSinceActive/241)*100, 
                   LoyaltyCategory = ifelse(ChurnRate < 20 & TotalActivedays > 110,'VIP',ifelse(ChurnRate < 65 & TotalActivedays > 50,'Elite','Normal')))


#GGR - Gross gaming revenue(a true measure of the economic value of gambling)
#ARPU - Average revenue per user (it is a measure of the revenue generated by one player, per unit time)
#DepositPerBet - Average deposit per bet
#ChurnRate - The chance in % of the customer to churn
#LoyaltyCategory - 3 categories based on the churn rate and total active days

#Other variables defined in the data mart manual

#Merging with poker table
MarketingDatamart <- merge(x=MarketingDatamart,y=Poker,by="UserID",all.x = TRUE)


#Merging with demographic table
MarketingDatamart <- merge(x=demographic,y=MarketingDatamart,by="UserID",all.x = TRUE)

#Creating new column with 0 and 1 to show wether user is active in poker
MarketingDatamart$PokerActivity <- ifelse(is.na(MarketingDatamart$POTotalbuy) == TRUE,0,1)

#Giving a churn rate of 100 to people who don't have betting activity
MarketingDatamart$ChurnRate[is.na(MarketingDatamart$TotalActiveday) == TRUE] <- 100

#Save the table as csv
write.csv(MarketingDatamart,"C:\\Users\\dkalapati\\Documents\\GitHub\\OpenSourceProgramming\\Group Assignment\\MarketingDatamart.csv", row.names = FALSE)









