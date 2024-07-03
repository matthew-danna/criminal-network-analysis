library(tidyverse)

# get data
currency <- read.csv("/Users/matthewdanna/Downloads/currency-seizures-fy21-fy24-may.csv",
                     stringsAsFactors = FALSE)
weapons <- read.csv("/Users/matthewdanna/Downloads/weapons-ammunition-seizures-fy21-fy24-may.csv",
                    stringsAsFactors = FALSE)
drugs <- read.csv("/Users/matthewdanna/Downloads/nationwide-drugs-fy21-fy24-may.csv",
                  stringsAsFactors = FALSE)

##### currency data
# 1. should we analyze inbound or outbound currency seizures? Outbound
# 2. should we analyze counts of seizures of amount or money seized? Amounts

subset.currency <- subset(currency, currency$Inbound.Outbound == 'Outbound')
subset.currency$ID <- paste(subset.currency$Fiscal.Year, subset.currency$Month..abbv.,
                            subset.currency$Area.of.Responsibility, sep = "-")
subset.currency$ID <- toupper(subset.currency$ID)

##### weapons
# 1. should we analyze inbound or outbound weapons seizures? Outbound
# 2. should we analyze counts of seizures or amount of weapons seized? Counts
# 3. we choose Land weapons seizures

subset.weapons.temp <- subset(weapons, weapons$Inbound.Outbound == 'OUTBOUND' &
                           weapons$Mode.of.Transportation == 'Land' &
                           weapons$Seizure.Type == 'Weapons')
subset.weapons <- subset.weapons.temp %>%
  group_by(Fiscal.Year, Month..abbv., Area.of.Responsibility) %>%
  summarise(Gun.Count = sum(Quantity.Seized))
subset.weapons$ID <- paste(subset.weapons$Fiscal.Year, subset.weapons$Month..abbv.,
                           subset.weapons$Area.of.Responsibility, sep = "-")
subset.weapons$ID <- toupper(subset.weapons$ID)

##### drugs
# 1. should we analyze all drugs or a specific drug type? All drugs
# 2. should we analyze seizure counts or drug weight? Weight
# 3. we choose only OFO (to match the other datas)
# 4. Land only

subset.drugs.temp <- subset(drugs, drugs$Component == 'Office of Field Operations' &
                              drugs$Land.Filter == 'Land Only')
subset.drugs <- subset.drugs.temp %>%
  group_by(FY, Month..abbv., Area.of.Responsibility) %>%
  summarise(Drug.Weight = sum(Sum.Qty..lbs.))
subset.drugs$ID <- paste(subset.drugs$FY, subset.drugs$Month..abbv.,
                         subset.drugs$Area.of.Responsibility, sep = "-")
subset.drugs$ID <- toupper(subset.drugs$ID)

##### JOINS
join1 <- subset.currency %>%
  full_join(subset.weapons, by = "ID")
join2 <- join1 %>%
  full_join(subset.drugs, by = "ID")

##### remove extra columns
events <- join2[c(9,1,2,10,11,14,15,4,5,12,16,8,13,17)]
names(events) <- c("ID", "Year1", "Month1", "Year2", "Month2", "Year3", "Month3", "Region", 
                   "Area1", "Area2", "Area3", "Cash.Amount","Gun.Count", "Drug.Weight")
events$Area2 <- toupper(events$Area2)

events <- events %>%
  mutate(Month1 = recode(Month1,
                        JAN = 01,
                        FEB = 02,
                        MAR = 03,
                        APR = 04,
                        MAY = 05,
                        JUN = 06,
                        JUL = 07,
                        AUG = 08,
                        SEP = 09,
                        OCT = 10,
                        NOV = 11,
                        DEC = 12))
events <- events %>%
  mutate(Month2 = recode(Month2,
                         JAN = 01,
                         FEB = 02,
                         MAR = 03,
                         APR = 04,
                         MAY = 05,
                         JUN = 06,
                         JUL = 07,
                         AUG = 08,
                         SEP = 09,
                         OCT = 10,
                         NOV = 11,
                         DEC = 12))
events <- events %>%
  mutate(Month3 = recode(Month3,
                         JAN = 01,
                         FEB = 02,
                         MAR = 03,
                         APR = 04,
                         MAY = 05,
                         JUN = 06,
                         JUL = 07,
                         AUG = 08,
                         SEP = 09,
                         OCT = 10,
                         NOV = 11,
                         DEC = 12))

write.csv(events, "/Users/matthewdanna/Downloads/events 2024 07 01.csv",
          row.names = FALSE)


