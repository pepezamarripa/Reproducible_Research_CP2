## Pepe Zamarripa
## Repoducible research - Course project 2

install.packages("R.utils")
install.packages("gridExtra")

library(plyr)
library(ggplot2)
library(gridExtra)


#Read the file
stormDB <- data.table::fread(input = "repdata_data_StormData.csv.bz2")
summary(stormDB)

# Impacts of the fatal events
fatality <- aggregate(FATALITIES ~ EVTYPE, data = stormDB, FUN = "sum")
fatality <- arrange(fatality, desc(fatality[, 2]))
top10fatality <- fatality[1:10,]
head(top10fatality)

# Create the fatal events
fatalityPlot <- ggplot(top10fatality, aes(x = reorder(EVTYPE, -FATALITIES), y = FATALITIES)) +
  geom_bar(stat = "identity") +
  xlab("Weather Event Type") +
  ylab("Number of Fatalities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Top 10 Fatalities')


# Impacts of injury events
injury <- aggregate(INJURIES ~ EVTYPE, data = stormDB, FUN=sum)
injury <- arrange(injury, desc(injury[, 2]))
top10injury <- injury[1:10,]
head(top10injury)

# Create the injury events chart
injuryPlot <- ggplot(top10injury, aes(x = reorder(EVTYPE, -INJURIES), y = INJURIES)) +
  geom_bar(stat = "identity") +
  xlab("Weather Event Type") +
  ylab("Number of Injuries") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Top 10 Injuries')

# Plot the fatal and injury charts
grid.arrange(fatalityPlot, injuryPlot, nrow = 1)

# Now, we will analyze the impact of the weather events on property and crop.

# Obtain the property damage
propertyDamageData <- aggregate(PROPDMG ~ EVTYPE, data = stormDB, FUN=sum)
propertyDamageData <- arrange(propertyDamageData, desc(propertyDamageData[, 2]))
top10propertyDamageData <- propertyDamageData[1:10,]
head(top10propertyDamageData)

# Obtain the crop damage
cropDamageData <- aggregate(CROPDMG ~ EVTYPE, data = stormDB, FUN=sum)
cropDamageData <- arrange(cropDamageData, desc(cropDamageData[, 2]))
top10cropDamageData <- cropDamageData[1:10,]
head(top10cropDamageData)

# Plot both charts
propertyPlot <- ggplot(top10propertyDamageData, aes(x = reorder(EVTYPE, -PROPDMG), y = PROPDMG)) +
  geom_bar(stat = "identity") +
  xlab("Weather Event Type") +
  ylab("Property Damage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Top 10 Property Damage')

cropDamagePlot <- ggplot(top10cropDamageData, aes(x = reorder(EVTYPE, -CROPDMG), y = CROPDMG)) +
  geom_bar(stat = "identity") +
  xlab("Weather Event Type") +
  ylab("Property Damage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Top 10 Crop Damage')
grid.arrange(propertyPlot, cropDamagePlot, nrow = 1)