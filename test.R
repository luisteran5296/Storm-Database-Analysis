library(dplyr)
library(ggplot2)

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile = 'repdata%2Fdata%2FStormData.csv.bz2')
db <- tbl_df(read.csv("repdata%2Fdata%2FStormData.csv.bz2"))

dfInj<-summarise(group_by(db, EVTYPE), totalInj = sum(INJURIES))
dfInj<- dfInj[order(dfInj$totalInj, decreasing = T),]

dfFat<-summarise(group_by(db, EVTYPE), totalInj = sum(FATALITIES))

ggplot(dfInj, aes(x=EVTYPE, y=totalInj))+geom_bar(stat="identity", 
                                                  fill="steelblue")

ggplot()+
  geom_bar(data=dfCrop, fill="red")+
  geom_bar(data=dfProp, fill="blue")


dfDamage <- merge(head(dfCrop,10),head(dfProp,10), by="EVTYPE", suffixes = c("Crop", "Property"))
m<-melt(dfDamage, id="EVTYPE", measure.vars= c("totalDmgCrop", "totalDmgProperty"))
        
ggplot(m, aes(x=EVTYPE, y=value, fill=variable)) + 
    geom_bar(stat="identity", position="dodge")+coord_flip()


dfInjFat <- merge(head(dfInj,10),head(dfFat,10), by="EVTYPE", suffixes = c("Injuries", "Fatalities"))
n<-melt(dfInjFat, id="EVTYPE", measure.vars= c("totalInj", "totalFat"))

ggplot(n, aes(x=EVTYPE, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge")+coord_flip()


objectdmg <- db %>%
  filter((PROPDMGEXP %in% c("h", "k", "m", "b") &
            CROPDMGEXP %in% c("h", "k", "m", "b"))) %>%
  select(c(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))
get_mult <- function(x) {
  if (x == "h") x <- 100
  if (x == "k") x <- 1000
  if (x == "m") x <- 1000000
  if (x == "b") x <- 1000000000
  x
}
objectdmg$PROPDMGEXP <- sapply(objectdmg$PROPDMGEXP, get_mult)
objectdmg$CROPDMGEXP <- sapply(objectdmg$CROPDMGEXP, get_mult)

dmg <- objectdmg %>%
  mutate(DAMAGE = PROPDMG * PROPDMGEXP + CROPDMG * CROPDMGEXP) %>%
  group_by(EVTYPE) %>%
  summarise(dmg = (sum(DAMAGE)/1000000)) %>%
  arrange(desc(dmg))
names(dmg) <- c("event", "dmg")

ggplot(dmg[1:15, ], aes(x = event, y = dmg, fill = event)) +
  geom_bar(stat = "identity") +
  scale_fill_hue(l=30) +
  coord_flip() +
  xlab("Event") +
  ylab("Damage in Million Dollars") +
  theme_minimal(base_size = 14) +
  guides(fill=FALSE) +
  ggtitle("Economic impact of weather events in the US")