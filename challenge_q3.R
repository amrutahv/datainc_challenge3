#### loading libraries ##########
library(ggplot2)
library(tidyverse)
library(plyr)
library(data.table)
##### setting options for correct plotting: avoid scientific annotation
options(scipen = 10000)
##### reading in data #########
DT <- fread("cites_all.csv", na.strings = c(""," ","NA"))
DT[,levels(as.factor(Term))]
#### need to condense some product types
item.type <- as.factor(DT$Term)
length(levels(item.type))
levels(item.type)[c(1,4,5,6,7,46,47,48,50,80,84,91,96)] <- "bone"
levels(item.type)[c(2,49,58,64,81,82,83,89,90)] <- "wood"
levels(item.type)[c(3,5,15,16,20,23,26,29,30,32,36,49,62,66,73,76,77,78)] <- "parts"
levels(item.type)[c(8,16,25,26,27,28,29,30,31,47)] <- "keratin"
levels(item.type)[c(14,30)] <- "live"
levels(item.type)[c(13,18,19,20,23,29,39,51)] <- "plant"
levels(item.type)[c(21,22,23,24,40,41,42)] <- "skin"
levels(item.type)[c(4,5,6,7,9,10,11,12,15,16,18,19,20,22,23,24,25,26,27,28,29,31,32,34,35,36,37,38,39,41,42)] <- "other"
levels(item.type)[8] <- "live"
levels(item.type)[10] <- "plant"
levels(item.type)[9] <- "parts"
levels(item.type)[9] <- "wood"
levels(item.type)
### datatable with reclassified type of product
DT <- data.table(cbind(DT,item.type))
setkeyv(DT, c("Year","item.type","Purpose","Class"))

###### How many items of different types are traded per year?#####
DT[,mean(count(Year)$freq), by = item.type]%>%
  ggplot(aes(x = item.type, y = V1)) + 
  geom_bar(stat = "identity")

#### Excluding zoo animals, breeding programmes and reintroduction
product_type <- DT[!is.na(Purpose) & (Purpose != c("B","Z","N")),mean(count(Year)$freq), by = item.type]
product_type$item.type <- reorder(product_type$item.type,-product_type$V1)
plot1 <- ggplot(product_type,aes(x = item.type, y = V1, fill = V1))+
  geom_bar(stat = "identity")+
  scale_fill_continuous(name = "Year average", low = "cadetblue", high = "coral")+
  theme_bw(base_size = 12)+
  labs(title = "Types of wildlife products traded (excluding zoos and conservation programs",
       x = "Product type",
       y = "Average entries in CITES per year")+
  theme(axis.text.x = element_text(size = 12))
plot1
###most live animals are traded for commercial purposes
#### what is taxa-wise distribution of commercially traded live animals #####
class_live <- DT[(item.type == "live") & (Purpose == "T") & (!is.na(Class)), count(Class)]
class_live <- class_live[class_live$freq >1000,]
class_live$x <- reorder(class_live$x, -class_live$freq)  
plot2 <- ggplot(class_live,aes(x = x, y = freq, fill = freq)) +
  geom_bar(stat = "identity")+
  scale_fill_continuous(name = "#CITES entries", low = "sandybrown", high = "plum")+
  theme_bw(base_size = 12)+
  labs(title = "Taxonomy-wise comparison of 'live' commercially traded CITES entries from 1975-2016",
       x = "Class",
       y = "Number of CITES entries")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
plot2