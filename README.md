```r
# Precipitation-and-runoff1
setwd("C:/Users/Dilanka/Dropbox/Thesis writting n papers/Publication/Graphs/Graphs/R graphs")
hydrographs <- "C:/Users/Dilanka/Dropbox/Thesis writting n papers/Publication/Graphs/Graphs/R graphs"
setwd(hydrographs)

install.packages("gdata")

require(gdata)
require(dplyr)

#test<-c(1:50)
#function
setlim<-function(x){if (x>60) {x<-60

}else x<-x}

library(readxl)

#To add a coloum called year 1
precip_y1$year="Year 1"
#make Date coloum as Date format
precip_y1[['Date']] <- as.Date.POSIXct(precip_y1[['Date']], format='%m/%d/%y')

Runoff_y1$year="Year 1"
Runoff_y1[['Date']] <- as.POSIXct(Runoff_y1[['Date']], format='%m/%d/%y')
Data_y1 <- merge(precip_y1,Runoff_y1, by = "Date", sort = TRUE, all.x = TRUE)
View(Data_y1)

Pre_y1 <- read_excel("Precipitation.xlsx", sheet = "Year 1")
Pre_y1$year="Year 1"
Pre_y1[['Date']] <- as.POSIXct(Pre_y1[['Date']], format='%m/%d/%y')
Runoff_y1 <- read_excel("Runoff.xlsx", sheet = "Year 1")
Runoff_y1[['Date']] <- as.POSIXct(Runoff_y1[['Date']], format='%m/%d/%y')
Data_y1 <- merge(Pre_y1,Runoff_y1, by = "Date", sort = TRUE, all.x = TRUE)
temp1 <-  read_excel("Stemp1.xlsx", sheet = "Year 1")
temp1[['Date']] <- as.POSIXct(temp1[['Date']], format='%m/%d/%y')

Pre_y2 <- read_excel("Precipitation.xlsx", sheet = "Year 2")
Pre_y2$year="Year 2"
Pre_y2[['Date']] <- as.POSIXct(Pre_y2[['Date']], format='%m/%d/%y')
Runoff_y2 <- read_excel("Runoff.xlsx", sheet = "Year 2")
Runoff_y2[['Date']] <- as.POSIXct(Runoff_y2[['Date']], format='%m/%d/%y')
Data_y2 <- merge(Pre_y2,Runoff_y2, by = "Date", sort = TRUE, all.x = TRUE)



Pre_y3 <- read_excel("Precipitation.xlsx", sheet = "Year 3")
Pre_y3$year="Year 3"
Pre_y3[['Date']] <- as.POSIXct(Pre_y3[['Date']], format='%m/%d/%y')
Runoff_y3 <- read_excel("Runoff.xlsx", sheet = "Year 3")
Runoff_y3[['Date']] <- as.POSIXct(Runoff_y3[['Date']], format='%m/%d/%y')
Data_y3 <- merge(Pre_y3,Runoff_y3, by = "Date", sort = TRUE, all.x = TRUE)



Pre_y4 <- read_excel("Precipitation.xlsx", sheet = "Year 4")
Pre_y4$year="Year 4"

Pre_y4[['Date']] <- as.POSIXct(Pre_y4[['Date']], format='%m/%d/%y')
Runoff_y4 <- read_excel("Runoff.xlsx", sheet = "Year 4")
Runoff_y4[['Date']] <- as.POSIXct(Runoff_y4[['Date']], format='%m/%d/%y')
Data_y4 <- merge(Pre_y4,Runoff_y4, by = "Date", sort = TRUE, all.x = TRUE)

Data_y4[is.na(Data_y4)] = 0


All <- rbind(Data_y1,Data_y2,Data_y3,Data_y4)
View(All)
write.csv(All, file = "All_Data.csv")

rm(list = ls())

library(tidyverse)
library(dplyr)

#Create all runoff, rain, snow and total together in one data set. Important in the future  
final_data <- All %>%
  select(Date, Rain, Snow, Total, year, Runoff) %>%
  gather(key = "variable", value = "value", Rain, Snow)

final_data[is.na(final_data)] = 0
final_data[is.na(final_data)] = 0

#Replace NA with 0
final_data[is.na(final_data)] = 0

#read 'Date' in date format
final_data[['Date']] <- as.POSIXct(final_data[['Date']], format='%m/%d/%y')

final_data[['Date']] <- as.POSIXct(final_data[['Date']], format='%m/%d/%y')
final_data$runoff_filtered<-unlist(lapply(final_data$Runoff,setlim))
final_data[is.na(final_data)] = 0  
view(final_data)

install.packages("xlsx")
library(xlsx)
write.xlsx(final_data, file = "final_Data.xlsx")
library(ggplot2)

color <- c("#56B4E9", "#F0E442")
plotsyear1 <- ggplot(final_data, aes(x= as.Date(Date), y=value, fill = variable)) + 
  geom_bar(data=subset(final_data,year=="Year 1"), stat = "identity" )+
  xlab("")+
  # set date breaks
  scale_x_date(date_breaks = "2 month", date_labels = "%b %d")+
  scale_fill_manual(values = c("skyblue", "pink"))+
  
  #add second Y axis
  scale_y_continuous(limits = c(0,35),
    
    # Features of the first axis
    name = "Precip(mm)",
    
    # Add a second axis and specify its features
    
  ) + theme_bw()+
  theme(legend.position = c(0.96,0.7), legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), 
        legend.background = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.spacing = unit(2, "lines") )

plotsyear2 <- ggplot(final_data, aes(x= as.Date(Date), y=value, fill = variable)) + 
  geom_bar(data=subset(final_data,year=="Year 2"), stat = "identity" )+
  xlab("")+
  # set date breaks
  scale_x_date(date_breaks = "2 month", date_labels = "%b %d")+
  scale_fill_manual(values = c("skyblue", "pink"))+
  
  #add second Y axis
  scale_y_continuous(limits = c(0,35),
    
    # Features of the first axis
    name = "Precip(mm)",
    
    # Add a second axis and specify its features
    
  ) + theme_bw()+
theme(legend.position = c(0.96,0.7), legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), 
      legend.background = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

plotsyear3 <- ggplot(final_data, aes(x= as.Date(Date), y=value, fill = variable)) + 
  geom_bar(data=subset(final_data,year=="Year 3"), stat = "identity" )+
  xlab("")+
  # set date breaks
  scale_x_date(date_breaks = "2 month", date_labels = "%b %d")+
  scale_fill_manual(values = c("skyblue", "pink"))+
  
  #add second Y axis
  scale_y_continuous(limits = c(0,35),
    
    # Features of the first axis
    name = "Precip(mm)",
    
    # Add a second axis and specify its features
    
  ) + theme_bw()+
  theme(legend.position = c(0.96,0.7), legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), 
        legend.background = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

plotsyear4 <- ggplot(final_data, aes(x= as.Date(Date), y=value, fill = variable)) + 
  geom_bar(data=subset(final_data,year=="Year 4"), stat = "identity" )+
  xlab("")+
  # set date breaks
  scale_x_date(date_breaks = "2 month", date_labels = "%b %d")+
  scale_fill_manual(values = c("skyblue", "pink"))+
  
  #add second Y axis
  scale_y_continuous(limits = c(0,35),
    
    # Features of the first axis
    name = "Precip(mm)", 
    
    # Add a second axis and specify its features
    
  ) + theme_bw()+
  theme(legend.position = c(0.96,0.7), legend.title = element_blank(), legend.key.size = unit(0.3, "cm"), 
        legend.background = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()  )

#Plot runoff graphs
#This is not working
plotrunoff1 <- ggplot(data = final_data, aes(x = as.Date(Date), y = Runoff))+
  geom_line(data=subset(final_data,year=="Year 1"), color = "#00AFBB", size = 1)+ 
              geom_point(data=subset(final_data,year=="Year 1"),color = "#00AFBB", size = 0.8) + 
  scale_y_continuous(name = expression("Runoff (mm)"),sec.axis = sec_axis(~.*0.5, name = "Soil temperature °C"), limits = c(0,30))+  
  geom_line(data = temp1, aes(x = as.Date(Date), y = temp))+
  theme_bw() + scale_x_date(date_breaks = "2 month", date_labels = "%b %d")+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

plotrunoff2 <- ggplot(data = final_data, aes(x = as.Date(Date), y = Runoff))+
  geom_line(data=subset(final_data,year=="Year 2"), color = "#00AFBB", size = 1)+ 
  geom_point(data=subset(final_data,year=="Year 2"),color = "#00AFBB", size = 0.8) + 
  labs(y="Runoff(mm)", x= "" , face = "bold") +  
  theme_bw() + ylim(0,50) + scale_x_date(date_breaks = "2 month", date_labels = "%b %d")+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

plotrunoff3 <- ggplot(data = final_data, aes(x = as.Date(Date), y = Runoff))+
  geom_line(data=subset(final_data,year=="Year 3"), color = "#00AFBB", size = 1)+ 
  geom_point(data=subset(final_data,year=="Year 3"),color = "#00AFBB", size = 0.8) + 
  labs(y="Runoff(mm)", x= "" , face = "bold") +  
  theme_bw() + ylim(0,50) + scale_x_date(date_breaks = "2 month", date_labels = "%b %d")+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

plotrunoff4 <- ggplot(data = final_data, aes(x = as.Date(Date), y = Runoff))+
  geom_line(data=subset(final_data,year=="Year 4"), color = "#00AFBB", size = 1)+ 
  geom_point(data=subset(final_data,year=="Year 4"),color = "#00AFBB", size = 0.8) + 
  labs(y="Runoff(mm)", x= "" , face = "bold") +  
  theme_bw() + ylim(0,50) + scale_x_date(date_breaks = "2 month", date_labels = "%b %d")

install.packages("ggpubr")
library(ggpubr)

plotfinal<- ggarrange(plotsyear1,plotrunoff1, plotsyear2,plotrunoff2,plotsyear3,plotrunoff3,plotsyear4,plotrunoff4, 
                      heights = c(1,2,1,2,1,2,1,2.5), ncol = 1, nrow = 8)
annotate_figure(plotfinal,
                top = text_grob("", color = "black", face = "bold", size = 14),
                bottom = text_grob("Date", color = "black",
                                   size = 14, vjust = -1),
                left = text_grob("", face = "bold", color = "Black", rot = 90),
                right = "",
                fig.lab = "", fig.lab.face = "bold",
) 

```

