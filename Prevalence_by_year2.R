library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)
library()

setwd("/Users/FILEPATH")
getwd() 

##Import spreadsheet
Cat_Mal_Jan24 <- read.csv("Cat_Mal_Jan24.csv")



#create dataframe from excel file
df <- Cat_Mal_Jan24
head(df)

##add in total_pos column
df$Total_Pos <- (ifelse(df$P_Positive ==1 & df$H_Positive ==1 & df$L_Positive ==1, 1, 0)
                 + ifelse(df$P_Positive ==1 & df$H_Positive ==1 & df$L_Positive ==0, 1, 0)
                 + ifelse(df$P_Positive ==1 & df$H_Positive ==0 & df$L_Positive ==1, 1, 0)
                 + ifelse(df$P_Positive ==0 & df$H_Positive ==1 & df$L_Positive ==1, 1, 0)
                 + ifelse(df$P_Positive ==1 & df$H_Positive ==0 & df$L_Positive ==0, 1, 0)
                 + ifelse(df$P_Positive ==0 & df$H_Positive ==1 & df$L_Positive ==0, 1, 0)
                 + ifelse(df$P_Positive ==0 & df$H_Positive ==0 & df$L_Positive ==1, 1, 0))

head(df)





##get those data frames
###THIS INCLUDES SPECIES, WHEN LOOKING JUST AT PATHOGEN NEED TO RESTART FROM HERE
####WITH SPECIES REMOVED
df.total <- df %>% group_by(Year, Season, Total_Pos) %>% tally()
df.total <- as.data.frame(df.total, header = TRUE)
head(df.total)

df.Plas <- df %>% group_by(Year, Season, P_Positive) %>% tally()
df.Plas <- as.data.frame(df.Plas, header = TRUE)
head(df.Plas)

df.HAEM <- df %>% group_by(Year, Season, H_Positive) %>% tally()
df.HAEM <- as.data.frame(df.HAEM, header = TRUE)
head(df.HAEM)

df.Leuco <- df %>% group_by(Year, Season, L_Positive) %>% tally()
df.Leuco <- as.data.frame(df.Leuco, header = TRUE)
head(df.Leuco)

##Replace 0/1 with neg/post
df.total[df.total$Total_Pos =="0",]$Total_Pos = "Neg"
df.total[df.total$Total_Pos =="1",]$Total_Pos = "Pos"

df.Plas[df.Plas$P_Positive =="0",]$P_Positive = "Neg"
df.Plas[df.Plas$P_Positive =="1",]$P_Positive = "Pos"

df.HAEM[df.HAEM$H_Positive =="0",]$H_Positive = "Neg"
df.HAEM[df.HAEM$H_Positive =="1",]$H_Positive = "Pos"

df.Leuco[df.Leuco$L_Positive =="0",]$L_Positive = "Neg"
df.Leuco[df.Leuco$L_Positive =="1",]$L_Positive = "Pos"


###get positive and negative into two separate columns 
df.total <- df.total %>% spread(key = Total_Pos, value = n)
head(df.total)

df.Plas <- df.Plas %>% spread(key = P_Positive, value = n)
head(df.Plas)

df.HAEM <- df.HAEM %>% spread(key = H_Positive, value = n)
head(df.HAEM)

df.Leuco <- df.Leuco %>% spread(key = L_Positive, value = n)
head(df.Leuco)

###replace NA with zero 
df.total$Pos[is.na(df.total$Pos)] <- 0
df.total$Neg[is.na(df.total$Neg)] <- 0
head(df.total)

df.Plas$Pos[is.na(df.Plas$Pos)] <- 0
df.Plas$Neg[is.na(df.Plas$Neg)] <- 0
head(df.Plas)

df.HAEM$Pos[is.na(df.HAEM$Pos)] <- 0
df.HAEM$Neg[is.na(df.HAEM$Neg)] <- 0
head(df.HAEM)

df.Leuco$Pos[is.na(df.Leuco$Pos)] <- 0
df.Leuco$Neg[is.na(df.Leuco$Neg)] <- 0
head(df.Leuco)


##total numbe screened per season of the year 
df.total$Total_Scr <- df.total$Neg + df.total$Pos
head(df.total)

df.Plas$Total_Scr <- df.Plas$Neg + df.Plas$Pos
head(df.Plas)

df.HAEM$Total_Scr <- df.HAEM$Neg + df.HAEM$Pos
head(df.HAEM)

df.Leuco$Total_Scr <- df.Leuco$Neg + df.Leuco$Pos
head(df.Leuco)


##create prevalence percentage
df.total$All_Prev <- (df.total$Pos/df.total$Total_Scr)*100
df.total$All_Prev <- format(round(df.total$All_Prev, 2), nsmall = 2)
as.numeric(df.total$All_Prev)

df.Plas$Plas_Prev <- (df.Plas$Pos/df.Plas$Total_Scr)*100
df.Plas$Plas_Prev <- format(round(df.Plas$Plas_Prev, 2), nsmall = 2)
as.numeric(df.Plas$Plas_Prev)

df.HAEM$HAEM_Prev <- (df.HAEM$Pos/df.HAEM$Total_Scr)*100
df.HAEM$HAEM_Prev <- format(round(df.HAEM$HAEM_Prev, 2), nsmall = 2)
as.numeric(df.HAEM$HAEM_Prev)

df.Leuco$Leuco_Prev <- (df.Leuco$Pos/df.Leuco$Total_Scr)*100
df.Leuco$Leuco_Prev <- format(round(df.Leuco$Leuco_Prev, 2), nsmall = 2)
as.numeric(df.Leuco$Leuco_Prev)


###join together 
Prevalence <- left_join(df.total, 
                        df.Plas[,c("Year", "Season", "Plas_Prev")],
                        by = c("Year" = "Year", "Season" = "Season"))
Prevalence <- left_join(Prevalence, 
                        df.HAEM[,c( "Year", "Season", "HAEM_Prev")],
                        by = c("Year" = "Year", "Season" = "Season"))
Prevalence <- left_join(Prevalence, 
                        df.Leuco[,c("Year", "Season", "Leuco_Prev")],
                        by = c("Year" = "Year", "Season" = "Season"))
head(Prevalence)

##1. Remove years before 1995
head(Prevalence)
Prevalence <- subset(Prevalence, Year !=1981)
Prevalence <- subset(Prevalence, Year !=1984)
Prevalence <- subset(Prevalence, Year !=1985)
Prevalence <- subset(Prevalence, Year !=1986)
Prevalence <- subset(Prevalence, Year !=1987)
Prevalence <- subset(Prevalence, Year !=1988)
Prevalence <- subset(Prevalence, Year !=1989)
Prevalence <- subset(Prevalence, Year !=1990)
Prevalence <- subset(Prevalence, Year !=1991)
Prevalence <- subset(Prevalence, Year !=1992)
Prevalence <- subset(Prevalence, Year !=1993)
Prevalence <- subset(Prevalence, Year !=1994)
Prevalence <- subset(Prevalence, Year !=1995)
head(Prevalence)







##filter to spring/fall 
###Remove species at this point <- DO NOT DO THIS
Prev_Fall <- Prevalence[Prevalence$Season == "Fall",]
Prev_Fall <- Prev_Fall[,c(1,2,6:9)]
head(Prev_Fall)

Prev_Spring <- Prevalence[Prevalence$Season == "Spring",]
Prev_Spring <- Prev_Spring[,c(1, 2,6:9)]
head(Prev_Spring)




###gather for plotting
#Fall
All_Fall <- gather(Prev_Fall, pathogen, "prevalence", 3:6 )
All_Fall$prevalence <- as.numeric(as.character(All_Fall$prevalence))
head(All_Fall)
#Spring
All_Spring <- gather(Prev_Spring, pathogen, "prevalence", 3:6 )
All_Spring$prevalence <- as.numeric(as.character(All_Spring$prevalence))
head(All_Spring)

##PLOT
Fall <- ggplot(All_Fall, aes(x = Year, y = prevalence, group = pathogen)) +
  geom_line(aes(color = pathogen)) +
  ylim(1, 100) +
  ggtitle("Fall, all pathogens")
Fall

Spring <- ggplot(All_Spring, aes(x = Year, y = prevalence, group = pathogen)) +
  geom_line(aes(color = pathogen)) +
  ylim(1, 100) +
  ggtitle("Spring, all pathogens")
Spring


All_Path_by_Year <- plot_grid(Fall, Spring, nrow = 2)
All_Path_by_Year




###Spring and fall together 

###this doesn't work -- need to fix
head(Prevalence)
Prev2 <- Prevalence[,c(1,2,6:9)]
head(Prev2)
Prev_All <- Prev2[,c(1,2,3)]
head(Prev_All)

All_Prev <- ggplot(Prev_All, aes(x = factor(Year, levels = Season)), 
                                 y = All_Prev, group = Season) +
  geom_col()
All_Prev














