library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(forcats)
library(reshape2)

acq1<-read_csv("Acquisition1.csv")
acq2<-read_csv("Acquisition2.csv")
acq3<-read_csv("Acquisition3.csv")
acq4<-read_csv("Acquisition4.csv")


my_number_parser <- function(str_vec){
  library(readr)
  if(all(str_detect(string = str_vec, pattern = "^\\$.+"))){
    return(parse_number(str_vec))
  } else if(all(str_detect(string = str_vec,pattern = ".+%$"))){
    return(parse_number(str_vec)/100)
  } else {
    return(str_vec)
  }
}


acq1<- acq1%>% mutate_all(my_number_parser)
acq2<- acq2%>% mutate_all(my_number_parser)
acq3<- acq3%>% mutate_all(my_number_parser)
acq4<- acq4%>% mutate_all(my_number_parser)

#goal 1 completions
acq1%>%
  ggplot(aes(x = fct_reorder(`Default Channel Grouping`, `Bike Registrations - Individual with Payment (Goal 1 Completions)`), y = `Bike Registrations - Individual with Payment (Goal 1 Completions)`, fill =`Default Channel Grouping` ))+
  geom_col()+
  scale_fill_brewer(palette="Set3")

#conversion rate
acq1%>%
  ggplot(aes(x = fct_reorder(`Default Channel Grouping`, `Bike Registrations - Individual with Payment (Goal 1 Conversion Rate)`), y = `Bike Registrations - Individual with Payment (Goal 1 Conversion Rate)`, fill =`Default Channel Grouping` ))+
  geom_col()+
  scale_fill_brewer(palette="Set3")

#bounce rate
acq1%>%
  ggplot(aes(x = fct_reorder(`Default Channel Grouping`, `Bounce Rate`), y = `Bounce Rate`, fill =`Default Channel Grouping` ))+
  geom_col()+
  scale_fill_brewer(palette="Set3")

# compare four goals
funnel <- cbind(acq1$`Bike Registrations - Individual with Payment (Goal 1 Completions)`, acq2$`Bike Registrations - Start New Team Member (Goal 2 Completions)`, acq3$`Bike Registration - Join a Team (Goal 3 Completions)`, acq4$`Bike Registration - Join Team from Team Page (Goal 4 Completions)`)
goals = c("1. Individual", "2. team member", "3. join team", "4. Join Team from Team Page")
channels = c("Direct", "Organic Search", "Social", "(Other)", "Referral", "Display" , "Paid Search","Email")

colnames(funnel) <- goals
rownames(funnel) <- channels

funnel<-as.data.frame(funnel)








