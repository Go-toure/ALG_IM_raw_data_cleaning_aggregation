setwd("C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_raw/")

library(tidyverse)
library(dplyr)
library(sf)
library(flextable)
library(stringr)
library(stringi)
library(lubridate)
library(readxl)
library(ggplot2)

DF <- read_csv("ALG_SIA.csv")
DF <- DF[!(is.na(DF$TotalFM)), ]
DF |>
  filter(Type_Monitoring == "EndProcess", `Response`!="n/a",`Response`!="NA" ,`roundNumber`!="n/a", Total_U5_Present!="n/a",`HH[1]/group1/Tot_child_Absent_HH`!="2", `HH[1]/group1/Tot_child_Absent_HH`!="3", `HH[2]/group1/Tot_child_Absent_HH`!="2",`HH[2]/group1/Tot_child_Absent_HH`!="3", `HH[3]/group1/Tot_child_Absent_HH`!="2", `HH[3]/group1/Tot_child_Absent_HH`!="3", `HH[4]/group1/Tot_child_Absent_HH`!="2", `HH[4]/group1/Tot_child_Absent_HH`!="3", `HH[5]/group1/Tot_child_Absent_HH`!="2", `HH[5]/group1/Tot_child_Absent_HH`!="3", `HH[6]/group1/Tot_child_Absent_HH`!="2" , `HH[6]/group1/Tot_child_Absent_HH`!="3" , `HH[7]/group1/Tot_child_Absent_HH`!="2",`HH[7]/group1/Tot_child_Absent_HH`!="3",, `HH[8]/group1/Tot_child_Absent_HH`!="2", `HH[8]/group1/Tot_child_Absent_HH`!="3", `HH[9]/group1/Tot_child_Absent_HH`!="2", `HH[9]/group1/Tot_child_Absent_HH`!="3", `HH[10]/group1/Tot_child_Absent_HH`!="2", `HH[10]/group1/Tot_child_Absent_HH`!="3" , `HH[1]/group1/Tot_child_NC_HH`!="2", `HH[1]/group1/Tot_child_NC_HH`!="3", `HH[2]/group1/Tot_child_NC_HH`!="2",`HH[2]/group1/Tot_child_NC_HH`!="3", `HH[3]/group1/Tot_child_NC_HH`!="2", `HH[3]/group1/Tot_child_NC_HH`!="3", `HH[4]/group1/Tot_child_NC_HH`!="2", `HH[4]/group1/Tot_child_NC_HH`!="3", `HH[5]/group1/Tot_child_NC_HH`!="2", `HH[5]/group1/Tot_child_NC_HH`!="3", `HH[6]/group1/Tot_child_NC_HH`!="2" , `HH[6]/group1/Tot_child_NC_HH`!="3" , `HH[7]/group1/Tot_child_NC_HH`!="2",`HH[7]/group1/Tot_child_NC_HH`!="3", `HH[8]/group1/Tot_child_NC_HH`!="2", `HH[8]/group1/Tot_child_NC_HH`!="3", `HH[9]/group1/Tot_child_NC_HH`!="2", `HH[9]/group1/Tot_child_NC_HH`!="3", `HH[10]/group1/Tot_child_NC_HH`!="2", `HH[10]/group1/Tot_child_NC_HH`!="3" ) 
DF<- DF |> 
  filter(`HH[1]/group1/Tot_child_Noparent_HH`!="2", `HH[1]/group1/Tot_child_Noparent_HH`!="3", `HH[2]/group1/Tot_child_Noparent_HH`!="2",`HH[2]/group1/Tot_child_Noparent_HH`!="3", `HH[3]/group1/Tot_child_Noparent_HH`!="2", `HH[3]/group1/Tot_child_Noparent_HH`!="3", `HH[4]/group1/Tot_child_Noparent_HH`!="2", `HH[4]/group1/Tot_child_Noparent_HH`!="3", `HH[5]/group1/Tot_child_Noparent_HH`!="2", `HH[5]/group1/Tot_child_Noparent_HH`!="3", `HH[6]/group1/Tot_child_Noparent_HH`!="2" , `HH[6]/group1/Tot_child_Noparent_HH`!="3" , `HH[7]/group1/Tot_child_Noparent_HH`!="2",`HH[7]/group1/Tot_child_Noparent_HH`!="3",, `HH[8]/group1/Tot_child_Noparent_HH`!="2", `HH[8]/group1/Tot_child_Noparent_HH`!="3", `HH[9]/group1/Tot_child_Noparent_HH`!="2", `HH[9]/group1/Tot_child_Noparent_HH`!="3", `HH[10]/group1/Tot_child_Noparent_HH`!="2", `HH[10]/group1/Tot_child_Noparent_HH`!="3", `HH[1]/group1/Tot_child_distance`!="2", `HH[1]/group1/Tot_child_distance`!="3", `HH[2]/group1/Tot_child_distance`!="2",`HH[2]/group1/Tot_child_distance`!="3", `HH[3]/group1/Tot_child_distance`!="2", `HH[3]/group1/Tot_child_distance`!="3", `HH[4]/group1/Tot_child_distance`!="2", `HH[4]/group1/Tot_child_distance`!="3", `HH[5]/group1/Tot_child_distance`!="2", `HH[5]/group1/Tot_child_distance`!="3", `HH[6]/group1/Tot_child_distance`!="2" , `HH[6]/group1/Tot_child_distance`!="3" , `HH[7]/group1/Tot_child_distance`!="2",`HH[7]/group1/Tot_child_distance`!="3", `HH[8]/group1/Tot_child_distance`!="2", `HH[8]/group1/Tot_child_distance`!="3", `HH[9]/group1/Tot_child_distance`!="2", `HH[9]/group1/Tot_child_distance`!="3", `HH[10]/group1/Tot_child_distance`!="2", `HH[10]/group1/Tot_child_distance`!="3",  `HH[1]/group1/Tot_child_travel_HH`!="2", `HH[1]/group1/Tot_child_travel_HH`!="3", `HH[2]/group1/Tot_child_travel_HH`!="2",`HH[2]/group1/Tot_child_travel_HH`!="3", `HH[3]/group1/Tot_child_travel_HH`!="2", `HH[3]/group1/Tot_child_travel_HH`!="3", `HH[4]/group1/Tot_child_travel_HH`!="2", `HH[4]/group1/Tot_child_travel_HH`!="3", `HH[5]/group1/Tot_child_travel_HH`!="2", `HH[5]/group1/Tot_child_travel_HH`!="3", `HH[6]/group1/Tot_child_travel_HH`!="2" , `HH[6]/group1/Tot_child_travel_HH`!="3" , `HH[7]/group1/Tot_child_travel_HH`!="2",`HH[7]/group1/Tot_child_travel_HH`!="3", `HH[8]/group1/Tot_child_travel_HH`!="2", `HH[8]/group1/Tot_child_travel_HH`!="3", `HH[9]/group1/Tot_child_travel_HH`!="2", `HH[9]/group1/Tot_child_travel_HH`!="3", `HH[10]/group1/Tot_child_travel_HH`!="2", `HH[10]/group1/Tot_child_travel_HH`!="3", `HH[1]/group1/Tot_child_Others_HH`!="2", `HH[1]/group1/Tot_child_Others_HH`!="3", `HH[2]/group1/Tot_child_Others_HH`!="2",`HH[2]/group1/Tot_child_Others_HH`!="3", `HH[3]/group1/Tot_child_Others_HH`!="2", `HH[3]/group1/Tot_child_Others_HH`!="3", `HH[4]/group1/Tot_child_Others_HH`!="2", `HH[4]/group1/Tot_child_Others_HH`!="3", `HH[5]/group1/Tot_child_Others_HH`!="2", `HH[5]/group1/Tot_child_Others_HH`!="3", `HH[6]/group1/Tot_child_Others_HH`!="2" , `HH[6]/group1/Tot_child_Others_HH`!="3" , `HH[7]/group1/Tot_child_Others_HH`!="2",`HH[7]/group1/Tot_child_Others_HH`!="3", `HH[8]/group1/Tot_child_Others_HH`!="2", `HH[8]/group1/Tot_child_Others_HH`!="3", `HH[9]/group1/Tot_child_Others_HH`!="2", `HH[9]/group1/Tot_child_Others_HH`!="3", `HH[10]/group1/Tot_child_Others_HH`!="2", `HH[10]/group1/Tot_child_Others_HH`!="3")
GF<-DF |> 
  select(`Country`, Region, `District`, `Response`, `roundNumber`, `Type_Monitoring`,`date_monitored`, `Total_U5_Present`, `TotalFM`, `sum_missed_children`, `Total_Absent`, `Total_refusal` , `HH[1]/Total_U6_Present_HH`, `HH[2]/Total_U6_Present_HH`, `HH[3]/Total_U6_Present_HH`, `HH[4]/Total_U6_Present_HH`, `HH[5]/Total_U6_Present_HH`, `HH[6]/Total_U6_Present_HH`, `HH[7]/Total_U6_Present_HH`, `HH[8]/Total_U6_Present_HH`, `HH[9]/Total_U6_Present_HH`, `HH[10]/Total_U6_Present_HH`, `HH[1]/U6_Vac_FM_HH`, `HH[2]/U6_Vac_FM_HH`, `HH[3]/U6_Vac_FM_HH`, `HH[4]/U6_Vac_FM_HH`, `HH[5]/U6_Vac_FM_HH`, `HH[6]/U6_Vac_FM_HH`, `HH[7]/U6_Vac_FM_HH`, `HH[8]/U6_Vac_FM_HH`, `HH[9]/U6_Vac_FM_HH`, `HH[10]/U6_Vac_FM_HH`, `HH[1]/group1/Tot_child_Absent_HH`, `HH[2]/group1/Tot_child_Absent_HH`, `HH[3]/group1/Tot_child_Absent_HH`, `HH[4]/group1/Tot_child_Absent_HH`, `HH[5]/group1/Tot_child_Absent_HH`, `HH[6]/group1/Tot_child_Absent_HH`, `HH[7]/group1/Tot_child_Absent_HH`, `HH[8]/group1/Tot_child_Absent_HH`, `HH[9]/group1/Tot_child_Absent_HH`, `HH[10]/group1/Tot_child_Absent_HH`, `HH[1]/group1/Tot_child_NC_HH`, `HH[2]/group1/Tot_child_NC_HH`, `HH[3]/group1/Tot_child_NC_HH`, `HH[4]/group1/Tot_child_NC_HH`, `HH[5]/group1/Tot_child_NC_HH`, `HH[6]/group1/Tot_child_NC_HH`, `HH[7]/group1/Tot_child_NC_HH`, `HH[8]/group1/Tot_child_NC_HH`, `HH[9]/group1/Tot_child_NC_HH`, `HH[10]/group1/Tot_child_NC_HH`, `HH[1]/group1/Tot_child_Noparent_HH`, `HH[2]/group1/Tot_child_Noparent_HH`, `HH[3]/group1/Tot_child_Noparent_HH`, `HH[4]/group1/Tot_child_Noparent_HH`, `HH[5]/group1/Tot_child_Noparent_HH`, `HH[6]/group1/Tot_child_Noparent_HH`, `HH[7]/group1/Tot_child_Noparent_HH`, `HH[8]/group1/Tot_child_Noparent_HH`, `HH[9]/group1/Tot_child_Noparent_HH`, `HH[10]/group1/Tot_child_Noparent_HH`, `HH[1]/group1/Tot_child_distance`, `HH[2]/group1/Tot_child_distance`, `HH[3]/group1/Tot_child_distance`, `HH[4]/group1/Tot_child_distance`, `HH[5]/group1/Tot_child_distance`, `HH[6]/group1/Tot_child_distance`, `HH[7]/group1/Tot_child_distance`, `HH[8]/group1/Tot_child_distance`, `HH[9]/group1/Tot_child_distance`, `HH[10]/group1/Tot_child_distance`, `HH[1]/group1/Tot_child_travel_HH`, `HH[2]/group1/Tot_child_travel_HH`, `HH[3]/group1/Tot_child_travel_HH`, `HH[4]/group1/Tot_child_travel_HH`, `HH[5]/group1/Tot_child_travel_HH`, `HH[6]/group1/Tot_child_travel_HH`, `HH[7]/group1/Tot_child_travel_HH`, `HH[8]/group1/Tot_child_travel_HH`, `HH[9]/group1/Tot_child_travel_HH`, `HH[10]/group1/Tot_child_travel_HH`,`HH[1]/group1/Tot_child_Others_HH`, `HH[2]/group1/Tot_child_Others_HH`, `HH[3]/group1/Tot_child_Others_HH`, `HH[4]/group1/Tot_child_Others_HH`, `HH[5]/group1/Tot_child_Others_HH`, `HH[6]/group1/Tot_child_Others_HH`, `HH[7]/group1/Tot_child_Others_HH`, `HH[8]/group1/Tot_child_Others_HH`, `HH[9]/group1/Tot_child_Others_HH`, `HH[10]/group1/Tot_child_Others_HH`)

#ABSENT

GF$`HH[1]/group1/Tot_child_Absent_HH`[GF$`HH[1]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_Absent_HH`[GF$`HH[2]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_Absent_HH`[GF$`HH[3]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_Absent_HH`[GF$`HH[4]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_Absent_HH`[GF$`HH[5]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_Absent_HH`[GF$`HH[6]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_Absent_HH`[GF$`HH[7]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_Absent_HH`[GF$`HH[8]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_Absent_HH`[GF$`HH[9]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_Absent_HH`[GF$`HH[10]/group1/Tot_child_Absent_HH` == "n/a"] <- "0"

#REFUS

GF$`HH[1]/group1/Tot_child_NC_HH`[GF$`HH[1]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_NC_HH`[GF$`HH[2]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_NC_HH`[GF$`HH[3]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_NC_HH`[GF$`HH[4]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_NC_HH`[GF$`HH[5]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_NC_HH`[GF$`HH[6]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_NC_HH`[GF$`HH[7]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_NC_HH`[GF$`HH[8]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_NC_HH`[GF$`HH[9]/group1/Tot_child_NC_HH` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_NC_HH`[GF$`HH[10]/group1/Tot_child_NC_HH` == "n/a"] <- "0"

#hh_not_noparent

GF$`HH[1]/group1/Tot_child_Noparent_HH`[GF$`HH[1]/group1/Tot_child_Noparent_HH` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_Noparent_HH`[GF$`HH[2]/group1/Tot_child_Noparent_HH` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_Noparent_HH`[GF$`HH[3]/group1/Tot_child_Noparent_HH` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_Noparent_HH`[GF$`HH[4]/group1/Tot_child_Noparent_HH` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_Noparent_HH`[GF$`HH[5]/group1/Tot_child_Noparent_HH` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_Noparent_HH`[GF$`HH[6]/group1/Tot_child_Noparent_HH` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_Noparent_HH`[GF$`HH[7]/group1/Tot_child_Noparent_HH` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_Noparent_HH`[GF$`HH[8]/group1/Tot_child_Noparent_HH` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_Noparent_HH`[GF$`HH[9]/group1/Tot_child_Noparent_HH` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_Noparent_HH`[GF$`HH[10]/group1/Tot_child_Noparent_HH` == "n/a"] <- "0"

#hh_not_distance

GF$`HH[1]/group1/Tot_child_distance`[GF$`HH[1]/group1/Tot_child_distance` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_distance`[GF$`HH[2]/group1/Tot_child_distance` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_distance`[GF$`HH[3]/group1/Tot_child_distance` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_distance`[GF$`HH[4]/group1/Tot_child_distance` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_distance`[GF$`HH[5]/group1/Tot_child_distance` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_distance`[GF$`HH[6]/group1/Tot_child_distance` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_distance`[GF$`HH[7]/group1/Tot_child_distance` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_distance`[GF$`HH[8]/group1/Tot_child_distance` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_distance`[GF$`HH[9]/group1/Tot_child_distance` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_distance`[GF$`HH[10]/group1/Tot_child_distance` == "n/a"] <- "0"

#hh_child_travel
GF$`HH[1]/group1/Tot_child_travel_HH`[GF$`HH[1]/group1/Tot_child_travel_HH` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_travel_HH`[GF$`HH[2]/group1/Tot_child_travel_HH` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_travel_HH`[GF$`HH[3]/group1/Tot_child_travel_HH` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_travel_HH`[GF$`HH[4]/group1/Tot_child_travel_HH` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_travel_HH`[GF$`HH[5]/group1/Tot_child_travel_HH` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_travel_HH`[GF$`HH[6]/group1/Tot_child_travel_HH` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_travel_HH`[GF$`HH[7]/group1/Tot_child_travel_HH` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_travel_HH`[GF$`HH[8]/group1/Tot_child_travel_HH` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_travel_HH`[GF$`HH[9]/group1/Tot_child_travel_HH` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_travel_HH`[GF$`HH[10]/group1/Tot_child_travel_HH` == "n/a"] <- "0"

#hh_other_reasons_non_vaccinated

GF$`HH[1]/group1/Tot_child_Others_HH`[GF$`HH[1]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[2]/group1/Tot_child_Others_HH`[GF$`HH[2]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[3]/group1/Tot_child_Others_HH`[GF$`HH[3]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[4]/group1/Tot_child_Others_HH`[GF$`HH[4]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[5]/group1/Tot_child_Others_HH`[GF$`HH[5]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[6]/group1/Tot_child_Others_HH`[GF$`HH[6]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[7]/group1/Tot_child_Others_HH`[GF$`HH[7]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[8]/group1/Tot_child_Others_HH`[GF$`HH[8]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[9]/group1/Tot_child_Others_HH`[GF$`HH[9]/group1/Tot_child_Others_HH` == "n/a"] <- "0"
GF$`HH[10]/group1/Tot_child_Others_HH`[GF$`HH[10]/group1/Tot_child_Others_HH` == "n/a"] <- "0"

#mutate_for_summarise
GG <- GF |>
  # replace(is.na("."), "0") |> 
  mutate(across((starts_with("HH[")),
                as.numeric))
GH <- GG |>
  mutate(
    u5_present = rowSums(across(
      c("HH[1]/Total_U6_Present_HH":"HH[10]/Total_U6_Present_HH")
    )),
    u5_FM1 = rowSums(across(
      c("HH[1]/U6_Vac_FM_HH":"HH[10]/U6_Vac_FM_HH")
    )),
    u5_FM = ifelse(u5_FM1>u5_present, u5_present, u5_FM1),
    missed_child = (u5_present - u5_FM),
    r_non_FM_Absent = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_Absent_HH":"HH[10]/group1/Tot_child_Absent_HH"
      )
    )),
    r_non_FM_NC = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_NC_HH":"HH[10]/group1/Tot_child_NC_HH"
      )
    )),
    r_non_FM_hh_Noparent = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_Noparent_HH":"HH[10]/group1/Tot_child_Noparent_HH"
      )
    )),
    r_non_FM_hh_distance = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_distance":"HH[10]/group1/Tot_child_distance"
      )
    )),
    r_non_FM_travel = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_travel_HH":"HH[10]/group1/Tot_child_travel_HH"
      )
    )),
    r_non_FM_other = rowSums(across(
      c(
        "HH[1]/group1/Tot_child_Others_HH":"HH[10]/group1/Tot_child_Others_HH"
      )
    ))
  )
# GH <- read_csv("C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/level1_IM/ALG_IM.csv")
GJ<- GH |> 
  select(Country, Region, District, Response, roundNumber, date_monitored, u5_present, u5_FM, missed_child, r_non_FM_Absent, r_non_FM_NC, r_non_FM_hh_Noparent, r_non_FM_hh_distance, r_non_FM_travel, r_non_FM_other ) |>
  group_by(Country, Region, District, Response, roundNumber) |>
  mutate(date_monitored = as_date(date_monitored)) |> 
  arrange(date_monitored) |> 
  mutate(date.diff = c(1, diff(date_monitored))) |> 
  mutate(period = cumsum(date.diff != 1)) |> 
  ungroup() |> 
  group_by(Country, Region, District, Response, roundNumber) |> 
  summarise(start_date = min(date_monitored),
            endate_date = max(date_monitored), 
            u5_present = sum(u5_present),
            u5_FM = sum(u5_FM),
            missed_child = sum(missed_child),
            r_non_FM_Absent = sum(r_non_FM_Absent),
            r_non_FM_NC = sum(r_non_FM_NC),
            r_non_FM_hh_Noparent = sum(r_non_FM_hh_Noparent),
            r_non_FM_hh_distance = sum(r_non_FM_hh_distance),
            r_non_FM_travel = sum(r_non_FM_travel),
            r_non_FM_other = sum(r_non_FM_other))|>
  ungroup() |>
  mutate(cv = round(u5_FM/u5_present, 2),
         year = year(start_date)) |>
    filter(start_date > 1-10-2019)
GK<-GJ |> 
  mutate(Vaccine.type = case_when(
    str_detect(Response, pattern = "nOPV") ~ "nOPV2",
    str_detect(Response, pattern = "nOPV") ~ "nOPV2022",
    str_detect(Response, pattern = "bOPV") ~ "bOPV",
    str_detect(Response, pattern = "WPV1") ~ "bOPV",
    str_detect(Response, pattern = "VPOn") ~ "nOPV2",
    str_detect(Response, pattern = "TSHUAPA") ~ "mOPV",
    str_detect(Response, pattern = "TSHUAPA") ~ "mOPV",
    str_detect(Response, pattern = "VPOb") ~ "bOPV",
    str_detect(Response, pattern = "Tanganyika") ~ "nOPV2",
    str_detect(Response, pattern = "Bangui 1") ~ "mOPV",
    str_detect(Response, pattern = "nVPO") ~ "nOPV",
    str_detect(Response, pattern = "GOTHEY") ~ "mOPV",
    str_detect(Response, pattern = "OPV") ~ "bOPV",
    str_detect(Response, pattern = "Liberia") ~ "nOPV2",
    str_detect(Response, pattern = "Mauritania") ~ "nOPV2",
    str_detect(Response, pattern = "Sierra Leone") ~ "nOPV2",
    str_detect(Response, pattern = "SEN") ~ "nOPV2",
    str_detect(Country, pattern = "MAL") ~ "nOPV2",
    str_detect(Country, pattern = "CAE") ~ "nOPV2"))
GT<-GK |>
  mutate(M1 = rowSums(across(c(r_non_FM_Absent, r_non_FM_NC, r_non_FM_hh_Noparent, r_non_FM_hh_distance, r_non_FM_travel))),
         Other_reasons = (missed_child - M1),
         Country = case_when(
           Country == "DRC" ~ "RDC",
           Country == "Camerooun" ~ "CAE",
           TRUE ~ Country))
F1<- GT |> 
  select(Country, Region, District, Response, roundNumber, Vaccine.type, start_date, endate_date, year, u5_present, u5_FM, missed_child, cv, r_non_FM_Absent, r_non_FM_NC, r_non_FM_hh_Noparent, r_non_FM_hh_distance, r_non_FM_travel, Other_reasons) |>
  arrange(start_date)

write_csv(F1,"C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_level/ALG_IM.csv")      




