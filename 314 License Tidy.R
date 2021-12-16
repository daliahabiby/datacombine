# 314 Data Cleaning 12/7

library(tidyverse)
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(xlsx)
library(readr)
library(plyr)
#reading in data
getwd()
read_excel("CHIROPRACTOR public list 12-2021.xlsx")-> chiropractor
chiropractor

read_excel("OPTH public list 12-2021.xlsx")-> OPTH
OPTH

read_excel("12-3-21 MED TECH LIST.xlsx")-> medtech
medtech

read_excel("Hearing Care list 12-2021.xls", col_types = c("guess", "guess", "list", "guess"))->hearing


hearing

Nth.delete<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
Nth.delete(hearing, 3)->hearing


hearing%>%
  mutate(isstatus = str_detect(`License status and Expiration Date:`, "^[A-Z]"))%>%
  mutate(isexpiration = str_detect(`License status and Expiration Date:`, "[1-9]"))%>%
  mutate(isname = str_detect(`Name and License Type:`, ","))%>%
  mutate(istype = str_detect(`Name and License Type:`, "Hearing\\sAid\\sDealer|Audiologist"))-> hearing1
  
hearing1

hearing1%>%
  mutate(key = case_when(
    isexpiration ~ "License Expiration",
    isstatus ~ "License Status",
    isname ~ "Name",
    istype ~ "License Type"
  ))-> hearing2

hearing2
hearing2%>%
  mutate(row = row_number())%>%
  pivot_wider(names_from = key, values_from = c(`Name and License Type:`, `License status and Expiration Date:`))->hearing3
hearing3

hearing3%>%
  select(-isstatus, -isexpiration, -isname, -istype, -row)-> hearing4

hearing4

hearing4%>%
  select(`Profession:Hearing Care Providers`, `Issue Date:`, `Name and License Type:_License Status`, `License status and Expiration Date:_License Status`)%>%
  drop_na()->toprow
  
unnest(toprow, `License status and Expiration Date:_License Status`)->toprow
toprow
hearing4%>%
  select(`Name and License Type:_License Expiration`, `License status and Expiration Date:_License Expiration`)%>%
  drop_na()->botrow

unnest(botrow, `License status and Expiration Date:_License Expiration`)->botrow
botrow
toprow%>%
  mutate(row = row_number())->toprow

botrow%>%
  mutate(row = row_number())->botrow

inner_join(toprow, botrow)->hearing5
hearing5%>%
  select(-row)-> hearingcare
hearingcare
hearingcare%>%
  rename("Name" = `Name and License Type:_License Status`)%>%
  rename("License Status" = `License status and Expiration Date:_License Status`)%>%
  rename("License Type" = `Name and License Type:_License Expiration`)%>%
  rename("Licesne Expiration Date" = `License status and Expiration Date:_License Expiration`)-> hearingcare
hearingcare

#made new hearing excel
write.csv("Hearing Care List 12-2021 Clean.xlsx")
write.xlsx(as.data.frame(hearingcare), "Hearing Care List 12-2021 Clean.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)

chiropractor%>%
  rename("LAST NAME" = LASTNAME)-> chiropractor

rbind(chiropractor, OPTH)->chiroOPTH
chiroOPTH

medtech$`LICENSE NUMBER`<- as.character(medtech$`LICENSE NUMBER`)
medtech
bind_rows(chiroOPTH, medtech)->OPTHchiromed
OPTHchiromed


hearingcare%>%
  separate(Name, into = c("LAST NAME", "FIRST NAME", "MIDDLE NAME"), sep = " ")->hearingnames
hearingnames

hearingnames%>%
  mutate("LAST NAME" = str_remove(`LAST NAME`, pattern = ","))%>%
  rename("EXPIRATION DATE" = `Licesne Expiration Date`)%>%
  rename("LICENSE STATUS" = `License Status`)%>%
  rename("LICENSE NUMBER/TYPE" = `License Type`)%>%
  rename("ISSUE DATE" = `Issue Date:`)%>%
  rename("PROFESSION:HEARING CARE PROVIDERS" = `Profession:Hearing Care Providers`)->hearingfinal
hearingfinal

OPTHchiromed%>%
  rename("LICENSE NUMBER/TYPE" = `LICENSE NUMBER`)->OPTHchiromed
rbind.fill(OPTHchiromed, hearingfinal)->licensefinal

write.xlsx(as.data.frame(licensefinal), "NH License Data.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)
