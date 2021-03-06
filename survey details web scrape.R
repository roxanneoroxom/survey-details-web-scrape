library(dplyr)
library(plyr)
library(rvest)
library(XML)
library(httr)
library(stringr)
library(tidyverse)
library(tidyr)
library(recipes)
library(labelled)

#=======================================
##### go to site with a list of all surveys and grab each survey URL
#=======================================

### store the main link and read it
link <- "https://dhsprogram.com/What-We-Do/survey-search.cfm?pgtype=main&SrvyTp=country"

webpage <- read_html(link)

### clean data of survey URLs
table <- html_nodes(webpage, xpath = "//td/a") %>% html_attr("href") %>% plyr::ldply() %>% filter(!str_detect(V1, 'javascript')) %>% mutate(V1 = str_replace_all(V1, "cfm2", "cfm")) %>% select(links=V1) %>% mutate(keep_dummy = ifelse(grepl('/methodology/survey/survey-display-', links), 1, 0)) %>% filter(keep_dummy==1) %>% dplyr::mutate(rowid = row_number())

### store complete survey URLs
all_urls <- paste0("https://dhsprogram.com",table$links) %>% unique( table$links )

#=======================================
##### go to each survey URL and grab details about each survey
#=======================================

### note: this section adapts code from cobrienudry's webscrape tutorial https://github.com/cobrienudry/webscrape

dat <- list()

for(i in 1: length(all_urls)){
  
  tryCatch({
    
    # read specific website
    
    webpage <- read_html(all_urls[i])

    # scrape initial information
    
    extract <- webpage %>% html_nodes("#title_container") %>% html_text()
    extract_all <- table(extract)
    extract_all <- as.data.frame(extract_all)

    # loop over other information to scrape
    items <- c("#dataset_container","#surveyLeftSide","#surveyRightSide","#wrapper")
    for (j in items) {
      extract <- webpage %>% html_nodes(j) %>% html_text()
      extract_other <- table(extract)
      extract_other <- as.data.frame(extract_other)
      extract_all <- bind_rows(extract_all, extract_other)
    }

    # store link extracted information came from (i.e., make a group variable)
    extract_all <- extract_all %>% mutate(link=all_urls[i]) 
    
    #store in list
    dat[[i]] <- extract_all
    
    cat(i, "page is done. \n")
  }, error=function(e){cat(i, "ERROR :",conditionMessage(e), "\n")})
}

### list to dataframe
dat2 <- rbind.fill(dat)

### factor to string
dat2 <- data.frame(lapply(dat2, as.character), stringsAsFactors=FALSE)
class(dat2$extract)

#=======================================
##### create a clean dataset for extract 1
#=======================================

extract1 <- dat2 %>% 
  
  ### add extract number information
  group_by(link) %>% dplyr::mutate(extract_num = row_number()) %>%
  
  ### keep only data for this extract
  filter(extract_num==1) %>%
  
  ### split
  separate(extract, c("country","extract"), sep = ":") %>%
  
  ### split
  separate(extract, c("survey_type","survey_years"), sep = ",") %>%
  
  ### final clean
  select(-Freq,-extract_num) %>% mutate(country=str_squish(country))
  
#=======================================
##### create a clean dataset for extract 2
#=======================================

extract2 <- dat2 %>% 
    
    ### add extract number information
    group_by(link) %>% dplyr::mutate(extract_num = row_number()) %>%
    
    ### keep only data for this extract
    filter(extract_num==2) %>%
    
    ### create delimiter
    mutate(extract = str_replace_all(extract , c("Survey Datasets" = "SplitHereSurvey Datasets", "HIV Testing" = "SplitHereHIV Testing","GPS Datasets" = "SplitHereGPS Datasets", "SPA Datasets" = "SplitHereSPA Datasets"))) %>%
    
    ### split
    separate(extract, c("extract2_1","extract2_2","extract2_3","extract2_4","extract2_5"), sep = "SplitHere") %>%
    
    ### remove excess string information
    mutate(extract2_2 = str_replace(extract2_2, 'Survey Datasets', '')) %>%
    mutate(extract2_3 = str_replace(extract2_3, 'HIV Testing', '')) %>%
    mutate(extract2_4 = str_replace(extract2_4, 'GPS Datasets', '')) %>%
    mutate(extract2_5 = str_replace(extract2_5, 'SPA Datasets', '')) %>%
    
    ### drop or rename certain variables
    select(-Freq,-extract2_1,-extract_num,survey_data=extract2_2,hiv_testing_data=extract2_3,gps_data=extract2_4,spa_data=extract2_5)

#=======================================
##### create a clean dataset for extract 3
#=======================================

extract3 <- dat2 %>% 
  
    ### add extract number information
    group_by(link) %>% dplyr::mutate(extract_num = row_number()) %>%
    
    ### keep only data for this extract
    filter(extract_num==3) %>%
    
    ### remove colons & squish
    mutate(extract = str_squish(str_replace_all(extract, ":", ""))) %>%
  
    ### create delimiter
    mutate(extract = str_replace_all(extract , c("Country" = "SplitHereCountry", "Contract Phase" = "SplitHereContract Phase", "Recode Structure"="SplitAgainHereRecode Structure", "Implementing Organization" = "SplitHereImplementing Organization","Fieldwork" = "SplitHereFieldwork","Status" = "SplitHereStatus"))) %>%
  
    ### split
    separate(extract, c("extract3_1","extract3_2","extract3_3","extract3_4","extract3_5","extract3_6"), sep = "SplitHere") %>%
    
    ### remove excess string information
    mutate(extract3_2 = str_replace(extract3_2, 'Country', '')) %>%
    mutate(extract3_3 = str_replace(extract3_3, 'Contract Phase', '')) %>%
    mutate(extract3_4 = str_replace(extract3_4, 'Implementing Organization', '')) %>%
    mutate(extract3_5 = str_replace(extract3_5, 'Fieldwork', '')) %>%
    mutate(extract3_6 = str_replace(extract3_6, 'Status', '')) %>%
    
    ### split using delimiter specific to recode structure
    separate(extract3_3, c("extract3_3a","extract3_3b"), sep = "SplitAgainHere") %>%
    
    ### drop or rename certain variables
    select(-Freq,-extract3_1,-extract_num,country=extract3_2,contract_phase=extract3_3a,recode_structure=extract3_3b,implementing_org=extract3_4,fieldwork=extract3_5,status=extract3_6) %>%

    ### final clean
    mutate(country=str_squish(country))


#=======================================
##### create a clean dataset for extract 4
#=======================================

extract4 <- dat2 %>% 
  
    ### add extract number information
    group_by(link) %>% dplyr::mutate(extract_num = row_number()) %>%
  
    ### keep only data for this extract
    filter(extract_num==4) %>%
    
    ### create delimiter
    mutate(extract = str_replace_all(extract , c("Households" = "SplitHereHouseholds", "Female" = "SplitHereFemale","Male" = "SplitHereMale", "Facilities" = "SplitHereFacilities"))) %>%
    
    ### split
    separate(extract, c("extract4_1","extract4_2","extract4_3","extract4_4","extract4_5"), sep = "SplitHere") %>%
    
    ### move facilities data to the right column for SPA datasets
    mutate(extract4_5 = if_else(grepl("Facilities",extract4_2),extract4_2,extract4_5,extract4_5)) %>% 
    mutate(extract4_2 = if_else(grepl("Facilities",extract4_2),"",extract4_2,extract4_2)) %>%

    ### remove excess string information
    mutate(extract4_2=str_remove(extract4_2,"Households:") ) %>% mutate(extract4_2=str_squish(str_remove(extract4_2,"Sample Size:"))) %>%
  
    ### remove excess string information
    mutate(extract4_5=str_squish(str_remove(extract4_5,"Facilities:"))) %>% mutate(extract4_5=str_squish(str_remove(extract4_5,"Sample Size:"))) %>% 
  
    ### remove colons & squish
    mutate(extract4_3 = str_squish(str_replace_all(extract4_3, ":", ""))) %>%
      
    ### create delimiter
    mutate(extract4_3 = str_replace_all(extract4_3 , c("Female" = "SplitAgainHereFemale", "Age" = "SplitAgainHereAge", "Sample Size"="SplitAgainHereSample Size"))) %>%
      
    ### split
    separate(extract4_3, c("drop","female_sample","female_sample_ages","female_sample_size"), sep = "SplitAgainHere") %>% select(-drop) %>%
  
    ### remove excess string information
    mutate(female_sample=str_squish(str_remove(female_sample,"Female"))) %>% mutate(female_sample_ages=str_squish(str_remove(female_sample_ages,"Age"))) %>% mutate(female_sample_size=str_squish(str_remove(female_sample_size,"Sample Size"))) %>%
      
    ### remove colons & squish
    mutate(extract4_4 = str_squish(str_replace_all(extract4_4, ":", ""))) %>%
      
    ### create delimiter
    mutate(extract4_4 = str_replace_all(extract4_4 , c("Male" = "SplitAgainHereMale", "Age" = "SplitAgainHereAge", "Sample Size"="SplitAgainHereSample Size"))) %>%
      
    ### split
    separate(extract4_4, c("drop","male_sample","male_sample_ages","male_sample_size"), sep = "SplitAgainHere") %>% select(-drop) %>%

    ### remove excess string information
    mutate(male_sample=str_squish(str_remove(male_sample,"Male"))) %>% mutate(male_sample_ages=str_squish(str_remove(male_sample_ages,"Age"))) %>% mutate(male_sample_size=str_squish(str_remove(male_sample_size,"Sample Size"))) %>%

    ### drop or rename certain variables
    select(-Freq,-extract4_1,-extract_num,household_sample_size=extract4_2,facility_sample_size=extract4_5) %>%
  
    ### introduce NA
    mutate(facility_sample_size=na_if(facility_sample_size,"N/A")) %>%
    mutate(household_sample_size=na_if(household_sample_size,"N/A"))

#=======================================
##### create a clean dataset for extract 5
#=======================================

### scrape unique survey characteristics from different DHS web page into a list

link2 <- "https://dhsprogram.com/Methodology/Survey-Search.cfm"

webpage2 <- read_html(link2)

survey_chars <- webpage2 %>% html_nodes("#showfeatures") %>% html_text()
survey_chars <- table(survey_chars)
survey_chars <- as.data.frame(survey_chars)

survey_chars_clean <- survey_chars %>% 
    mutate(survey_chars=str_remove(survey_chars,"Select All")) %>%
    mutate(survey_chars=str_squish(str_remove(survey_chars,"Clear All"))) %>%
    mutate(survey_chars = str_replace_all(survey_chars,"\\d","")) %>% 
    mutate(survey_chars = str_replace_all(survey_chars,"\\(\\)","!")) %>%
    separate_rows(survey_chars,sep="!") %>%
    mutate(survey_chars=str_squish(survey_chars)) %>%
    filter(!str_detect(survey_chars, "This list")) 

all_words <- sort(unique(unlist(strsplit(survey_chars_clean$survey_chars, "!"))))

### prepare intial extract5 dataset

extract5 <- dat2 %>% 
  
  # add extract number information
  group_by(link) %>% dplyr::mutate(extract_num = row_number()) %>%
  
  # keep only data for this extract
  filter(extract_num==5) %>%
  
  # drop variables
  select(-Freq,-extract_num)

### create dummy variables based on whether characteristic appears in extract

counter <- 0
for( i in all_words ) {
  counter = counter + 1
  name <- paste0("var", counter )
  extract5[[name]] <- str_detect(extract5$extract, i)
  var_label(extract5[[name]]) <- i
}

### final clean

extract5 <- extract5 %>% select(-extract)

#=======================================
##### combine extracts 1-5 and do final clean
#=======================================

### combine

  final_extract <- full_join(extract1, extract2,by="link")
  final_extract <- full_join(final_extract, extract3,by="link")
  final_extract <- full_join(final_extract, extract4,by="link")
  final_extract <- full_join(final_extract, extract5,by="link")

### extract survey number
  
  final_extract <- final_extract %>%
    mutate(survey_num=str_remove(link,"https://dhsprogram.com/methodology/survey/survey-display-")) %>%
    mutate(survey_num=str_remove(survey_num,".cfm")) %>%
    mutate(survey_num=as.numeric(survey_num))
  
### check that both country variables are the same and rename as needed

final_extract$match <- as.integer(ifelse(final_extract$country.x==final_extract$country.y, T, F))

if_else(min(final_extract$match)==0,"Issue","No Issue","NA")

final_extract <- final_extract %>% select(-country.y , -match)

names(final_extract)[names(final_extract) == 'country.x'] <- 'country'

### export

haven::write_dta(final_extract, "final_extract.dta")