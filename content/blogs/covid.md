---
categories:
- ""
- ""
date: "`r Sys.Date()`"
description: 'CDC COVID-19 Public Use Data'
draft: false
image: pic07.jpg
keywords: ""
slug: Covid-19
title: Covid-19
---
#CDC COVID-19 Public Use Data

Visit the [CDC Covid-19 Case Surveillance Data](https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf). There are well over 3 million entries of individual, de-identified patient data. Since this is a large file.

```{r, cache=TRUE}
# file contains 11 variables and 3.66m rows and is well over 380Mb. 
# It will take time to download

# URL link to CDC to download data
url <- "https://data.cdc.gov/api/views/vbim-akqf/rows.csv?accessType=DOWNLOAD"

covid_data <- vroom::vroom(url)%>% # If vroom::vroom(url) doesn't work, use read_csv(url)
  clean_names()
  glimpse(covid_data)

```

Given the data, I would like to produce two graphs that show death % rate:

1. by age group, sex, and whether the patient had co-morbidities or not
2. by age group, sex, and whether the patient was admited to Intensive Care Unit (ICU) or not.

##Creating graph No. 1
```{r covid_challenge, echo=FALSE, out.width="100%"}

# Step 1: filtering variables of interest to exclude missing values
covid_death_rate_comorbidities <- covid_data %>% 
  filter(sex %in% c('Male','Female'), !age_group %in% c('Unknown', NA), medcond_yn %in% c('Yes','No'),death_yn %in% c('Yes','No')) %>%  

# Step 2: selecting only the four variables needed for this graph 
select(sex,age_group,death_yn,medcond_yn) %>%

# Step 3: grouping the variables since we re interested in the death rate across age group, sex and whether the patient had co-morbities or not
group_by(sex,age_group,medcond_yn) %>% 

# Step 4: calculating the death rate as a percentage value
summarize(death_tot=sum(death_yn=='Yes'),total=n()) %>% 
mutate(death_rate=(death_tot/total*100)) %>%
select(sex,age_group,death_rate,medcond_yn) 

new_labels<-c('Yes'='With Comorbidities','No'='Without Comorbidities')

# Step 5: plotting 
ggplot(covid_death_rate_comorbidities,aes(x=death_rate,y=age_group))+
  geom_col(fill='#8b9dc3')+
  facet_grid(medcond_yn~sex,as.table=FALSE,labeller=labeller(medcond_yn=new_labels))+
  geom_text(aes(label=round(death_rate,digits=1)),hjust=-0.1,size=3)+
  expand_limits(x=c(2))+
  theme_bw()+
  scale_x_continuous ()+
  labs(title='Covid death % by age group, sex, and presence of co-morbidities',x='',y='',caption='Source: CDC')+
  theme(plot.title=element_text(size=12))
```

```{r covid_challenge, echo=FALSE, out.width="100%"}  
## Creating graph No. 2
knitr::include_graphics(here::here("images", "covid_death_rate_icu.png"), error = FALSE)

# Step 1: filtering variables of interest to exclude missing values
covid_death_rate_icu <- covid_data %>% 
  filter(sex %in% c('Male','Female'), !age_group %in% c('Unknown', NA), icu_yn %in% c('Yes','No'),death_yn %in% c('Yes','No')) %>%  

# Step 2: selecting only the four variables needed for this graph 
select(sex,age_group,death_yn,icu_yn) %>%

# Step 3: grouping the variables since we re interested in the death rate across age group, sex and whether the patient had co-morbities or not
group_by(sex,age_group,icu_yn) %>% 

# Step 4: calculating the death rate as a percentage value
summarize(death_tot=sum(death_yn=='Yes'),total=n()) %>% 
mutate(death_rate=(death_tot/total*100)) %>%
select(sex,age_group,death_rate,icu_yn) 

new_labels_ICU<-c('Yes'='Admitted to ICU','No'='No ICU')

# Step 5: plotting 
ggplot(covid_death_rate_icu,aes(x=death_rate,y=age_group))+
  geom_col(fill='#ff9984')+
  facet_grid(icu_yn~sex,as.table=FALSE,labeller=labeller(icu_yn=new_labels_ICU))+
  geom_text(aes(label=round(death_rate,digits=1)),hjust=-0.1,size=3)+
  expand_limits(x=c(2))+
  theme_bw()+
  scale_x_continuous ()+
  labs(title='Covid death % by age group, sex, and whether patient was admitted to ICU',x='',y='',caption='Source: CDC')+
  theme(plot.title=element_text(size=12))
```
