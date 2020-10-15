---
categories:
- ""
- ""
date: "`r Sys.Date()`"
description: "2016 California Contributors Plots"
draft: false
image: pic10.jpg
keywords: ""
slug: US Election
title: US Election
---

```{r load-libraries, include=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(httr)
library(readxl)
library(vroom)
library(infer)
```

```{r challenge2, echo=FALSE, out.width="100%"}
knitr::include_graphics("challenge2.png", error = FALSE)
```

I would like to reproduce the above plot that shows the top ten cities in highest amounts raised in political contributions in California during the 2016 US Presidential election.

# Useful Package and Dataframes

```{r,install package}
# Useful packages for plot
# install.packages('patchwork')
# install.packages('tidytext')
library(patchwork)
library(tidytext)
library(ggplot2)
library(dplyr)
```
## Match zipcodes with cities 

To get this plot, first I join two dataframes; one you has with all contributions, and other used for translate zipcodes to cities.
Then, I use zipcodes in main database to match cities in zipcodes database.

```{r, load_CA_data, warnings= FALSE, message=FALSE}
# Import data, use vroom() as it is significantly faster than read.csv()
CA_contributors_2016 <- vroom::vroom(here::here("data","CA_contributors_2016.csv"))
zip_code<-vroom::vroom(here::here("data","zip_code_database.csv"),col_types=cols(
  zip=col_double()))
CA_zip<-zip_code%>%
  filter(state=="CA")
CA_have_city<-left_join(CA_contributors_2016,CA_zip,by='zip')
```

# Ordinary Methods putting two plots together

First, I filter contributors to Trump, Donald J. and use ggplot funtion to creat top 10 cities for him in descending order. Also I use same method to generate a same plot for Clinton, Hillary Rodham.

```{r, plots with patchwork, warnings= FALSE,message=FALSE}
#draw plot for Donald
D_contb<-CA_have_city%>%
  filter(cand_nm =='Trump, Donald J.')%>%
  group_by(primary_city)%>%
  summarize(D_total_city=sum(contb_receipt_amt))%>%
  arrange(desc(D_total_city))%>%
  head(10)
plot_1<-ggplot(D_contb,aes(x=D_total_city,y=reorder(primary_city,D_total_city)))+
  geom_col(fill='#CC3333')+
  labs(x='Raised money',y='',title='Trump, Donald J.')+
  theme(plot.title = element_text(size=12,hjust = 0.5, margin = margin(t = 5, b = 5)))+ 
  scale_x_continuous(limits = c(0,600000),labels=scales::dollar_format())+
  NULL
plot_1

#draw plot for Hillary
H_contb<-CA_have_city%>%
  filter(cand_nm =='Clinton, Hillary Rodham')%>%
  group_by(primary_city)%>%
  summarize(H_total_city=sum(contb_receipt_amt))%>%
  arrange(desc(H_total_city))%>%
  head(10)
theme_set(theme_bw())
plot_2<-ggplot(H_contb,aes(x=H_total_city,y=reorder(primary_city,H_total_city)))+
  geom_col(fill='#0066CC')+
  labs(x='Raised money',y='',title='Clinton, Hillary Rodham')+
  theme(plot.title = element_text(size=12,hjust = 0.5, margin = margin(t = 5, b = 5)),legend.background = element_rect(fill='grey'))+
  scale_x_continuous(limits = c(0,13000000),labels=scales::dollar_format())+
  NULL
plot_2
```
Then, I put these two plots together for a combined plot and put a overall title.

```{r, plots with patchwork, warnings= FALSE,message=FALSE}
#combine two plots
plot_2+plot_1+
  plot_annotation(title='Where did candidates raise most money?',theme=theme(plot.title = element_text(size = 12)))+
  plot_layout(ncol=NULL,nrow=NULL)
```

#Make two plots at same time with facet function

To make work easier, I try to generate the plot in one step.
So, I try to filter 10 cities databases for two candidates and combine them in one database. Then, I can create the plot in that simplified and combind database.

```{r,plots with facet,warnings= FALSE,message=FALSE}
#get 10 cities for Hillary
H<-CA_have_city%>%
  filter(cand_nm=='Clinton, Hillary Rodham')%>%
  group_by(primary_city) %>%
  summarise(total_contb=sum(contb_receipt_amt),cand_nm='Clinton, Hillary Rodham')%>%
  arrange(desc(total_contb))%>%
  head(10)

#get 10 cities for Trump
D<-CA_have_city%>%
  filter(cand_nm=='Trump, Donald J.')%>%
  group_by(primary_city) %>%
  summarise(total_contb=sum(contb_receipt_amt),cand_nm='Trump, Donald J.')%>%
  arrange(desc(total_contb))%>%
  head(10)

X<-bind_rows(H,D,id=NULL)

X%>%
  mutate(cand_nm = as.factor(cand_nm),
           primary_city = reorder_within(primary_city, total_contb, cand_nm)) %>%
  ggplot(aes(x=reorder(primary_city,total_contb), y=total_contb, fill = cand_nm)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~cand_nm, scales = "free") +
  coord_flip() +
  labs(y = "Raised Money",
        x = NULL,
        title = "Where did candidates raise most money?")+
  scale_y_continuous(labels=scales::dollar_format())+
  scale_x_reordered()+
  NULL
```

#Expand: Same plot for top 10 candidates

After this, I want to expand my plot and create the same plot for the top 10 candidates and not just the top two. So, I get top 10 candidates names from original dataframe, and use name list to filter their contributors in dataframe and calculate sum of cities. Then, I keep the top 10 cites for every catagory(candidates) in dataframe. Last, use the 100 rows dataframe to create plot.

```{r,top10 candidates,warnings= FALSE,message=FALSE}
#get top 10 candidates' names
top10<-CA_have_city%>%
  group_by(cand_nm)%>%
  summarise(total=sum(contb_receipt_amt))%>%
  arrange(desc(total))%>%
  head(10)%>%
  ungroup

#get top 10 candidates' total amounts in cities
top10_contb<-CA_have_city%>%
  filter(cand_nm==top10$cand_nm)%>%
  group_by(cand_nm) %>%
  group_by(primary_city) %>%
  mutate(total_contb=sum(contb_receipt_amt))%>%
  distinct(primary_city,.keep_all=TRUE)%>%
  ungroup

#filter top 10 cities in dataframe for everyone
library(data.table)
top10_contb<-setorder(setkey(setDT(top10_contb), cand_nm), cand_nm, -total_contb)[
                                          ,.SD[1:10], by=cand_nm]
#uese facet again
top10_contb%>%
  mutate(cand_nm = as.factor(cand_nm),
           primary_city = reorder_within(primary_city, total_contb, cand_nm)) %>%
  ggplot(aes(reorder(primary_city,total_contb), total_contb, fill = cand_nm)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~cand_nm, scales = "free") +
  coord_flip() +
  labs(y = "Raised Money",
        x = NULL,
        title = "Where did candidates raise most money?")+
  scale_y_continuous(labels=scales::dollar_format())+
  scale_x_reordered() +
  NULL
```
