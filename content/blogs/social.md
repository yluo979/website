---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Analysis GSS
draft: false
image: pic09.jpg
keywords: ""
slug: Social Uses
title: Social Uses
---

# General Social Survey (GSS)

The [General Social Survey (GSS)](http://www.gss.norc.org/) gathers data on American society in order to monitor and explain trends in attitudes, behaviours, and attributes. Many trends have been tracked for decades, so one can see the evolution of attitudes, etc in American Society.

In this project I want to analyze data from the **2016 GSS sample data**, using it to estimate values of *population parameters* of interest about US adults. 

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

```{r, read_gss_data, cache=TRUE}
gss <- read_csv(here::here("data", "smallgss2016.csv"), 
                na = c("", "Don't know",
                       "No answer", "Not applicable"))
```

I notice that many responses should not be taken into consideration, like "No Answer", "Don't Know", "Not applicable", "Refused to Answer".

I will be creating 95% confidence intervals for population parameters. The variables we have are the following:

- hours and minutes spent on email weekly. The responses to these questions are recorded in the `emailhr` and `emailmin` variables. For example, if the response is 2.50 hours, this would be recorded as emailhr = 2 and emailmin = 30.
- `snapchat`, `instagrm`, `twitter`: whether respondents used these social media in 2016
- `sex`: Female - Male
- `degree`: highest education level attained

## Instagram and Snapchat, by sex

First, I want to estimate the *population* proportion of Snapchat or Instagram users in 2016

### New variable snap_insta
Create a  new variable, `snap_insta` that is *Yes* if the respondent reported using any of Snapchat (`snapchat`) or Instagram (`instagrm`), and *No* if not. If the recorded value was NA for both of these questions, the value in your new variable should also be NA.

```{r}
gss<-gss%>%
  mutate(snap_insta=ifelse(snapchat=='Yes'|instagrm=='Yes','Yes', ifelse(snapchat=='NA'&instagrm=='NA','NA','No')))
gss
```

###Calculate proportion
Calculate the proportion of Yes’s for `snap_insta` among those who answered the question, excluding NAs.

```{r}
excluding_NA<-gss%>%
  filter(snap_insta!='NA')
list_exluding_NA<-table(excluding_NA$snap_insta)/sum(table(excluding_NA$snap_insta))
list_exluding_NA[2]
```

###CI formula
Using the CI formula for proportions, construct 95% CIs for men and women who used either Snapchat or Instagram.

```{r}
#calculate proportion in men
men<-gss%>%
  filter(snap_insta!='NA'& sex=='Male')
list_men<-table(men$snap_insta)/sum(table(men$snap_insta))
list_men
men_prop<-list_men[2]
#construct men CI
men_formula_ci <- men %>% 
  summarise(count=n(),
            t_critical=qt(0.975,count-1),
            se_men_prop=sqrt(men_prop*(1-men_prop)/count),
            margin_of_error=t_critical*se_men_prop,
            men_prop_low=men_prop-margin_of_error,
            men_prop_high=men_prop+margin_of_error)
men_formula_ci

#calculate proportion in women
women<-gss%>%
  filter(snap_insta!='NA'& sex=='Female')
list_women<-table(women$snap_insta)/sum(table(women$snap_insta))
list_women
women_prop<-list_women[2]
#construct women CI
women_formula_ci <- women %>% 
  summarise(count=n(),
            t_critical=qt(0.975,count-1),
            se_women_prop=sqrt(women_prop*(1-women_prop)/count),
            margin_of_error=t_critical*se_women_prop,
            women_prop_low=women_prop-margin_of_error,
            women_prop_high=women_prop+margin_of_error)
women_formula_ci
```
The 95% CI for men is [0.281,0.356] and for women is [0.384,0.454].

## Twitter, by education level

Estimate the *population* proportion of Twitter users by education level in 2016.

There are 5 education levels in variable `degree` which, in ascending order of years of education, are Lt high school, High School, Junior college, Bachelor, Graduate. 

###Create factor variable
So, Turn `degree` from a character variable into a factor variable. Make sure the order is the correct one and that levels are not sorted alphabetically which is what R by default does. 

```{r}
gss$degree<-factor(gss$degree,ordered = TRUE,levels = c("Lt high school", "High School", "Junior college", "Bachelor","Graduate"))
skim(gss$degree)
```

###Create variable
Create a  new variable, `bachelor_graduate` that is *Yes* if the respondent has either a `Bachelor` or `Graduate` degree. As before, if the recorded value for either was NA, the value in your new variable should also be NA.


```{r}
gss<-gss%>%
  mutate(bachelor_graduate=ifelse(degree=='Bachelor'|degree=='Graduate','Yes',ifelse(degree=='NA','NA','No')))
skim(gss$bachelor_graduate)
```

###Calculate proportion
Then, Calculate the proportion of `bachelor_graduate` who do (Yes) and who don't (No) use twitter. 

```{r}
bac_grad<-gss%>%
  filter(bachelor_graduate=='Yes'& twitter!='NA')
use_list<-table(bac_grad$twitter)/sum(table(bac_grad$twitter))
use_list
no_prop<-use_list[1]
yes_prop<-use_list[2]
```
###Construct CI
Using the CI formula for proportions, construct two 95% CIs for `bachelor_graduate` vs whether they use (Yes) and don't (No) use twitter. 

```{r}
yes_formula_ci <- bac_grad %>% 
  summarise(count=n(),
            t_critical=qt(0.975,count-1),
            se_yes_prop=sqrt(yes_prop*(1-yes_prop)/count),
            margin_of_error=t_critical*se_yes_prop,
            yes_prop_low=yes_prop-margin_of_error,
            yes_prop_high=yes_prop+margin_of_error)
yes_formula_ci

no_formula_ci <- bac_grad %>% 
  summarise(count=n(),
            t_critical=qt(0.975,count-1),
            se_no_prop=sqrt(no_prop*(1-no_prop)/count),
            margin_of_error=t_critical*se_no_prop,
            no_prop_low=no_prop-margin_of_error,
            no_prop_high=no_prop+margin_of_error)
no_formula_ci
```
95% CIs for `bachelor_graduate` use twitter is [0.196,0.271] and don't use twitter is [0.7290.804].


These two Confidence Intervals don't overlap. The following graph could prove that:
```{r}
formula_ci_twitter<- bac_grad %>% 
  mutate(prop=ifelse(twitter=='Yes',0.233,0.767))%>%
  mutate(count=n())%>%
  group_by(twitter)%>%
  summarise(t_critical=qt(0.975,count-1),
            se_prop=sqrt(prop*(1-prop)/count),
            margin_of_error=t_critical*se_prop,
            prop_low=prop-margin_of_error,
            prop_high=prop+margin_of_error,
            prop=prop)%>%
  distinct(twitter,.keep_all = TRUE)

ggplot(formula_ci_twitter, aes(x=reorder(twitter, prop), y=prop, colour=twitter)) +
  geom_point() +
  geom_errorbar(width=.5, aes(ymin=prop_low, ymax=prop_high)) + 
  labs(x=" ",
       y= "Proportion whether use Twitter", 
       title="Twitter Use Proportion for Bachelor and Graduate") + 
  theme_bw()+
  coord_flip()+
  theme(legend.position = "none")+
  NULL
```

## Email usage

I want to estimate the *population* parameter on time spent on email weekly.

###Create variable
Create a new variable called `email` that combines `emailhr` and `emailmin` to reports the number of minutes the respondents spend on email weekly.
```{r}
gss <- gss %>% 
  mutate(emailmin = as.numeric(emailmin), emailhr = as.numeric(emailhr)) %>% 
  mutate(email= emailmin + emailhr * 60)
gss
```
###Visualization
Visualise the distribution of this new variable. Find the mean and the median number of minutes respondents spend on email weekly. 
```{r}
gss_email <- gss %>% 
  drop_na(email) 

ggplot(gss_email, aes(x= email)) +
  geom_histogram() +
  theme_bw() +
  xlab("Time spent on email") +
  ylab("Frequancy") +
  ggtitle("Distribution of Email")
```

```{r}
summarise(gss_email, mean_mins_email =  mean(gss_email$email), median_mins_email = median(gss_email$email))
```
Median is a better measurement to represent the typical amount of time Americans spend on email weekly. This is because there are outliers on the weekly usage of email which skewed the mean and caused the standard devation to be high.

###Calculate CI with infer 
Using the `infer` package, calculate a 95% bootstrap confidence interval for the mean amount of time Americans spend on email weekly. 
```{r}
library(infer)
gss_boot_email <- gss %>% 
  drop_na(email) %>% 
  specify(response = email) %>% 
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

email_ci <- gss_boot_email %>% 
  get_confidence_interval(
    level = 0.95,
    type = "percentile",
    point_estimate = NULL
  ) 

email_lower_ci_hour <- email_ci$lower_ci%/%60
email_lower_ci_min <- email_ci$lower_cilower_ci%%60
email_upper_ci_hour <- email_ci$upper_ci%/%60
email_upper_ci_min <- email_ci$upper_ci%%60


paste("Lower 95% CI email usage:", email_ci$lower_ci%/%60 , "hours and", round(email_ci$lower_ci%%60, digits = 0), "minutes") 
paste("Upper 95% CI email usage:", email_ci$upper_ci%/%60, "hours and", round(email_ci$upper_ci%%60, digits = 0), "minutes") 
```

As comparison to the 95% confidence interval, I would expect the 99% confidence interval to be wider. For the 95% confidence interval, we are confident that 95% of the sample fall between 6 hours and 27 minutes and 7 hours and 29 minutes. Thus, the 99% confience interval would be wider as the lower interval would need to be smaller than 6 hours and 27 minutes and the upper interval would need to be larger than 7 hours and 29 minutes, resulting that we can predict with 99% confidence that the value within this range.

