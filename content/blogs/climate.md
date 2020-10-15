---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description: ‘Climate change and temperature anomalies’
draft: false
image: pic08.jpg
keywords: ""
slug: Climate Change
title: Climate Change
---

I want to study climate change, with data on the *Combined Land-Surface Air and Sea-Surface Water Temperature Anomalies* in the Northern Hemisphere at [NASA's Goddard Institute for Space Studies](https://data.giss.nasa.gov/gistemp). The [tabular data of temperature anomalies can be found here](https://data.giss.nasa.gov/gistemp/tabledata_v3/NH.Ts+dSST.txt)

To define temperature anomalies I need to have a reference, or base, period which NASA clearly states that it is the period between 1951-1980.

Run the code below to load the file:

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

```{r weather_data, cache=TRUE}

weather <- 
  read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v3/NH.Ts+dSST.csv", 
           skip = 1, 
           na = "***")

```

Notice that, when using this function, I add two options: `skip` and `na`.

1. The `skip=1` option is there as the real data table only starts in Row 2, so I need to skip one row. 
1. `na = "***"` option informs R how missing observations in the spreadsheet are coded. When looking at the spreadsheet, I can see that missing data is coded as "***". It is best to specify this here, as otherwise some of the data is not recognized as numeric data.

For each month and year, the dataframe shows the deviation of temperature from the normal (expected). Further the dataframe is in wide format. 

I have two objectives in this section:

1. Select the year and the twelve month variables from the `weather` dataset. I do not need the others (J-D, D-N, DJF, etc.) for analysis. 

2. Convert the dataframe from wide to 'long' format. Name the new dataframe as `tidyweather`, name the variable containing the name of the month as `month`, and the temperature deviation values as `delta`.


```{r tidyweather}
tidyweather <- weather %>% 
  select(-c("J-D","D-N","DJF","MAM","JJA","SON")) %>% 
  pivot_longer(cols="Jan":"Dec", names_to = "Month", values_to = "delta")

tidyweather

```

Inspect dataframe. It has three variables now, one each for 

1. year, 
2. month, and 
3. delta, or temperature deviation.

## Plotting Information

First, plot the data using a time-series scatter plot, and add a trendline. To do that, I first need to create a new variable called `date` in order to ensure that the `delta` values are plot chronologically. 

```{r scatter_plot, eval=TRUE}

tidyweather <- tidyweather %>%
  mutate(date = ymd(paste(as.character(Year), Month, "1")),
         month = month(date, label=TRUE),
         year = year(date))

ggplot(tidyweather, aes(x=date, y = delta))+
  geom_point()+
  geom_smooth(color="red") +
  theme_bw() +
  labs (
    title = "Climate change is real!",
    subtitle = "The graph shows a growing deviation from the 1951 to 1981 reference period",
    x = "Date",
    y = "Delta"
  ) +
  theme_economist()

```

Is the effect of increasing temperature more pronounced in some months? I use `facet_wrap()` to produce a seperate scatter plot for each month, again with a smoothing line. 

```{r facet_wrap, echo=FALSE}

#Your code goes here...
ggplot(tidyweather, aes(x=year, y = delta))+
  geom_point()+
  geom_smooth(color="red") +
  theme_bw() +
  labs (
    title = "Weather anomalies are evident in all months",
    x = "Year",
    y = ""
  ) +
  facet_wrap(~month, scales = 'free', labeller = 'label_value') +
  theme_clean()
```


It is sometimes useful to group data into different time periods to study historical data. For example, we often refer to decades such as 1970s, 1980s, 1990s etc. to refer to a period of time. NASA calculates a temperature anomaly, as difference form the base period of 1951-1980. The code below creates a new data frame called `comparison` that groups data in five time periods: 1881-1920, 1921-1950, 1951-1980, 1981-2010 and 2011-present. 

I remove data before 1800 and before using `filter`. Then, I use the `mutate` function to create a new variable `interval` which contains information on which period each observation belongs to. I assign the different periods using `case_when()`.

```{r intervals}
comparison <- tidyweather %>% 
  filter(Year>= 1881) %>%     #remove years prior to 1881
  #create new variable 'interval', and assign values based on criteria below:
  mutate(interval = case_when(
    Year %in% c(1881:1920) ~ "1881-1920",
    Year %in% c(1921:1950) ~ "1921-1950",
    Year %in% c(1951:1980) ~ "1951-1980",
    Year %in% c(1981:2010) ~ "1981-2010",
    TRUE ~ "2011-present"
  ))
```

Inspect the `comparison` dataframe by clicking on it in the `Environment` pane.

Now that I have the `interval` variable and can create a density plot to study the distribution of monthly deviations (`delta`), grouped by the different time periods.

```{r density_plot}

ggplot(comparison, aes(x=delta, fill=interval))+
  geom_density(alpha=0.2) +   #density plot with tranparency set to 20%
  theme_bw() +                #theme
  labs (
    title = "Density Plot for Monthly Temperature Anomalies",
    y     = "Density"         #changing y-axis label to sentence case
  )

```

So far, I have been working with monthly anomalies. However, I'm also interested in average annual anomalies. Do this by using `group_by()` and `summarise()`, followed by a scatter plot to display the result. 

```{r averaging}

#creating yearly averages
average_annual_anomaly <- tidyweather %>% 
  group_by(Year) %>%   #grouping data by Year
  
  # creating summaries for mean delta 
  # use `na.rm=TRUE` to eliminate NA (not available) values 
  summarise(annual_average_delta = mean(delta, na.rm=TRUE)) 

#plotting the data:
ggplot(average_annual_anomaly, aes(x=Year, y= annual_average_delta))+
  geom_point()+
  
  #Fit the best fit line, using LOESS method
  geom_smooth() +
  
  #change to theme_bw() to have white background + black frame around plot
  theme_bw() +
  labs (
    title = "Climate change has been accelerating exponentially since 1960",
    y     = "Average Annual Delta"
  ) +
  theme_clean()


```

## Confidence Interval for `delta`

[NASA points out on their website](https://earthobservatory.nasa.gov/world-of-change/decadaltemp.php) that 

> A one-degree global change is significant because it takes a vast amount of heat to warm all the oceans, atmosphere, and land by that much. In the past, a one- to two-degree drop was all it took to plunge the Earth into the Little Ice Age.

I want to construct a confidence interval for the average annual delta since 2011, both using a formula and using a bootstrap simulation with the `infer` package. Recall that the dataframe `comparison` has already grouped temperature anomalies according to time intervals; we are only interested in what is happening  between 2011-present.

```{r, calculate_CI_using_formula}

formula_ci <- comparison %>% 
  filter(interval == "2011-present" & !is.na(delta)) %>% 
  summarise(mean_delta = mean(delta),
            sd_delta = sd(delta),
            count = n(),
            # get t-critical value with (n-1) degrees of freedom
            t_critical = qt(0.975, count-1),
            se_delta = sd(delta)/sqrt(count),
            margin_of_error = t_critical * se_delta,
            delta_low = mean_delta - margin_of_error,
            delta_high = mean_delta + margin_of_error
  ) 

#print out formula_CI
formula_ci
```


```{r, calculate_CI_using_bootstrap}

# use the infer package to construct a 95% CI for delta

set.seed(24)
comparison %>% 
  filter(interval == "2011-present" & !is.na(delta)) %>%
  specify(response = delta) %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "mean") %>% 
  visualise()

```
> The above graph shows that when repeated 1000 times, the data shows an almost normal distribution. and the mean is relatively close to the formula calculation at around 0.96. The more times the test is repeated, the closer the graph moves towards a normal distribution and the closer the mean moves towards the formula calculated value.