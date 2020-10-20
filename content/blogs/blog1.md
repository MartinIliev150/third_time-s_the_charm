---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: ""
draft: false
image: pic10.jpg
keywords: ""
slug: ipsum
title: Analyzing returns of financial stocks vs S&P500
---


```{r, setup}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, warning=FALSE, message=FALSE, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(fivethirtyeight)
library(here)
library(skimr)
library(janitor)
library(vroom)
library(tidyquant)
library(ggrepel)
```




# Returns of financial stocks


> You may find useful the material on [finance data sources](https://mam2021.netlify.app/reference/finance_data/). 

```{r load_nyse_data, message=FALSE, warning=FALSE}
nyse <- read_csv(here::here("data","nyse.csv"))
nyse
```


```{r companies_per_sector}

companies_per_sector <- nyse %>%
	group_by(sector) %>%
	summarise(No_companies = n(), .groups = 'drop') %>%
	arrange(desc(No_companies))
companies_per_sector

companies_per_sector %>%
  slice_max(No_companies, n=12)%>%
  ggplot(aes(x = No_companies  , y = reorder(sector, No_companies)))+ geom_col(fill="blue") + 
  labs( x = " Number of Companies", y = " ")



```

Next, let's choose some stocks and their ticker symbols and download some data. You **MUST** choose 6 different stocks from the ones listed below; You should, however, add `SPY` which is the SP500 ETF (Exchange Traded Fund).


```{r get_price_data, message=FALSE, warning=FALSE, cache=TRUE}

myStocks <- c("AAPL","JPM","HAL","GS","KKR","TSLA","ING","CS", "SPY" ) %>%
  tq_get(get  = "stock.prices",
         from = "2011-01-01",
         to   = "2020-08-31") %>%
  group_by(symbol) 

glimpse(myStocks) 
```


```{r calculate_returns, message=FALSE, warning=FALSE, cache=TRUE}
#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

#calculate yearly returns
myStocks_returns_annual <- myStocks %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic",
               col_rename = "yearly_returns",
               cols = c(nested.col))
```


```{r summarise_monthly_returns}

summarise_monthly_returns <- myStocks_returns_monthly %>%
  filter(date >= "2017-01-01")  %>%
  group_by(symbol) %>%
  summarize(min = min(monthly_returns), max =max(monthly_returns), median =median(monthly_returns), mean= mean(monthly_returns), sd=sd(monthly_returns), .groups = 'drop')
summarise_monthly_returns


```

```{r density_monthly_returns}

summ_monthly_returns_plot <- myStocks_returns_monthly %>%
  filter(date >= "2017-01-01")  %>%
  group_by(symbol) 
 summ_monthly_returns_plot  
 
ggplot(summ_monthly_returns_plot, aes(x = monthly_returns, color = symbol))+ geom_density() +labs( x = " Returns", y = " Density")+facet_wrap(~symbol)

```

## Distribution of Returns and Risk

The spread of returns (a.k.a the standard deviation) is the most widely used form of risk measure when it comes to stock market performance. The narrower the spread the less risk is involved. From the above graph we can see that the SPY is the safest among the chosen instruments, but that is hardly surprising given the fact that an index aggregates the performance of a wide basket of securities. 

One of the most risky assets on the graph above is Tesla Inc., as indicated by the spread of its returns. Keeping this in mind its also worthwhile to note that riskier stocks also carry the potential for a higher reward, as indicated by the considerable density of the Tesla distribution at and above 3%, compared to the almost zero density of the SPY at the same mark.


```{r risk_return_plot}

ggplot(data= summarise_monthly_returns, mapping = aes(x = sd, y = mean, color = symbol, label= symbol)) +
geom_point() + geom_text_repel() + theme(legend.position="none") + labs( x = " Risk ", y = " Return")



```

## Risk vs Return Trade Off

The risk return relationship is best exemplified by the graph above, which is a scater plot of the expected return vs standard deviation of said returns for specific securities. Ideally an investor would like to get a high return for a considerably low amount of risk, and position himself in the top left quadrant of the graph. 

On the other hand, every investor would avoid the lower right quadrant, which equals low expected returns and high risk. There are also those securities which although bearing a similar amount of risk, have different expected returns. For example - Goldman Sachs Inc. and Apple have roughly the same risk profile, and yet Apple's expected return is much higher than that of GS.

