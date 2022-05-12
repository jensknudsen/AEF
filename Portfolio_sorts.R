###################################################################################
library(quadprog)
library(tidyverse)
library(lubridate)
library(scales)
library(frenchdata)
library(readxl)
library(googledrive)
library(RSQLite)
library(dbplyr)
library(RPostgres)
library(tidyquant)
library(slider)
library(furrr)
library(lmtest)
library(purrr)
library(sandwich)
library(lmtest)
library(glue)
library(ggplot2)
library(patchwork)

#Import of data set 
tidy_finance <- dbConnect(SQLite(), "/Users/Jens/Desktop/AEF/data/tidy_finance.sqlite",
                          extended_types = TRUE
)

crsp_monthly <- tbl(tidy_finance, "crsp_monthly") %>% # collect the crsp_monthly data
  collect()

factors_ff_monthly <- tbl(tidy_finance, "factors_ff_monthly") %>% # collect the factors_ff_monthly data 
  collect()

###################################################################################
#EXERCISE 1
# Calculate the mktcap and add this to the crsp_monthly dataset: 
crsp_monthly <- crsp_monthly %>%
  mutate(
    mktcap = abs(shrout * altprc) / 1000000,
    mktcap = if_else(mktcap == 0, as.numeric(NA), mktcap)
  )

#use the left join to include the factors_ff_monthly in our dataset and select only some of the data:
crsp_monthly <- crsp_monthly %>%
  left_join(factors_ff_monthly, by = "month") %>%
  select(permno, month, ret_excess, mktcap, rf)


# Download the CPI to deflate the mktcap:
start_date <- as.Date("1960-01-01")
end_date <- as.Date("2020-12-31")

cpi_monthly <- tq_get("CPIAUCNS",
                      get = "economic.data",
                      from = start_date, to = end_date
) %>%
  transmute(
    month = floor_date(date, "month"),
    cpi = price / price[month == max(crsp_monthly$month)]
  )

#store the cpi_monthly in the database
cpi_monthly %>%
  dbWriteTable(tidy_finance, "cpi_monthly", ., overwrite = TRUE)

# leftjoin the cpi_monthly to the datafram crsp_mothly
crsp_monthly <- crsp_monthly %>%
  left_join(cpi_monthly, by = "month") 

#deflate the mktcap with the index equal to 2020-12-01
crsp_monthly <- crsp_monthly %>% 
  mutate(mktcap = mktcap/cpi)

# Not quite sure if you need to make summary statisitics across permno...?
# Make summary statistics for each of the variables (ret_excess and mktcap)
#summary statistics for ret_excess:
crsp_monthly %>%
  summarize(
    mean = mean(ret_excess),
    sd = sd(ret_excess),
    min = min(ret_excess),
    q05 = quantile(ret_excess, 0.05),
    q25 = quantile(ret_excess, 0.25),
    q50 = quantile(ret_excess, 0.50),
    q75 = quantile(ret_excess, 0.75),
    q95 = quantile(ret_excess, 0.95),
    max = max(ret_excess),
    n = n()
  )
#summary statistics for mktcap
crsp_monthly %>%
  summarize(
    mean = mean(mktcap),
    sd = sd(mktcap),
    min = min(mktcap),
    q05 = quantile(mktcap, 0.05),
    q25 = quantile(mktcap, 0.25),
    q50 = quantile(mktcap, 0.50),
    q75 = quantile(mktcap, 0.75),
    q95 = quantile(mktcap, 0.95),
    max = max(mktcap),
    n = n()
  )

# Make summary statistics (for both in one)
New_list <- crsp_monthly %>%
  select(ret_excess, mktcap)

funs <- lst(min, median, mean, max, sd)

map_dfr(funs,
        ~ summarize(New_list, across(where(is.numeric), .x, na.rm = TRUE)),
        .id = "statistic")


###################################################################################
#EXERCISE 2
#We add one month to the current date
ret_excess_lag <- crsp_monthly %>%
  mutate(month = month %m+% months(1)) %>%
  select(permno, month, ret_excess_lag = ret_excess) %>%
  drop_na()

#Join the resulting information with our return data with inner_join to remove NA values
crsp_monthly <- crsp_monthly %>%
  inner_join(ret_excess_lag, by = c("permno", "month"))

## visualisation of the present and lagged values of the re_excess
#crsp_monthly %>%
#  ggplot(aes(x = ret_excess_lag, y = ret_excess)) +
#  geom_point() +
#  geom_abline(aes(intercept = 0, slope = 1),
#              color = "red",
#              linetype = "dotted"
#  ) +
#  labs(
#    x = "Previous day aggregate trading volume (billion USD)",
#    y = "Aggregate trading volume (billion USD)",
#    title = "Persistence of trading volume"
#  )

#This is the ONE!
# make a pearson correlation test with p value
res <- cor.test(crsp_monthly$ret_excess_lag, crsp_monthly$ret_excess, 
                method = "pearson")
res
#The p-value of the test is < 2.2e-16, which is less than the significance level alpha = 0.05. 
#We can conclude that ret_excess_lag and ret_excess are significantly correlated with a correlation coefficient of -0.01685588 
#and p-value of < 2.2e-16.


#See whether they correlate
crsp_monthly %>%
  select(ret_excess_lag,ret_excess) %>%
  cor(., use = "complete.obs")
#

# make a lm-test to investegate the correlation:
coeftest(lm(ret_excess ~ ret_excess_lag, data = crsp_monthly)) 

#coeftest(lm(ret_excess_lag ~ ret_excess, data = crsp_monthly), vcov = NeweyWest)

#Null hypothesis is that the correlation is equal to zero
#We reject the nullhypothesis due to low p-value.
#We have a statistically significant result leading us to think that the nul-hypothesis is not true 
#meaning that we reject that the correlation is equal to zero. We have a significant excess return equal to -0.0168.



###################################################################################
#Excersice 3

#Generate a new columm name mktcap_lag_1:
mktcap_lag_1 <- crsp_monthly %>%
  mutate(month = month %m+% months(1)) %>%
  select(permno, month, mktcap_lag_1 = mktcap) %>%
  drop_na()

crsp_monthly <- crsp_monthly %>%
  inner_join(mktcap_lag_1, by = c("permno", "month"))


#Generate a new column name mktcap_lag_12
mktcap_lag_12 <- crsp_monthly %>%
  mutate(month = month %m+% months(12)) %>%
  select(permno, month, mktcap_lag_12 = mktcap) %>%
  drop_na()

crsp_monthly <- crsp_monthly %>%
  inner_join(mktcap_lag_12, by = c("permno", "month"))

#Creating new momentum variable
mom <- crsp_monthly %>%
  mutate(mom = 100*(mktcap_lag_1-mktcap_lag_12)/mktcap_lag_12) %>%
  select(permno, month, mom)

crsp_monthly <- crsp_monthly %>%
  inner_join(mom, by = c("permno", "month"))


#Tidligere definition af mom
#crsp_monthly <- crsp_monthly %>%
#group_by(permno,month) %>%
#mutate(mom = 100*(mktcap_lag_1-mktcap_lag_12)/mktcap_lag_12)

# Create summary statistics for Mom
summary_mom <- crsp_monthly %>%
  group_by(month) %>%
  summarize(
    mean = mean(mom),
    sd = sd(mom),
    min = min(mom),
    q05 = quantile(mom, 0.05),
    q25 = quantile(mom, 0.25),
    q50 = quantile(mom, 0.50),
    q75 = quantile(mom, 0.75),
    q95 = quantile(mom, 0.95),
    max = max(mom),
    n = n()
  )
summary_mom

# Report the time-series means for each cross-sectional value (also the sd- later question)
mean_of_mean <- sapply(summary_mom,FUN=mean)
mean_of_mean

#What is the mean value of Mom in the average month?
# it is 15.195711
#What is the cross-sectional standard deviation?
#it is 1.822643 

# make a pearson correlation test with p value
crsp_monthly <- crsp_monthly %>%
  mutate(logmc = log(mktcap))

corre <- cor.test(crsp_monthly$mom, crsp_monthly$logmc, 
                  method = "pearson")
corre
#The p-value of the test is < 2.2e-16, which is less than the significance level alpha = 0.05. 
#We can conclude that momentum and log(mc) are significantly correlated with a correlation coefficient of 0.06610802 
#and p-value of < 2.2e-16.

###################################################################################
#EXERCISE 4
#Make the function 

assign_portfolio <- function(data, var, n_portfolios) {
  breakpoints <- data %>%
    summarize(breakpoint = quantile({{ var }},
                                    probs = seq(0, 1, length.out = n_portfolios + 1),
                                    na.rm = TRUE
    )) %>%
    pull(breakpoint) %>%
    as.numeric()
  
  data %>%
    mutate(portfolio = findInterval({{ var }},
                                    breakpoints,
                                    all.inside = TRUE
    )) %>%
    pull(portfolio)
}

# present the equal-weighted average values of mom and mktcap
# Present the portfolios where we split up into portfolios based on the mom values (and summarize the corresponding mom and mktcap values)
m_portfolios <- crsp_monthly %>%
  group_by(month) %>%
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = mom,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio)
  ) %>%
  group_by(portfolio, month, permno) %>%
  summarize(mom,mktcap)

#count the number of individual stocks for each month in each portfolios 
port_month_count <- m_portfolios %>%
  group_by(portfolio, month) %>%
  summarize(count=n())

#count the total stocks in each month across all portfolio
total <- port_month_count %>%
  group_by(month) %>%
  summarize(total = sum(count))

#join the two results 
port_month_count <- port_month_count %>%
  inner_join(total, by = c("month"))

#join the result with the results from the calculation of the mom and mktcap 
m_portfolios <- m_portfolios %>% 
  inner_join(port_month_count, by = c("month","portfolio"))

#calculate each weight and multiply with the respective value of mom and mktcap
m_portfolios <- m_portfolios %>% 
  mutate(ew_mom = mom * (count/total)) %>%
  mutate(ew_mktcap = mktcap *(count/total))

#calculate the simple mean across each portfolio (this must be the equal-weighted average values for each of the 10 portfolios)
final <- m_portfolios %>% 
  group_by(portfolio) %>%
  summarise(ew_mom = mean(ew_mom),ew_mktcap = mean(ew_mktcap))


#Compute value-weighted monthly excess returns for the decile portfolios
mom_portfolios <- crsp_monthly %>%
  group_by(month) %>%
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = mom,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio)
  ) %>%
  group_by(portfolio, month) %>%
  summarize(ret = weighted.mean(ret_excess, mktcap_lag_1),mktcap,mom,permno, .groups = "drop")

# make mom_portfolios_summary where we group by the portfolio and summarise
mom_portfolios_summary <- mom_portfolios %>%
  left_join(factors_ff_monthly, by = "month") %>%
  group_by(portfolio) %>%
  summarise(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[2]),
    ret = mean(ret),
    mktcap_mean = mean(mktcap),
    mom_mean = mean(mom)
  )

# plot for alpha for each portfolio:
alpha_plot <- mom_portfolios_summary %>%
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Alphas of mom-sorted portfolios",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")

# plot for beta for each portfolio:
beta_plot <- mom_portfolios_summary %>%
  ggplot(aes(x = portfolio, y = beta, fill = portfolio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Betas of mom-sorted portfolios",
    x = "Portfolio",
    y = "CAPM beta",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")

#plot for ret for each portfolio:
ret_plot <- mom_portfolios_summary %>%
  ggplot(aes(x = portfolio, y = ret, fill = portfolio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Avg. return of mom-sorted portfolios",
    x = "Portfolio",
    y = "Return",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")

# plot for the marketcap for each portfolio
mktcap_plot <- mom_portfolios_summary %>%
  ggplot(aes(x = portfolio, y = mktcap_mean, fill = portfolio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Avg. mktcap of mom-sorted portfolios",
    x = "Portfolio",
    y = "mktcap",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")

# plot for the mom for each portfolio:
mom_plot <- mom_portfolios_summary %>%
  ggplot(aes(x = portfolio, y = mom_mean, fill = portfolio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Avg. mom of mom-sorted portfolios",
    x = "Portfolio",
    y = "Mom",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")


# Print the combined plots for the stats of each of the portfolio:
#Intall the following:

#plot the alpha blot as the main result in for the risk-adjusted returns:
alpha_plot
# plot the combined summary statistics as guidance to the other questions:
combined_plot <- beta_plot + ret_plot + mktcap_plot + mom_plot + 
  plot_layout(ncol = 2)
combined_plot

### This should be the text in the assignment: 
# The tables underneath are the summary statistics for the portfolios generated from momentum.
# We observe the value weighted realizations of the momentum sorted portfolios, and we observe that the sorting seems to be in accordance with the intended outcome.
# We observe from looking at the returns that the past winners (in relation to a high momentum strategy) perform better than past losers.
# This does not tells us if we get an higher compensation for taking on a higher risk, which is a crusial question we wish to ask ourselves. 
# The u-shape in the beta-table and the upward sloping tendency in returns.
# This makes us look at risk adjusted returns, the CAPM-alphas. This is our main result, indicating that pas losers sorted with mom are going to underpreform and past winners are going to overpreform. 

# For the turnover and the different strategy, the focus can be turned to the mktcap-figure.
# We observe that in both ends of the sorting the avg. mktcap is the smallest among the portfolio sorting. 
# This means that these portfolios are properly the most costly stocks to trade, and in the strategy proposed we would trade with these more costly stocks. 
# The sort strategy is clearly the most costly to trade (esspecially to sort sell) 
# go long on the high alpha and short on the low alphas
# We now test the hypothesis of going long and short (see example 3 for help)
mom_longshort <- mom_portfolios %>%
  ungroup() %>%
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high", # define "high" portfolio 
    portfolio == min(as.numeric(portfolio)) ~ "low" # define "low" portfolio 
  )) %>%
  filter(portfolio %in% c("low", "high")) %>%
  pivot_wider(month, names_from = portfolio, values_from = ret, values_fn = list(ret=list)) %>%
  mutate(long_short = high - low) %>% # this is the long short strategy
  inner_join(factors_ff_monthly, by = "month")

# Test if the average portfolio return is equal to zero via. lm() and make a test:
longshort_test <- coeftest(lm(long_short ~ 1, data = mom_longshort), vcov = NeweyWest)
longshort_test

# the CAPM result for the long short (see 3)
CAPM_test <- coeftest(lm(long_short ~ 1 + mkt_excess, data = mom_longshort), vcov = NeweyWest) 
CAPM_test 
# We observe that we get a negative statistically risk adjusted alpha for the portfolio:
# We also get a negative beta value, which indicates an inverse relation to the market.



# all code below is old code and should be deleted: 
#long_short <- mean(mom_portfolios_summary$ret[10] - mom_portfolios_summary$ret[1])

# Another way to get the same result as above (here we just take the difference between each month for the return and take the sum of all of these values)
#tester <- mom_portfolios %>%
#  filter(portfolio== 1 | portfolio == 10) %>%
#  group_by(portfolio, month)#

#svar <- tester$ret[tester$portfolio==10]-tester$ret[tester$portfolio==1]
#final <- mean(svar)

# We are also able to do a lm model where we also make a t-test to see that our result is significant!
#model_fit <- lm(svar ~ 1)
#coeftest(model_fit, vcov = NeweyWest, lag = 6)
#Our portfolio strategy using the highest and lowest decil as a breakpoint hence does yield an abnormal returns equal to 0.0070993 (0,709%) (almost one procent)
###################################################################################
# Exercise 5:
# Instead of moving the ret forward we will move the MOM backwards (this will yeild the sam result)
# All the following code takes 2-3 hours to run, i wish just to lag the mom column and add this column to the crsp_monthly data set:

mom_lag_1 <- crsp_monthly %>%
  mutate(month = month %m+% months(1)) %>%
  select(permno, month, mom_lag_1 = mom) %>%
  drop_na()

crsp_monthly <- crsp_monthly %>%
  inner_join(mom_lag_1, by = c("permno", "month"))

mom_lag_3 <- crsp_monthly %>%
  mutate(month = month %m+% months(3)) %>%
  select(permno, month, mom_lag_3 = mom) %>%
  drop_na()

crsp_monthly <- crsp_monthly %>%
  inner_join(mom_lag_3, by = c("permno", "month"))

mom_lag_6 <- crsp_monthly %>%
  mutate(month = month %m+% months(6)) %>%
  select(permno, month, mom_lag_6 = mom) %>%
  drop_na()

crsp_monthly <- crsp_monthly %>%
  inner_join(mom_lag_6, by = c("permno", "month"))

mom_lag_12 <- crsp_monthly %>%
  mutate(month = month %m+% months(12)) %>%
  select(permno, month, mom_lag_12 = mom) %>%
  drop_na()

crsp_monthly <- crsp_monthly %>%
  inner_join(mom_lag_12, by = c("permno", "month"))

# for the portfolio with lag 1:
lag_one <- crsp_monthly %>%
  group_by(month) %>%
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = mom_lag_1,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio)
  ) %>%
  group_by(portfolio, month) %>%
  summarize(ret = weighted.mean(ret_excess, mktcap_lag_1), .groups = "drop")

# Summary statistics for the portfolios:
lag_one_summary <- lag_one %>%
  left_join(factors_ff_monthly, by = "month") %>%
  group_by(portfolio) %>%
  summarise(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[2]),
    ret = mean(ret)
  )

#summary for long-short strategy
lag_1_long_short <- lag_one %>%
  ungroup() %>%
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) %>%
  filter(portfolio %in% c("low", "high")) %>%
  pivot_wider(month, names_from = portfolio, values_from = ret) %>%
  mutate(long_short = high - low) %>%
  left_join(factors_ff_monthly, by = "month")


lag_1_test <- coeftest(lm(long_short ~ 1 + mkt_excess, data = lag_1_long_short), vcov = NeweyWest)
lag_1_test


#for the portfolio with lag 3:
lag_three <- crsp_monthly %>%
  group_by(month) %>%
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = mom_lag_3,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio)
  ) %>%
  group_by(portfolio, month) %>%
  summarize(ret = weighted.mean(ret_excess, mktcap_lag_1), .groups = "drop")

lag_three_summary <- lag_three %>%
  left_join(factors_ff_monthly, by = "month") %>%
  group_by(portfolio) %>%
  summarise(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[2]),
    ret = mean(ret)
  )

#summary for long-short strategy
lag_3_long_short <- lag_three %>%
  ungroup() %>%
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) %>%
  filter(portfolio %in% c("low", "high")) %>%
  pivot_wider(month, names_from = portfolio, values_from = ret) %>%
  mutate(long_short = high - low) %>%
  left_join(factors_ff_monthly, by = "month")


lag_3_test <- coeftest(lm(long_short ~ 1 + mkt_excess, data = lag_3_long_short), vcov = NeweyWest)
lag_3_test


#for the portfolio with lag 6:
lag_six <- crsp_monthly %>%
  group_by(month) %>%
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = mom_lag_6,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio)
  ) %>%
  group_by(portfolio, month) %>%
  summarize(ret = weighted.mean(ret_excess, mktcap_lag_1), .groups = "drop")

lag_six_summary <- lag_six %>%
  left_join(factors_ff_monthly, by = "month") %>%
  group_by(portfolio) %>%
  summarise(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[2]),
    ret = mean(ret)
  )

lag_6_long_short <- lag_six %>%
  ungroup() %>%
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) %>%
  filter(portfolio %in% c("low", "high")) %>%
  pivot_wider(month, names_from = portfolio, values_from = ret) %>%
  mutate(long_short = high - low) %>%
  left_join(factors_ff_monthly, by = "month")


lag_6_test <- coeftest(lm(long_short ~ 1 + mkt_excess, data = lag_6_long_short), vcov = NeweyWest)
lag_6_test



# for the portfolio with lag 12:
lag_twelve <- crsp_monthly %>%
  group_by(month) %>%
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = mom_lag_12,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio)
  ) %>%
  group_by(portfolio, month) %>%
  summarize(ret = weighted.mean(ret_excess, mktcap_lag_1), .groups = "drop")

lag_twelve_summary <- lag_twelve %>%
  left_join(factors_ff_monthly, by = "month") %>%
  group_by(portfolio) %>%
  summarise(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[2]),
    ret = mean(ret)
  )

lag_12_long_short <- lag_twelve %>%
  ungroup() %>%
  mutate(portfolio = case_when(
    portfolio == max(as.numeric(portfolio)) ~ "high",
    portfolio == min(as.numeric(portfolio)) ~ "low"
  )) %>%
  filter(portfolio %in% c("low", "high")) %>%
  pivot_wider(month, names_from = portfolio, values_from = ret) %>%
  mutate(long_short = high - low) %>%
  left_join(factors_ff_monthly, by = "month")

lag_12_test <- coeftest(lm(long_short ~ 1 + mkt_excess, data = lag_12_long_short), vcov = NeweyWest)
lag_12_test


#Code for Claes for markdown:
##########################################################################################################################################################################################################################################
```{r}
# This code collects the risk-adjusted alpha estimates for each time horizon
test_1 <- lag_1_test %>% 
  broom::tidy() %>%
  add_column(Time_horizon = "t+1")

test_3 <- lag_3_test %>% 
  broom::tidy() %>%
  add_column(Time_horizon = "t+3")

test_6 <- lag_6_test %>% 
  broom::tidy() %>%
  add_column(Time_horizon = "t+6")

test_12 <- lag_12_test %>% 
  broom::tidy() %>%
  add_column(Time_horizon = "t+12")

t_ahead_table <- rbind(test_1, test_3, test_6, test_12)%>% #combine tables
  filter(term == "(Intercept)") %>%
  select(Time_horizon, estimate, std.error, statistic, p.value)

t_ahead_table %>%
  knitr::kable(digits=3,caption = "Time horizon long-short strategy performance") %>%
  kable_styling(latex_options = "HOLD_position")
```
##########################################################################################################################################################################################################################################

# the highest risk adjusted performance is the time horizon t+1 with an alpha equal to 0.0119139 

#########################################################################################################################################################################################################################################################
#Exercise 6
# alternative strategy where we only invest in the stocks where the marketcap is greater than the median
# lets say we only wish to trade stocks that are among the highest quantile:
upper_quantile <- quantile(crsp_monthly$mktcap)
upper_quantile[[4]]

alternative_portfolios <- subset(crsp_monthly, mktcap > upper_quantile[[4]])


alternative_portf <- alternative_portfolios %>%
  group_by(month) %>%
  mutate(
    portfolio = assign_portfolio(
      data = cur_data(),
      var = mom,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio)
  ) %>%
  group_by(portfolio, month) %>%
  summarize(month,permno, ret = weighted.mean(ret_excess, mktcap_lag_1),mom, mktcap,ret_excess)


new_alternative_summary <- alternative_portf %>%
  left_join(factors_ff_monthly, by = "month") %>%
  group_by(portfolio) %>%
  summarise(
    alpha = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[1]),
    beta = as.numeric(lm(ret ~ 1 + mkt_excess)$coefficients[2]),
    ret = mean(ret),
    mktcap_mean = mean(mktcap),
    mom_mean = mean(mom)
  )


# plot for alpha for each portfolio:
A_alpha_plot <- new_alternative_summary %>%
  ggplot(aes(x = portfolio, y = alpha, fill = portfolio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Alphas of mom-sorted alternative portfolios",
    x = "Portfolio",
    y = "CAPM alpha",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")

# plot for beta for each portfolio:
A_beta_plot <- new_alternative_summary %>%
  ggplot(aes(x = portfolio, y = beta, fill = portfolio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Betas of mom-sorted alternative portfolios",
    x = "Portfolio",
    y = "CAPM beta",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")

#plot for ret for each portfolio:
A_ret_plot <- new_alternative_summary %>%
  ggplot(aes(x = portfolio, y = ret, fill = portfolio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Avg. return of mom-sorted alternative portfolios",
    x = "Portfolio",
    y = "Return",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")

# plot for the marketcap for each portfolio
A_mktcap_plot <- new_alternative_summary %>%
  ggplot(aes(x = portfolio, y = mktcap_mean, fill = portfolio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Avg. mktcap of mom-sorted alternative portfolios",
    x = "Portfolio",
    y = "mktcap",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")

# plot for the mom for each portfolio:
A_mom_plot <- new_alternative_summary %>%
  ggplot(aes(x = portfolio, y = mom_mean, fill = portfolio)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Avg. mom of mom-sorted alternative portfolios",
    x = "Portfolio",
    y = "Mom",
    fill = "Portfolio"
  ) +
  theme(legend.position = "None")


#plot the alpha blot as the main result in for the risk-adjusted returns:
A_alpha_plot
# plot the combined summary statistics as guidance to the other questions:
A_combined_plot <- A_beta_plot + A_ret_plot + A_mktcap_plot + A_mom_plot + 
  plot_layout(ncol = 2)
A_combined_plot


# We now test the hypothesis of going long with sort of mktcap (see example 3 for help)
alternative_longshort <- alternative_portf[alternative_portf$portfolio == 10, ] %>%
  inner_join(factors_ff_monthly, by = "month")

  
# Test if the average portfolio return is equal to zero via. lm() and make a test:
A_longshort_test <- coeftest(lm(alternative_longshort$ret ~ 1, data = alternative_longshort), vcov = NeweyWest)
A_longshort_test

# the CAPM result for the long with sort of mktcap(see 3)
A_CAPM_test <- coeftest(lm(alternative_longshort$ret ~ 1 + mkt_excess, data = alternative_longshort), vcov = NeweyWest) 
A_CAPM_test 



#Calculate the sharpe ratio:
sharpe_ratio_old <- mean(mom_longshort$long_short)/sd(mom_longshort$long_short)
sharpe_ratio_old

sharpe_ratio_new <- mean(alternative_longshort$ret)/sd(alternative_longshort$ret)
sharpe_ratio_new

#Risk free rate
#crsp_monthly <- crsp_monthly %>%
#  mutate(rf=mean(rf))
#crsp_monthly

############################################################################################################################
#This is the code with the right nested loop that i cannot make work!!!!
#y <- list(n_permno_1,n_permno_2,n_permno_3,n_permno_4,n_permno_5,n_permno_6,n_permno_7,n_permno_8,n_permno_9,n_permno_10)
#x <- seq(1, length(n_permno_1)-1, by=1)
#turno = c()
#answer = c()
#
#for(element in y){
#  for(i in x){
#    combined <- c(element[[i]][[1]] , element[[i+1]][[1]])
#    new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
#    answer <- append(answer, new_value)
#    meannn <- mean(answer)
#    turno <- append(turno,meannn)
#    answer = c()
#  }
#}


###################################################################################
#6.1 code to find number of stocks in each portfolio at any time
# This code gives you the permno-code for each portfolio each month
# (you can call n_permno_(number from 1-10) to get all the individual permno codes for that portfolio) )
# We first perform this analysis on the alternative portfolio (where we discriminated against the size of the mctcap)
x <- c(1,2,3,4,5,6,7,8,9,10)
count <- 0
for (i in x) {
  X <- subset(alternative_portf, portfolio == i) %>%
    select(portfolio,month,permno) %>%
    group_by(portfolio,month) %>%
    group_split(.keep = FALSE)
  assign(paste0("n_permno_", i), X)
}
# We can run all the portfolio code to get the aveage turnover for each portfolio
# For portfolio 1:
x <- seq(1, length(n_permno_1)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_1[[i]][[1]] , n_permno_1[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover1 <- mean(answer)
turnover1

# For portfolio 2:
x <- seq(1, length(n_permno_2)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_2[[i]][[1]] , n_permno_2[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover2 <- mean(answer)
turnover2

# For portfolio 3:
x <- seq(1, length(n_permno_3)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_3[[i]][[1]] , n_permno_3[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover3 <- mean(answer)
turnover3

# For portfolio 4:
x <- seq(1, length(n_permno_4)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_4[[i]][[1]] , n_permno_4[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover4 <- mean(answer)
turnover4

# For portfolio 5:
x <- seq(1, length(n_permno_5)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_5[[i]][[1]] , n_permno_5[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover5 <- mean(answer)
turnover5


# For portfolio 6:
x <- seq(1, length(n_permno_6)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_6[[i]][[1]] , n_permno_6[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover6 <- mean(answer)
turnover6


# For portfolio 7:
x <- seq(1, length(n_permno_7)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_7[[i]][[1]] , n_permno_7[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover7 <- mean(answer)
turnover7


# For portfolio 8:
x <- seq(1, length(n_permno_8)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_8[[i]][[1]] , n_permno_8[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover8 <- mean(answer)
turnover8


# For portfolio 9:
x <- seq(1, length(n_permno_9)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_9[[i]][[1]] , n_permno_9[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover9 <- mean(answer)
turnover9


# For portfolio 10:
x <- seq(1, length(n_permno_10)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_10[[i]][[1]] , n_permno_10[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover10 <- mean(answer)
turnover10


# secondly we perform this on the baseline strategy: (we only change the dataframe to the m_portfolios dataframe) and now name our results turnover1_B for the baseline model:
x <- c(1,2,3,4,5,6,7,8,9,10)
count <- 0
for (i in x) {
  X <- subset(m_portfolios, portfolio == i) %>%
    select(portfolio,month,permno) %>%
    group_by(portfolio,month) %>%
    group_split(.keep = FALSE)
  assign(paste0("n_permno_", i), X)
}

# We can run all the portfolio code to get the aveage turnover for each portfolio
# For portfolio 1:
x <- seq(1, length(n_permno_1)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_1[[i]][[1]] , n_permno_1[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover1_B <- mean(answer)
turnover1_B

# For portfolio 2:
x <- seq(1, length(n_permno_2)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_2[[i]][[1]] , n_permno_2[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover2_B <- mean(answer)
turnover2_B

# For portfolio 3:
x <- seq(1, length(n_permno_3)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_3[[i]][[1]] , n_permno_3[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover3_B <- mean(answer)
turnover3_B

# For portfolio 4:
x <- seq(1, length(n_permno_4)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_4[[i]][[1]] , n_permno_4[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover4_B <- mean(answer)
turnover4_B

# For portfolio 5:
x <- seq(1, length(n_permno_5)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_5[[i]][[1]] , n_permno_5[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover5_B <- mean(answer)
turnover5_B


# For portfolio 6:
x <- seq(1, length(n_permno_6)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_6[[i]][[1]] , n_permno_6[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover6_B <- mean(answer)
turnover6_B


# For portfolio 7:
x <- seq(1, length(n_permno_7)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_7[[i]][[1]] , n_permno_7[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover7_B <- mean(answer)
turnover7_B


# For portfolio 8:
x <- seq(1, length(n_permno_8)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_8[[i]][[1]] , n_permno_8[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover8_B <- mean(answer)
turnover8_B


# For portfolio 9:
x <- seq(1, length(n_permno_9)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_9[[i]][[1]] , n_permno_9[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover9_B <- mean(answer)
turnover9_B


# For portfolio 10:
x <- seq(1, length(n_permno_10)-1, by=1)
answer = c()
for (i in x) {
  combined <- c(n_permno_10[[i]][[1]] , n_permno_10[[i+1]][[1]])
  new_value <- (1-length(combined[duplicated(combined)])/length(combined))*100
  answer <- append(answer, new_value)
}

turnover10_B <- mean(answer)
turnover10_B


# display all the results for each portfolio and the the average turnover for each of the strategies: 

average_turnover_S <- (turnover1 + turnover2 + turnover3 + turnover4 + turnover5 + turnover6 +turnover7 + turnover8 + turnover9 + turnover10)/10
average_turnover_B <- (turnover1_B + turnover2_B + turnover3_B + turnover4_B + turnover5_B + turnover6_B +turnover7_B + turnover8_B + turnover9_B + turnover10_B)/10

mean_1_and_10_B <- mean(turnover1_B,turnover10_B)


result_lag.data <- c(turnover1_B, turnover2_B, turnover3_B, turnover4_B, turnover5_B, turnover6_B, turnover7_B, turnover8_B, turnover9_B,turnover10_B, average_turnover_B,mean_1_and_10_B, turnover1, turnover2, turnover3, turnover4, turnover5, turnover6,turnover7, turnover8, turnover9,turnover10,average_turnover_S,turnover10)
result_lag <- matrix(result_lag.data,nrow=2,ncol=12,byrow=TRUE)
rownames(result_lag) <- c("Baseline","Alternative Strategy")
colnames(result_lag) <- c("Turnover p. 1","Turnover p. 2","Turnover p. 3","Turnover p. 4","Turnover p. 5","Turnover p. 6","Turnover p. 7","Turnover p. 8","Turnover p. 9","Turnover p. 10","Average Turnover","Turnover for Each Strategy")
df_turnover <- as.data.frame(result_lag)
df_turnover

# We observe that there is a higher turnover for the alternative stategy, but we make around 75 % less trades and therefore this but mean that trading cost are less.
