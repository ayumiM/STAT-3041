#final project

library(tidyverse)
load('C:~/final project/covid19_dataset.Rdata')

#objective 1
ThreeState <- dataset%>%
  filter(date == '2020-04-13') %>%
  arrange(desc(total_cases))

HighestStates <- ThreeState[1:3,]
HighestStates$state

#get total cases and total death for each states
NewYorkData <- HighestStates%>%
  filter(state == 'New York')

NewJerseyData <- HighestStates%>%
  filter(state == 'New Jersey')

MassachusettsData <- HighestStates%>%
  filter(state == 'Massachusetts')

NewYorkTotalCases <- NewYorkData$total_cases
NewYorkTotalDeath <- NewYorkData$total_deaths

NewJerseyTotalCases <- NewJerseyData$total_cases
NewJerseyTotalDeath <- NewJerseyData$total_deaths

MassachusettsTotalCases <- MassachusettsData$total_cases
MassachusettsTotalDeath <- MassachusettsData$total_deaths

#creadible interval for New York
paste('Bayesian CI New York Lower Limit', qbeta(0.025, 1/3+NewYorkTotalDeath, 1/3+NewYorkTotalCases-NewYorkTotalDeath), 
      'Upper Limit:', qbeta(0.975, 1/3+NewYorkTotalDeath, 1/3+NewYorkTotalCases-NewYorkTotalDeath))

#creadible interval for New Jersey
paste('Bayesian CI New Jersey Lower Limit', qbeta(0.025, 1/3+NewJerseyTotalDeath, 1/3+NewJerseyTotalCases-NewJerseyTotalDeath), 
      'Upper Limit:', qbeta(0.975, 1/3+NewJerseyTotalDeath, 1/3+NewJerseyTotalCases-NewJerseyTotalDeath))

#credible interval for Massachusetts
paste('Bayesian CI Massachusetts Lower Limit', qbeta(0.025, 1/3+MassachusettsTotalDeath, 1/3+MassachusettsTotalCases-MassachusettsTotalDeath), 
      'Upper Limit:', qbeta(0.975, 1/3+MassachusettsTotalDeath, 1/3+MassachusettsTotalCases-MassachusettsTotalDeath))

# plot of death probability
demo <- tribble(
  ~State, ~DeathProb,
  "New York", NewYorkTotalDeath/NewYorkTotalCases,
  "New Jersey", NewJerseyTotalDeath/NewJerseyTotalCases,
  "Massachusetts", MassachusettsTotalDeath/MassachusettsTotalCases
)
ggplot(data = demo)+
  geom_bar(mapping = aes(x = State, y=DeathProb, fill = State ), stat = 'identity')

-------------------------------------------------------------------------------------------------------
#objective 2
#get a data 
NewYorkDailyData <- dataset %>%
  filter((date<="2020-04-13"&date>="2020-04-07"), state == "New York")

NewJerseyDailyData <- dataset %>%
  filter((date<="2020-04-13"&date>="2020-04-07"), state == "New Jersey")

MassachusettsDailyData <- dataset %>%
  filter((date<="2020-04-13"&date>="2020-04-07"), state == "Massachusetts")

#compute mean of daily cases 
NYmean <- mean(NewYorkDailyData$daily_cases)
NJmean <- mean(NewJerseyDailyData$daily_cases)
MAmean <- mean(MassachusettsDailyData$daily_cases)

#bootstrapping and confidence interval
n = nrow(NewYorkDailyData)
M <- 1000000

#new york
NY_bar = rep(NA, M)

for (i in 1:M) {
  NY_sample <- rpois(n, NYmean)
  NY_bar[i] <- mean(NY_sample)
}

#confidence interval 
c(quantile(NY_bar, 0.025), quantile(NY_bar, 0.975))

#credible interval and prediction interval for NY
NYDailyCasesSum <- sum(NewYorkDailyData$daily_cases)
paste('Bayesian NY CI Lower Limit:', qgamma(0.025, 1/3+NYDailyCasesSum, 0+n),
      'Upper Limit:', qgamma(0.975, 1/3+NYDailyCasesSum, 0+n))

NY_lambda_noninfo <- rgamma(M,1/3+NYDailyCasesSum, 0+n)
NY_info <- rpois(M, NY_lambda_noninfo)
paste('Bayesian NY PI Lower Limit:', quantile(NY_info,0.025),'Upper Limit:', quantile(NY_info, 0.975))

#new jersey
NJ_bar = rep(NA, M)

for (i in 1:M) {
  NJ_sample <- rpois(n, NJmean)
  NJ_bar[i] <- mean(NJ_sample)
}
#confidence interval 
c(quantile(NJ_bar, 0.025), quantile(NJ_bar, 0.975))

#credible interval and prediction interval for NJ
NJDailyCasesSum <- sum(NewJerseyDailyData$daily_cases)
paste('Bayesian NJ CI Lower Limit:', qgamma(0.025, 1/3+NJDailyCasesSum, 0+n),
      'Upper Limit:', qgamma(0.975, 1/3+NJDailyCasesSum, 0+n))

NJ_lambda_noninfo <- rgamma(M,1/3+NJDailyCasesSum, 0+n)
NJ_info <- rpois(M, NJ_lambda_noninfo)
paste('Bayesian NJ PI Lower Limit:', quantile(NJ_info,0.025),'Upper Limit:', quantile(NJ_info, 0.975))

#massachusetts
MA_bar = rep(NA, M)

for (i in 1:M) {
  MA_sample <- rpois(n, MAmean)
  MA_bar[i] <- mean(MA_sample)
}
#confidence interval
c(quantile(MA_bar, 0.025), quantile(MA_bar, 0.975))

#credible interval and prediction interval for MA 
MADailyCasesSum <- sum(MassachusettsDailyData$daily_cases)
paste('Bayesian MA CI Lower Limit:', qgamma(0.025, 1/3+MADailyCasesSum, 0+n),
      'Upper Limit:', qgamma(0.975, 1/3+MADailyCasesSum, 0+n))

MA_lambda_noninfo <- rgamma(M,1/3+MADailyCasesSum, 0+n)
MA_info <- rpois(M, MA_lambda_noninfo)
paste('Baysian MA PI Lower Limit:', quantile(MA_info,0.025),'Upper Limit:', quantile(MA_info, 0.975))

#plot 
plotdata <- dataset %>%
  filter((date<="2020-04-13"&date>="2020-04-07"), (state == "New York" | state == 'New Jersey' | state == 'Massachusetts'))

ggplot(data = plotdata, mapping = aes(x = date, y = daily_cases, color = state))+
  geom_point()+
  geom_smooth()


------------------------------------------------------------------------------------------------------
#objective 3
#get the dataset
#new york
NYGrowthRate1 <- dataset %>%
  filter((date<="2020-04-06"& date>="2020-03-31"), state == "New York")

NYGR1 <- NYGrowthRate1$daily_rates

NYGrowthRate2 <- dataset %>%
  filter((date<="2020-04-13"&date>="2020-04-07"), state == "New York")

NYGR2 <- NYGrowthRate2$daily_rates

#new jersey
NJGrowthRate1 <- dataset %>%
  filter((date<="2020-04-06"& date>="2020-03-31"), state == "New Jersey")

NJGR1 <- NJGrowthRate1$daily_rates

NJGrowthRate2 <- dataset %>%
  filter((date<="2020-04-13"&date>="2020-04-07"), state == "New Jersey")

NJGR2 <- NJGrowthRate2$daily_rates

#Massachusetts 
MAGrowthRate1 <- dataset %>%
  filter((date<="2020-04-06"& date>="2020-03-31"), state == "Massachusetts")

MAGR1 <- MAGrowthRate1$daily_rates

MAGrowthRate2 <- dataset %>%
  filter((date<="2020-04-13"&date>="2020-04-07"), state == "Massachusetts")

MAGR2 <- MAGrowthRate2$daily_rates

#data for bootstrapping
n = nrow(NYGrowthRate1)
M <- 1000000

#NY bootstrapping
#3/31/2020-4/6/2020 
NY_bar1 <- rep(NA, M)

for (i in 1:M) {
  NY_sample1 <- sample(NYGR1, n, replace = TRUE)
  
  NY_bar1[i] <- mean(NY_sample1)
}

#confidence interval 
c(quantile(NY_bar1, 0.025), quantile(NY_bar1, 0.975))

#4/7/2020-4/13/2020 
NY_bar2 <- rep(NA, M)

for (i in 1:M) {
  NY_sample2 <- sample(NYGR2, n, replace = TRUE)
  
  NY_bar2[i] <- mean(NY_sample2)
}

#confidence interval 
c(quantile(NY_bar2, 0.025), quantile(NY_bar2, 0.975))


#NJ bootstrapping
#3/31/2020-4/6/2020 
NJ_bar1 <- rep(NA, M)

for (i in 1:M) {
  NJ_sample1 <- sample(NJGR1, n, replace = TRUE)
  
  NJ_bar1[i] <- mean(NJ_sample1)
}

#confidence interval 
c(quantile(NJ_bar1, 0.025), quantile(NJ_bar1, 0.975))

#4/7/2020-4/13/2020 
NJ_bar2 <- rep(NA, M)

for (i in 1:M) {
  NJ_sample2 <- sample(NJGR2, n, replace = TRUE)
  
  NJ_bar2[i] <- mean(NJ_sample2)
}

#confidence interval 
c(quantile(NJ_bar2, 0.025), quantile(NJ_bar2, 0.975))


#MA bootstrapping
#3/31/2020-4/6/2020 
MA_bar1 <- rep(NA, M)

for (i in 1:M) {
  MA_sample1 <- sample(MAGR1, n, replace = TRUE)
  
  MA_bar1[i] <- mean(MA_sample1)
}

#confidence interval 
c(quantile(MA_bar1, 0.025), quantile(MA_bar1, 0.975))

#4/7/2020-4/13/2020 
MA_bar2 <- rep(NA, M)

for (i in 1:M) {
  MA_sample2 <- sample(MAGR2, n, replace = TRUE)
  
  MA_bar2[i] <- mean(MA_sample2)
}

#confidence interval 
c(quantile(MA_bar2, 0.025), quantile(MA_bar2, 0.975))

#plot 
plotdata2 <- dataset %>%
  filter((date>="2020-03-31"), (state == "New York" | state == 'New Jersey' | state == 'Massachusetts'))

ggplot(data = plotdata2, mapping = aes(x = date, y = daily_rates, color = state))+
  geom_point()+
  geom_smooth()

  



