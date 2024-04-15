#read packages
library(data.table)
library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)

#import data
rm(list=ls())

setwd("/Users/zhouhui/Desktop/Harvard/2023-2024 spring/GHP201")

#generate income distribution
set.seed(123) 

# set parameters for income distribution
#Average income source: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=IN
#GINI source: https://data.worldbank.org/indicator/SI.POV.GINI?locations=IN
avg_income <- 3500
gini <- 0.347 #G=1−1/2k (k=the shape parameter of the gamma distribution)

# set gamma distribution parameters
phi <- 1/gini
beta <- (1/phi)*(avg_income)
pop_size <- 100000000 # total population = 100million

# generate gamma distribution for income
incomes <- rgamma(pop_size,phi,rate=1/beta)

# extract income quintile cutoffs
#qt <- quantile(sort(incomes), probs=seq(0,1, by=0.20), na.rm=TRUE) 

income_data <- data.frame(income = incomes)
income_data <- income_data %>% 
  mutate(quintile = ntile(income, 5))

#generate cutoffs for each quintile
quintile_cutoffs <- quantile(income_data$income, probs = seq(0, 1, by = 0.20), na.rm = TRUE)

#read household survey microdata
data <- read_dta("data_br_nfhs4.dta")

#caseid is the household id, for every individual child in the household, we need to generate a unique id
data <- data %>%
  group_by(caseid) %>%
  mutate(birthid = row_number()) %>%
  mutate(id = paste0(caseid, birthid)) %>%
  ungroup()

#remove those who did not report place of delivery and whose place of delivery is other places than public facility, private facility, and home
data_1 <- data %>%
  filter(if_all(c(m15), ~ !is.na(.))) %>%
  filter(m15 !=96) 

#transform m15-place of delivery into binary variables: public facility delivery (20-29), private facility delivery (30-39), home delivery (11,12,13)
data_1 <- data_1 %>%
  mutate(
    public_facility_delivery = (m15 >= 20 & m15 <= 29),
    private_facility_delivery = (m15 >= 30 & m15 <= 39),
    home_delivery = (m15 == 11 | m15 == 12 | m15 == 13)
  ) 

#oop-data cleaning
#home delivery reference: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6166517/#:~:text=The%20mean%20expense%20for%20a,the%20home%20(Rs%201888) * 1.08 (increase in average household income)
data_1 <- data_1 %>%
  mutate(across(c(s448aa, s448ba, s448bb, s448bc, s448bd),
                ~ if_else(. == 99998, NA_real_, .))) %>%
  mutate(
    oop_health = s448ba + s448bb + s448bc,
    oop_non_health = s448aa + s448bd
  ) %>%
  mutate(oop_health = if_else(home_delivery == 1 & is.na(oop_health) == TRUE, 3756, oop_health),
         oop_non_health = if_else(home_delivery == 1 & is.na(oop_health) == TRUE, 293, oop_non_health)) %>%
  mutate(oop_total = oop_health + oop_non_health)

data_1 <- data_1 %>%
  mutate(across(.cols = c("oop_health", "oop_non_health", "oop_total"), 
                ~ ifelse(is.na(.), 0, .)))

#jsy-data cleaning
data_1 <- data_1 %>%
  mutate(jsy = ifelse(home_delivery==1, 0, s452)) %>%
  mutate(jsy = if_else(is.na(jsy),0,jsy))

#assign income based on original wealth index and basic parameters according to gamma distribution
data_counts <- data_1 %>%
  ungroup() %>%
  count(v190)

pop_incomes <- vector("list", length = 5)
names(pop_incomes) <- paste0("Quintile_", 1:5)

qt <- quantile(income_data$income, probs = seq(0, 1, by = 0.20), na.rm = TRUE)

for (i in 1:5) {
  required_n <- data_counts$n[data_counts$v190 == i]
  incomes <- income_data$income[income_data$quintile == i]
  if (i == 1) {
    # First quintile: incomes less than the first quintile cutoff
    pop_incomes[[i]] <- sample(incomes[incomes < qt[i+1]], size = required_n, replace = TRUE)
  } else if (i == 5) {
    # Last quintile: incomes greater than the last quintile cutoff
    pop_incomes[[i]] <- sample(incomes[incomes > qt[i]], size = required_n, replace = TRUE)
  } else {
    # Middle quintiles
    pop_incomes[[i]] <- sample(incomes[incomes < qt[i+1] & incomes > qt[i]], size = required_n, replace = TRUE)
  }
}

# Combine all incomes into one dataframe
sampled_incomes <- do.call(rbind, lapply(1:5, function(i) {
  data.frame(income_quintile = i, income = pop_incomes[[i]])
}))

# Create an auxiliary index for each income_quintile in both dataframes
sampled_incomes$aux_index <- ave(sampled_incomes$income_quintile, sampled_incomes$income_quintile, FUN = seq_along)
data_1$aux_index <- ave(data_1$v190, data_1$v190, FUN = seq_along)

data_1 <- data_1 %>%
  mutate(income_quintile = v190)

# Merge the dataframes using income_quintile and the auxiliary index
merged_data <- merge(data_1, sampled_incomes[, c("income", "income_quintile", "aux_index")],
                     by = c("income_quintile", "aux_index"))

# Drop the auxiliary index if it's no longer needed
merged_data$aux_index <- NULL

#generate dataset for ECEA analysis
analysis_data <- merged_data %>%
  mutate(income = ifelse(income < 200, 200, income)) %>%
  select(nfhs, id, income_quintile, income, public_facility_delivery, private_facility_delivery, home_delivery, oop_health, oop_non_health, oop_total, jsy, v131, s116, m17)

#plotting simulated distribution of income
plot(density(analysis_data$income),main="Individual income distribution",xlab='Income ($)',lwd=3)	

#exchange rate
analysis_data <- analysis_data %>%
  mutate(oop_health = oop_health * 0.012,
         oop_non_health = oop_non_health * 0.012,
         oop_total = oop_total * 0.012)

#setting a threshold of total household consumption expenditures
threshold_10 <- 0.10
threshold_25 <- 0.25

#Incidence of CHE tied to OOP payments
#estimating the ratio of monthly OOP medical payments to monthly consumption expenditures 
analysis_data <- analysis_data %>%
  mutate(oop_total_ratio = oop_total/income,
         oop_health_ratio = oop_health/income,
         oop_non_health_ratio = oop_non_health/income) %>%
  filter(oop_total_ratio <= 1 & oop_health_ratio <= 1 & oop_non_health_ratio <= 1)

plot(density(analysis_data$oop_total_ratio,bw=0.005),xlim=c(0,1),
     main="Ratio of Total OOP to income",
     lwd=3,xlab='Ratio value')
abline(v=0.10,lwd=2,col='red')
abline(v=0.25,lwd=2,col='blue')
legend("topright", c("10% threshold","25% threshold"), pch=15, 
       col=c("red","blue"), 
       bty="n")

#computing CHE cases for distinct thresholds
che_10_total <- analysis_data %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  filter(oop_total_ratio>threshold_10) %>%
  summarise(n_10_total = n(),
            oop_10_sum = sum(oop_total))

che_25_total <- analysis_data %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  filter(oop_total_ratio>threshold_25) %>%
  summarise(n_25_total = n(),
            oop_25_sum = sum(oop_total))

che_10_health <- analysis_data %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  filter(oop_health_ratio>threshold_10) %>%
  summarise(n_10_health = n())

che_25_health <- analysis_data %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  filter(oop_health_ratio>threshold_25) %>%
  summarise(n_25_health = n())

che_10_non_health <- analysis_data %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  filter(oop_non_health_ratio>threshold_10) %>%
  summarise(n_10_non_health = n())

che_25_non_health <- analysis_data %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  filter(oop_non_health_ratio>threshold_25) %>%
  summarise(n_25_non_health = n())

number_quintile <- analysis_data %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  summarise(n_quintile = n())

nfhs4_che_table <- number_quintile %>%
  left_join(che_10_total, by = c("income_quintile", "public_facility_delivery", "private_facility_delivery", "home_delivery")) %>%
  left_join(che_25_total, by = c("income_quintile", "public_facility_delivery", "private_facility_delivery", "home_delivery")) %>%
  left_join(che_10_health, by = c("income_quintile", "public_facility_delivery", "private_facility_delivery", "home_delivery")) %>%
  left_join(che_25_health, by = c("income_quintile", "public_facility_delivery", "private_facility_delivery", "home_delivery")) %>%
  left_join(che_10_non_health, by = c("income_quintile", "public_facility_delivery", "private_facility_delivery", "home_delivery")) %>%
  left_join(che_25_non_health, by = c("income_quintile", "public_facility_delivery", "private_facility_delivery", "home_delivery")) 

nfhs4_che_prop <- nfhs4_che_table %>%
  mutate(che_10_total_prop = n_10_total/n_quintile,
         che_25_total_prop = n_25_total/n_quintile,
         che_10_health_prop = n_10_health/n_quintile,
         che_25_health_prop = n_25_health/n_quintile,
         che_10_non_health_prop = n_10_non_health/n_quintile,
         che_25_non_health_prop = n_25_non_health/n_quintile) %>%
  select(-n_10_total,-n_25_total,-n_10_health,-n_25_health,-n_10_non_health,-n_25_non_health)

#for nfhs5, average income increases from 1600 to 2000
#generate income distribution
set.seed(123) 

# set parameters for income distribution
#Average income source: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=IN
#GINI source: https://data.worldbank.org/indicator/SI.POV.GINI?locations=IN
avg_income_intervention <- 3800
gini_intervention <- 0.338 #G=1−1/2k (k=the shape parameter of the gamma distribution)
pop_size <- 100000000 # total population = 100million

# set gamma distribution parameters
phi_intervention <- 1/gini_intervention
beta_intervention <- (1/phi_intervention)*(avg_income_intervention)

# generate gamma distribution for income
incomes_intervention <- rgamma(pop_size,phi_intervention,rate=1/beta_intervention)

# extract income quintile cutoffs
#qt <- quantile(sort(incomes), probs=seq(0,1, by=0.20), na.rm=TRUE) 

income_data_intervention <- data.frame(income = incomes_intervention)
income_data_intervention <- income_data_intervention %>% 
  mutate(quintile = ntile(income, 5))

#generate cutoffs for each quintile
quintile_cutoffs_intervention <- quantile(income_data_intervention$income, probs = seq(0, 1, by = 0.20), na.rm = TRUE)

#import nfhs5 data
data_intervention <- read_dta("data_br_nfhs5.dta")

#caseid is the household id, for every individual child in the household, we need to generate a unique id
data_intervention <- data_intervention %>%
  group_by(caseid) %>%
  mutate(birthid = row_number()) %>%
  mutate(id = paste0(caseid, birthid))

#remove those who did not report place of delivery and whose place of delivery is other places than public facility, private facility, and home
data_intervention_1 <- data_intervention %>%
  filter(if_all(c(m15), ~ !is.na(.))) %>%
  filter(m15 !=96) 

#transform m15-place of delivery into binary variables: public facility delivery, private facility delivery, home delivery
data_intervention_1 <- data_intervention_1 %>%
  mutate(
    public_facility_delivery = (m15 >= 20 & m15 <= 29),
    private_facility_delivery = (m15 >= 30 & m15 <= 39),
    home_delivery = (m15 == 11 | m15 == 12 | m15 == 13)
  )

#oop-data cleaning
data_intervention_1 <- data_intervention_1 %>%
  mutate(across(c(s451, s452a, s452b, s452c, s452d),
                ~ if_else(. == 99998, NA_real_, .))) %>%
  mutate(
    oop_health = s452a + s452b + s452c,
    oop_non_health = s451 + s452d,
  ) %>%
  mutate(oop_health = if_else(home_delivery == 1 & is.na(oop_health) == TRUE, 4057, oop_health),
         oop_non_health = if_else(home_delivery == 1 & is.na(oop_health) == TRUE, 315, oop_non_health)) %>%
  mutate(oop_total = oop_health + oop_non_health)

data_intervention_1 <- data_intervention_1 %>%
  mutate(across(.cols = c("oop_health", "oop_non_health", "oop_total"), 
                ~ ifelse(is.na(.), 0, .)))

#jsy-data cleaning
data_intervention_1 <- data_intervention_1 %>%
  mutate(jsy = ifelse(home_delivery==1, 0, s458a)) %>%
  mutate(jsy = if_else(is.na(jsy),0,jsy))

#assign income based on original wealth index and basic parameters according to gamma distribution
data_counts_intervention <- data_intervention_1 %>%
  ungroup() %>%
  count(v190)

pop_incomes_intervention <- vector("list", length = 5)
names(pop_incomes_intervention) <- paste0("Quintile_", 1:5)

qt_intervention <- quantile(income_data_intervention$income, probs = seq(0, 1, by = 0.20), na.rm = TRUE)

for (i in 1:5) {
  required_n <- data_counts_intervention$n[data_counts_intervention$v190 == i]
  incomes <- income_data_intervention$income[income_data_intervention$quintile == i]
  if (i == 1) {
    # First quintile: incomes less than the first quintile cutoff
    pop_incomes_intervention[[i]] <- sample(incomes[incomes < qt_intervention[i+1]], size = required_n, replace = TRUE)
  } else if (i == 5) {
    # Last quintile: incomes greater than the last quintile cutoff
    pop_incomes_intervention[[i]] <- sample(incomes[incomes > qt_intervention[i]], size = required_n, replace = TRUE)
  } else {
    # Middle quintiles
    pop_incomes_intervention[[i]] <- sample(incomes[incomes < qt_intervention[i+1] & incomes > qt_intervention[i]], size = required_n, replace = TRUE)
  }
}

# Combine all incomes into one dataframe
sampled_incomes_intervention <- do.call(rbind, lapply(1:5, function(i) {
  data.frame(income_quintile = i, income = pop_incomes_intervention[[i]])
}))

# Create an auxiliary index for each income_quintile in both dataframes
sampled_incomes_intervention$aux_index <- ave(sampled_incomes_intervention$income_quintile, sampled_incomes_intervention$income_quintile, FUN = seq_along)
data_intervention_1$aux_index <- ave(data_intervention_1$v190, data_intervention_1$v190, FUN = seq_along)

data_intervention_1 <- data_intervention_1 %>%
  mutate(income_quintile = v190)

# Merge the dataframes using income_quintile and the auxiliary index
merged_data_intervention <- merge(data_intervention_1, sampled_incomes_intervention[, c("income", "income_quintile", "aux_index")],
                     by = c("income_quintile", "aux_index"))

# Drop the auxiliary index if it's no longer needed
merged_data_intervention$aux_index <- NULL

#generate dataset for ECEA analysis
analysis_data_intervention <- merged_data_intervention %>%
  mutate(ifelse(income < 200, 200, income)) %>%
  select(nfhs, id, income_quintile, income, public_facility_delivery, private_facility_delivery, home_delivery, oop_health, oop_non_health, oop_total, jsy, v131, s116, m17)

#exchange rate
analysis_data_intervention <- analysis_data_intervention %>%
  mutate(oop_health = oop_health * 0.012,
         oop_non_health = oop_non_health * 0.012,
         oop_total = oop_total * 0.012)

#Incidence of CHE tied to OOP payments
#estimating the ratio of monthly OOP medical payments to monthly consumption expenditures 
analysis_data_intervention <- analysis_data_intervention %>%
  mutate(oop_total_ratio = oop_total/income,
         oop_health_ratio = oop_health/income,
         oop_non_health_ratio = oop_non_health/income) %>%
  filter(oop_total_ratio <= 1 & oop_health_ratio <= 1 & oop_non_health_ratio <= 1)

plot(density(analysis_data_intervention$oop_total_ratio,bw=0.005),xlim=c(0,1),
     main="Ratio of Total OOP to income",
     lwd=3,xlab='Ratio value')
abline(v=0.10,lwd=2,col='red')
abline(v=0.25,lwd=2,col='blue')
legend("topright", c("10% threshold","25% threshold"), pch=15, 
       col=c("red","blue"), 
       bty="n")

#computing CHE cases for distinct thresholds
che_10_total_intervention <- analysis_data_intervention %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  filter(oop_total_ratio>threshold_10) %>%
  summarise(n_10_total = n(),
            oop_10_sum = sum(oop_total))

che_25_total_intervention <- analysis_data_intervention %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  filter(oop_total_ratio>threshold_25) %>%
  summarise(n_25_total = n(),
            oop_25_sum = sum(oop_total))

che_10_health_intervention <- analysis_data_intervention %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  filter(oop_health_ratio>threshold_10) %>%
  summarise(n_10_health = n())

che_25_health_intervention <- analysis_data_intervention %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  filter(oop_health_ratio>threshold_25) %>%
  summarise(n_25_health = n())

che_10_non_health_intervention <- analysis_data_intervention %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  filter(oop_non_health_ratio>threshold_10) %>%
  summarise(n_10_non_health = n())

che_25_non_health_intervention <- analysis_data_intervention %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  filter(oop_non_health_ratio>threshold_25) %>%
  summarise(n_25_non_health = n())

number_quintile_intervention <- analysis_data_intervention %>%
  group_by(income_quintile, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  summarise(n_quintile = n())

nfhs5_che_table <- number_quintile_intervention %>%
  left_join(che_10_total_intervention, by = c("income_quintile", "public_facility_delivery", "private_facility_delivery", "home_delivery")) %>%
  left_join(che_25_total_intervention, by = c("income_quintile", "public_facility_delivery", "private_facility_delivery", "home_delivery")) %>%
  left_join(che_10_health_intervention, by = c("income_quintile", "public_facility_delivery", "private_facility_delivery", "home_delivery")) %>%
  left_join(che_25_health_intervention, by = c("income_quintile", "public_facility_delivery", "private_facility_delivery", "home_delivery")) %>%
  left_join(che_10_non_health_intervention, by = c("income_quintile", "public_facility_delivery", "private_facility_delivery", "home_delivery")) %>%
  left_join(che_25_non_health_intervention, by = c("income_quintile", "public_facility_delivery", "private_facility_delivery", "home_delivery")) 

nfhs5_che_prop <- nfhs5_che_table %>%
  mutate(che_10_total_prop = n_10_total/n_quintile,
         che_25_total_prop = n_25_total/n_quintile,
         che_10_health_prop = n_10_health/n_quintile,
         che_25_health_prop = n_25_health/n_quintile,
         che_10_non_health_prop = n_10_non_health/n_quintile,
         che_25_non_health_prop = n_25_non_health/n_quintile) %>%
  select(-n_10_total,-n_25_total,-n_10_health,-n_25_health,-n_10_non_health,-n_25_non_health)

#scenario2: based on nfhs5, add intervention characteristics: 
#1. for those who did not receive jsy, expand the coverage to 1.2times (use )
#2. benefit increase?
#3. from private to public?

#figures for poster
#figure1. public/private/home delivery by quintile
#nfhs4
data_delivery_graph <- analysis_data %>%
  group_by(income_quintile) %>%
  summarise(n = n(),
            prop_public = sum(public_facility_delivery)/n,
            prop_private = sum(private_facility_delivery)/n,
            prop_home = sum(home_delivery)/n,
            prop = prop_public+prop_private+prop_home
            )

data_delivery_graph_long <- reshape2::melt(data_delivery_graph, id.vars = 'income_quintile', 
                          measure.vars = c('prop_public', 'prop_private', 'prop_home'),
                          variable.name = 'place_of_delivery', value.name = 'percentage')

data_delivery_graph_long$place_of_delivery <- factor(data_delivery_graph_long$place_of_delivery,
                                    levels = c('prop_public', 'prop_private', 'prop_home'),
                                    labels = c('Public Facility', 'Private Facility', 'Home Delivery'))

ggplot(data_delivery_graph_long, aes(x = factor(income_quintile), y = percentage, fill = place_of_delivery)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_manual(values = c("#fc8d59", "#99d594", "#3288bd"),
                    name = "Place of Delivery",
                    labels = c("Public Facility", "Private Facility", "Home Delivery")) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0), labels = function(x) paste0("Q", x)) +
  labs(x = 'Quintile', y = 'Proportion of Births', fill = 'Place of Delivery', title = "(a) NFHS4") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(size = 12, hjust = 0))

#nfhs5
data_delivery_graph_intervention <- analysis_data_intervention %>%
  group_by(income_quintile) %>%
  summarise(n = n(),
            prop_public = sum(public_facility_delivery)/n,
            prop_private = sum(private_facility_delivery)/n,
            prop_home = sum(home_delivery)/n,
            prop = prop_public+prop_private+prop_home
  )

data_delivery_graph_long_intervention <- reshape2::melt(data_delivery_graph_intervention, id.vars = 'income_quintile', 
                                           measure.vars = c('prop_public', 'prop_private', 'prop_home'),
                                           variable.name = 'place_of_delivery', value.name = 'percentage')

data_delivery_graph_long_intervention$place_of_delivery <- factor(data_delivery_graph_long_intervention$place_of_delivery,
                                                     levels = c('prop_public', 'prop_private', 'prop_home'),
                                                     labels = c('Public Facility', 'Private Facility', 'Home Delivery'))

ggplot(data_delivery_graph_long_intervention, aes(x = factor(income_quintile), y = percentage, fill = place_of_delivery)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_manual(values = c("#fc8d59", "#99d594", "#3288bd"),
                    name = "Place of Delivery",
                    labels = c("Public Facility", "Private Facility", "Home Delivery")) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0), labels = function(x) paste0("Q", x)) +
  labs(x = 'Quintile', y = 'Proportion of Births', fill = 'Place of Delivery', title = "(b) NFHS5") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(size = 12, hjust = 0))

#concentration curve for oop spending
#nfhs4
con_curve_oop <- analysis_data %>%
  group_by(income_quintile) %>%
  summarise(oop_sum = sum(oop_health)) %>%
  mutate(oop_cumsum = cumsum(oop_sum)) %>%
  mutate(q_prop = income_quintile / max(income_quintile),
         oop_cumprop = oop_cumsum / max(oop_cumsum)) %>%
  add_row(income_quintile = 0,
          oop_cumsum = 0,
          q_prop = 0,
          oop_cumprop = 0,
  )

con_curve_oop %>%
  ggplot(aes(x = q_prop, y = oop_cumprop)) +
  geom_point() +
  geom_line() +
  geom_abline(intercept = 0, slope = 1,linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2)) +
  theme_classic() +
  labs(title = "(a) NFHS4 - Concentration curve for OOP health spending",
       x = "Relative proportion (%) of population (poorest to richest)",
       y = "Cumulative proportion (%) of OOP spending"
  )

con_curve_oop_total <- analysis_data %>%
  group_by(income_quintile) %>%
  summarise(oop_sum = sum(oop_total)) %>%
  mutate(oop_cumsum = cumsum(oop_sum)) %>%
  mutate(q_prop = income_quintile / max(income_quintile),
         oop_cumprop = oop_cumsum / max(oop_cumsum)) %>%
  add_row(income_quintile = 0,
          oop_cumsum = 0,
          q_prop = 0,
          oop_cumprop = 0,
  )

con_curve_oop_total %>%
  ggplot(aes(x = q_prop, y = oop_cumprop)) +
  geom_point() +
  geom_line() +
  geom_abline(intercept = 0, slope = 1,linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2)) +
  theme_classic() +
  labs(title = "(b) NFHS4 - Concentration curve for total OOP spending",
       x = "Relative proportion (%) of population (poorest to richest)",
       y = "Cumulative proportion (%) of OOP spending"
  )

#nfhs5
con_curve_oop_intervention <- analysis_data_intervention %>%
  group_by(income_quintile) %>%
  summarise(oop_sum = sum(oop_health)) %>%
  mutate(oop_cumsum = cumsum(oop_sum)) %>%
  mutate(q_prop = income_quintile / max(income_quintile),
         oop_cumprop = oop_cumsum / max(oop_cumsum)) %>%
  add_row(income_quintile = 0,
          oop_cumsum = 0,
          q_prop = 0,
          oop_cumprop = 0,
  )

con_curve_oop_intervention %>%
  ggplot(aes(x = q_prop, y = oop_cumprop)) +
  geom_point() +
  geom_line() +
  geom_abline(intercept = 0, slope = 1,linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2)) +
  theme_classic() +
  labs(title = "(c) NFHS5 - Concentration curve for OOP health spending",
       x = "Relative proportion (%) of population (poorest to richest)",
       y = "Cumulative proportion (%) of OOP spending"
  )

con_curve_oop_total_intervention <- analysis_data_intervention %>%
  group_by(income_quintile) %>%
  summarise(oop_sum = sum(oop_total)) %>%
  mutate(oop_cumsum = cumsum(oop_sum)) %>%
  mutate(q_prop = income_quintile / max(income_quintile),
         oop_cumprop = oop_cumsum / max(oop_cumsum)) %>%
  add_row(income_quintile = 0,
          oop_cumsum = 0,
          q_prop = 0,
          oop_cumprop = 0,
  )

con_curve_oop_total_intervention %>%
  ggplot(aes(x = q_prop, y = oop_cumprop)) +
  geom_point() +
  geom_line() +
  geom_abline(intercept = 0, slope = 1,linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2)) +
  theme_classic() +
  labs(title = "(d) NFHS5 - Concentration curve for total OOP spending",
       x = "Relative proportion (%) of population (poorest to richest)",
       y = "Cumulative proportion (%) of OOP spending"
  )


#jsy reception by quintile 
#nfhs4
con_curve_jsy_total <- analysis_data %>%
  group_by(income_quintile) %>%
  summarise(jsy_sum = sum(jsy)) %>%
  mutate(jsy_cumsum = cumsum(jsy_sum)) %>%
  mutate(q_prop = income_quintile / max(income_quintile),
         jsy_cumprop = jsy_cumsum / max(jsy_cumsum)) %>%
  add_row(income_quintile = 0,
          jsy_cumsum = 0,
          q_prop = 0,
          jsy_cumprop = 0,
  )

con_curve_jsy_total %>%
  ggplot(aes(x = q_prop, y = jsy_cumprop)) +
  geom_point() +
  geom_line() +
  geom_abline(intercept = 0, slope = 1,linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2)) +
  theme_classic() +
  labs(title = "(a) NFHS4 - Concentration curve for JSY Reception",
       x = "Relative proportion (%) of population (poorest to richest)",
       y = "Cumulative proportion (%) of JSY recipients"
  )

#nfhs5
con_curve_jsy_total_intervention <- analysis_data_intervention %>%
  group_by(income_quintile) %>%
  summarise(jsy_sum = sum(jsy)) %>%
  mutate(jsy_cumsum = cumsum(jsy_sum)) %>%
  mutate(q_prop = income_quintile / max(income_quintile),
         jsy_cumprop = jsy_cumsum / max(jsy_cumsum)) %>%
  add_row(income_quintile = 0,
          jsy_cumsum = 0,
          q_prop = 0,
          jsy_cumprop = 0,
  )

con_curve_jsy_total_intervention %>%
  ggplot(aes(x = q_prop, y = jsy_cumprop)) +
  geom_point() +
  geom_line() +
  geom_abline(intercept = 0, slope = 1,linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2)) +
  theme_classic() +
  labs(title = "(b) NFHS5 - Concentration curve for JSY Reception",
       x = "Relative proportion (%) of population (poorest to richest)",
       y = "Cumulative proportion (%) of JSY recipients"
  )

#prop of households with che in difference scenario by quintile
#nfhs4
nfhs4_che_prop$che_10_total_prop[is.na(nfhs4_che_prop$che_10_total_prop)] <- 0
nfhs4_che_prop$che_25_total_prop[is.na(nfhs4_che_prop$che_25_total_prop)] <- 0

graph_che_10_total <- nfhs4_che_prop %>%
  select(income_quintile, che_10_total_prop, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  mutate(delivery_scenario = case_when(
    public_facility_delivery ~ "Public Facility",
    private_facility_delivery ~ "Private Facility",
    home_delivery ~ "Home Delivery",
    TRUE ~ as.character(NA)  # For rows where none of the above is TRUE
  )) %>%
  select(-public_facility_delivery, -private_facility_delivery, -home_delivery)  

ggplot(graph_che_10_total, aes(x = as.factor(income_quintile), y = che_10_total_prop, group = delivery_scenario, color = delivery_scenario)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Public Facility" = "#fc8d59", "Private Facility" = "#99d594", "Home Delivery" = "#3288bd")) +
  labs(
    title = "(a) NFHS4",
    x = "Quintile",
    y = "Proportion of Households with CHE (10% threshold)",
    color = "Place of Delivery"
  ) +
  theme_classic()

#nfhs5
nfhs5_che_prop$che_10_total_prop[is.na(nfhs5_che_prop$che_10_total_prop)] <- 0
nfhs5_che_prop$che_25_total_prop[is.na(nfhs5_che_prop$che_25_total_prop)] <- 0

graph_che_10_total_intervention <- nfhs5_che_prop %>%
  select(income_quintile, che_10_total_prop, public_facility_delivery, private_facility_delivery, home_delivery) %>%
  mutate(delivery_scenario = case_when(
    public_facility_delivery ~ "Public Facility",
    private_facility_delivery ~ "Private Facility",
    home_delivery ~ "Home Delivery",
    TRUE ~ as.character(NA)  # For rows where none of the above is TRUE
  )) %>%
  select(-public_facility_delivery, -private_facility_delivery, -home_delivery)  

ggplot(graph_che_10_total_intervention, aes(x = as.factor(income_quintile), y = che_10_total_prop, group = delivery_scenario, color = delivery_scenario)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Public Facility" = "#fc8d59", "Private Facility" = "#99d594", "Home Delivery" = "#3288bd")) +
  labs(
    title = "(b) NFHS5",
    x = "Quintile",
    y = "Proportion of Households with CHE (10% threshold)",
    color = "Place of Delivery"
  ) +
  theme_classic()



#end of codes


#below are draft codes for potential use maybe?
#understanding what is involved
plot(density(data_ratio$ratio,bw=0.005),xlim=c(0,1),
     main="OOP medical & transport expenditures to income",
     lwd=3,xlab='Ratio value')
points(density(data_ratio$ratio_health,bw=0.005),xlim=c(0,1),type='l',
       lty=2,lwd=3,xlab='Ratio value')
abline(v=0.10,lwd=2,col='red')
abline(v=0.25,lwd=2,col='blue')
legend("topright", c("10% threshold","25% threshold",
                     "OOP medical costs only","Both OOP and transport costs"),
       lty=c(1,1,1,2),lwd=2, 
       col=c("red","blue","black","black"), 
       bty="n")

data_for_barplot <- t(data_matrix[, c('oop_health', 'oop_total')])

barplot(data_for_barplot,beside=T,col=c('pink','red'),
        ylab='Incidence (% of households)',xlab='Threshold of 10 or 25%')
legend("topright", c("OOP medical costs only","OOP total costs"),
       col=c("pink","red"),pch=15, 
       bty="n")

#death distribution across quintiles
#higher probability of death in poorer income quintiles
#source: Randive et al. (2014)
#with different MMR (320; 300; 280; 260; 240)                                    

data_intervention_1 <- data_intervention_1 %>%
  mutate(death.quint = ifelse(income_quintile==1, rbinom(n.quint[1],1,0.0032), 
                              ifelse(income_quintile==2, rbinom(n.quint[2],1,0.0030),
                                     ifelse(income_quintile==3, rbinom(n.quint[3],1,0.0028),
                                            ifelse(income_quintile==4,rbinom(n.quint[4],1,0.0026),
                                                   rbinom(n.quint[5],1,0.0024))))))

data_intervention_1 <- data_intervention_1[order(data_intervention_1$income), ]

#Calculate cumulative deaths within each quintile
data_intervention_1 <- data_intervention_1 %>%
  group_by(income_quintile) %>%
  mutate(cum_death_quint = cumsum(death.quint)) %>%
  ungroup()

#number of deaths by income quintile (using the diff function)
death_intervention_counts <- data_intervention_1 %>%
  group_by(income_quintile) %>%
  summarise(total_deaths = sum(death.quint))

death_intervention_counts$difference_in_deaths <- c(0, diff(death_intervention_counts$total_deaths))

# Plot distribution of mortality
plot(death_intervention_counts$total_deaths,
     pch = 19, col = "purple", type='b',
     main = "Deaths, per income quintile", 
     xlab = "Deaths (from poorest to richest income quintile)",
     ylab='Number of deaths',
     lwd=3)

#understanding what is involved
plot(density(data_intervention_ratio$ratio,bw=0.005),xlim=c(0,1),
     main="OOP medical & transport expenditures to income",
     lwd=3,xlab='Ratio value')
points(density(data_ratio$ratio_health,bw=0.005),xlim=c(0,1),type='l',
       lty=2,lwd=3,xlab='Ratio value')
abline(v=0.10,lwd=2,col='red')
abline(v=0.25,lwd=2,col='blue')
legend("topright", c("10% threshold","25% threshold",
                     "OOP medical costs only","Both OOP and transport costs"),
       lty=c(1,1,1,2),lwd=2, 
       col=c("red","blue","black","black"), 
       bty="n")


data_for_barplot_intervention <- t(data_intervention_matrix[, c('oop_health', 'oop_total')])

barplot(data_for_barplot_intervention,beside=T,col=c('pink','red'),
        ylab='Incidence (% of households)',xlab='Threshold of 10 or 25%')
legend("topright", c("OOP medical costs only","OOP total costs"),
       col=c("pink","red"),pch=15, 
       bty="n")

#CHE in different delivery methods
data_10_intervention_delivery <- data_intervention_ratio %>%
  filter(ratio>0.1) %>%
  group_by(public_facility_delivery, private_facility_delivery, home_delivery) %>%
  summarise(n = n())

data_25_intervention_delivery <- data_intervention_ratio %>%
  filter(ratio>0.25) %>%
  group_by(public_facility_delivery, private_facility_delivery, home_delivery) %>%
  summarise(n = n())

#incidence of facility birth
table_birth <- data_analysis %>%
  group_by(public_facility_delivery, private_facility_delivery, home_delivery) %>%
  summarise(n=n())

table_birth_public <- data_ratio %>%
  group_by(public_facility_delivery) %>%
  filter(ratio <=1 & ratio >=0.1) %>%
  summarise(n=n())

table_intervention_birth <- data_intervention_analysis %>%
  group_by(public_facility_delivery, private_facility_delivery, home_delivery) %>%
  summarise(n=n())

table_quintile_10 <- data_ratio %>%
  filter(ratio <=1 & ratio >=0.1) %>%
  group_by(income_quintile) %>%
  summarise(n=n())

table_quintile_25 <- data_ratio %>%
  filter(ratio <=1 & ratio >=0.25) %>%
  group_by(income_quintile) %>%
  summarise(n=n())

table_quintile_intervention_10 <- data_intervention_ratio %>%
  filter(ratio <=1 & ratio >=0.1) %>%
  group_by(income_quintile) %>%
  summarise(n=n())

table_quintile_intervention_25 <- data_intervention_ratio %>%
  filter(ratio <=1 & ratio >=0.25) %>%
  group_by(income_quintile) %>%
  summarise(n=n())

#death distribution across quintiles
#higher probability of death in poorer income quintiles
#source: Randive et al. (2014)
#with different MMR (320; 300; 280; 260; 240)                                    

data_1 <- data_1 %>%
  mutate(death.quint = ifelse(income_quintile==1, rbinom(n.quint[1],1,0.0032), 
                              ifelse(income_quintile==2, rbinom(n.quint[2],1,0.0030),
                                     ifelse(income_quintile==3, rbinom(n.quint[3],1,0.0028),
                                            ifelse(income_quintile==4,rbinom(n.quint[4],1,0.0026),
                                                   rbinom(n.quint[5],1,0.0024))))))

data_1 <- data_1[order(data_1$income), ]

#Calculate cumulative deaths within each quintile
data_1 <- data_1 %>%
  group_by(income_quintile) %>%
  mutate(cum_death_quint = cumsum(death.quint)) %>%
  ungroup()

#number of deaths by income quintile (using the diff function)
death_counts <- data_1 %>%
  group_by(income_quintile) %>%
  summarise(total_deaths = sum(death.quint))

death_counts$difference_in_deaths <- c(0, diff(death_counts$total_deaths))

# Plot distribution of mortality
plot(death_counts$total_deaths,
     pch = 19, col = "purple", type='b',
     main = "Deaths, per income quintile", 
     xlab = "Deaths (from poorest to richest income quintile)",
     ylab='Number of deaths',
     lwd=3)
  