# Library/Paths -----------------------------------------------------------
rm(list = ls())
gc(reset = T) # memory free-up
library(haven) # read_sav
library(dplyr) # mutate, ...
library(lfe) # felm
library(magrittr) # %>% 
library(lubridate) # date
library(DescTools) # winsorize
library(expss)

my_path <- r'(C:\Users\Arnaud\OneDrive\Bureau\cultural finance\Projet)'

# declare useful functions
my_wins <- function(x, lvl, vars){
  for(v in vars){
    x[[v]] <- Winsorize(x[[v]], probs=c(lvl, 1-lvl), na.rm=T) # Inf values are winsorized
  }
  return(x)
}

standardize <- function(x){
  m <- mean(x, na.rm=T)
  s <- sd(x, na.rm=T)
  y <- (x-m)/s
  return(y)
}

scale <- function(x){
  mi <- min(x, na.rm=T)
  ma <- max(x, na.rm=T)
  y <- (x-mi)/(ma-mi)
  return(y)
}

# Data Preparation --------------------------------------------------------
# read and winsorize
data <- read_sav(file.path(my_path, 'PANDA data_2021_11_17.sav'))
data <- my_wins(data, 0.05, c('eu_inc_ppp', 'ppl_hh')) # some crazy values to change
data <- my_wins(data, 0.01, c('age')) # less crazy values

# Attitude toward learning:
#degree -> 1 to 5 scale (5=PhD or equivalent)
# no clue what is the difference with hdegree but the last one has less nan values
#ALA4 -> 1 to 5 scale (5=Agree with Somebody who studies hard should be admired)
#ALC2 -> 1 to 5 scale (5=wants to be firend with diligent student)
# unigrade -> 1 to 100 (100=best grade)
# patience (MS X.T1) -> 1 = a payment of 1700 this month / 2 = a payment of 1900 next month 
# eu_inc_ppp = monthly income adjusted for difference in purchasing power (CONTROL)
# ppl_hh = n_pers in household (CONTROL)
educ_cols <- c('hdegree', 'degree', 'ALA4', 'ALC2', 'unigrade')


#######################################################################
## TEST 1: Link between highest diploma obtained and patience
#######################################################################

# Data preparation
test1 <- data %>% 
  mutate(wealth_hh_per_ppl = log(eu_inc_ppp/ppl_hh),
         date = lubridate::dmy(substr(datetime, start=1, stop=11)),
         year = lubridate::year(date),
         is_patient = case_when(patience==1 ~ 0, T ~ 1),
         age = log(age)) %>% 
         #across(all_of(educ_cols), ~ scale(.x), .names = '{.col}')) %>% 
  select(is_patient, wealth_hh_per_ppl, date, cntry, all_of(educ_cols), age, year)

# drop nan
colSums(is.na(test1))
test1_col1 <- na.omit(test1)
test1_col2 <- na.omit(test1[, c('is_patient', 'wealth_hh_per_ppl', 'date', 
                              'cntry', 'hdegree', 'age', 'year')])

# regression
formula_1_col1 <- as.formula(paste(
  'is_patient ~ 1 +', paste(educ_cols, collapse='+'), 
  '+ wealth_hh_per_ppl + age | 0 | 0 |cntry', sep=''))
reg_1_col1 <- felm(formula_1_col1, data=test1_col1)
summary(reg_1_col1)

formula_1_col2 <- as.formula(paste(
  'is_patient ~ hdegree',
  '+ wealth_hh_per_ppl + age |cntry| 0 |cntry', sep=''))
reg_1_col2 <- felm(formula_1_col2, data=test1_col2)
summary(reg_1_col2)

# Arnaud: I am not sure whether, we should consider only 'degree' or other variables
# which have many nan values. We could combine them also
# We see in summary(reg_1_col2), that degree is positively linked to patience
# the effect is robust to country fixed effects which include time-invariants country
# charcateristics (some aspects of culture should therefore be captured)

#######################################################################
## TEST 2: Link between highest diploma obtained and patience per Country
#######################################################################

# Data preparation
test2 <- data %>% 
  mutate(wealth_hh_per_ppl = log(eu_inc_ppp/ppl_hh),
         date = lubridate::dmy(substr(datetime, start=1, stop=11)),
         is_patient = case_when(patience==1 ~ 0, T ~ 1),
         age = log(age)) %>% 
         #across(all_of(educ_cols), ~ standardize(.x), .names = '{.col}')) %>% 
  select(is_patient, wealth_hh_per_ppl, date, cntry, hdegree, age, bachelor)

# barplot
test2_plot <- test2 %>% group_by(cntry) %>% 
  mutate(
    mean_is_patient = mean(is_patient, na.rm=T),
    mean_bachelor = mean(bachelor, na.rm=T)) %>% ungroup() %>% 
  select(cntry, mean_bachelor, mean_is_patient) %>% unique() %>% 
  arrange(cntry)
barplot(cbind(mean_is_patient, mean_bachelor) ~ cntry, data=test2_plot, 
        beside=T, names.arg=c('Germany', 'Estonia', 'Taiwan', 'China', 
                              'Vietnam', 'Japan', 'Hong Kong'),
        xlab = '', ylab = 'Proportion', col=c('dark blue', 'light blue'), 
        ylim=c(0,1))
legend('top', col=c('dark blue', 'light blue'), 
       c('Is_patient', 'Has_bachelor'), lwd=2, inset = c(0, -0.02), ncol = 2,
       box.lty = 0)

# scatterplot
scatter.smooth(test2_plot$mean_bachelor, test2_plot$mean_is_patient, span = 1,
               degree = 1, xlab = 'Proportion of people having a bachelor degree',
               ylab = 'Proportion of people choosing to wait')

# drop nan
colSums(is.na(test2))
test2 <- na.omit(test2[c('is_patient', 'wealth_hh_per_ppl', 'date', 'cntry', 
                         'hdegree', 'age')])

# run regression for each country
countries <- sort(as.numeric(unique(test2$cntry)))
formula_2 <- as.formula('is_patient ~ 1 + hdegree + wealth_hh_per_ppl + age')
results_2 <- list()

for(i in seq(1, length(countries))){
  ci <- countries[[i]]
  sub <- test2[test2$cntry==ci,]
  reg_2 <- lm(formula_2, data=sub)
  coef_2 <- summary(reg_2)$coefficients
  results_2[[i]] <- coef_2
}

# Arnaud: Within a country, degree is not linked to patience
# Differences in patience seems to be linked with country cultural difference
# and not so much by personal traits
# maybe hdegree proxies some cultural traits that are predicitve of patience

#######################################################################
## TEST 3: Is the link between education and patience stronger for some countries?
#######################################################################

# Data preparation
test3 <- data %>% 
  mutate(wealth_hh_per_ppl = log(eu_inc_ppp/ppl_hh),
         date = lubridate::dmy(substr(datetime, start=1, stop=11)),
         year = lubridate::year(date),
         is_patient = case_when(patience==1 ~ 0, T ~ 1),
         age = log(age)) %>% 
         #across(all_of(educ_cols), ~ standardize(.x), .names = '{.col}')) %>% 
  select(is_patient, wealth_hh_per_ppl, date, cntry, hdegree, age, year)

# drop nan
colSums(is.na(test3))
test3 <- na.omit(test3)

# create interactions terms within some countries
test3 <- test3 %>% mutate(
  hdegree_estonia = hdegree * case_when(cntry == 2 ~ 1, T ~ 0),
  hdegree_taiwan = hdegree * case_when(cntry == 3 ~ 1, T ~ 0),
  hdegree_china = hdegree * case_when(cntry == 4 ~ 1, T ~ 0),
  hdegree_vietnam = hdegree * case_when(cntry == 5 ~ 1, T ~ 0),
  hdegree_japan = hdegree * case_when(cntry == 6 ~ 1, T ~ 0),
  hdegree_hk = hdegree * case_when(cntry == 7 ~ 1, T ~ 0))
expla_var <- sort(colnames(test3 %>% select(-is_patient, -date, -cntry, -year)))

# regression
# cant use fixed effects as degree_xxx is fixed for some countries
formula_3 <- as.formula(paste('is_patient ~', paste(expla_var, collapse='+'), 
                              sep=''))
reg_3 <- lm(formula_3, data=test3)
summary(reg_3)

#######################################################################
## TEST 4: Can the link between education and patience be explained by cultural differences?
#######################################################################

# Data preparation
test4 <- data %>% 
  mutate(wealth_hh_per_ppl = log(eu_inc_ppp/ppl_hh),
         date = lubridate::dmy(substr(datetime, start=1, stop=11)),
         year = lubridate::year(date),
         is_patient = case_when(patience==1 ~ 0, T ~ 1),
         age = log(age)) %>% 
  #across(all_of(educ_cols), ~ standardize(.x), .names = '{.col}')) %>% 
  select(is_patient, wealth_hh_per_ppl, date, cntry, degree, age, year)

# drop nan
colSums(is.na(test3))
test3 <- na.omit(test3)

# create interactions terms within some countries
test3 <- test3 %>% mutate(
  degree_estonia = degree * case_when(cntry == 2 ~ 1, T ~ 0),
  degree_taiwan = degree * case_when(cntry == 3 ~ 1, T ~ 0),
  degree_china = degree * case_when(cntry == 4 ~ 1, T ~ 0),
  degree_vietnam = degree * case_when(cntry == 5 ~ 1, T ~ 0),
  degree_japan = degree * case_when(cntry == 6 ~ 1, T ~ 0),
  degree_hk = degree * case_when(cntry == 7 ~ 1, T ~ 0))
expla_var <- sort(colnames(test3 %>% select(-is_patient, -date, -cntry, -year)))

# regression
# formula_3 <- as.formula(paste('is_patient ~', paste(expla_var, collapse='+'), 
#   '|date + cntry| 0 |date + cntry', sep=''))
# reg_3 <- felm(formula_3, data=test3)
# summary(reg_3)
formula_3 <- as.formula(paste('is_patient ~', paste(expla_var, collapse='+'), 
                              sep=''))
reg_3 <- lm(formula_3, data=test3)
summary(reg_3)








