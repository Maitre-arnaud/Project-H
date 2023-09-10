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
library(readr)
library(lme4)

user <- 'Arnaud'
if(user=='Arnaud'){
  my_path <- r'(C:\Users\Arnaud\OneDrive\Bureau\cultural finance\Projet\Data)'
} else if(user==''){
  my_path <- r'()'
}

# general params
d <- 2
m <- 1

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

put_stars <- function(coef, se, scale=1, decimal=2){
  
  if(is.na(coef)==T){
    x <- ''
  } else{
    tstat <- abs(coef / se)
    stars <- 0
    
    if(tstat >= 2.575){
      stars <- 3
    } else if(tstat >= 1.96){
      stars <- 2
    } else if(tstat >= 1.645){
      stars <- 1
    }
    
    coef <- format(round(scale*coef, decimal), scientific=F)
    
    if(stars>0){
      x <- paste('$', coef, '^{', strrep('*', stars), '}$', sep='') 
    } else{
      x <- paste('$', coef, '$', sep='')
    } 
  }
  return(x)
}

# Data Preparation --------------------------------------------------------
# read and winsorize
data <- read_sav(file.path(my_path, 'PANDA data_2021_11_17.sav'))
#intra <- read_sav(file.path(my_path, '54-countries-6792-published-INTRA-data.sav'))
data <- my_wins(data, 0.05, c('eu_inc_ppp', 'ppl_hh')) # some crazy values to change
data <- my_wins(data, 0.01, c('age')) # less crazy values
data <- data[data$cntry==data$nationality,]

# wws <- readr::read_csv(file.path(my_path, 
#                                  'WVS_Cross-National_Inverted_Wave_7_csv_v5_0.csv'))

# Attitude toward learning:
#degree -> 1 to 5 scale (5=PhD or equivalent)
# no clue what is the difference with hdegree but the last one has less nan values
# XB2 -> I see myself as trusting
#ALA4 -> 1 to 5 scale (5=Agree with Somebody who studies hard should be admired)
#ALC2 -> 1 to 5 scale (5=wants to be firend with diligent student)
# unigrade -> 1 to 100 (100=best grade)
# patience (MS X.T1) -> 1 = a payment of 1700 this month / 2 = a payment of 1900 next month 
# eu_inc_ppp = monthly income adjusted for difference in purchasing power (CONTROL)
# ppl_hh = n_pers in household (CONTROL)
att_learning <- c('ALA1', 'ALA2', 'ALA3', 'ALA4', 'ALB1', 'ALB2', 'ALC2', 
                  'ALC3', 'ALC5', 'ALC6', 'ALD1', 'ALD4')
# reverse them for highest score = good attitude toward learning
att_learning_to_inv <- c('ALA2', 'ALA3', 'ALC5', 'ALC6')
hof <- c('IDV', 'PDI1', 'MAS', 'UAI') # 'PDI2'

# Data preparation
data <- data %>% 
  mutate(wealth_hh_per_ppl = log(eu_inc_ppp/ppl_hh),
         date = lubridate::dmy(substr(datetime, start=1, stop=11)),
         year = lubridate::year(date),
         is_patient = case_when(patience==1 ~ 0, T ~ 1),
         age = log(age),
         trust = scale(XB2),
         female = case_when(gender==0 ~ 1, T ~ 0),
         across(all_of(att_learning_to_inv), ~ .x*-1, .names = '{.col}'),
         across(all_of(att_learning), ~ scale(.x), .names = '{.col}'),
         across(all_of(hof), ~ scale(.x), .names = '{.col}')) %>% 
  select(is_patient, wealth_hh_per_ppl, date, cntry, 
         all_of(att_learning), hdegree, age, year, trust, female, 
         all_of(hof), EU_cntry, Asia_cntry)

# create a score attitude toward learning
data$n_var_atl <- apply(!is.na(data[, att_learning]), 1, sum) # n non-missing var
data <- data[data$n_var_atl>0,]
data$atl <-  rowSums(data[,att_learning], na.rm = T) / data$n_var_atl

# plot data points
plot(data$is_patient, data$atl)

#######################################################################
## TEST 1: Link between highest diploma obtained and patience
#######################################################################

# drop nan
test1 <- data
test1 <- na.omit(test1[, c('is_patient', 'wealth_hh_per_ppl', 'date', 
                              'cntry', 'hdegree', 'age', 'trust', 
                           'female', 'atl')])

#regression (panel regression)
formulaff_1 <- as.formula(
  'is_patient ~ atl + hdegree + wealth_hh_per_ppl + age + trust + female |cntry| 0 |cntry',
  )
regff_1 <- felm(formulaff_1, data=test1, exactDOF = T, cmethod='reghdfe')
summary(regff_1)

# output in nice table
res <- summary(regff_1)$coefficients

# results -> nice table
coef_str <- c()
clean_var <- c('ATL_{i}', 'Degree_{i}', 'Wealth_{i}', 'Age_{i}', 'Trust_{i}', 
               'IsFemale_{i}')

for(i in seq(1, length(clean_var))){
  coef_str[i] <- paste('\\multirow{2}{*}{$', clean_var[i], '$} & ', 
                       put_stars(res[i,1], res[i,2], m, d), 
                       '\\\\ \n & $(', format(round(res[i,3],d), scientific=F) ,
                       ')$\\\\[3pt]\n', sep='')
}
cat(coef_str)


#regression multilevel
# with random intercept for each group (=cntry)
xs <- c('atl', 'hdegree', 'wealth_hh_per_ppl', 'age', 'trust', 'female')
formulaml_1 <- as.formula(paste(
  'is_patient ~ ', paste(xs, collapse = '+'), ' + (1 |cntry)', sep=''))
regml_1 <- lmer(formulaml_1, data = test1)
summary(regml_1)

#regression basic ols
xs <- c('atl', 'hdegree', 'wealth_hh_per_ppl', 'age', 'trust', 'female')
formula_1 <- as.formula(paste(
  'is_patient ~ 1+', paste(xs, collapse = '+'), sep=''))
reg_1 <- lm(formula_1, data = test1)
summary(reg_1)

# We see in summary(regff_1), that atl is positively linked to patience
# the effect is robust to country fixed effects which include time-invariants country
# characteristics (some aspects of culture should therefore be captured)

#######################################################################
## TEST 2: Link between highest diploma obtained and patience per Country
#######################################################################

# barplot
test2_plot <- data %>% group_by(cntry) %>% 
  mutate(
    mean_is_patient = mean(is_patient, na.rm=T),
    mean_atl = mean(atl, na.rm=T)) %>% ungroup() %>% 
  select(cntry, mean_atl, mean_is_patient) %>% unique() %>% 
  arrange(cntry)
barplot(cbind(mean_is_patient, mean_atl) ~ cntry, data=test2_plot, 
        beside=T, names.arg=c('Germany', 'Estonia', 'Taiwan', 'China', 
                              'Vietnam', 'Japan', 'Hong Kong'),
        xlab = '', ylab = 'Proportion', col=c('dark blue', 'light blue'), 
        ylim=c(0,1))
legend('top', col=c('dark blue', 'light blue'), 
       c('Patience score', 'Atitude Toward learning score'), lwd=2, inset = c(0, -0.02), 
       ncol = 2, box.lty = 0)

# scatterplot
scatter.smooth(test2_plot$mean_atl, test2_plot$mean_is_patient, span = 1,
               degree = 1, xlab = 'Atitude Toward learning score',
               ylab = 'Patience score')

# drop nan
test2 <- data
colSums(is.na(test2))
test2 <- na.omit(test2[c('is_patient', 'wealth_hh_per_ppl', 'date', 'cntry', 
                         'hdegree', 'age', 'female', 'trust', 'atl')])

# run regression for each country
countries <- sort(as.numeric(unique(test2$cntry)))
formula_2 <- as.formula(
  'is_patient ~ 1 + atl + hdegree + wealth_hh_per_ppl + age + trust + female')
results_2 <- list()

for(i in seq(1, length(countries))){
  ci <- countries[[i]]
  sub <- test2[test2$cntry==ci,]
  reg_2 <- lm(formula_2, data=sub)
  coef_2 <- summary(reg_2)$coefficients
  results_2[[i]] <- coef_2
}

# results -> nice table
coef_str <- c()
clean_var <- c('ATL_{i}', 'Degree_{i}', 'Wealth_{i}', 'Age_{i}', 'Trust_{i}', 
               'IsFemale_{i}')

for(i in seq(1, length(clean_var))){
  coef_str[i] <- paste('\\multirow{2}{*}{$', clean_var[i], '$} & ', 
                       put_stars(res[i,1], res[i,2], m, d), 
                       '\\\\ \n & $(', format(round(res[i,3],d), scientific=F) ,
                       ')$\\\\[3pt]\n', sep='')
}
cat(coef_str)

# Arnaud: Within countries, atl is not linked to patience
# Differences in patience seems to be linked with country cultural difference
# and not so much by personal traits

#######################################################################
## TEST 3: Is the link between education and patience stronger for some countries?
#######################################################################

# drop nan
test3 <- data
colSums(is.na(test3))
test3 <- na.omit(test3[, c('is_patient', 'wealth_hh_per_ppl', 'date', 
                           'cntry', 'hdegree', 'age', 'trust', 
                           'female', 'atl')])

# create interactions terms within some countries
test3 <- test3 %>% mutate(
  atl_estonia = atl * case_when(cntry == 2 ~ 1, T ~ 0),
  atl_taiwan = atl * case_when(cntry == 3 ~ 1, T ~ 0),
  atl_china = atl * case_when(cntry == 4 ~ 1, T ~ 0),
  atl_vietnam = atl * case_when(cntry == 5 ~ 1, T ~ 0),
  atl_japan = atl * case_when(cntry == 6 ~ 1, T ~ 0))
  #atl_hk = atl * case_when(cntry == 7 ~ 1, T ~ 0))
expla_var <- c('atl', 'atl_estonia', 'atl_taiwan', 'atl_china', 'atl_vietnam', 
               'atl_japan', 'hdegree', 'wealth_hh_per_ppl', 'age', 'trust', 
               'female')

# regression
# cant use fixed effects as degree_xxx is fixed for some countries
formula_3 <- as.formula(paste('is_patient ~ 1 +', paste(expla_var, collapse='+'), 
                              sep=''))
reg_3 <- lm(formula_3, data=test3)
summary(reg_3)

#######################################################################
## TEST 4: Is the link between education and patience driven by cultural differences?
#######################################################################

# get average hofstedte values per country
# test4 <- data %>% group_by(cntry) %>% 
#   mutate(across(all_of(hof), ~mean(.x, na.rm=T), .names='{.col}')) %>% 
#   ungroup()

# drop nan
test4 <- data# %>% filter(cntry!=7) %>% filter(cntry!=2)
colSums(is.na(test4))
test4 <- na.omit(test4[, c('is_patient', 'wealth_hh_per_ppl', 'date', 
                          'cntry', 'age', 'trust', 
                          'female', 'atl', hof)])

expla_var <- c('atl', 'wealth_hh_per_ppl', 'age', 'trust', 'female', hof)

# regression
# formula_4 <- as.formula(paste('is_patient ~', paste(expla_var, collapse='+'), 
#                               sep=''))
# reg_4 <- lm(formula_4, data=test4)
# summary(reg_4)

formula_4 <- as.formula(paste('is_patient ~', paste(expla_var, collapse='+'),
                              '| cntry | 0 | 0', sep=''))
reg_4 <- felm(formula_4, data=test4, exactDOF = T, cmethod='reghdfe')
summary(reg_4)








