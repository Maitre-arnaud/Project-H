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
library(openxlsx)
library(tidyr) # pivot wider

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
  y <- as.numeric(x)
  mi <- min(y, na.rm=T)
  ma <- max(y, na.rm=T)
  z <- (y-mi)/(ma-mi)
  return(z)
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
data <- read.xlsx(xlsxFile=file.path(
  my_path, 'PANDA-Excel-version-with-documentation.xlsx'), sheet='data')
data <- my_wins(data, 0.05, c('eu_ppp_income')) # some crazy values to change
data <- data[data$base_pack_quality>0,] # drop bad quality answers (NA stays)
data <- data[data$tech_pack_quality>0,]

# Attitude toward learning:
#degree -> 1 to 5 scale (5=PhD or equivalent)
# no clue what is the difference with hdegree but the last one has less nan values
# XB2 -> I see myself as trusting
#ALA4 -> 1 to 5 scale (5=Agree with Somebody who studies hard should be admired)
#ALC2 -> 1 to 5 scale (5=wants to be friend with diligent student)
# unigrade -> 1 to 100 (100=best grade)
# patience (MS X.T1) -> 1 = a payment of 1700 this month / 2 = a payment of 1900 next month 
# eu_ppp_income = monthly income adjusted for difference in purchasing power
att_learning <- c('ALA1', 'ALA2', 'ALA3', 'ALA4', 'ALB1', 'ALB2', 'ALC2', 
                  'ALC3', 'ALC5', 'ALC6', 'ALD1', 'ALD4')
# reverse them for highest score = good attitude toward learning
att_learning_to_inv <- c('ALA2', 'ALA3', 'ALC5', 'ALC6')
hof <- c('IDV', 'PDI', 'MAS', 'UAI')

# some columns names are duplicated drop them -> mess with dplyr
dupl_cols <- colnames(data)[which(duplicated(names(data)))]
data <- data[ , -which(names(data) %in% dupl_cols)]

# Data preparation
data <- data %>% 
  mutate(
    date = as.Date(datetime, origin = "1899-12-30"),
    year = lubridate::year(date),
    wealth = log(eu_ppp_income),
         family_size = 1 + as.numeric(married) + as.numeric(children),
         is_patient = case_when(XT1==2 ~ 1, T ~ 0), # 2 = choose to wait
         age = log(as.numeric(age)),
         trust = scale(XB2),
         across(all_of(att_learning_to_inv), ~ as.numeric(.x)*-1, .names = '{.col}'),
         across(all_of(att_learning), ~ scale(.x), .names = '{.col}'),
         across(all_of(hof), ~ scale(.x), .names = '{.col}')) %>% 
  select(is_patient, family_size, country_code, wealth,
         all_of(att_learning), uni_degree, age, trust, female, 
         all_of(hof), eu_country, Asia, nationality, year)

# create a score attitude toward learning
data$n_var_atl <- apply(!is.na(data[, att_learning]), 1, sum) # n non-missing var
data <- data[data$n_var_atl>0,]
data$atl <-  rowSums(data[,att_learning], na.rm = T) / data$n_var_atl

# process IMF data
gdp <- read.xlsx(xlsxFile=file.path(my_path, 'GDP.xlsx'), startRow=7)
gdp$country_code <- 0
gdp$country_code[gdp$Country=='China, P.R.: Mainland'] <- 4
gdp$country_code[gdp$Country=='China, P.R.: Hong Kong'] <- 9
gdp$country_code[gdp$Country=='Estonia, Rep. of'] <- 1
gdp$country_code[gdp$Country=='Germany'] <- 8
gdp$country_code[gdp$Country=='Japan'] <- 10
gdp$country_code[gdp$Country=='Vietnam'] <- 4
gdp <- gdp[gdp$country_code>0,]
gdp <- gdp %>% select(-Country, - Scale, -Base.Year) %>% 
  mutate_all(~ as.numeric(.x))
gdp <- pivot_longer(gdp, 
                        cols = 1:6,  # Columns to pivot
                        names_to = "year",  # Name of the new variable column
                        values_to = "GDP"    # Name of the new value column
)

ir <- read.xlsx(xlsxFile=file.path(my_path, 'Interest_Rates.xlsx'), startRow=7)
ir$country_code <- 0
ir$country_code[ir$Country=='China, P.R.: Mainland'] <- 4
ir$country_code[ir$Country=='China, P.R.: Hong Kong'] <- 9
ir$country_code[ir$Country=='Estonia, Rep. of'] <- 1
ir$country_code[ir$Country=='Germany'] <- 8
ir$country_code[ir$Country=='Japan'] <- 10
ir$country_code[ir$Country=='Vietnam'] <- 4
ir <- ir[ir$country_code>0,]
ir <- ir %>% select(-Country, - Scale, -Base.Year) %>% 
  mutate_all(~ as.numeric(.x))
ir <- pivot_longer(ir, 
                    cols = 1:6,  # Columns to pivot
                    names_to = "year",  # Name of the new variable column
                    values_to = "GDP"    # Name of the new value column
)

#######################################################################
## TEST 1: Link between highest diploma obtained and patience
#######################################################################

# drop nan
test1 <- data
colSums(is.na(test1))
test1 <- na.omit(test1[, c('is_patient', 'wealth', 'country_code', 'uni_degree', 
                           'age', 'trust', 'female', 'atl', 'family_size')])
xs <- colnames(test1 %>% select(-country_code, -atl))

#regression (panel regression)
formulaff_1 <- as.formula(paste(
  'atl ~ ', paste(xs, collapse = '+'), '|country_code| 0 |country_code',
  ), sep='')
regff_1 <- felm(formulaff_1, data=test1, exactDOF = T, cmethod='reghdfe')
summary(regff_1)

#regression multilevel
# with random intercept for each group (=country_code)
formulaml_1 <- as.formula(paste(
  'atl ~ ', paste(xs, collapse = '+'), ' + (1 |country_code)', sep=''))
regml_1 <- lmer(formulaml_1, data = test1)
summary(regml_1)

#regression basic ols
formula_1 <- as.formula(paste(
  'atl ~ 1 +', paste(xs, collapse = '+'), sep=''))
reg_1 <- lm(formula_1, data = test1)
summary(reg_1)

# results -> nice table
res <- summary(reg_1)$coefficients
coef_str <- c()
clean_var <- c('Intercept', 'IsPatient_{i}', 'Wealth_{i}', 'HasUniDegree_{i}', 
               'Age_{i}', 'Trust_{i}', 'IsFemale_{i}', 'FamilySize_{i}')

for(i in seq(1, length(clean_var))){
  coef_str[i] <- paste('\\multirow{2}{*}{$', clean_var[i], '$} & ', 
                       put_stars(res[i,1], res[i,2], m, d), 
                       '\\\\ \n & $(', format(round(res[i,3],d), scientific=F) ,
                       ')$\\\\[3pt]\n', sep='')
}
cat(coef_str)

# We see in summary(regff_1), that atl is positively linked to patience
# the effect is robust to country fixed effects which include time-invariants country
# characteristics (some aspects of culture should therefore be captured)

#######################################################################
## TEST 2: Link between highest diploma obtained and patience per Country
#######################################################################

# barplot
test2_plot <- data %>% group_by(country_code) %>% 
  mutate(
    mean_is_patient = mean(is_patient, na.rm=T),
    mean_atl = mean(atl, na.rm=T)) %>% ungroup() %>% 
  select(country_code, mean_atl, mean_is_patient) %>% unique() %>% 
  arrange(country_code)
barplot(cbind(mean_is_patient, mean_atl) ~ country_code, data=test2_plot, 
        beside=T, names.arg=c('Estonia', 'Taiwan', 'China', 'Vietnam', 
                              'Germany', 'Hong Kong', 'Japan'),
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
test2 <- na.omit(test2[, c('is_patient', 'wealth', 'country_code', 'uni_degree', 
                           'age', 'trust', 'female', 'atl', 'family_size', 'Asia')])
xs <- colnames(test2 %>% select(-country_code, -atl, -Asia))

# run regression for each country
countries <- seq(1,2) #sort(as.numeric(unique(test2$country_code)))
formula_2 <- as.formula(paste(
  'atl ~ 1 + ', paste(xs, collapse = '+'), sep=''))
results_2 <- list()
n_obs <- c()

for(i in seq(1, length(countries))){
  #ci <- countries[[i]]
  #sub <- test2[test2$country_code==ci,]
  if(i==1){
    sub <- test2[test2$Asia==0,]
  }else{
    sub <- test2[test2$Asia==1,]
  }
  reg_2 <- lm(formula_2, data=sub)
  coef_2 <- summary(reg_2)$coefficients
  results_2[[i]] <- coef_2
  n_obs[i] <- dim(sub)[1]
}

# results -> nice table
coef_str <- c()
clean_var <- c('Intercept', 'IsPatient_{i}', 'Wealth_{i}', 'HasUniDegree_{i}', 
               'Age_{i}', 'Trust_{i}', 'IsFemale_{i}', 'FamilySize_{i}')

for(i in seq(1, length(clean_var))){
  ci <- paste('\\multirow{2}{*}{$', clean_var[i], '$}', sep='')
  si <- ''
  for(mi in seq(1, length(countries))){
    reg <- results_2[[mi]]
    ci <- paste(ci, ' & ', put_stars(reg[i,1], reg[i,2], m, d), sep='')
    si <- paste(si, ' & $(' , format(round(reg[i,3],d), scientific=F), ')$', sep='')

  }
  coef_str[i] <- paste(ci, '\\\\ \n ', si, '\\\\[3pt]\n', sep='')
}
cat(coef_str)
cat(paste(paste0('$', n_obs, '$'), collapse = ' & '))

# Arnaud: Within countries, atl is not linked to patience
# Differences in patience seems to be linked with country cultural difference
# and not so much by personal traits

#######################################################################
## TEST 3: Is the link between education and patience stronger for some countries?
#######################################################################

# drop nan
# test3 <- data
# colSums(is.na(test3))
# test3 <- na.omit(test3[, c('is_patient', 'wealth_hh_per_ppl', 'date', 
#                            'cntry', 'hdegree', 'age', 'trust', 
#                            'female', 'atl')])
# 
# # create interactions terms within some countries
# test3 <- test3 %>% mutate(
#   atl_estonia = atl * case_when(cntry == 2 ~ 1, T ~ 0),
#   atl_taiwan = atl * case_when(cntry == 3 ~ 1, T ~ 0),
#   atl_china = atl * case_when(cntry == 4 ~ 1, T ~ 0),
#   atl_vietnam = atl * case_when(cntry == 5 ~ 1, T ~ 0),
#   atl_japan = atl * case_when(cntry == 6 ~ 1, T ~ 0))
#   #atl_hk = atl * case_when(cntry == 7 ~ 1, T ~ 0))
# expla_var <- c('atl', 'atl_estonia', 'atl_taiwan', 'atl_china', 'atl_vietnam', 
#                'atl_japan', 'hdegree', 'wealth_hh_per_ppl', 'age', 'trust', 
#                'female')
# 
# # regression
# # cant use fixed effects as degree_xxx is fixed for some countries
# formula_3 <- as.formula(paste('is_patient ~ 1 +', paste(expla_var, collapse='+'), 
#                               sep=''))
# reg_3 <- lm(formula_3, data=test3)
# summary(reg_3)

#######################################################################
## TEST 4: Is the link between education and patience driven by cultural differences?
#######################################################################

# get average hofstedte values per country
# test4 <- data %>% group_by(cntry) %>% 
#   mutate(across(all_of(hof), ~mean(.x, na.rm=T), .names='{.col}')) %>% 
#   ungroup()

# drop nan
test4 <- data
colSums(is.na(test4))
test4 <- na.omit(test4[, c('is_patient', 'wealth', 'country_code', 'uni_degree', 
                           'age', 'trust', 'female', 'atl', 'family_size', 
                           'Asia', hof)])
test4 <- test4 %>% mutate(
  inter_patience_asia = is_patient * Asia) %>% 
  select(-Asia)

xs <- colnames(test4 %>% select(-country_code, -atl))

# regression
formula_4 <- as.formula(paste('atl ~', paste(xs, collapse='+'), sep=''))
reg_4 <- lm(formula_4, data=test4)
summary(reg_4)

# results -> nice table
res <- summary(reg_4)$coefficients
coef_str <- c()
clean_var <- c('Intercept', 'IsPatient_{i}', 'Wealth_{i}', 'HasUniDegree_{i}', 
               'Age_{i}', 'Trust_{i}', 'IsFemale_{i}', 'FamilySize_{i}', 
               'IDV_{i}', 'PDI_{i}', 'MAS_{i}', 'UAI_{i}', 'IsPatient*Asia_{i}')

for(i in seq(1, length(clean_var))){
  coef_str[i] <- paste('\\multirow{2}{*}{$', clean_var[i], '$} & ', 
                       put_stars(res[i,1], res[i,2], m, d), 
                       '\\\\ \n & $(', format(round(res[i,3],d), scientific=F) ,
                       ')$\\\\[3pt]\n', sep='')
}
cat(coef_str)






