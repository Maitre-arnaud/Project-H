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
library(psych)

user <- 'USB'
if(user=='Arnaud'){
  my_path <- r'(C:\Users\Arnaud\OneDrive\Bureau\cultural finance\Projet\Data)'
} else if(user=='USB'){
  my_path <- r'(E:\cultural finance\Projet\Data)'
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
data <- data[data$base_pack_quality!=0,] # drop bad quality answers (NA stays)
data <- data[data$tech_pack_quality!=0,]
#data <- data[data$residence_local==1,]

# Attitude toward learning:
# XB2 -> I see myself as trusting
# patience (MS X.T1) -> 1 = a payment of 1700 this month / 2 = a payment of 1900 next month 
# eu_ppp_income = monthly income adjusted for difference in purchasing power
atl_all <- c('ALA1', 'ALA2', 'ALA3', 'ALA4', 'ALB1', 'ALB2', 'ALC2', 
                  'ALC3', 'ALC5', 'ALC6', 'ALD1', 'ALD4')
atl_intra <- c('ALA1', 'ALA4', 'ALB1', 'ALB2', 'ALD1', 'ALD4')
atl_extra <- c('ALC2', 'ALC3', 'ALC5', 'ALC6')
# reverse them for highest score = good attitude toward learning
atl_to_inv <- c('ALA2', 'ALA3', 'ALB1', 'ALB2', 'ALC2', 'ALC3', 'ALC5') # what I think -> ALC5 is asked by alpha
hof <- c('IDV', 'PDI', 'MAS', 'UAI')
atl_any <- c(atl_all, 'AC1', 'AC3', 'AC8', 'AC9', 'AC10', 'AC11', 'AC14', 'AIN0', 'AIN1')
all_inv <- c(atl_to_inv, 'AIN0', 'AIN1')

# some columns names are duplicated drop them -> mess with dplyr
dupl_cols <- colnames(data)[which(duplicated(names(data)))]
data <- data[ , -which(names(data) %in% dupl_cols)]

# convert grades high school
data$ID <- 1:nrow(data)
grades <- data %>% group_by(country_code) %>% mutate(
  new_grade = scale(as.numeric(grade) * case_when(eu_country==1 ~ -1, T ~ 1))
) %>% ungroup() %>% select(new_grade, ID)
data2 <- data

# Data preparation
data <- data %>% left_join(grades, by=c('ID')) %>% 
  mutate(
    date = as.Date(datetime, origin = "1899-12-30"),
    year = lubridate::year(date),
    wealth = log(eu_ppp_income),
    crt = scale(correct_answers),
         family_size = 1 + as.numeric(married) + as.numeric(children),
         is_patient = case_when(XT1==2 ~ 1, T ~ 0), # 2 = choose to wait
         age = log(as.numeric(age)),
         trust = scale(XB2),
    new_unigrade = scale(unigrade),
    berlin1 = case_when(berlin1==25~1, T~0),
    berlin2 = case_when(berlin2==30~1, T~0),
    berlin3 = case_when(berlin3==20~1, T~0),
    berlin4 = case_when(berlin4==50~1, T~0),
         across(all_of(all_inv), ~ as.numeric(.x)*-1, .names = '{.col}'),
         across(all_of(atl_any), ~ scale(.x), .names = '{.col}'),
         across(all_of(hof), ~ scale(.x), .names = '{.col}'),
    education = scale(case_when(hdegree=='PhD' ~ 5, hdegree=='Master' ~ 4, 
                          hdegree=='Bachelor' ~ 3, hdegree=='Others' ~ 2, # most of others are currently at uni
                          hdegree=='High school' ~ 1, T ~ 0))) %>% 
  select(is_patient, family_size, country_code, wealth,
         all_of(atl_any), uni_degree, age, trust, female, education, crt,
         all_of(hof), eu_country, Asia, nationality, year, master, bachelor, 
         new_unigrade, new_grade, berlin1, berlin2, berlin3, berlin4)

# create a score attitude toward learning
data$n_var_atl_all <- apply(!is.na(data[, atl_all]), 1, sum) # n non-missing var
data$n_var_atl_intra <- apply(!is.na(data[, atl_intra]), 1, sum)
data$n_var_atl_extra <- apply(!is.na(data[, atl_extra]), 1, sum)
data <- data[data$n_var_atl_all>0,]
data$atl_all <-  rowSums(data[,atl_all], na.rm = T) / data$n_var_atl_all
data$atl_intra <-  rowSums(data[,atl_intra], na.rm = T) / data$n_var_atl_intra
data$atl_extra <-  rowSums(data[,atl_extra], na.rm = T) / data$n_var_atl_extra

# create ATL based on grades
data$n_var_grades <- apply(!is.na(data[, c('new_grade', 'new_unigrade')]), 1, sum)
data$atl_grade <-  rowSums(data[,c('new_grade', 'new_unigrade')], na.rm = T) / data$n_var_grades

# create berlin score -> has not answered = dont know
data$berlin <- scale(rowSums(data[,c('berlin1', 'berlin2', 'berlin3', 'berlin4')], na.rm = T))

# create de-meaned atl variables
atls <- c('atl_all', 'atl_intra', 'atl_extra')
data <- data %>% group_by(country_code) %>% 
  mutate(across(all_of(atls), ~mean(.x, na.rm=T), .names = 'mean_{.col}')) %>% 
  ungroup() %>% 
  mutate(
    datl_all = atl_all - mean_atl_all,
    datl_intra = atl_intra - mean_atl_intra,
    datl_extra = atl_extra - mean_atl_extra)

# patience 2
data$n_var_pat2 <- apply(!is.na(data[, c('is_patient', 'crt')]), 1, sum)
data$is_patient2 <- rowSums(data[,c('is_patient', 'crt')], na.rm = T) / data$n_var_pat2

# weight atl col based on how much they correlate with grades
new_data <- c()
#countries <- unique(data$country_code)
#for(ci in countries){
  sub <- data %>% select(-atl_all)# %>% filter(country_code==ci)
  m <- cor(sub %>% select(atl_grade, all_of(atl_all)), 
           use = 'pairwise.complete.obs')
  v <- m[1,-1]
  
  for(var_i in atl_all){
    sub[[var_i]] <- sub[[var_i]] * v[match(var_i, atl_all)]
  }
  sub$atl_weight = rowSums(sub[,atl_all], na.rm = T)
  new_data <- bind_rows(new_data, sub)
#}


# Some Checks -------------------------------------------------------------
#you may use Cronbach's Alpha with questions that are not on a common scale, 
#as long as they are conceptually related and intended to measure the same 
#construct. In such cases, you may need to make adjustments to ensure that the 
#responses are comparable. Here are some considerations: (Standardization)
# Alpha > 0.7 is good (consistency is good)

sub <- data[, atl_all]

# beware each participant only had question from ALB and ALD questions
atl_a <- c("ALA1", "ALA2", "ALA3", "ALA4", "ALB1", "ALC2", "ALC3", "ALC5", "ALC6", "ALD1")
sub <- na.omit(sub[,atl_a])
kron_alpha_a <- alpha(sub, check.keys = F) # do not inverse scale of (-) cor variables

# alpha is low (0.36) -> check corr matrix whether some should be dropped
cor(sub)

# dropping ALA3, ALB1, ALC5, ALC6 -> negative corr with ALA1
atl_b <- c("ALA1", "ALA2", "ALA4", "ALC2", "ALC3", "ALD1")
sub <- na.omit(data[,atl_b])
kron_alpha_b <- alpha(sub, check.keys = F) # 0.31
cor(sub)

# dropping ALA2 -> negative corr with ALA1 as there are more obs
atl_c <- c("ALA1", "ALA4", "ALC2", "ALC3", "ALD1")
sub <- na.omit(data[,atl_c])
kron_alpha_c <- alpha(sub, check.keys = F) # 0.31
cor(sub)

# keeping best correlated pair
atl_d <- c("ALA1", "ALD1")
sub <- na.omit(data[,atl_d])
kron_alpha_d <- alpha(sub, check.keys = F) # 0.14
cor(sub)

#######################################################################
## TEST 1: Link between highest diploma obtained and patience
#######################################################################

# params
y <- 'berlin'
x <- 'is_patient'
df <- data

# drop nan
cor(df %>% select(atl_extra, atl_intra, atl_all, atl_grade, berlin), use='complete.obs')
test1 <- df
colSums(is.na(test1))
test1 <- na.omit(test1[, c(x, 'wealth', 'country_code', 'education', 
                           'age', 'trust', 'female', y, 'family_size')])
xs <- colnames(test1 %>% select(-country_code, -y))

#regression (panel regression)
# formulaff_1 <- as.formula(paste(
#   'atl ~ ', paste(xs, collapse = '+'), '|country_code| 0 |country_code',
#   ), sep='')
# regff_1 <- felm(formulaff_1, data=test1, exactDOF = T, cmethod='reghdfe')
# summary(regff_1)
# 
# #regression multilevel
# # with random intercept for each group (=country_code)
# formulaml_1 <- as.formula(paste(
#   'atl ~ ', paste(xs, collapse = '+'), ' + (1 |country_code)', sep=''))
# regml_1 <- lmer(formulaml_1, data = test1)
# summary(regml_1)

#regression basic ols
formula_1 <- as.formula(paste(
  y, ' ~ 1 +', paste(xs, collapse = '+'), sep=''))
reg_1 <- lm(formula_1, data = test1)
summary(reg_1)

# results -> nice table
res <- summary(reg_1)$coefficients
coef_str <- c()
clean_var <- c('Intercept', 'IsPatient_{i}', 'Wealth_{i}', 'Education_{i}', 
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

# params
y <- 'berlin'
x <- 'is_patient'

# barplot country
test2_plot <- data %>% group_by(country_code) %>% 
  mutate(
    mean_is_patient = mean(get(x), na.rm=T),
    mean_atl = mean(get(y), na.rm=T),
    mean_HasUniDegree = mean(uni_degree, na.rm=T)) %>% 
  ungroup() %>% 
  select(country_code, mean_atl, mean_is_patient, mean_HasUniDegree) %>% 
  unique() %>% 
  arrange(country_code)
barplot(cbind(mean_is_patient, mean_atl, mean_HasUniDegree) ~ country_code, 
        data=test2_plot, 
        beside=T, names.arg=c('Estonia', 'Taiwan', 'China', 'Vietnam', 
                              'Germany', 'Hong Kong', 'Japan'),
        xlab = '', ylab = 'Score', col=c('dark blue', 'light blue', 'dark green'), 
        ylim=c(0,1))
legend('top', col=c('dark blue', 'light blue', 'dark green'), 
       c('Patience', 'ATL', 'Has Uni Degree'), lwd=2, inset = c(0, -0.02), 
       ncol = 2, box.lty = 0)

# barplot continent
test2_plot_cont <- data %>% group_by(eu_country) %>% 
  mutate(
    mean_is_patient = mean(get(x), na.rm=T),
    mean_atl = mean(get(y), na.rm=T),
    mean_HasUniDegree = mean(uni_degree, na.rm=T)) %>% 
  ungroup() %>% 
  select(eu_country, mean_atl, mean_is_patient, mean_HasUniDegree) %>% 
  unique() %>% 
  arrange(eu_country)
barplot(cbind(mean_is_patient, mean_atl, mean_HasUniDegree) ~ eu_country, 
        data=test2_plot_cont, 
        beside=T, names.arg=c('Asia', 'Europe'),
        xlab = '', ylab = 'Score', col=c('dark blue', 'light blue', 'dark green'), 
        ylim=c(0,1))
legend('top', col=c('dark blue', 'light blue', 'dark green'), 
       c('Patience', 'ATL', 'Has Uni Degree'), lwd=2, inset = c(0, -0.02), 
       ncol = 3, box.lty = 0)

# scatterplot
scatter.smooth(test2_plot$mean_atl, test2_plot$mean_is_patient, span = 1,
               degree = 1, xlab = 'Atitude Toward learning score',
               ylab = 'Patience score')

# drop nan
test2 <- data
colSums(is.na(test2))
test2 <- na.omit(test2[, c(x, 'wealth', 'country_code', 'education', 
                           'age', 'trust', 'female', y, 'family_size', 'Asia')])
xs <- colnames(test2 %>% select(-country_code, -y, -Asia))

# run regression for each country
countries <- seq(1,2) #sort(as.numeric(unique(test2$country_code)))
formula_2 <- as.formula(paste(
  y, ' ~ 1 + ', paste(xs, collapse = '+'), sep=''))
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
clean_var <- c('Intercept', 'IsPatient_{i}', 'Wealth_{i}', 'Education_{i}', 
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

data %>% group_by(Asia) %>% summarise(wealth = mean(wealth, na.rm=T), 
                                      age = mean(age, na.rm=T), 
                                      educ = mean(education, na.rm=T), 
                                      crt = mean(crt, na.rm=T), 
                                      patience = mean(is_patient, na.rm=T),
                                      extra = mean(atl_extra, na.rm=T),
                                      intra = mean(atl_intra, na.rm=T),
                                      all = mean(atl_all, na.rm=T))

# Params
y <- 'berlin'
x <- 'is_patient'

# create interaction
test4 <- data %>% mutate(
  inter_patience_asia = is_patient * Asia,
  inter_education_patience = is_patient * education)
  #inter_age_patience = is_patient * age,
  #inter_wealth_patience = is_patient * wealth)

# drop nan
colSums(is.na(test4))
test4 <- na.omit(test4[, c(x, 'wealth', 'country_code', 'education', 
                           'age', 'trust', 'female', y, 'family_size', 
                           'Asia', hof, 'inter_patience_asia', 
                           'inter_education_patience')])#, 'inter_age_patience', 'inter_wealth_patience')])
xs <- colnames(test4 %>% select(-country_code, -y))

# run 2 regressions
regs <- seq(1,2)
results_4 <- list()
n_obs <- c()

for(i in seq(1, length(regs))){
  
  if(i==1){
    new_xs <- xs[! xs %in% c('inter_education_patience')]
  }else{
    new_xs <- xs
  }
  formula_4 <- as.formula(paste(y, ' ~', paste(new_xs, collapse='+'), sep=''))
  reg_4 <- lm(formula_4, data=test4)
  coef_4 <- summary(reg_4)$coefficients
  results_4[[i]] <- coef_4
  n_obs[i] <- dim(test4)[1]
}

# results -> nice table
coef_str <- c()
clean_var <- c('Intercept', 'IsPatient_{i}', 'Wealth_{i}', 'Education_{i}', 
               'Age_{i}', 'Trust_{i}', 'IsFemale_{i}', 'FamilySize_{i}', 
               'Asia_{i}', 'IDV_{i}', 'PDI_{i}', 'MAS_{i}', 'UAI_{i}', 
               'IsPatient*Asia_{i}', 'IsPatient*Education_{i}')

for(i in seq(1, length(clean_var))){
  ci <- paste('\\multirow{2}{*}{$', clean_var[i], '$}', sep='')
  si <- ''
  for(mi in seq(1, length(regs))){
    if(mi==1 & i==length(clean_var)){
      ci <- paste(ci, ' & ', sep='')
      si <- paste(si, ' & ', sep='')
    } else{
      reg <- results_4[[mi]]
      ci <- paste(ci, ' & ', put_stars(reg[i,1], reg[i,2], m, d), sep='')
      si <- paste(si, ' & $(' , format(round(reg[i,3],d), scientific=F), ')$', sep='')
    }
  }
  coef_str[i] <- paste(ci, '\\\\ \n ', si, '\\\\[3pt]\n', sep='')
}
cat(coef_str)
cat(paste(paste0('$', n_obs, '$'), collapse = ' & '))






