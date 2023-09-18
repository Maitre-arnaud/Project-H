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

user <- 'Arnaud'
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
    x[[v]] <- Winsorize(as.numeric(x[[v]]), probs=c(lvl, 1-lvl), na.rm=T) # Inf values are winsorized
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
hs <- read.xlsx(xlsxFile=file.path(my_path, 'Highschool Data.xlsx'), 
                sheet='Modified', startRow = 2)
data <- my_wins(data, 0.05, c('eu_ppp_income', 'XD14')) # some crazy values to change
data <- data[data$base_pack_quality!=0,] # drop bad quality answers (NA stays)
data <- data[data$tech_pack_quality!=0,]
#data <- data[data$residence_local==1,]

# some columns names are duplicated drop them -> mess with dplyr
dupl_cols <- colnames(data)[which(duplicated(names(data)))]
data <- data[ , -which(names(data) %in% dupl_cols)]

# CHECK IF SCALE IS CONSISTENT WITH DOCUMENTATION
unique(data$ALB1) # ALB1, ALB2 -> have to correct...
data <- data %>% mutate(
  ALD1 = case_when(ALD1=='2'~'2', ALD1=='1'~'1', T~NA_character_),
  AIN0 = case_when(AIN0=='1'~'1', AIN0=='0'~'0', T~NA_character_),
  ALB1 = case_when(ALB1=='6'~'1', ALB1=='7'~'2', ALB1=='8'~'3', ALB1=='9'~'4', 
                   ALB1=='1'~'1', ALB1=='2'~'2', ALB1=='3'~'3', ALB1=='4'~'4', 
                   T~NA_character_), # scale is 1-4 and 6-9
  ALB2 = case_when(ALB2=='6'~'1', ALB2=='7'~'2', ALB2=='8'~'3', ALB2=='9'~'4', 
                   ALB2=='1'~'1', ALB2=='2'~'2', ALB2=='3'~'3', ALB2=='4'~'4', 
                   T~NA_character_)) # scale is 1-4 and 6-9

# As ALD and ALB are two times the same question with a small twist
# combine obs for the rest of analysis -> Or I skip them -> Or I just use one part of the pair
data$ALB <- data$ALB1
data$ALB[is.na(data$ALB)] <- data$ALB2[is.na(data$ALB)]
data$ALD <- data$ALD1
data$ALD[is.na(data$ALD)] <- data$ALD4[is.na(data$ALD)]

# Attitude toward learning:
# XB2 -> I see myself as trusting
# patience (MS X.T1) -> 1 = a payment of 1700 this month / 2 = a payment of 1900 next month 
# eu_ppp_income = monthly income adjusted for difference in purchasing power
atl_all <- c('ALA1', 'ALA2', 'ALA3', 'ALA4', 'ALB', 'ALC2', 
                  'ALC3', 'ALC5', 'ALC6', 'ALD')
# reverse them for highest score = good attitude toward learning
atl_to_inv <- c('ALA2', 'ALA3', 'ALB', 'ALC2', 'ALC3', 'ALC5') # given by alpha
hof <- c('IDV', 'PDI', 'MAS', 'UAI')
atl_any <- c(atl_all, 'AC1', 'AC3', 'AC8', 'AC9', 'AC10', 'AC11', 'AC14', 'AIN0',
             'AIN1', 'ACTS1', 'ACTS3', 'AIT1', 'AIT2')
all_inv <- c(atl_to_inv, 'ACTS1', 'AIT1')

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
         family_size = as.numeric(XD14),
         is_patient = case_when(XT1==2 ~ 1, XT1==1 ~ 0), # 2 = choose to wait
         age = log(as.numeric(age)),
         trust = scale(XB2),
    new_unigrade = scale(unigrade),
    berlin1 = case_when(berlin1==25~1, T~0),
    berlin2 = case_when(berlin2==30~1, T~0),
    berlin3 = case_when(berlin3==20~1, T~0),
    berlin4 = case_when(berlin4==50~1, T~0),
         across(all_of(all_inv), ~ as.numeric(.x)*-1, .names = '{.col}'),
         across(all_of(atl_any), ~ scale(.x), .names = '{.col}'),
         across(all_of(hof), ~ scale(.x), .names = '{.col}')) %>% 
    # education = scale(case_when(hdegree=='PhD' ~ 5, hdegree=='Master' ~ 4, 
    #                       hdegree=='Bachelor' ~ 3, hdegree=='Others' ~ 2, # most of others are currently at uni
    #                       hdegree=='High school' ~ 1, T ~ 0))) %>% 
  select(is_patient, family_size, country_code, wealth,
         all_of(atl_any), uni_degree, age, trust, female, crt, all_of(hof), 
         eu_country, Asia, nationality, year, master, bachelor, 
         new_unigrade, new_grade, berlin1, berlin2, berlin3, berlin4)

# create a score attitude toward learning
data$n_var_atl_all <- apply(!is.na(data[, atl_all]), 1, sum) # n non-missing var
data <- data[data$n_var_atl_all>0,]
data$atl_all <-  rowSums(data[,atl_all], na.rm = T) / data$n_var_atl_all

# create ATL based on grades
data$n_var_grades <- apply(!is.na(data[, c('new_grade', 'new_unigrade')]), 1, sum)
data$atl_grade <-  rowSums(data[,c('new_grade', 'new_unigrade')], na.rm = T) / data$n_var_grades

# create berlin score -> has not answered = dont know
data$berlin <- scale(rowSums(data[,c('berlin1', 'berlin2', 'berlin3', 'berlin4')], na.rm = T))

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

# Factor analysis (1) ---------------------------------------------------------
# the best one to use
atl_any <- c(atl_all, 'AC1', 'AC3', 'AC8', 'AC9', 'AC10', 'AC11', 'AC14', 'AIN0',
             'AIN1', 'ACTS1', 'ACTS3', 'AIT1', 'AIT2')
  
colSums(is.na(data[,atl_any]))
sub <- data[, atl_any]

# finding number of factors using Kaiser's criterion
fa_test <- fa(sub, nfactors = 10, rotate = "varimax")
eigenvalues <- fa_test$values[1:10]
nf <- sum(eigenvalues > 1) # ev greate than 1

# Perform Factor analysis
fa_res <- fa(sub, nfactors = nf, rotate = "varimax")
print(fa_res) # as a rule of thumb |loading| > 0.3 -> means that the question contributes significantly to the factor

# compute a score to see how each participant loads on the respective factors
scores <- factor.scores(f=fa_res, x = sub)
data <- cbind(data, scores$scores)

# check that each factor is consistent -> pick abs(loading) >= 0.3
mr1_var <- c('AC10', 'AC9', 'AC1', 'AC3', 'AC8', 'AC11', 'AC14') # AT innovation
mr3_var <- c('AIT1', 'AIT2', 'AIN0', 'AIN1') # AT technology
mr2_var <- c('ALC3', 'ALC2', 'ALC5') # AT diligent students
mr4_var <- c('AC3', 'AC1', 'ALA1', 'ACTS1', 'ALC6') # AT personal development
kron_alpha_mr1 <- alpha(na.omit(sub[, mr1_var]), check.keys = T) # 0.77
kron_alpha_mr2 <- alpha(na.omit(sub[, mr2_var]), check.keys = T) # 0.55
kron_alpha_mr3 <- alpha(na.omit(sub[, mr3_var]), check.keys = T) # 0.64
kron_alpha_mr4 <- alpha(na.omit(sub[, mr4_var]), check.keys = T) # 0.41 -> loading seems ok from theoretical PoV

# report nicely factor analysis
m <- fa_res$loadings
coef_str <- c()

for(i in seq(1, length(atl_any))){
  ci <- paste('$', atl_any[i], '$ ', sep='')
  for(fi in seq(1, nf)){
    ci <- paste(ci, ' & $', format(round(m[i,fi], d)), '$', sep='')
  }
  coef_str[i] <- paste(ci, '\\\\[3pt]\n', sep='')
}
cat(coef_str)


# Some questions do not clearly belong to one factor: ALA3, ALA4, ALB, ALD
# drop them
# some question belong to multiple factors, but they have clear focal points
atl_any_2 <- atl_any[!atl_any %in% c('ALA3', 'ALA4', 'ALB', 'ALD')]
sub2 <- data[, atl_any_2]

# finding number of factors using Kaiser's criterion
fa_test2 <- fa(sub2, nfactors = 10, rotate = "varimax")
eigenvalues2 <- fa_test2$values[1:10]
nf2 <- sum(eigenvalues2 > 1) # ev greate than 1

# Perform Factor analysis
fa_res2 <- fa(sub2, nfactors = nf2, rotate = "varimax")
print(fa_res2)

# Some questions do not clearly belong to one factor: ALA2, ALC6 -> drop
atl_any_3 <- atl_any_2[!atl_any_2 %in% c('ALA2', 'ALC6')]
sub3 <- data[, atl_any_3]
fa_test3 <- fa(sub3, nfactors = 5, rotate = "varimax")
eigenvalues3 <- fa_test3$values[1:5]
nf3 <- sum(eigenvalues3 > 1) # ev greate than 1
fa_res3 <- fa(sub3, nfactors = 4, rotate = "varimax")
print(fa_res3)

# alternative path using nf3
# drop ALA1, ACTS1, ACTS3
# atl_any_4 <- atl_any_3[!atl_any_3 %in% c('ALA1', 'ACTS1', 'ACTS3')]
# sub4 <- data[, atl_any_4]
# fa_test4 <- fa(sub4, nfactors = 5, rotate = "varimax")
# eigenvalues4 <- fa_test4$values[1:5]
# nf4 <- sum(eigenvalues4 > 1)
# fa_res4 <- fa(sub4, nfactors = nf4, rotate = "varimax")
# print(fa_res4)
# # we are heading toward a innovation / technology solution with this path....

# Perform Factor analysis
fa_res3 <- fa(sub3, nfactors = 4, rotate = "varimax")
print(fa_res3)

# Some questions do not clearly belong to one factor: AC3 -> has same loading for factor
atl_any_4 <- atl_any_3[!atl_any_3 %in% c('AC3')]
sub4 <- data[, atl_any_4]
fa_res4 <- fa(sub4, nfactors = 4, rotate = "varimax")
print(fa_res4)

# -> all questions have a clear focal point
# check that each factor is consistent -> pick abs(loading) >= 0.3
mr1_var4 <- c('AC1', 'AC8', 'AC9', 'AC10', 'AC11', 'AC14') # innovation/achieve smth
mr2_var4 <- c('AIT1', 'AIT2', 'AIN0', 'AIN1') # technology
mr3_var4 <- c('ALC3', 'ALC2', 'ALC5') # diligent students perception
mr4_var4 <- c('ALA1', 'ACTS1', 'ACTS3') # Curiosity
kron_alpha_mr14 <- alpha(na.omit(sub4[, mr1_var4]), check.keys = T) # 0.75
kron_alpha_mr24 <- alpha(na.omit(sub4[, mr2_var4]), check.keys = T) # 0.64
kron_alpha_mr34 <- alpha(na.omit(sub4[, mr3_var4]), check.keys = T) # 0.55
kron_alpha_mr44 <- alpha(na.omit(sub4[, mr4_var4]), check.keys = T) # 0.45 -> loading seems ok from theoretical PoV

# report nicely factor analysis
m <- fa_res4$loadings
coef_str <- c()

for(i in seq(1, length(atl_any_4))){
  ci <- paste('$', atl_any_4[i], '$ ', sep='')
  for(fi in seq(1, nf)){
    ci <- paste(ci, ' & $', format(round(m[i,fi], d)), '$', sep='')
  }
  coef_str[i] <- paste(ci, '\\\\[3pt]\n', sep='')
}
cat(coef_str)

# compute a score to see how each participant loads on the respective factors
scores4 <- factor.scores(f=fa_res4, x = sub4)
s <- scores4$scores
colnames(s) <- c('MR1_4', 'MR2_4', 'MR3_4', 'MR4_4')
data <- cbind(data, s)

# Going to far in the iterative process make the factor too distinct although atl_all
# is compoed of many traits (motivation. curiosity, ...)

# Factor analysis (2) ---------------------------------------------------------
# using Pure ATL variables
colSums(is.na(data[,atl_all]))
sub <- data[, atl_all]

# finding number of factors using Kaiser's criterion
fa_test <- fa(sub, nfactors = 10, rotate = "varimax")
eigenvalues <- fa_test$values[1:10]
nf <- sum(eigenvalues > 1) # ev greate than 1
# -> only 1 factor....
fa_res <- fa(sub, nfactors = nf, rotate = "varimax")

kron_alpha <- alpha(na.omit(sub[, atl_all]), check.keys = T) # 0.36

# ALD should be dropped to increase alpha
sub2 <- data[, atl_all[!atl_all %in% c('ALD')]]
fa_test2 <- fa(sub2, nfactors = 5, rotate = "varimax")
eigenvalues2 <- fa_test2$values[1:10]
nf2 <- sum(eigenvalues2 > 1) # not even one factor...
kron_alpha <- alpha(na.omit(sub2), check.keys = T) # 0.43
# but there are no clear gains from removing one question anymore

# Factor analysis (3) ---------------------------------------------------------
# Test many variables, but the motivation for some is unclear
atl_any <- c(atl_all, paste0('AC', seq(1,18)), 'AIN0', 'AIN1', 'ACTS1', 'ACTS3')
colSums(is.na(data[,atl_any])) # [!atl_any %in% c('ALC5', 'ALC6')]
sub <- data[, atl_any]

# finding number of factors using Kaiser's criterion
fa_test <- fa(sub, nfactors = 10, rotate = "varimax")
eigenvalues <- fa_test$values[1:10]
nf <- sum(eigenvalues > 1) # ev greate than 1

# Perform Factor analysis
fa_res <- fa(sub, nfactors = nf, rotate = "varimax")
print(fa_res) # as a rule of thumb |loading| > 0.3 -> means that the question contributes significantly to the factor

# factor MR1: Entrepreneur (AC6, AC7, AC16 & AC10)
# factor MR3: Money (AC4, AC5 & AC12)
# factor MR2: Personal growth (AC2, AC3, ALA1, ACTS3 & AC15)
# factor MR4: Perception of diligent students (ALC2, ALC3, ALA4 & ALC5)

# compute a score to see how each participant loads on the respective factors
scores <- factor.scores(f=fa_res, x = sub)
data <- cbind(data, scores$scores)

# check that each factor is consistent -> top 5
mr1_var <- c('AC10', 'AC6', 'AC7', 'AC8', 'AC16')
mr3_var <- c('AC4', 'AC5', 'AC12', 'AC13', 'AC11')
mr2_var <- c('AC3', 'AC2', 'AC15', 'AC1', 'AC9')
mr4_var <- c('ALC2', 'ALC3', 'ALC5', 'ALA2', 'AC14')
kron_alpha_mr1 <- alpha(na.omit(sub[, mr1_var]), check.keys = T) # 0.78
kron_alpha_mr2 <- alpha(na.omit(sub[, mr2_var]), check.keys = T) # 0.82
kron_alpha_mr3 <- alpha(na.omit(sub[, mr3_var]), check.keys = T) # 0.79 -> More reliable but validity is less clear...
# MR3 is not working in regression...
kron_alpha_mr4 <- alpha(na.omit(sub[, mr4_var]), check.keys = T) # 0.51
#######################################################################
## TEST 1: Link between highest diploma obtained and patience
#######################################################################

# params
y <- 'MR4' # 'MR4_4' / 'MR4' <- Only MR4 is working
x <- 'is_patient'
df <- data

if(y=='MR4'){
  df[[y]] <- scale(df[[y]])
}

# drop nan
cor(df %>% select(atl_all, atl_grade, berlin, MR1, MR2, MR3, MR4, is_patient, crt), 
    use='complete.obs')
test1 <- df
colSums(is.na(test1))
test1 <- na.omit(test1[, c(x, 'wealth', 'country_code', 'master', 'bachelor', 
                           'age', 'trust', 'female', y, 'family_size', 'Asia')])
xs <- colnames(test1 %>% select(-country_code, -y, -Asia))

#regression (panel regression)
# formulaff_1 <- as.formula(paste(
#   'atl ~ ', paste(xs, collapse = '+'), '|country_code| 0 |country_code',
#   ), sep='')
# regff_1 <- felm(formulaff_1, data=test1, exactDOF = T, cmethod='reghdfe')
# summary(regff_1)

#regression basic ols
formula_1 <- as.formula(paste(y, ' ~  +', paste(xs, collapse = '+'), sep=''))
formulaff_1 <- as.formula(paste(y, ' ~ ', paste(xs, collapse = '+'), '|0| 0 |Asia', sep=''))
reg_1 <- felm(formulaff_1, data = test1, exactDOF = T, cmethod='reghdfe')
summary(reg_1)

# results -> nice table
res <- summary(reg_1)$coefficients
coef_str <- c()
clean_var <- c('Intercept', 'IsPatient_{i}', 'Wealth_{i}', 'Master_{i}',
               'Bachelor_{i}', 'Age_{i}', 'Trust_{i}', 'IsFemale_{i}', 
               'FamilySize_{i}')

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
# Germany 8, Estonia 1, China 4, Vietnam 5, Japan 10, Taiwan 3

# params
y <- 'MR4' # atl_all / MR4
x <- 'is_patient' # crt / is_patient
undersampling <- F

# barplot country
test2_plot <- data %>% group_by(country_code) %>% 
  mutate(
    mean_is_patient = mean(get(x), na.rm=T),
    mean_atl = mean(get(y), na.rm=T),
    mean_HasUniDegree = mean(uni_degree, na.rm=T),
    n_obs = n()) %>% 
  ungroup() %>% 
  select(country_code, mean_atl, mean_is_patient, mean_HasUniDegree, n_obs) %>% 
  unique() %>% arrange(country_code)
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
colours <- c('dark blue', 'light blue', 'dark green', 'red', 'yellow', 'orange', 'black')
test2_plot_cont <- data %>% group_by(eu_country) %>% 
  mutate(
    mean_is_patient = mean(get(x), na.rm=T),
    mean_atl = mean(get(y), na.rm=T),
    mean_HasUniDegree = mean(uni_degree, na.rm=T),
    across(all_of(hof), ~mean(.x, na.rm=T))) %>% 
  ungroup() %>% 
  select(eu_country, mean_atl, mean_is_patient, mean_HasUniDegree, all_of(hof)) %>% 
  unique() %>% 
  arrange(eu_country)
barplot(cbind(mean_is_patient, mean_atl, mean_HasUniDegree, IDV, PDI, MAS, UAI) ~ eu_country, 
        data=test2_plot_cont, 
        beside=T, names.arg=c('Asia', 'Europe'),
        xlab = '', ylab = 'Score', col=colours, ylim=c(0,1))
legend('top', col=colours, c('Patience', 'ATL', 'Has Uni Degree', hof), 
       lwd=2, inset = c(0, -0.02), ncol = 3, box.lty = 0)

# scatterplot
scatter.smooth(test2_plot$mean_atl, test2_plot$mean_is_patient, span = 1,
               degree = 1, xlab = 'Atitude Toward learning score',
               ylab = 'Patience score')

# drop nan
test2 <- data
colSums(is.na(test2))
test2 <- na.omit(test2[, c(x, 'wealth', 'country_code', 'master', 'bachelor', 
                           'age', 'trust', 'female', y, 'family_size', 'Asia',
                           all_of(hof))])
xs <- colnames(test2 %>% select(-country_code, -y, -Asia))

if(y=='MR4'){
  test2[[y]] <- scale(test2[[y]])
}

# run regression for each country
models <- seq(1,4) #sort(as.numeric(unique(test2$country_code)))
formula_2 <- as.formula(paste(
  y, ' ~ 1 + ', paste(xs, collapse = '+'), sep=''))
results_2 <- list()
n_obs <- c()

for(i in seq(1, length(models))){
  if(i%%2==1){
    sub <- test2[test2$Asia==0,]
  }else{
    sub <- test2[test2$Asia==1,]
  }
  
  if(undersampling==T){
    x0 <- sub[sub[[x]]==0,]
    x1 <- sub[sub[[x]]==1,]
    set.seed(14684)
    random_indices <- sample(1:dim(x1)[1], dim(x0)[1]*3)
    x1 <- x1[random_indices, ]
    sub <- rbind(x0, x1)
  }
  
  if(i>2){
    sub$w <- ifelse(sub[[x]] == 1, 1 / sum(sub[[x]] == 1), 1 / sum(sub[[x]] == 0)) / 2
    reg_2 <- lm(formula_2, data=sub, weights = w)
  }else{
    reg_2 <- lm(formula_2, data=sub) 
  }
  coef_2 <- summary(reg_2)$coefficients
  results_2[[i]] <- coef_2
  n_obs[i] <- dim(sub)[1]
}

# results -> nice table
coef_str <- c()
clean_var <- c('Intercept', 'IsPatient_{i}', 'Wealth_{i}', 'Master_{i}',
               'Bachelor_{i}', 'Age_{i}', 'Trust_{i}', 'IsFemale_{i}', 
               'FamilySize_{i}', 'IDV_{i}', 'PDI_{i}', 'MAS_{i}', 'UAI_{i}')

for(i in seq(1, length(clean_var))){
  ci <- paste('\\multirow{2}{*}{$', clean_var[i], '$}', sep='')
  si <- ''
  for(mi in seq(1, length(models))){
    if(mi==3){csep<-' && '}else{csep<-' & '}
    
    reg <- results_2[[mi]]
    ci <- paste(ci, csep, put_stars(reg[i,1], reg[i,2], 1, d), sep='')
    si <- paste(si, csep, '$(' , format(round(reg[i,3],d), scientific=F), ')$', sep='')
  }
  coef_str[i] <- paste(ci, '\\\\ \n ', si, '\\\\[3pt]\n', sep='')
}
cat(coef_str)
cat(paste(paste0('$', n_obs, '$'), collapse = ' & '))


#######################################################################
## TEST 3: Is the link between education and patience driven by cultural differences?
#######################################################################

data %>% group_by(Asia) %>% summarise(wealth = mean(wealth, na.rm=T),
                                      age = mean(age, na.rm=T),
                                      mast = mean(master, na.rm=T),
                                      crt = mean(crt, na.rm=T),
                                      patience = mean(is_patient, na.rm=T),
                                      all = mean(atl_all, na.rm=T))

# Params
y <- 'MR4'
x <- 'is_patient'
use_wls <- 'NO' # 'cond' | 'uncond' | 'NO'

# create interaction
test4 <- data %>% mutate(
  inter_asia_patience = is_patient * Asia,
  inter_idv_patience = is_patient * IDV,
  inter_pdi_patience = is_patient * PDI,
  inter_mas_patience = is_patient * MAS,
  inter_uai_patience = is_patient * UAI)

# drop nan
colSums(is.na(test4))
test4 <- na.omit(test4[, c(x, 'wealth', 'country_code', 'master', 'bachelor',
                           'age', 'trust', 'female', y, 'family_size',
                           hof, 'Asia', 'inter_asia_patience', 'inter_idv_patience',
                           'inter_pdi_patience', 'inter_mas_patience', 
                           'inter_uai_patience')])
xs <- colnames(test4 %>% select(-country_code, -y))

if(y=='MR4'){
  test4[[y]] <- scale(test4[[y]])
}

# run 2 regressions
regs <- seq(1,2)
results_4 <- list()
n_obs <- c()
if(use_wls=='cond'){
  test4$w[test4$eu_country==1] <- ifelse(test4[[x]][test4$eu_country==1] == 1, 1 / 
                                           sum(test4[[x]][test4$eu_country==1] == 1), 
                                         1 / sum(test4[[x]][test4$eu_country==1]==0))
  test4$w[test4$eu_country==0] <- ifelse(test4[[x]][test4$eu_country==0] == 1, 1 / 
                                           sum(test4[[x]][test4$eu_country==0] == 1), 
                                         1 / sum(test4[[x]][test4$eu_country==0]==0))
} else if(use_wls=='uncond'){
  test4$w <- ifelse(test4[[x]] == 1, 1 / sum(test4[[x]] == 1), 1 / sum(test4[[x]]==0))
}

for(i in regs){
  if(i==1){
    new_xs <- xs[! xs %in% c('inter_idv_patience', 'inter_pdi_patience', 
                             'inter_mas_patience', 'inter_uai_patience')]
  }else{
    new_xs <- xs
  }
  formula_4 <- as.formula(paste(y, ' ~', paste(new_xs, collapse='+'), sep=''))
  if(use_wls!='NO'){
    reg_4 <- lm(formula_4, data=test4, weights = w)
  } else if(use_wls=='NO'){
    reg_4 <- lm(formula_4, data=test4) 
  }
  coef_4 <- summary(reg_4)$coefficients
  results_4[[i]] <- coef_4
  n_obs[i] <- dim(test4)[1]
}

# results -> nice table
coef_str <- c()
clean_var <- c('Intercept', 'IsPatient_{i}', 'Wealth_{i}', 'Master_{i}',
               'Bachelor_{i}', 'Age_{i}', 'Trust_{i}', 'IsFemale_{i}',
               'FamilySize_{i}', 'IDV_{i}', 'PDI_{i}', 'MAS_{i}',
               'UAI_{i}', 'Asia_{i}', 'IsPatient*Asia_{i}', 'IsPatient*IDV_{i}', 
               'IsPatient*PDI_{i}', 'IsPatient*MAS_{i}', 'IsPatient*UAI_{i}')

for(i in seq(1, length(clean_var))){
  ci <- paste('\\multirow{2}{*}{$', clean_var[i], '$}', sep='')
  si <- ''
  for(mi in seq(1, length(regs))){
    if(mi==1 & i>length(clean_var)-4){
      ci <- paste(ci, ' & ', sep='')
      si <- paste(si, ' & ', sep='')
    } 
    # else if(mi==2 & i==length(clean_var)){
    #   ci <- paste(ci, ' & ', sep='')
    #   si <- paste(si, ' & ', sep='')
    else{
      reg <- results_4[[mi]]
      ci <- paste(ci, ' & ', put_stars(reg[i,1], reg[i,2], 1, 2), sep='')
      si <- paste(si, ' & $(' , format(round(reg[i,3],d), scientific=F), ')$', sep='')
    }
  }
  coef_str[i] <- paste(ci, '\\\\ \n ', si, '\\\\[3pt]\n', sep='')
}
cat(coef_str)
cat(paste(paste0('$', n_obs, '$'), collapse = ' & '))

new_xs <- xs[! xs %in% c('inter_idv_patience', 'inter_pdi_patience', 
                         'inter_mas_patience', 'inter_uai_patience')]
cor(test4[, new_xs]) # inter_eu_patience is quite corr with eu_country (0.9) // pdi_patience and patience (0.88)
sum(test4$is_patient, na.rm=T) / dim(test4)[1] # 76% are patient

# Not used anymore --------------------------------------------------------

atl_intra <- c('ALA1', 'ALA4', 'ALB', 'ALD')
atl_extra <- c('ALC2', 'ALC3', 'ALC5', 'ALC6')



data$n_var_atl_intra <- apply(!is.na(data[, atl_intra]), 1, sum)
data$n_var_atl_extra <- apply(!is.na(data[, atl_extra]), 1, sum)
data$atl_intra <-  rowSums(data[,atl_intra], na.rm = T) / data$n_var_atl_intra
data$atl_extra <-  rowSums(data[,atl_extra], na.rm = T) / data$n_var_atl_extra


atl_f <- atl_any[!atl_any %in% c('ALB2', 'ALD1', 'ALD4')]
data$n_var_atl_any <- apply(!is.na(data[, atl_f]), 1, sum)
data <- data[data$n_var_atl_any>0,]
data$atl_any <-  rowSums(data[,atl_f], na.rm = T) / data$n_var_atl_any

# create de-meaned atl variables
# atls <- c('atl_all', 'atl_intra', 'atl_extra')
# data <- data %>% group_by(country_code) %>% 
#   mutate(across(all_of(atls), ~mean(.x, na.rm=T), .names = 'mean_{.col}')) %>% 
#   ungroup() %>% 
#   mutate(
#     datl_all = atl_all - mean_atl_all,
#     datl_intra = atl_intra - mean_atl_intra,
#     datl_extra = atl_extra - mean_atl_extra)








# CRONBACH ALPHA
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

# trying more comprehensive pair
atl_e <- atl_any[!atl_any %in% c('ALB2', 'ALD4')]
sub <- na.omit(data[,atl_e])
kron_alpha_e <- alpha(sub, check.keys = T) # 0.71

# trying more comprehensive pair'
atl_f <- atl_any[!atl_any %in% c('ALB2', 'ALD1', 'ALD4')]
sub <- na.omit(data[,atl_f])
kron_alpha_f <- alpha(sub, check.keys = T) # 0.76

# #regression multilevel
# # with random intercept for each group (=country_code)
# formulaml_1 <- as.formula(paste(
#   'atl ~ ', paste(xs, collapse = '+'), ' + (1 |country_code)', sep=''))
# regml_1 <- lmer(formulaml_1, data = test1)
# summary(regml_1)
