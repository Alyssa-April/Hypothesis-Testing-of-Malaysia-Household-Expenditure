### DESCRIPTIVE STATISTICS (INCOME, EXPENDITURE, SAVINGS)

library(readxl)
library(psych)

# read in data that contains expenditure and income for 2014, 2016 and 2019
exp_inc_2014 <- read_excel("C:/Users/User/Desktop/Maths Stats with Computing/Project_Data/Household Ex&Inc 2014.xlsx")

exp_inc_2016 <- read_excel("C:/Users/User/Desktop/Maths Stats with Computing/Project_Data/Household Ex&Inc 2016.xlsx")

exp_inc_2019 <- read_excel("C:/Users/User/Desktop/Maths Stats with Computing/Project_Data/Household Ex&Inc 2019.xlsx")

# check if there are any missing values for the data in all three years
sum(is.na(exp_inc_2014))
sum(is.na(exp_inc_2016))
sum(is.na(exp_inc_2019))


# The data contains no missing values at all. No data cleaning is needed. 
# It would also be unfit to remove any outliers from the dataset as this household expenditure survey 
# is from a legitimate source and represents the natural distribution of income and expenditure 
# for Malaysian citizens.

#____________________________________________________________________________________________________

# create a new column called savings for each year
exp_inc_2014$savings_2014 <- exp_inc_2014$Jum_pendapatan_kasar - exp_inc_2014$Jum_perbelanjaan

exp_inc_2016$savings_2016 <- exp_inc_2016$Jumlah_Pendapatan_Kasar_Bulanan - exp_inc_2016$Jumlah_Perbelanjaan_Bulanan_01_1

exp_inc_2019$savings_2019 <- exp_inc_2019$Total_Inc - exp_inc_2019$Total_Exp_01_12

# create three new dataframes containing expenditure, income and savings for each of the three years
# the analysis (Objective 1 and 2) will only utilise these nine variables
data_2014 <- data.frame(exp_2014 = exp_inc_2014$Jum_perbelanjaan,
                        inc_2014 = exp_inc_2014$Jum_pendapatan_kasar,
                        sav_2014 = exp_inc_2014$savings_2014)

data_2016 <- data.frame(exp_2016 = exp_inc_2016$Jumlah_Perbelanjaan_Bulanan_01_1,
                        inc_2016 = exp_inc_2016$Jumlah_Pendapatan_Kasar_Bulanan,
                        sav_2016 = exp_inc_2016$savings_2016)

data_2019 <- data.frame(exp_2019 = exp_inc_2019$Total_Exp_01_12,
                        inc_2019 = exp_inc_2019$Total_Inc,
                        sav_2019 = exp_inc_2019$savings_2019)

#________________________________________________________________________________________________-


# histogram for income in 2014, 2016 and 2019 separately
layout(matrix(c(1,2), nrow = 1))

hist(data_2014$inc_2014, breaks = 50, col = "orange", 
     xlab = "Income", main = "Income Distribution in 2014")

hist(data_2016$inc_2016, breaks = 60, ylim = c(0,5000), col = "orange",
     xlab = "Income", main = "Income Distribution in 2016")

hist(data_2019$inc_2019, breaks = 70, ylim = c(0,5000), col = "orange",
     xlab = "Income", main = "Income Distribution in 2019")

# histogram for expenditure in 2014, 2016 and 2019 separately
layout(matrix(c(1,2), nrow = 1))

hist(data_2014$exp_2014, breaks = 50, ylim = c(0,5000), col = "orange", 
     xlab = "Expenditure", main = "Expenditure Distribution in 2014")

hist(data_2016$exp_2016, breaks = 50, ylim = c(0,4000), col = "orange", 
     xlab = "Expenditure", main = "Expenditure Distribution in 2016")

hist(data_2019$exp_2019, breaks = 60, ylim = c(0,4000), col = "orange", 
     xlab = "Expenditure", main = "Expenditure Distribution in 2019")


# histogram for savings in 2014, 2016 and 2019 separately
layout(matrix(c(1,2), nrow = 1))

hist(data_2014$sav_2014, breaks = 50, col = "orange", 
     xlab = "Savings", main = "Savings Distribution in 2014")

hist(data_2016$sav_2016, breaks = 70, ylim = c(0,8000), col = "orange", 
     xlab = "Savings", main = "Savings Distribution in 2016")

hist(data_2019$sav_2019, breaks = 70, col = "orange", 
     xlab = "Savings", main = "Savings Distribution in 2019")

# summary statistics
describe(data_2014)
describe(data_2016)
describe(data_2019)


#_________________________________________________________________________________


### HYPOTHESIS TESTING - ANOVA (OBJECTIVE 1)


exp_data <- data.frame(year = rep(c("2014", "2016", "2019"), times=c(14838, 14551, 16354)),
                       expenditure = c(data_2014$exp_2014,
                                       data_2016$exp_2016,
                                       data_2019$exp_2019))

# to use later when considering expenditure adjusted for inflation
exp_data_adjusted <- exp_data

# observe if expenditure distribution for all three years is normally distributed or not
hist(exp_data$expenditure, breaks = 100, col = "orange", 
     xlab = "Expenditure", main = "Expenditure Distribution in 2014, 2016 and 2019")


# log transform the expenditure values
exp_data$log_expenditure <- log(exp_data$expenditure)

# observe if log expenditure distribution for all three years is normally distributed or not
layout(1)
hist(exp_data$log_expenditure, breaks = 100, col = "orange", ylim = c(0,2000),
     xlab = "Log Expenditure", main = "Expenditure Distribution in 2014, 2016 and 2019 \nAfter Log Transformation")

# to view skewness of log expenditure distribution
describe(exp_data$log_expenditure)

# inspect equality of variances visually
boxplot(log_expenditure ~ year, xlab = 'Year', ylab = 'Log Expenditure', data = exp_data,
        main = "Boxplot for Log Expenditure in 2014, 2016 and 2019")

# use a statistical test for equality of variances
bartlett.test(log_expenditure ~ year, data = exp_data)

# ANOVA 
anova_test_exp <- aov(log_expenditure ~ year, data = exp_data)
summary(anova_test_exp)

# post-hoc analysis
TukeyHSD(anova_test_exp)
plot(TukeyHSD(anova_test_exp))


### ADJUST EXPENDITURE FOR INLATION (ACCORDING TO 2019)

library(priceR)

# adjust for inflation
exp_data_adjusted$adjusted_expenditure <- adjust_for_inflation(exp_data_adjusted$expenditure,
                                                               exp_data_adjusted$year, "MY", to_date = 2019)

# observe if expenditure distribution for all three years is normally distributed or not
hist(exp_data_adjusted$adjusted_expenditure, breaks = 100, col = "orange", ylim = c(0,12000),
     xlab = "Expenditure Adjusted for Inflation", main = "Adjusted Expenditure Distribution in 2014, 2016 and 2019")

# log transform adjusted expenditures
exp_data_adjusted$log_adj_exp <- log(exp_data_adjusted$adjusted_expenditure)


# observe if log of adjusted expenditure distribution for all three years is normally distributed or not
hist(exp_data_adjusted$log_adj_exp, breaks = 100, col = "orange", ylim = c(0,2000),
     xlab = "Log Expenditure Adjusted for Inflation", main = "Adjusted Expenditure Distribution in 2014, 2016 and 2019 \nAfter Log Transformation")

# to view skewness of log of adjusted expenditure distribution
describe(exp_data_adjusted$log_adj_exp)

# inspect equality of variances visually
boxplot(log_adj_exp ~ year, xlab = 'Year', ylab = 'Log Expenditure Adjusted for Inflation', data = exp_data_adjusted,
        main = "Boxplot for Log Adjusted Expenditure in 2014, 2016 and 2019")

# use a statistical test for equality of variances
bartlett.test(log_adj_exp ~ year, data = exp_data_adjusted)

# get the mean for expenditures adjusted for inflation in 2014, 2016 and 2019
mean(exp_data_adjusted$adjusted_expenditure[exp_data_adjusted$year == 2014])
mean(exp_data_adjusted$adjusted_expenditure[exp_data_adjusted$year == 2016])
mean(exp_data_adjusted$adjusted_expenditure[exp_data_adjusted$year == 2019])

# ANOVA
anova_test_exp <- aov(log_adj_exp ~ year, data = exp_data_adjusted)
summary(anova_test_exp)

# post-hoc analysis
TukeyHSD(anova_test_exp)
plot(TukeyHSD(anova_test_exp))


#__________________________________________________________________________________


### HYPOTHESIS TESTING - PROPORTION (OBJECTIVE 2)

# create a new column called perc_inc for each year
exp_inc_2016$perc_inc_2016 <- 0.2 * exp_inc_2016$Jumlah_Pendapatan_Kasar_Bulanan

exp_inc_2019$perc_inc_2019 <- 0.2 * exp_inc_2019$Total_Inc

exp_inc_2016$g_b_spender_2016 <- ifelse(exp_inc_2016$savings_2016 < exp_inc_2016$perc_inc_2016, 1, 0)

exp_inc_2019$g_b_spender_2019 <- ifelse(exp_inc_2019$savings_2019 < exp_inc_2019$perc_inc_2019, 1, 0)

# number of bad spenders in 2016
sum(exp_inc_2016$g_b_spender_2016)

# number of bad spenders in 2019
sum(exp_inc_2019$g_b_spender_2019)

# hypothesis testing for proportions
prop.test(x = c(3915, 3886), n = c(14551, 16354),
          alternative = "greater")

### BAR PLOT FOR THE NUMBER OF BAD VS GOOD SPENDERS IN 2016 AND 2019

g_b_spender <- data.frame(year = c(2016, 2016, 2019, 2019),
                          spender = c('Bad Spender', 'Good Spender', 'Bad Spender', 'Good Spender'),
                          count = c(sum(exp_inc_2016$g_b_spender_2016),
                                    length(exp_inc_2016$g_b_spender_2016) - sum(exp_inc_2016$g_b_spender_2016),
                                    sum(exp_inc_2019$g_b_spender_2019),
                                    length(exp_inc_2019$g_b_spender_2019) - sum(exp_inc_2019$g_b_spender_2019)))

library(ggplot2)

ggplot(g_b_spender, aes(x = factor(year), y = count, fill = spender)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = 'Year', y = 'Frequency', title = 'Number of Bad vs Good Spenders in 2016 and 2019',
       fill = 'Type of Spender') +
  theme_bw() 

