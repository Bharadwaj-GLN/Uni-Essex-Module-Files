getwd()
setwd("C:/Users/GLNB/Desktop/UNI-Essex-MSc-AI/Numerical-Analysis-NA_PCOM7E/NAPCOM7E_AssignmentData")


mydata<- read_sav("HSE 2011.sav") # Importing the data from local.
str(mydata) # Determining the structure of the data all at once. 

# The data has 10617 instances based on 58 features or columns.
# We will be considering approximately 70% the rows for our analysis.
sample_data<-  mydata[sample(nrow(mydata), 7000), ]


#### 2. Descriptive Statistics ####

# a. How many people are there in the sample?
# There are 7000 people in the sample.Execute the below line of code for the sample dataset.
sample_data

# b. What is the percentage of people who drink alcohol?

# install.packages("gmodels") #library installation
library(gmodels) # calling the library

# help("CrossTable") # function to be used to determine what % of people drink alcohol

ppl_drink<- CrossTable(sample_data$Sex, sample_data$dnnow) # executing the command using the cross table function.

# Total Observations in Table:  5632 
# 
# 
# | sample_data$dnnow 
# sample_data$Sex |         1 |         2 | Row Total | 
#   ----------------|-----------|-----------|-----------|
#   1 |      2117 |       390 |      2507 | 
#   |    10.738 |    39.532 |           | 
#   |     0.844 |     0.156 |     0.445 | 
#   |     0.478 |     0.324 |           | 
#   |     0.376 |     0.069 |           | 
#   ----------------|-----------|-----------|-----------|
#   2 |      2312 |       813 |      3125 | 
#   |     8.614 |    31.714 |           | 
#   |     0.740 |     0.260 |     0.555 | 
#   |     0.522 |     0.676 |           | 
#   |     0.411 |     0.144 |           | 
#   ----------------|-----------|-----------|-----------|
#   Column Total |      4429 |      1203 |      5632 | 
#   |     0.786 |     0.214 |           | 
#   ----------------|-----------|-----------|-----------|

#Let's assume 1 is female and 2 is male for Sex variable and 1 is drinks alcohol and 2 is does not drink alcohol for dnnow variable.
# Based on the matrix, there are 2117 females that drink & 390 females that don't and 2312 males that drink 
# and 813 of them don't. Missing values are not included in the analysis.

# We can determine the % of people who drink alcohol by adding the values for both 1 and 2 for Sex variables and 
# just the 1 value for the dnnow variable. The total number of people is 4429, which is about 78.6% of the sample population.


# c. What is the percentage of women in the sample?
women_in_sample<- CrossTable(sample_data$Sex)
# Let's assume 1 is female and 2 is male for Sex variable. In that case, we have 3231 females, which is 46.2% and 3769 males, which is 53.8% 
# in the sample data.
# Total Observations in Table:  7000 
# 
# 
# |         1 |         2 | 
#   |-----------|-----------|
#   |      3231 |      3769 | 
#   |     0.462 |     0.538 | 
#   |-----------|-----------|
  


# d. what is the highest educational level?
highest_educ_level<- sample_data$topqual3
highest_educ_level

# Labels:
#
# value                  label
# -9                    Refused
# -8                 Don't know
# -7       Refused/not obtained
# -6      Schedule not obtained
# -2    Schedule not applicable
# -1             Not applicable
#  1  NVQ4/NVQ5/Degree or equiv
#  2     Higher ed below degree
#  3     NVQ3/GCE A Level equiv
#  4     NVQ2/GCE O Level equiv
#  5 NVQ1/CSE other grade equiv
#  6              Foreign/other
#  7           No qualification

# Based on the above command 1 is the highest educational level, which is,  NVQ4/NVQ5/Degree or equivalent.


# e. What is percentage of divorced and separated people?

Div_Sep_ppl<- sample_data$marstatc
Div_Sep_ppl
# # The following are the labels for Marital status variable:
# Labels:
# value                                         label
# -9                                         Refused
# -8                                      Don't know
# -7                            Refused/not obtained
# -6                           Schedule not obtained
# -2                         Schedule not applicable
# -1                                  Not applicable
#  1                                          Single
#  2                                         Married
#  3 Civil partnership including spontaneous answers
#  4                                       Separated
#  5                                        Divorced
#  6                                         Widowed
#  7                                      Cohabitees

pct_Div_Sep_ppl<- CrossTable(sample_data$marstatc)

# 
# Total Observations in Table:  5653 
# 
# 
# |         1 |         2 |         3 |         4 |         5 | 
#   |-----------|-----------|-----------|-----------|-----------|
#   |      1055 |      2977 |         4 |       138 |       389 | 
#   |     0.187 |     0.527 |     0.001 |     0.024 |     0.069 | 
#   |-----------|-----------|-----------|-----------|-----------|
#   
#   
#   |         6 |         7 | 
#   |-----------|-----------|
#   |       463 |       627 | 
#   |     0.082 |     0.111 | 
#   |-----------|-----------|

# Based on the above labels, 4 is separated and 5 is divorced. 
# The percentages of them respectively are determined by executing the pct_Div_Sep_ppl command.
# The percentage for separated (4) is 2.3% and for divorced(5) is 6.9% respectively. 
# The above results are calculated by excluding missing values in the sample data.


# f. Find the mean, median, mode, minimum, maximum, range and standard deviation of household size,
# BMI and age at last birthday.

#install.packages("modeest")
library(modeest) # for calculating mode
# Household Size
mean_hhold_size<- mean(sample_data$HHSize, na.rm = T) # mean
median_hhold_size<- median(sample_data$HHSize, na.rm = T) # median
mode_hhold_size<- mfv(sample_data$HHSize, na.rm = T) # mode
min_hhold_size<- min(sample_data$HHSize, na.rm = T) # minimum
max_hhold_size<- max(sample_data$HHSize, na.rm = T) # maximum
range_hhold_size<- diff(range(sample_data$HHSize), na.rm = T) # range
std_hhold_size<- sd(sample_data$HHSize, na.rm = T) # Std deviation

# BMI
# Checking and converting data type of column to numeric for descriptive stats.
str(sample_data$bmival)
sample_data$bmival<- as.numeric(sample_data$bmival)
typeof(sample_data$bmival)
is.numeric(sample_data$bmival)

mean_BMI<- mean(sample_data$bmival, na.rm = T) # mean
median_BMI<- median(sample_data$bmival, na.rm = T) # median
mode_BMI<- mfv(sample_data$bmival, na.rm = T) # mode
min_BMI<- min(sample_data$bmival, na.rm = T) # minimum
max_BMI<- max(sample_data$bmival, na.rm = T) # maximum
range_BMI<- diff(range(sample_data$bmival, na.rm = T)) # range
std_BMI<- sd(sample_data$bmival, na.rm = T) # Std deviation


# Age at last birthday
# Checking and converting data type of column to numeric for descriptive stats.
str(sample_data$Age)
sample_data$Age<- as.numeric(sample_data$Age)
typeof(sample_data$Age)
is.numeric(sample_data$Age)

mean_Age_bday<- mean(sample_data$Age, na.rm = T) # mean
median_Age_bday<- median(sample_data$Age, na.rm = T) # median
mode_Age_bday<- mfv(sample_data$Age, na.rm = T) # mode
min_Age_bday<- min(sample_data$Age, na.rm = T) # minimum
max_Age_bday<- max(sample_data$Age, na.rm = T) # maximum
range_Age_bday<- diff(range(sample_data$Age, na.rm = T)) # range
std_Age_bday<- sd(sample_data$Age, na.rm = T) # Std deviation



#### 3. Inferential Statistics ####

# a. Run a significance test to find out which gender drinks more alcohol.

ppl_drink<- CrossTable(sample_data$Sex, sample_data$dnnow) # This can also be used to determine which gender drinks more alcohol.

ppl_drink_data<- table(sample_data$Sex, sample_data$dnnow) # Converting cross table into a normal table
print(ppl_drink_data)

# We perform Chi-Squared test to interpret who drinks more alcohol on a descriptive level.
test1<-(chisq.test(ppl_drink_data))
print(test1)

#Pearson's Chi-squared test with Yates' continuity correction

# data:  ppl_drink_data
# X-squared = 89.977, df = 1, p-value < 2.2e-16

# Based on the above chi squared test output, it is evident that the P-value is lesser than 0.05.
# If null hypothesis was that gender plays a significant role in consuming alcohol, we can reject it by saying,
# it does not play a role in consuming alcohol.


# b. Run a significance test to find out which region drinks the most alcohol.

region_drink<- CrossTable(sample_data$gor1, sample_data$dnnow) # This can be used to determine which region drinks more alcohol.

# | sample_data$dnnow 
# sample_data$gor1 |         1 |         2 | Row Total | 
#   -----------------|-----------|-----------|-----------|
#   1 |       379 |        89 |       468 | 
#   |     0.327 |     1.203 |           | 
#   |     0.810 |     0.190 |     0.083 | 
#   |     0.086 |     0.074 |           | 
#   |     0.067 |     0.016 |           | 
#   -----------------|-----------|-----------|-----------|
#   2 |       535 |       181 |       716 | 
#   |     1.399 |     5.149 |           | 
#   |     0.747 |     0.253 |     0.127 | 
#   |     0.121 |     0.150 |           | 
#   |     0.095 |     0.032 |           | 
#   -----------------|-----------|-----------|-----------|
#   3 |       459 |       139 |       598 | 
#   |     0.270 |     0.994 |           | 
#   |     0.768 |     0.232 |     0.106 | 
#   |     0.104 |     0.116 |           | 
#   |     0.081 |     0.025 |           | 
#   -----------------|-----------|-----------|-----------|
#   4 |       410 |        90 |       500 | 
#   |     0.718 |     2.643 |           | 
#   |     0.820 |     0.180 |     0.089 | 
#   |     0.093 |     0.075 |           | 
#   |     0.073 |     0.016 |           | 
#   -----------------|-----------|-----------|-----------|
#   5 |       440 |       132 |       572 | 
#   |     0.214 |     0.789 |           | 
#   |     0.769 |     0.231 |     0.102 | 
#   |     0.099 |     0.110 |           | 
#   |     0.078 |     0.023 |           | 
#   -----------------|-----------|-----------|-----------|
#   6 |       513 |       113 |       626 | 
#   |     0.872 |     3.209 |           | 
#   |     0.819 |     0.181 |     0.111 | 
#   |     0.116 |     0.094 |           | 
#   |     0.091 |     0.020 |           | 
#   -----------------|-----------|-----------|-----------|
#   7 |       446 |       191 |       637 | 
#   |     6.025 |    22.181 |           | 
#   |     0.700 |     0.300 |     0.113 | 
#   |     0.101 |     0.159 |           | 
#   |     0.079 |     0.034 |           | 
#   -----------------|-----------|-----------|-----------|
#   8 |       744 |       176 |       920 | 
#   |     0.582 |     2.141 |           | 
#   |     0.809 |     0.191 |     0.163 | 
#   |     0.168 |     0.146 |           | 
#   |     0.132 |     0.031 |           | 
#   -----------------|-----------|-----------|-----------|
#   9 |       503 |        92 |       595 | 
#   |     2.632 |     9.690 |           | 
#   |     0.845 |     0.155 |     0.106 | 
#   |     0.114 |     0.076 |           | 
#   |     0.089 |     0.016 |           | 
#   -----------------|-----------|-----------|-----------|
#   Column Total |      4429 |      1203 |      5632 | 
#   |     0.786 |     0.214 |           | 

region_drink_data<- table(sample_data$gor1, sample_data$dnnow) # Converting cross table into a normal table
print(region_drink_data)

#   1       2

# 1 379    89
# 2 535   181
# 3 459   139
# 4 410    90
# 5 440   132
# 6 513   113
# 7 446   191
# 8 744   176
# 9 503   92

# If we assume 1-9 as regions for gor1 variable and 1 and 2 for drinks and not drinks alcohol respectively for dnnow variable,
# we can see that region 8 has most people who consume alcohol.

# We perform Chi-Squared test to interpret which region drinks more alcohol on a descriptive level.
test2<-(chisq.test(region_drink_data))
print(test2)

# Pearson's Chi-squared test
# 
# data:  region_drink_data
# X-squared = 65.941, df = 8, p-value = 3.144e-11


# Based on the above chi squared test output, it is evident that the P-value is lesser than 0.05.
# If null hypothesis was that region plays a significant role in consuming alcohol, we can reject it by saying,
# it does not play a role in consuming alcohol.


# c. Investigate whether there is a statistical difference between men and women on the following variables:

# i. Valid height

# ii. Valid weight

# In order to determine whether there is a statistical difference between men and women on the above variables,
# we will have to:
# 1. form a null hypothesis
# 2. run an independent t test
# 3. Obtain the p-value and interpret the result based on it.

# The null hypothesis for the above values are:
# There is no statistical difference between men and women on valid height.
# There is no statistical difference between men and women on valid weight.

# Below are the t-tests for both height and weight

# Valid Height
val_heit<- t.test(htval~Sex,data = sample_data, var.equal=TRUE, alternative="less")
val_heit


# Two Sample t-test

# data:  htval by Sex
# t = 21.445, df = 5670, p-value = 1
# alternative hypothesis: true difference in means between group 1 and group 2 is less than 0
# 95 percent confidence interval:
#   -Inf 11.09919
# sample estimates:
#   mean in group 1 mean in group 2 
# 167.6007        157.2924 

# Based on the obtained P-value, we fail to reject the null hypothesis.
# The null hypothesis stands correct that there is no significant difference between men and women on valid height.

# Valid Weight
val_weit<- t.test(wtval~Sex,data = sample_data, var.equal=TRUE, alternative="less")
val_weit

# Two Sample t-test
# 
# data:  wtval by Sex
# t = 14.348, df = 5752, p-value = 1
# alternative hypothesis: true difference in means between group 1 and group 2 is less than 0
# 95 percent confidence interval:
#   -Inf 10.30207
# sample estimates:
#   mean in group 1 mean in group 2 
# 73.99762        64.75525 

# Based on the obtained P-value, we fail to reject the null hypothesis.
# The null hypothesis stands correct that there is no significant difference between men and women on valid weight.


# d. What is the correlation between whether a person drinks nowadays, total household income, age at last birthday and gender?

# Using cor function and omitting NA values to obtain correlation between said variables.
corr_sample_data<- cor(na.omit(sample_data[, c('dnnow','totinc','Age','Sex')]))
corr_sample_data

#           dnnow       totinc          Age          Sex
# dnnow  1.00000000  0.059394118  0.095697415  0.116136622
# totinc 0.05939412  1.000000000  0.047687107 -0.007977555
# Age    0.09569742  0.047687107  1.000000000 -0.008957291
# Sex    0.11613662 -0.007977555 -0.008957291  1.000000000
