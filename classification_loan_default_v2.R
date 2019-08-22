#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# India ML Hiring Hacakthon
# Code submitted by Shubhra Karmahe
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#Problem Statement - Loan Delinquency Prediction

#Loan default prediction is one of the most critical and crucial problem faced by financial 
#institutions and organizations as it has a noteworthy effect on the profitability of these 
#institutions. In recent years, there is a tremendous increase in the volume of non - performing
#loans which results in a jeopardizing effect on the growth of these institutions.

#Therefore, to maintain a healthy portfolio, the banks put stringent monitoring and evaluation
# measures in place to ensure timely repayment of loans by borrowers. Despite these measures, 
#a major proportion of loans become delinquent. Delinquency occurs when a borrower misses a 
#payment against his/her loan.

#Given the information like mortgage details, borrowers related details and payment details, 
# our objective is to identify the delinquency status of loans for the next month given the
#delinquency status for the previous 12 months (in number of months).

#-----------------------------------------------------------------------------------------------
# load the required libraries

load.libraries <- c('data.table', 
                    'lubridate', 
                    'stringr', 
                    
                    'tidyr', 
                    'ggplot2',
                    'h2o',
                    'DataExplorer',
                    'Information',
                    'gridExtra')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]

for(libs in install.lib) install.packages(libs, 
                                          dependencies = TRUE)
sapply(load.libraries, 
       require, 
       character = TRUE)

# turn off scientific notataion display of numbers
options(scipen = 999)

#------------------------------Import train and test datasets------------------
train <- fread('train.csv')
test <- fread('test.csv')

# Preliminary analysis
head(train)
str(train)
summary(train)
dim(train) # 116058 * 29
uniqueN(train) # no duplicates

head(test)
str(test)
summary(test)
dim(test) # 35866 * 28
uniqueN(test) # no duplicates

#--------------------------------Data Preparation ------------------------------

# origination_date - different formats in test(d/m/y) and train(y-m-d) sets
train$origination_date<- ymd(train$origination_date)
test$origination_date <- dmy(test$origination_date)

#first_payment_date - 
train$first_payment_date <- myd(train$first_payment_date,truncated = 1)
test$first_payment_date <-  myd(test$first_payment_date,truncated = 1)


# Let's merge train and test to begin with data prep.
# Add a new column is_train to differentiate between train and test records
test$m13 <- 0
train$is_train <- TRUE
test$is_train <- FALSE

# loan id removed before data merge
dataset <- rbind(train[,-1],test[,-1])

#Missing values - none
sapply(dataset, function(x) sum(is.na(x))/nrow(dataset)*100)

# target variable - m13 (unbalanced classes - need to use "balance classes" parameter in model building)
summary(factor(train$m13))

#-------------------------Profiling Report on raw dataset -------------------------------------

create_report(dataset)

# Basic EDA report is attached as report.html

#-------------------------Univariate Analysis ------------------------------------------------

train$m13 <- factor(train$m13)

# Payment default is high when Source value is X and least for Z
ggplot(train, aes(source, fill = m13)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = round(..count../nrow(train)*100,2)),vjust=-1) +
  scale_fill_brewer(palette = "Dark2")+
  xlab("Source") +
  ylab("Frequency")
  
# Payment default is highre for Financial institutions tagged as OTHERS
ggplot(train, aes(financial_institution, fill = m13)) + 
  geom_bar(color = "black") + 
  #geom_text(stat = "count", aes(label = round(..count../nrow(train)*100,2)),vjust=-1) +
  scale_fill_brewer(palette = "Dark2") + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Financial Institution") +
  ylab("Frequency")
  
# payment default is higher when number of borrower = 1
ggplot(train, aes(factor(number_of_borrowers), fill = m13)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = round(..count../nrow(train)*100,2)),vjust=-1) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("Number of borrowers") +
  ylab("Frequency")
 
# Payment default is similar for all the mentioned values of loan purpose
ggplot(train, aes(loan_purpose, fill = m13)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = round(..count../nrow(train)*100,2)),vjust=-1) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("Loan Purpose") +
  ylab("Frequency")
  
# Payment default is high when insurance type is 0 (i.e no insurance is avaialble)
ggplot(train, aes(factor(insurance_type), fill = m13)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = round(..count../nrow(train)*100,2)),vjust=-1) +
  scale_fill_brewer(palette = "Dark2") + 
  xlab("Insurance Type") +
  ylab("Frequency")

# Payment default is high when origination month is Jan or Feb
ggplot(train, aes(factor(months(train$origination_date,abbreviate = T)), fill = m13)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = round(..count../nrow(train)*100,2)),vjust=-1) +
  scale_fill_brewer(palette = "Dark2") + 
  xlab("Origination Month") +
  ylab("Frequency")

# Payment default is high when first payment month is Apr or March
ggplot(train, aes(factor(months(train$first_payment_date,abbreviate = T)), fill = m13)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "count", aes(label = round(..count../nrow(train)*100,2)),vjust=-1) +
  scale_fill_brewer(palette = "Dark2") + 
  xlab("First Payment Month") +
  ylab("Frequency")

# Payment default is high when interest rate is between 4 to 4.5
ggplot(train, aes(interest_rate, fill = m13)) + 
  geom_histogram(color = "black", bins = 30) + 
  scale_fill_brewer(palette = "Dark2") + 
  xlab("Interest Rate") +
  ylab("Frequency")

# Payment default is high when unpaid principal balance is belwo 250000
ggplot(train, aes(unpaid_principal_bal, fill = m13)) + 
  geom_histogram(color = "black", bins = 70) + 
  scale_fill_brewer(palette = "Dark2") + 
  xlab("Unpaid Principal Balance") +
  ylab("Frequency")

# Payment default is high when loan term is near by maximum value
ggplot(train, aes(loan_term, fill = m13)) + 
  geom_histogram(color = "black", bins = 30) + 
  scale_fill_brewer(palette = "Dark2") + 
  xlab("Loan Term") +
  ylab("Frequency")

# Payment default is high when loan to value ration is between 70 to 80
ggplot(train, aes(loan_to_value, fill = m13)) + 
  geom_histogram(color = "black", bins = 30) + 
  scale_fill_brewer(palette = "Dark2") + 
  xlab("Loan to Value Ratio") +
  ylab("Frequency")

# Payment default is high when debt to income ration is between 35 to 45
ggplot(train, aes(debt_to_income_ratio, fill = m13)) + 
  geom_histogram(color = "black", bins = 20) + 
  scale_fill_brewer(palette = "Dark2") + 
  xlab("Debt to income Ratio") +
  ylab("Frequency")

ggplot(train, aes(borrower_credit_score, fill = m13)) + 
  geom_histogram(color = "black", bins = 100) + 
  scale_fill_brewer(palette = "Dark2") + 
  xlab("Borrower credit score") +
  ylab("Frequency")

#-------------------------------------------WOE /IV analysis-----------------------------------

#deriving WOE /IV values for all columns

IV <- create_infotables(data = train,
                        y = "m13",
                        bins = 10,
                        parallel = T)

head(IV)
IV_Value = data.frame(IV$Summary)

grid.table(IV$Summary[seq(from = 1, to = 20, by = 1),], rows = NULL)

IV_plot <- IV$Summary[order(-IV$Summary$IV), ]

IV_plot$Variable <-
  factor(IV_plot$Variable, 
         levels = IV_plot$Variable[order(-IV_plot$IV)])
ggplot(IV_plot,
       aes(x = Variable, y = IV)) +
  geom_bar(width = .35,stat = "identity",
           color = "blue",fill = "red") +
  ggtitle("Woe- Important variables based on Information value analysis") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))

IV_plot

master_test <- train

# Change the levels to "good " and "bad"
master_test$m13 <- ifelse(master_test$m13 == 1, 
                          "bad", 
                          "good")

# Rechange the levels to "1" for good customer and "0" for bad customers
#(This change is only for the IV package)
master_test$m13 <- ifelse(master_test$m13 == "good",
                          1, 
                          0)

IV_new <- create_infotables(data = train,
                            y = "m13",
                            bins = 10,
                            parallel = T)

IV_new$Summary
head(IV_new)

IV_Value = data.frame(IV_new$Summary)

IV_dataframe <- IV_new$Summary
str(IV_dataframe)

# Following the thumb rule, a variable is:
# Useless if IV is < 0.02
# Weak if IV is [0.02, 0.1)
# Medium if IV is [0.1, 0.3)
# Strong if IV is[0.3, 0.5) and suspicious thereafter
for (i in 1:nrow(IV_dataframe)) {
  if (IV_dataframe$IV[i] < 0.02) {
    IV_dataframe$feedback[i] = "Useless"
    
  } else if (IV_dataframe$IV[i] >= 0.02 &
             IV_dataframe$IV[i] < 0.1) {
    IV_dataframe$feedback[i] = "Weak"
    
  } else if (IV_dataframe$IV[i] >= 0.1 &
             IV_dataframe$IV[i] < 0.28) {
    IV_dataframe$feedback[i] = "Medium"
    
  } else if (IV_dataframe$IV[i] >= 0.29 &
             IV_dataframe$IV[i] < 0.5) {
    IV_dataframe$feedback[i] = "Strong"
    
  } else if (IV_dataframe$IV[i] > 0.1 & IV_dataframe$IV[i] < 0.3) {
    IV_dataframe$feedback[i] = "Suspicious"
  }
}

str(IV_dataframe)
IV_dataframe$var <- as.factor(IV_dataframe$Variable)
IV_dataframe$predictor <- as.factor(IV_dataframe$feedback)
IV_dataframe
## Extract "Strong" and "Medium" variables
imp_vars <-
  which(IV_dataframe$feedback == "Strong" |
          IV_dataframe$feedback == "Medium")
df1 <- IV_dataframe[imp_vars, 1]
imp <- which(colnames(master_test) %in% df1)
str(master_test)

IV_Value <-
  create_infotables(
    data = master_test,
    y = "m13",
    bins = 10,
    parallel = T
  )

IV_Value$Summary

woe_table <- data.frame(IV_Value$Summary)

knitr::kable(head(IV_Value$Summary))


plot_infotables(IV_Value, IV_Value$Summary$Variable[1:9], show_values = TRUE)

#-------------------------------Derived variables ---------------------------------------

# derived variable missed payment between m1 and m6
dataset[which((dataset$m9 + dataset$m8 + dataset$m7+  dataset$m6 + dataset$m5 +
                 dataset$m4 + dataset$m3 +
                dataset$m2 + dataset$m1) > 0),'missed_payment'] <- 1

dataset[which((dataset$m9 + dataset$m8 + dataset$m7+
                 dataset$m6 + dataset$m5 + dataset$m4 + dataset$m3 +
                 dataset$m2 + dataset$m1) == 0),'missed_payment'] <- 0

# interest_rate_bin
summary(dataset$interest_rate)

dataset[which(dataset$interest_rate < 1),
        'interest_rate_bin'] <- '0-1'
dataset[which(dataset$interest_rate >= 1 & dataset$interest_rate < 2),
        'interest_rate_bin'] <- '1-2'
dataset[which(dataset$interest_rate >= 2 & dataset$interest_rate < 3),
        'interest_rate_bin'] <- '2-3'
dataset[which(dataset$interest_rate >= 3 & dataset$interest_rate < 4),
        'interest_rate_bin'] <- '3-4'
dataset[which(dataset$interest_rate >= 4 & dataset$interest_rate < 5),
        'interest_rate_bin'] <- '4-5'
dataset[which(dataset$interest_rate >= 5 & dataset$interest_rate < 6),
        'interest_rate_bin'] <- '5-6'
dataset[which(dataset$interest_rate >= 6 & dataset$interest_rate < 7),
        'interest_rate_bin'] <- '6-7'
dataset[which(dataset$interest_rate >= 7 ),
        'interest_rate_bin'] <- '7+'

# unpaid_principal_bal_bin
summary(dataset$unpaid_principal_bal)
# Outliers are present in unpaid_principal_bal
boxplot(dataset$unpaid_principal_bal)

dataset[which(dataset$unpaid_principal_bal < 50000),
        'unpaid_principal_amt_bin'] <- '<50'
dataset[which(dataset$unpaid_principal_bal < 100000 & dataset$unpaid_principal_bal >= 50000),
        'unpaid_principal_amt_bin'] <- '50-100'
dataset[which(dataset$unpaid_principal_bal < 150000 & dataset$unpaid_principal_bal >= 100000),
        'unpaid_principal_amt_bin'] <- '100-150'
dataset[which(dataset$unpaid_principal_bal < 200000 & dataset$unpaid_principal_bal >= 150000),
        'unpaid_principal_amt_bin'] <- '150-200'
dataset[which(dataset$unpaid_principal_bal < 250000 & dataset$unpaid_principal_bal >= 200000),
        'unpaid_principal_amt_bin'] <- '200-250'
dataset[which(dataset$unpaid_principal_bal >= 250000 ),
        'unpaid_principal_amt_bin'] <- '250+'

#origination_month from origination_date (as year and day part is similar)
dataset$origination_month <- months(dataset$origination_date,
                                    abbreviate = T)

#first_payment_month from origination_date (as year and day part is similar)
dataset$first_payment_month <- months(dataset$first_payment_date,
                                      abbreviate = T)

# diff_origination_date_first_payment_date
dataset$diff_origination_fp_date <- as.integer(dataset$first_payment_date - dataset$origination_date)

#loan_to_value_bin
unique(dataset$loan_to_value)

dataset[which(dataset$loan_to_value < 30),
        'ltv_bin'] <- '<30'
dataset[which(dataset$loan_to_value < 60 & dataset$loan_to_value >= 30),
        'ltv_bin'] <- '30-60'
dataset[which(dataset$loan_to_value < 90 & dataset$loan_to_value >= 60),
        'ltv_bin'] <- '60-90'
dataset[which(dataset$loan_to_value >= 90),
        'ltv_bin'] <- '90+'

#loan_term_bin
unique(dataset$loan_term)
summary(dataset$loan_term)
dataset[which(dataset$loan_term < 60),
        'loan_term_bin'] <- '<60'
dataset[which(dataset$loan_term >= 60 & dataset$loan_term < 120),
        'loan_term_bin'] <- '60-120'
dataset[which(dataset$loan_term >= 120 & dataset$loan_term < 180),
        'loan_term_bin'] <- '120-180'
dataset[which(dataset$loan_term >= 180 & dataset$loan_term < 240),
        'loan_term_bin'] <- '180-240'
dataset[which(dataset$loan_term >= 240 & dataset$loan_term < 300),
        'loan_term_bin'] <- '240-300'
dataset[which(dataset$loan_term >= 300 & dataset$loan_term < 360),
        'loan_term_bin'] <- '300-360'
dataset[which(dataset$loan_term >= 360 ),
        'loan_term_bin'] <- '360+'

#dti_ratio_bin
summary(dataset$debt_to_income_ratio)

dataset[which(dataset$debt_to_income_ratio < 20),
        'dti_ratio_bin'] <- '<20'
dataset[which(dataset$debt_to_income_ratio >= 20 & dataset$debt_to_income_ratio < 40),
        'dti_ratio_bin'] <- '20-40'
dataset[which(dataset$debt_to_income_ratio >= 40 & dataset$debt_to_income_ratio < 60),
        'dti_ratio_bin'] <- '40-60'
dataset[which(dataset$debt_to_income_ratio >= 60),
        'dti_ratio_bin'] <- '60+'

#borrower_credit_score_bin
summary(dataset$borrower_credit_score)
unique(dataset$borrower_credit_score)

dataset[which(dataset$borrower_credit_score < 600),
        'borrower_credit_score_bin'] <- '<600'
dataset[which(dataset$borrower_credit_score >= 600 & dataset$borrower_credit_score < 700),
        'borrower_credit_score_bin'] <- '600-700'
dataset[which(dataset$borrower_credit_score >= 700 & dataset$borrower_credit_score < 800),
        'borrower_credit_score_bin'] <- '700-800'
dataset[which(dataset$borrower_credit_score >= 800),
        'borrower_credit_score_bin'] <- '800+'

#borrower_credit_score_avl
dataset[which(dataset$borrower_credit_score == 0),'
        borrower_credit_score_avl'] <- 0
dataset[which(dataset$borrower_credit_score != 0),
        'borrower_credit_score_avl'] <- 1

#co_borrower_credit_score
summary(dataset$`co-borrower_credit_score`)
unique(dataset$`co-borrower_credit_score`)

dataset[which(dataset$`co-borrower_credit_score` < 600),
        'co_borrower_credit_score_bin'] <- '<600'
dataset[which(dataset$`co-borrower_credit_score` >= 600 & dataset$`co-borrower_credit_score` < 700),
        'co_borrower_credit_score_bin'] <- '600-700'
dataset[which(dataset$`co-borrower_credit_score` >= 700 & dataset$`co-borrower_credit_score` < 800),
        'co_borrower_credit_score_bin'] <- '700-800'
dataset[which(dataset$`co-borrower_credit_score` >= 800),
        'co_borrower_credit_score_bin'] <- '800+'

#borrower_credit_score_avl
dataset[which(dataset$`co-borrower_credit_score` == 0),
        'co_borrower_credit_score_avl'] <- 0
dataset[which(dataset$`co-borrower_credit_score` != 0),
        'co_borrower_credit_score_avl'] <- 1


#insurance_percent_bin
summary(dataset$insurance_percent)
unique(dataset$insurance_percent)

dataset[which(dataset$insurance_percent < 20),
        'insurance_percent_bin'] <- '<20'
dataset[which(dataset$insurance_percent >= 20 & dataset$insurance_percent < 30),
        'insurance_percent_bin'] <- '20-30'
dataset[which(dataset$insurance_percent >= 30 & dataset$insurance_percent < 40),
        'insurance_percent_bin'] <- '30-40'
dataset[which(dataset$insurance_percent >= 40 ),
        'insurance_percent_bin'] <- '40+'

#insurance_percent_zero
dataset[which(dataset$insurance_percent == 0),
        'insurance_percent_zero'] <- 1
dataset[which(dataset$insurance_percent != 0),
        'insurance_percent_zero'] <- 0

#first_payment_default
#unique(dataset$m1)
dataset[which(dataset$m1 > 0),
        'First_payment_default'] <- 1
dataset[which(dataset$m1 == 0),
        'First_payment_default'] <- 0

unique(dataset$m1) # 0,1,2,3
unique(dataset$m2) # 0,1,2,3,4
unique(dataset$m3) #0,1,2,3,4,5
unique(dataset$m4) # 0,1,2,3,4,5,6
unique(dataset$m5) #0,1,2,3,4,5,6,7
unique(dataset$m6) #0,1,2,3,4,5,6,7,8
unique(dataset$m7) #0,1,2,3,4,5,6,7,8,9
unique(dataset$m8) # 0:10 except 8
unique(dataset$m9) # 0:11 except 9
unique(dataset$m10) # 0:12 except 10
unique(dataset$m11)  # 0:13 except 12
unique(dataset$m12)  # 0:14 except 12,13

# convert to proper data type
dataset$source <- factor(str_to_lower(dataset$source))
dataset$financial_institution <- factor(str_to_lower(dataset$financial_institution))
dataset$loan_purpose <- factor(str_to_lower(dataset$loan_purpose))

dataset$insurance_type <- factor(dataset$insurance_type)
dataset$number_of_borrowers <- as.factor(dataset$number_of_borrowers)

dataset$borrower_credit_score_avl <- as.factor(dataset$borrower_credit_score_avl)
dataset$co_borrower_credit_score_avl <- as.factor(dataset$co_borrower_credit_score_avl)
dataset$insurance_percent_zero <- as.factor(dataset$insurance_percent_zero)
dataset$First_payment_default <- as.factor(dataset$First_payment_default)


#--------------------------PCA for m1 to m12 -----------------------------------------------

pc_m12_1 <- stats::prcomp(dataset[,c('m12','m11','m10', 'm9','m8','m7','m6','m5','m4','m3','m2','m1')])
summary(pc_m12_1)
#-----------------------------Final dataset  ---------------------------------------------------------

# Based on WOE/IV Analysis - strong predictor columns are used for model building

final_data <- dataset[,c('m13',
                         'm12','m11','m10',
                         'm9','m8',
                         #'m7','missed_payment',
                         'debt_to_income_ratio',
                         'borrower_credit_score',
                         #'interest_rate','loan_term',
                         'is_train')]

# seperate test and train
train_data <- final_data[which(final_data$is_train == TRUE),]
test_data <- final_data[which(final_data$is_train == F),]

train_data$is_train <- NULL
test_data$is_train <- NULL
test_data$m13 <- NULL
train_data$m13 <- factor(train_data$m13)

summary(train_data$m13)

str(train_data)

?glm
#------------------------Logistic Regression-----------------------------------------

h2o.init()

lr_h2o <-h2o.glm(y = 'm13',
                training_frame = as.h2o(train_data),
                family = 'binomial',
                nfolds = 10,
                remove_collinear_columns = T,
                seed = 1,
                balance_classes = T)

summary(lr_h2o)

h2o.std_coef_plot(lr_h2o, 
                  num_of_features = 20)

lr_pred <- h2o.predict(lr_h2o, 
                       as.h2o(test_data))

write.csv(cbind(loan_id = test$loan_id,
                as.data.frame(lr_pred$predict)),
          'sub_lr_1.csv',
          row.names = F)

# F1 -score of 0.319148936170213 (quite acceptable score) 

#-------------------------------------Naive bayes-------------------------
#h2o.init()

#train_data_nb <- dataset[which(dataset$is_train == TRUE),]
#test_data_nb <- dataset[which(dataset$is_train == F),]

#train_data_nb$is_train <- NULL
#test_data_nb$is_train <- NULL
#test_data_nb$m13 <- NULL
#train_data_nb$m13 <- factor(train_data_nb$m13)

nb_h2o <- h2o.naiveBayes(y = 'm13',
                   training_frame = as.h2o(train_data))

summary(nb_h2o)

nb_pred <- h2o.predict(nb_h2o, 
                       as.h2o(test_data))

write.csv(cbind(loan_id = test$loan_id,
                as.data.frame(nb_pred)), 
          'sub_nb.csv',
          row.names = F)

# 0.21409921671018275. F1-score at leaderboard

#---------------------------- random forest ---------------------------------
#h2o.init()

rf_h2o <- h2o.randomForest(y = 'm13',
                         training_frame = as.h2o(train_data),
                         balance_classes = T,
                         seed = 1)

summary(rf_h2o)

rf_pred <- h2o.predict(rf_h2o, 
                       as.h2o(test_data))

write.csv(cbind(loan_id = test$loan_id,
                as.data.frame(rf_pred)), 
          'sub_rb.csv',
          row.names = F)

#0.0419899589228663 F1-score at leaderboard (worst score among all models)

#---------------------------GBM-------------------------------------

#h2o.init()

gbm_h2o <- h2o.gbm(y = 'm13',
                   training_frame = as.h2o(train_data),
                   balance_classes = T)

summary(gbm_h2o)

gbm_pred <- h2o.predict(gbm_h2o, 
                        as.h2o(test_data))

write.csv(cbind(loan_id = test$loan_id,
                as.data.frame(gbm_pred$predict)), 
          'sub_gbm.csv',
          row.names = F)

#0.303030303030303 F1-sc0ore at leaderboard

#--------------------------------Auto ML-----------------------------

#h2o.init()

aml_final <- h2o.automl(y = 'm13',
                        training_frame = as.h2o(train_data),
                        max_models = 20,
                        sort_metric = "logloss",
                        balance_classes = T,
                        seed = 1,
                        nfolds = 10)
                        
# View the AutoML Leaderboard
lb <- aml_final@leaderboard
lb
head(lb,nrow(lb)) # Entire leaderboard

pred_auto_ml_final <- h2o.predict(aml_final, as.h2o(test_data))

#------------------------------------Submission File creation---------------------------------------------------------

# As per domain knowledege and EDA results, default is more likely to occur when -
# 1. debt to income ration >= 35
# 2. borrower credit score <= 650
# 3. loan to value ratio >= 95
# 4. past missed payment in the past months

# on the top of model predictions, above assumptions are taken into consideration 
# to pick more default cases.


final_cal_m13_default <- cbind(loan_id = test$loan_id,
                               test_data,
                               dataset[which(dataset$is_train == F),
                                       c('missed_payment','loan_to_value','insurance_percent')],
                               as.data.frame(pred_auto_ml_final$predict))


final_cal_m13_default$m13 <- ifelse(as.integer(final_cal_m13_default$predict) == 2, 1,0)



final_cal_m13_default[which(final_cal_m13_default$predict == 0 & 
                                                   final_cal_m13_default$m12 != 0 &
                              final_cal_m13_default$debt_to_income_ratio >= 35),'m13'] <- 1

final_cal_m13_default[which(final_cal_m13_default$predict == 0 & 
                              final_cal_m13_default$m12 == 0 &
                              final_cal_m13_default$missed_payment == 1 &
                              final_cal_m13_default$debt_to_income_ratio >= 44),'m13'] <- 1

final_cal_m13_default[which(final_cal_m13_default$predict == 0 & 
                              final_cal_m13_default$m12 == 0 &
                              final_cal_m13_default$missed_payment == 1 &
                              final_cal_m13_default$borrower_credit_score <= 650),'m13'] <- 1

final_cal_m13_default[which(final_cal_m13_default$predict == 0 & 
                              final_cal_m13_default$m12 == 0 &
                              final_cal_m13_default$missed_payment == 1 &
                              final_cal_m13_default$loan_to_value >= 95),'m13'] <- 1


write.csv(final_cal_m13_default[,c('loan_id','m13')], 
          'sub_final.csv',
          row.names = F)

# 0.3362831858 - F1 score at leaderboard

#-------------------------------Final Model & Conculsion------------------------------

# Logistic regression performed pretty well as compared to Random Forest, GBM and Naive bayes.
# Auto ML of H2O package is used to choose the best model(with least logloss).
# Final Model selected is "StackedEnsemble_AllModels_AutoML_20190822_095104" (logloss : 0.01935976) 

# Important predictor variables to determine m13 (payment default on 13th month)
# 1. m8 to m12 - previous four months delinquency status will increases the chances for default at 13th month.
# 2.debt_to_income_ratio is directly proportional to chances of being default
# 3.As borrower_credit_score decreases the chances of being default at payment increases.



