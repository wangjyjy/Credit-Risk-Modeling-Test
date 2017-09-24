setwd("E:/test/test data")

account_train <- read.csv("./account_70_derived_var.csv", header = T, na.strings = '', stringsAsFactors = F)
enquiry_train <- read.csv("./enquiry_70_derived_var.csv", header = T, na.strings = '', stringsAsFactors = F)
raw_data_70 <- read.csv("./raw_data_70_new.csv", header = T, na.strings = '', stringsAsFactors = F)

account_test <- read.csv("./account_30_derived_var.csv", header = T, na.strings = '', stringsAsFactors = F)
enquiry_test <- read.csv("./enquiry_30_derived_var.csv", header = T, na.strings = '', stringsAsFactors = F)
raw_data_30 <- read.csv("./raw_data_30_new.csv", header = T, na.strings = '', stringsAsFactors = F)


############################ merge all variables ########################################################

df_account_train <- as.data.frame(account_train[,-1])
df_enquiry_train <- as.data.frame(enquiry_train[,-1])
df_raw_data_70 <- as.data.frame(raw_data_70)

derived_var_train <- merge(df_account_train, df_enquiry_train, by = "customer_no")
all_var_train <- merge(derived_var_train, df_raw_data_70, by = "customer_no")

records_train <- nrow(all_var_train)

df_account_test <- as.data.frame(account_test[,-1])
df_enquiry_test <- as.data.frame(enquiry_test[,-1])
df_raw_data_30 <- as.data.frame(raw_data_30)

derived_var_test <- merge(df_account_test, df_enquiry_test, by = "customer_no")
all_var_test <- merge(derived_var_test, df_raw_data_30, by = "customer_no")

records_test <- nrow(all_var_test)

all_var <- rbind(all_var_train,all_var_test)

## check number of customers with good credit record and
## fall into 30 DPD+ bucket in training set

table(all_var$Bad_label[1:records_train])
#     0     1 
# 22892  1004 

## If value is 0 ¨C it means customer has good credit history
## If value is 1 ¨C it means customer has falls into 30 DPD + bucket


############################ delete data with more than 95% NAs #########################################


sapply(all_var[1:records_train,], function(x) sum(is.na(x)))

# customer_no payment_history_avg_dpd_0_29_bucket      total_diff_lastpaymt_opened_dt 
#           0                                 558                                  28 
# min_months_last_30_plus                   utilisation_trend       ratio_currbalance_creditlimit 
#                       73                                4120                                4024 
# mean_diff_lastpaymt_opened_dt         payment_history_mean_length           count_enquiry_recency_365 
#                           27                                   0                                 109 
# mean_diff_open_enquiry_dt                    max_freq_enquiry            count_enquiry_recency_90 
#                      109                                 109                                 109 
# perc_unsecured_others                           dt_opened                          entry_time 
#                   109                                   0                                  15 
# feature_1                           feature_2                           feature_3 
# 15                                2836                                2836 
# feature_4                           feature_5                           feature_6 
# 15                                  15                                  15 
# feature_7                           feature_8                           feature_9 
# 15                               22635                               22635 
# feature_10                          feature_11                          feature_12 
# 23845                                  15                                  15 
# feature_13                          feature_14                          feature_15 
# 13004                                7733                                  23 
# feature_16                          feature_17                          feature_18 
# 27                               22869                               23878 
# feature_19                          feature_20                          feature_21 
# 15                                   0                                  15 
# feature_22                          feature_23                          feature_24 
# 0                                  15                                 640 
# feature_25                          feature_26                          feature_27 
# 15                                  15                                3637 
# feature_28                          feature_29                          feature_30 
# 15                                  15                                  15 
# feature_31                          feature_32                          feature_33 
# 15                                  15                                  15 
# feature_34                          feature_35                          feature_36 
# 15                                  15                                5682 
# feature_37                          feature_38                          feature_39 
# 5682                                5682                                  15 
# feature_40                          feature_41                          feature_42 
# 15                                  15                                  15 
# feature_43                          feature_44                          feature_45 
# 15                                  15                               13713 
# feature_46                          feature_47                          feature_48 
# 94                                   0                               18399 
# feature_49                          feature_50                          feature_51 
# 23792                                  15                               11422 
# feature_52                          feature_53                          feature_54 
# 15                               11610                                  15 
# feature_55                          feature_56                          feature_57 
# 15                                  15                               21503 
# feature_58                          feature_59                          feature_60 
# 15                                  15                                  15 
# feature_61                          feature_62                          feature_63 
# 23887                                  15                                  15 
# feature_64                          feature_65                          feature_66 
# 15                                  15                                  15 
# feature_67                          feature_68                          feature_69 
# 15                                  15                                  15 
# feature_70                          feature_71                          feature_72 
# 15                                  15                                  15 
# feature_73                          feature_74                          feature_75 
# 20951                               23879                                  15 
# feature_76                          feature_77                          feature_78 
# 15                                   0                                  15 
# feature_79                           Bad_label 
# 15  


## check for all variables
insufficient_vars <- all_var[1:records_train ,sapply(all_var,function(x) {sum(is.na(x))/nrow(all_var) > 0.95})]
index_insufficient_vars <- which(colnames(all_var) %in% colnames(insufficient_vars))
# colnames(all_var)[index_insufficient_vars]
# [1] "feature_10" "feature_17" "feature_18" "feature_49" "feature_61" "feature_74"

all_var_del_insufficient_vars <- all_var[ ,-index_insufficient_vars]

## check for all customers' records
insufficient_customer <- all_var_del_insufficient_vars[apply(all_var_del_insufficient_vars, 1, function(x) {sum(is.na(x))/ncol(all_var_del_insufficient_vars) > 0.95}), ]
# insufficient_customer$customer_no
# integer(0)

rm(insufficient_customer)

## all customers have enough records

## dt_opened and entry_time has already be included in the derived variables
already_included_in_derived_var <- which(colnames(all_var_del_insufficient_vars) %in% c("dt_opened", "entry_time"))
# [1] 14 15

model_set <- all_var_del_insufficient_vars[, c(-1, -already_included_in_derived_var)]


############################ formatting #########################################

sapply(model_set, class)

# payment_history_avg_dpd_0_29_bucket      total_diff_lastpaymt_opened_dt             min_months_last_30_plus 
# "numeric"                           "integer"                           "integer" 
# utilisation_trend       ratio_currbalance_creditlimit       mean_diff_lastpaymt_opened_dt 
# "numeric"                           "numeric"                           "numeric" 
# payment_history_mean_length           count_enquiry_recency_365           mean_diff_open_enquiry_dt 
# "numeric"                           "integer"                           "numeric" 
# max_freq_enquiry            count_enquiry_recency_90               perc_unsecured_others 
# "integer"                           "integer"                           "numeric" 
# feature_1                           feature_2                           feature_3 
# "character"                         "character"                           "numeric" 
# feature_4                           feature_5                           feature_6 
# "numeric"                         "character"                           "numeric" 
# feature_7                           feature_8                           feature_9 
# "numeric"                         "character"                         "character" 
# feature_11                          feature_12                          feature_13 
# "character"                         "character"                         "character" 
# feature_14                          feature_15                          feature_16 
# "numeric"                         "character"                         "character" 
# feature_19                          feature_20                          feature_21 
# "numeric"                         "character"                         "character" 
# feature_22                          feature_23                          feature_24 
# "character"                         "character"                         "character" 
# feature_25                          feature_26                          feature_27 
# "numeric"                           "numeric"                         "character" 
# feature_28                          feature_29                          feature_30 
# "character"                           "numeric"                           "numeric" 
# feature_31                          feature_32                          feature_33 
# "numeric"                         "character"                         "character" 
# feature_34                          feature_35                          feature_36 
# "numeric"                           "numeric"                         "character" 
# feature_37                          feature_38                          feature_39 
# "character"                         "character"                           "numeric" 
# feature_40                          feature_41                          feature_42 
# "numeric"                           "numeric"                           "numeric" 
# feature_43                          feature_44                          feature_45 
# "character"                           "numeric"                         "character" 
# feature_46                          feature_47                          feature_48 
# "character"                         "character"                         "character" 
# feature_50                          feature_51                          feature_52 
# "character"                         "character"                           "numeric" 
# feature_53                          feature_54                          feature_55 
# "character"                         "character"                           "numeric" 
# feature_56                          feature_57                          feature_58 
# "numeric"                         "character"                         "character" 
# feature_59                          feature_60                          feature_62 
# "character"                         "character"                         "character" 
# feature_63                          feature_64                          feature_65 
# "character"                           "numeric"                           "numeric" 
# feature_66                          feature_67                          feature_68 
# "numeric"                           "numeric"                           "numeric" 
# feature_69                          feature_70                          feature_71 
# "numeric"                         "character"                           "numeric" 
# feature_72                          feature_73                          feature_75 
# "character"                         "character"                         "character" 
# feature_76                          feature_77                          feature_78 
# "numeric"                         "character"                           "numeric" 
# feature_79                           Bad_label 
# "character"                           "integer" 



model_set$Bad_label <- as.factor(model_set$Bad_label)

## delete duplicate features representing all date_of_birth
## and have lot 0 value or misformatted

dob <- c("feature_21", "feature_63", "feature_70", "feature_75")

## feature_2 is the same as entry_time
dup <- c("feature_2")

## feature_5 is either 'Card Setup' or NA; feature_6 is either 14 or NA
var_with_only_one_level <- c("feature_5","feature_6")

to_be_del_from_var <- which(colnames(model_set) %in% c(dob, dup, var_with_only_one_level))
# to_be_del_from_var
# [1] 14 17 18 30 70 77 81

model_set <- model_set[, -to_be_del_from_var]


## change all character vars to factor

fun_char_to_fact <- function(x) (if(class(x) == "character") {
  as.factor(x)
} else x)
dat <- lapply(model_set, fun_char_to_fact)

dat <- data.frame(dat)


sapply(dat, class)

# payment_history_avg_dpd_0_29_bucket      total_diff_lastpaymt_opened_dt             min_months_last_30_plus 
# "numeric"                           "integer"                           "integer" 
# utilisation_trend       ratio_currbalance_creditlimit       mean_diff_lastpaymt_opened_dt 
# "numeric"                           "numeric"                           "numeric" 
# payment_history_mean_length           count_enquiry_recency_365           mean_diff_open_enquiry_dt 
# "numeric"                           "integer"                           "numeric" 
# max_freq_enquiry            count_enquiry_recency_90               perc_unsecured_others 
# "integer"                           "integer"                           "numeric" 
# feature_1                           feature_3                           feature_4 
# "factor"                           "numeric"                           "numeric" 
# feature_7                           feature_8                           feature_9 
# "numeric"                            "factor"                            "factor" 
# feature_11                          feature_12                          feature_13 
# "factor"                            "factor"                            "factor" 
# feature_14                          feature_15                          feature_16 
# "numeric"                            "factor"                            "factor" 
# feature_19                          feature_20                          feature_22 
# "numeric"                            "factor"                            "factor" 
# feature_23                          feature_24                          feature_25 
# "factor"                            "factor"                           "numeric" 
# feature_26                          feature_27                          feature_28 
# "numeric"                            "factor"                            "factor" 
# feature_29                          feature_30                          feature_31 
# "numeric"                           "numeric"                           "numeric" 
# feature_32                          feature_33                          feature_34 
# "factor"                            "factor"                           "numeric" 
# feature_35                          feature_36                          feature_37 
# "numeric"                            "factor"                            "factor" 
# feature_38                          feature_39                          feature_40 
# "factor"                           "numeric"                           "numeric" 
# feature_41                          feature_42                          feature_43 
# "numeric"                           "numeric"                            "factor" 
# feature_44                          feature_45                          feature_46 
# "numeric"                            "factor"                            "factor" 
# feature_47                          feature_48                          feature_50 
# "factor"                            "factor"                            "factor" 
# feature_51                          feature_52                          feature_53 
# "factor"                           "numeric"                            "factor" 
# feature_54                          feature_55                          feature_56 
# "factor"                           "numeric"                           "numeric" 
# feature_57                          feature_58                          feature_59 
# "factor"                            "factor"                            "factor" 
# feature_60                          feature_62                          feature_64 
# "factor"                            "factor"                           "numeric" 
# feature_65                          feature_66                          feature_67 
# "numeric"                           "numeric"                           "numeric" 
# feature_68                          feature_69                          feature_71 
# "numeric"                           "numeric"                           "numeric" 
# feature_72                          feature_73                          feature_76 
# "factor"                            "factor"                           "numeric" 
# feature_77                          feature_78                          feature_79 
# "factor"                           "numeric"                            "factor" 
# Bad_label 
# "factor" 

############################ allocate NA's in factor vars to other level #########################

library(plyr)

fun_na_to_new_level <- function(x) (if(class(x) == "factor") {
  levels(x) <- c(levels(x), "level_na")
  x[is.na(x)] <- "level_na"
  x
} else x)

tmp <- colwise(fun_na_to_new_level)(dat[,-ncol(dat)])
dat <- data.frame(tmp, Bad_label = dat[,ncol(dat)])

rm(tmp)

class(dat[,13])
## [1] "factor"

levels(dat[,13])
# [1] "Golf Card"       "Insignia"        "Platinum Cricke" "Platinum Deligh" "Platinum Maxima"
# [6] "RBL Bank Fun+"   "Titanium Deligh" "level_na"  

sum(is.na(dat[,13]))
## [1] 0



############################ binning to reduce levels #########################################

library(woeBinning)

model_iv <- woe.binning(dat[1:records_train,], 'Bad_label', dat[1:records_train,])

iv_value <- model_iv[, c(1,3)]
# iv_value[1:10, ]
# [,1]         [,2]     
# [1,] "feature_20" 10.49605 
# [2,] "feature_47" 7.491298 
# [3,] "feature_45" 1.299038 
# [4,] "feature_38" 1.13217  
# [5,] "feature_22" 1.128002 
# [6,] "feature_77" 1.068198 
# [7,] "feature_16" 0.1733808
# [8,] "feature_7"  0.1587323
# [9,] "feature_53" 0.1280448
# [10,] "feature_15" 0.122556 


dat_with_binned_vars_added <- woe.binning.deploy(dat, model_iv)

dat_binned <- dat_with_binned_vars_added[,(ncol(dat_with_binned_vars_added)-78):(ncol(dat_with_binned_vars_added))]


fun_count_of_level <- function(x) ( tmp <- length(levels(x)) )

count_of_level <- colwise(fun_count_of_level)(dat_binned)

# count_of_level[,1:4]
#   Bad_label feature_20.binned feature_47.binned feature_45.binned
# 1         2                 3                 3                 4


fun_base_level <- function(x) ( tmp <- levels(x)[1] )

base_level <- colwise(fun_base_level)(dat_binned)

# base_level[,1:4]
# Bad_label feature_20.binned feature_47.binned feature_45.binned
# 1         0  misc. level neg.  misc. level neg.  misc. level neg.


############################ transform into dummy variables #########################################

fun_into_dummy_var <- function(x){
  
  if(class(x) == "factor"){
    n = length(x)
    data.fac = data.frame(x = x,y = 1:n)
    output = model.matrix(y~x,data.fac)[,-1]
    ## Convert factor into dummy variable matrix
  }else{
    output = x
    ## if x is numeric, output is x
  }
  output
}

dummy_var <- colwise(fun_into_dummy_var)(dat_binned)
dummy_var <- do.call(cbind, dummy_var)
dummy_var <-  as.data.frame(dummy_var)

colnames(dummy_var) <- paste(rep(paste(colnames(dat_binned),"_"),(count_of_level-1)), 
                             colnames(dummy_var))

# head(dummy_var[,1:9])
#   Bad_label _ Bad_label feature_20.binned _ xmisc. level pos. feature_20.binned _ xunknown
# 1                     0                                     0                            0
# 2                     0                                     0                            0
# 3                     0                                     0                            0
# 4                     0                                     0                            0
# 5                     0                                     0                            0
# 6                     0                                     0                            0
#   feature_47.binned _ xmisc. level pos. feature_47.binned _ xunknown feature_45.binned _ xmisc. level pos.
# 1                                     0                            0                                     0
# 2                                     0                            0                                     0
# 3                                     0                            0                                     0
# 4                                     0                            0                                     0
# 5                                     0                            0                                     0
# 6                                     0                            0                                     0
#   feature_45.binned _ xlevel_na + @GMAIL.COM feature_45.binned _ xunknown
# 1                                          0                            0
# 2                                          1                            0
# 3                                          1                            0
# 4                                          0                            0
# 5                                          1                            0
# 6                                          0                            0
#   feature_38.binned _ xmisc. level pos.
# 1                                     0
# 2                                     0
# 3                                     0
# 4                                     0
# 5                                     0
# 6                                     0





############################ xgboost #########################################

library(xgboost)

training_set <- dummy_var[1:records_train,]
testing_set <- dummy_var[(records_train+1):nrow(dummy_var),]


dtrain <- xgb.DMatrix(as.matrix(training_set[,-1]), label = training_set[,"Bad_label _ Bad_label"])
xg.model <- xgboost(data = dtrain, max.depth = 2, eta = 0.05, gamma = 4,
                    nthread = 3, nrounds = 50, verbose = 1,
                    objective = "binary:logistic", nfold = 5,
                    min.child.weight = 10, eval.metric = 'auc')

t <- xgb.importance(colnames(training_set[,-1]), model = xg.model)

res <- predict(xg.model, as.matrix(testing_set[,-1]))
pre <- ifelse(res > 0.5, 1,0)

table(pre, as.matrix(testing_set[,1]))
# pre    0    1
#   0 9712  459
#   1   66    3

accuracy <- mean(pre == as.matrix((testing_set[,1])))
# accuracy
# [1] 0.9487305