setwd("E:/test/test data")
account <- read.csv("./account_70_derived_var.csv", header = T, na.strings = '', stringsAsFactors = F)
enquiry <- read.csv("./enquiry_70_derived_var.csv", header = T, na.strings = '', stringsAsFactors = F)
raw_data_70 <- read.csv("./raw_data_70_new.csv", header = T, na.strings = '', stringsAsFactors = F)


############################ merge all variables ########################################################

df_account <- as.data.frame(account[,-1])
df_enquiry <- as.data.frame(enquiry[,-1])
df_raw_data_70 <- as.data.frame(raw_data_70)

derived_var <- merge(df_account, df_enquiry, by = "customer_no")
all_var <- merge(derived_var, df_raw_data_70, by = "customer_no")


## check number of customers with good credit record and
## fall into 30 DPD+ bucket 

table(all_var$Bad_label)
#     0     1 
# 22892  1004 

## If value is 0 ¨C it means customer has good credit history
## If value is 1 ¨C it means customer has falls into 30 DPD + bucket


############################ delete data with more than 95% NAs #########################################

## check for all variables
insufficient_vars <- all_var[ ,sapply(all_var,function(x) {sum(is.na(x))/nrow(all_var) > 0.95})]
# colnames(insufficient_vars)
# [1] "feature_10" "feature_17" "feature_18" "feature_49" "feature_61" "feature_74"

all_var_del_insufficient_vars <- all_var[ ,sapply(all_var,function(x) {sum(is.na(x))/nrow(all_var) <= 0.95})]

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


class(model_set$Bad_label)
# [1] "integer"

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
fun_char_to_fact <- function(x) {if(class(x) == "character") x <- as.factor(x)}
dat <- apply(model_set, 2, fun_char_to_fact)

dat <- as.data.frame(dat)

## several features need to be transformed as numeric
col_to_be_num <- c("feature_3", "feature_7", "feature_29", "feature_30",
                   "feature_35", "feature_44", "feature_52", "feature_56",
                   "feature_64", "feature_65", "feature_66", "feature_69",
                   "feature_71")

var_col_to_be_num <- which(colnames(model_set) %in% col_to_be_num)
# var_col_to_be_num
# [1] 14 16 34 35 40 49 56 60 66 67 68 71 72

dat[,var_col_to_be_num][[1]] <- as.numeric(dat[,var_col_to_be_num][[1]])


############################ variables' IV and binning #########################################

library(woeBinning)

model_iv <- woe.binning(dat, 'Bad_label', dat)

iv_value <- model_iv[, c(1,3)]
# iv_value[1:10, ]
# [,1]                             [,2]    
# [1,] "mean_diff_open_enquiry_dt"      12.73144
# [2,] "ratio_currbalance_creditlimit"  11.36733
# [3,] "feature_20"                     10.49605
# [4,] "mean_diff_lastpaymt_opened_dt"  10.38277
# [5,] "total_diff_lastpaymt_opened_dt" 9.950712
# [6,] "feature_47"                     7.491298
# [7,] "utilisation_trend"              1.518612
# [8,] "feature_45"                     1.299038
# [9,] "feature_38"                     1.13217 
# [10,] "feature_22"                     1.128002
# 
# iv_value[which(iv_value[,1] == "payment_history_avg_dpd_0_29_bucket"), ]
# [[1]]
# [1] "payment_history_avg_dpd_0_29_bucket"
# 
# [[2]]
#
# iv.total.final 
# 0.08047071 
# 
## here "payment_history_avg_dpd_0_29_bucket" does not contributes the most to the model, which is different from the 
## given "Feature-Gain-Remarks" chart

# range(iv_value[,2])
# [1] 1.876582e-04 1.273144e+01
# 
#
## set the minimum IV be the 40th greatest

min_iv <- as.numeric(iv_value[40,2])

dat_with_binned_vars_added <- woe.binning.deploy(dat, model_iv, min.iv.total = min_iv)


############################ simplify model #########################################

## as we set the minimum IV being the IV of the 40th greatest var, we have 40 binned var now
## model Bad_label against binned vars

dat_binned <- dat_with_binned_vars_added[,(ncol(dat_with_binned_vars_added)-40):(ncol(dat_with_binned_vars_added))]


## fit in all vars first
tmp <- glm(Bad_label ~., dat_binned, family = binomial(link = 'logit'))

glm_all_var <- glm(Bad_label ~ ., dat_binned, family = binomial(link = 'logit'), mustart = fitted(tmp))

glm_step <- step(glm_all_var, direction = "both", trace = 0)

