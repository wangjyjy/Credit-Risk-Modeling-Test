setwd("E:/test/test data")

raw_account_testing <- read.csv("./raw_account_30_new.csv", header = T,
                                 
                                 na.strings = '', stringsAsFactors = F)





####################### payment_history_avg_dpd_0_29_bucket ##################################################



# mean count of accounts that is in 0-29 dpd bucket throughout 

# the payment history





account_customer_no <- raw_account_testing$customer_no[!duplicated(raw_account_testing$customer_no)]



number_of_account_each_customer <- as.matrix(table(unlist(raw_account_testing$customer_no)))

# head(number_of_account_each_customer)

# [,1]
# 1   16
# 2    1
# 3   11
# 4   25
# 5    6
# 6    6



number_of_account_each_customer <- cbind(row.names(number_of_account_each_customer), number_of_account_each_customer)

# tail(number_of_account_each_customer)

# [,1]    [,2]
# 10235 "10235" "26"
# 10236 "10236" "2" 
# 10237 "10237" "6" 
# 10238 "10238" "6" 
# 10239 "10239" "2" 
# 10240 "10240" "5" 



colnames(number_of_account_each_customer) <- c("customer_no", "number_of_account")



number_of_account_each_customer <- as.data.frame(number_of_account_each_customer)



payment_history <- paste(substr(raw_account_testing$paymenthistory1, 4, nchar(raw_account_testing$paymenthistory1)-3),
                         
                         ifelse(is.na(nchar(raw_account_testing$paymenthistory2)), "", 
                                
                                substr(raw_account_testing$paymenthistory2, 4, nchar(raw_account_testing$paymenthistory2)-3)),
                         
                         sep = "")

# head(payment_history)

# [1] "000"                                  "000000000000000"                     
# [3] "000000000000000"                      "000000000000000000000000000000000000"
# [5] "000000000"                            "000000000"  



number_of_payment_month <- nchar(payment_history)/3

# head(number_of_payment_month)

# [1] 1  5  5 12  3  3



max_number_of_payment_month <- max(number_of_payment_month)

# max_number_of_payment_month

# [1] 36





separate_payment_history <- matrix(nrow = nrow(raw_account_testing), ncol = max_number_of_payment_month)



for(i in 1:36){
  
  
  
  separate_payment_history[,i] <- substr(payment_history, 
                                         
                                         rep(1+3*(i-1), max_number_of_payment_month), 
                                         
                                         rep(3*i, max_number_of_payment_month))
  
  
  
}



num_separate_payment_history <- apply(separate_payment_history, 2, as.integer)



logi_separate_payment_history <- (num_separate_payment_history < 30)



logi_separate_payment_history_with_total_mth <- cbind(number_of_payment_month,logi_separate_payment_history)



fun_recognize_good_credit_history <- function(x) (sum(is.na(x[2:(x[1]+1)])) == 0) & (mean(na.omit(x[-1])) == 1)



logi_good_credit_history <- apply(logi_separate_payment_history_with_total_mth, 1, fun_recognize_good_credit_history)

## 0-29 dpd bucket throughout the payment history



amend_customer_no <- logi_good_credit_history * raw_account_testing$customer_no

# head(amend_customer_no)

# [1] 8226 8226 8226 8226 3439 3439



number_of_good_account_each_customer <- as.matrix(table(unlist(amend_customer_no)))

number_of_good_account_each_customer <- cbind(row.names(number_of_good_account_each_customer), number_of_good_account_each_customer)





colnames(number_of_good_account_each_customer) <- c("customer_no", "number_of_good_account")



number_of_good_account_each_customer <- as.data.frame(number_of_good_account_each_customer)





tmp <- merge(number_of_account_each_customer, 
             
             number_of_good_account_each_customer, by = "customer_no", all.x = T)



tmp <- apply(tmp, 2, as.numeric)



tmp <- cbind(tmp, payment_history_avg_dpd_0_29_bucket = tmp[,3]/tmp[,2])



mean_count_of_good_account_each_customer <- tmp[order(tmp[,1]),]



rm(tmp)



# colnames(mean_count_of_good_account_each_customer)

# [1] "customer_no"                         "number_of_account"                  

# [3] "number_of_good_account"              "payment_history_avg_dpd_0_29_bucket"

# 

# head(mean_count_of_good_account_each_customer[,4])

# [1] 0.3125000 1.0000000 0.1818182 0.3600000 0.5000000 0.5000000



derived_var <- mean_count_of_good_account_each_customer[,c(1,4)]









################################################################################################################

####################### total_diff_lastpaymt_opened_dt #########################################################



# The total duration between last payment date and account

# opened date of all accounts





opened_date <- raw_account_testing$opened_dt

last_payment_date <- raw_account_testing$last_paymt_dt

closed_date <- raw_account_testing$closed_dt



lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")



as_date_opened_date <- as.Date(opened_date, "%d-%b-%y")

as_date_last_payment_date <- as.Date(last_payment_date, "%d-%b-%y")

as_date_closed_date <- as.Date(closed_date, "%d-%b-%y")



account_date <- cbind(as_date_opened_date, as_date_last_payment_date, as_date_closed_date)



fun_diff_lastpaymt_opened_dt <- function(x) ifelse(is.na(x[1]), NA,ifelse(is.na(x[2]), x[3]-x[1], x[2]-x[1]))



diff_lastpaymt_opened_dt <- apply(account_date, 1, fun_diff_lastpaymt_opened_dt)



# head(diff_lastpaymt_opened_dt)

# [1]  NA 135 129 333  38  38




tmp <- diff_lastpaymt_opened_dt

tmp[is.na(tmp)] <- 0



replace_na_by_0_diff_lastpaymt_opened_dt <- tmp




rm(tmp)



diff_lastpaymt_opened_dt_with_customer_no <- 
  
  cbind(raw_account_testing$customer_no, replace_na_by_0_diff_lastpaymt_opened_dt)



tmp_total_diff_lastpaymt_opened_dt <- tapply(diff_lastpaymt_opened_dt_with_customer_no[,2], diff_lastpaymt_opened_dt_with_customer_no[,1], sum)

total_diff_lastpaymt_opened_dt <- as.matrix(tmp_total_diff_lastpaymt_opened_dt)



total_diff_lastpaymt_opened_dt[which(total_diff_lastpaymt_opened_dt == 0)] <- NA



derived_var <- cbind(derived_var, total_diff_lastpaymt_opened_dt)

colnames(derived_var)[3] <- "total_diff_lastpaymt_opened_dt"







################################################################################################################

####################### min_months_last_30_plus ################################################################



# The smallest number of months passed before first 30+ dpd

# appeared for each account.





fun_months_last_30_plus <- function(x) ifelse(sum(is.na(x[2:(x[1]+1)])) == x[1], NA, 
                                              
                                              x[1]-max(ifelse(length(which(x == 0)) > 0, max(which(x == 0))-1, 0)))



months_last_30_plus <- apply(logi_separate_payment_history_with_total_mth, 1, fun_months_last_30_plus)



# months_last_30_plus[length(months_last_30_plus)]

# [1] 2

# separate_payment_history[length(separate_payment_history[,1]),]

# [1] "000" "332" "361" "330" "302" "273" "272" "239" "209" "302" "332" "XXX" "296" "XXX"
# [15] "270" "242" "241" "212" "199" "XXX" "122" "150" "XXX" "119" "092" "118" "118" "091"
# [29] "118" "063" "062" "089" "090" "043" "003" "STD"



months_last_30_plus_with_customer_no <- cbind(raw_account_testing$customer_no, months_last_30_plus)



fun_min_months_last_30_plus <- function(x) ifelse(sum(is.na(x)) == length(x), NA, min(na.omit(x))) 



tmp_min_months_last_30_plus <- tapply(months_last_30_plus, raw_account_testing$customer_no, fun_min_months_last_30_plus)

min_months_last_30_plus <- as.matrix(tmp_min_months_last_30_plus)

# head(min_months_last_30_plus)

# [,1]
# 1    0
# 2   36
# 3    0
# 4    1
# 5    4
# 6    5


derived_var <- cbind(derived_var, min_months_last_30_plus)

colnames(derived_var)[4] <- "min_months_last_30_plus"









################################################################################################################

####################### utilisation_trend ################################################################



# [total cur_bal_amt / total credit limit] /

#   [mean cur_bal_amt / (mean credit limit+ mean_cashlimit)]



cur_bal_amt <- raw_account_testing$cur_balance_amt

credit_limit <- raw_account_testing$creditlimit

cash_limit <- raw_account_testing$cashlimit



fun_total_bal_amt_or_limit <- function(x) ifelse(sum(is.na(x)) == length(x), NA, sum(na.omit(x))) 

fun_mean_bal_amt_or_limit <- function(x) ifelse(sum(is.na(x)) == length(x), NA, sum(na.omit(x))/length(na.omit(x))) 



# numerator

tmp_total_cur_bal_amt <- tapply(cur_bal_amt, raw_account_testing$customer_no, fun_total_bal_amt_or_limit)

tmp_total_credit_limit <- tapply(credit_limit, raw_account_testing$customer_no, fun_total_bal_amt_or_limit)

tmp_total_cash_limit <- tapply(cash_limit, raw_account_testing$customer_no, fun_total_bal_amt_or_limit)



total_cur_bal_amt <- as.matrix(tmp_total_cur_bal_amt)

total_credit_limit <- as.matrix(tmp_total_credit_limit)

total_cash_limit <- as.matrix(tmp_total_cash_limit)



# denominator

tmp_mean_cur_bal_amt <- tapply(cur_bal_amt, raw_account_testing$customer_no, fun_mean_bal_amt_or_limit)

tmp_mean_credit_limit <- tapply(credit_limit, raw_account_testing$customer_no, fun_mean_bal_amt_or_limit)

tmp_mean_cash_limit <- tapply(cash_limit, raw_account_testing$customer_no, fun_mean_bal_amt_or_limit)



mean_cur_bal_amt <- as.matrix(tmp_mean_cur_bal_amt)

mean_credit_limit <- as.matrix(tmp_mean_credit_limit)

mean_cash_limit <- as.matrix(tmp_mean_cash_limit)





element_utilisation_trend <- cbind(total_cur_bal_amt, total_credit_limit, mean_cur_bal_amt, mean_credit_limit, mean_cash_limit)

colnames(element_utilisation_trend) <- c("total_cur_bal_amt", "total_credit_limit", "mean_cur_bal_amt", "mean_credit_limit", "mean_cash_limit")



element_utilisation_trend_na_to_0 <- element_utilisation_trend

element_utilisation_trend_na_to_0[is.na(element_utilisation_trend_na_to_0)] <- 0



tmp_utilisation_trend <- (element_utilisation_trend_na_to_0[,1] / element_utilisation_trend_na_to_0[,2]) / 
  
  (element_utilisation_trend_na_to_0[,3] / (element_utilisation_trend_na_to_0[,4] + element_utilisation_trend_na_to_0[,5]))

# head(tmp_utilisation_trend)

# 1        2        3        4        5        6 
# 3.561469      NaN 8.362651 8.598488 4.796875 3.192000 



tmp <- tmp_utilisation_trend

tmp[!is.finite(tmp)] <- NA

# head(tmp)

# 1        2        3        4        5        6 
# 3.561469       NA 8.362651 8.598488 4.796875 3.192000 



utilisation_trend <- tmp



rm(tmp)





derived_var <- cbind(derived_var, utilisation_trend)

colnames(derived_var)[5] <- "utilisation_trend"





################################################################################################################

####################### Ratio_currbalance_creditlimit ##########################################################



# [total cur_bal_amt / total credit limit]





colnames(element_utilisation_trend_na_to_0)

# [1] "total_cur_bal_amt"  "total_credit_limit" "mean_cur_bal_amt"   "mean_credit_limit"  "mean_cash_limit"  



tmp_ratio_currbalance_creditlimit <- element_utilisation_trend_na_to_0[,1] / element_utilisation_trend_na_to_0[,2]



tmp <- tmp_ratio_currbalance_creditlimit

tmp[!is.finite(tmp)] <- NA



ratio_currbalance_creditlimit <- tmp



rm(tmp)





orig_derived_var <- derived_var



derived_var <- cbind(derived_var, ratio_currbalance_creditlimit)

colnames(derived_var)[6] <- "ratio_currbalance_creditlimit"







################################################################################################################

####################### mean_diff_lastpaymt_opened_dt ##########################################################



# The average duration between last payment date and account

# opened date of all accounts



fun_mean_diff <- function(x) ifelse(sum(is.na(x)) == length(x), NA, sum(na.omit(x))/length(na.omit(x))) 



tmp_mean_diff_lastpaymt_opened_dt <- tapply(diff_lastpaymt_opened_dt, raw_account_testing$customer_no, fun_mean_diff)



mean_diff_lastpaymt_opened_dt <- as.matrix(tmp_mean_diff_lastpaymt_opened_dt)





orig_derived_var <- derived_var



derived_var <- cbind(derived_var, mean_diff_lastpaymt_opened_dt)

colnames(derived_var)[7] <- "mean_diff_lastpaymt_opened_dt"







################################################################################################################

####################### payment_history_mean_length ############################################################



# average length of payment_history variable



payment_history_length <- cbind(raw_account_testing$customer_no, number_of_payment_month*3)



fun_mean_length <- function(x) ifelse(sum(is.na(x)) == length(x), NA, sum(na.omit(x))/length(na.omit(x))) 



tmp_payment_history_mean_length <- tapply(payment_history_length[,2], payment_history_length[,1], fun_mean_length)



payment_history_mean_length <- as.matrix(tmp_payment_history_mean_length)





orig_derived_var <- derived_var



derived_var <- cbind(derived_var, payment_history_mean_length)

colnames(derived_var)[8] <- "payment_history_mean_length"







################################################################################################################

####################### derived variable export ################################################################



t <- derived_var



t[is.na(t)] <- ""



write.csv(t,"./account_30_derived_var.csv")
