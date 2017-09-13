setwd("E:/test/test data")
raw_enquiry_training <- read.csv("./raw_enquiry_70_new.csv", header = T,
                                 na.strings = '', stringsAsFactors = F)
enquiry_customer_no <- raw_enquiry_training$customer_no[!duplicated(raw_enquiry_training$customer_no)]


####################################################################################################
####################### count_enquiry_recency_365 ##################################################

# number of enquiry made in past 365days


enquiry_dt_opened <- raw_enquiry_training$dt_opened
enquiry_dt <- raw_enquiry_training$enquiry_dt

Sys.setlocale("LC_TIME", "C")

as_date_enquiry_dt_opened <- as.Date(enquiry_dt_opened, "%d-%b-%y")
as_date_enquiry_dt <- as.Date(enquiry_dt, "%d-%b-%y")

open_enquiry_dt <- cbind(as_date_enquiry_dt_opened, as_date_enquiry_dt)

fun_diff_open_enquiry_dt <- function(x) ifelse(is.na(x[1]), NA,ifelse(is.na(x[2]), NA, x[1]-x[2]))

tmp_diff_open_enquiry_dt <- apply(open_enquiry_dt, 1, fun_diff_open_enquiry_dt)

diff_open_enquiry_dt <- as.matrix(tmp_diff_open_enquiry_dt)

fun_count_enquiry_recency_365 <- function(x) ifelse(sum(!is.na(x))==0, NA, sum(na.omit(x) <= 365))

tmp_count_enquiry_recency_365 <- tapply(diff_open_enquiry_dt, raw_enquiry_training$customer_no, fun_count_enquiry_recency_365)

count_enquiry_recency_365 <- as.matrix(tmp_count_enquiry_recency_365)
# head(count_enquiry_recency_365)
# [,1]
# 1    1
# 2   19
# 3    0
# 4    0
# 5    0
# 6    3
# 
# diff_open_enquiry_dt[which(raw_enquiry_training$customer_no == 6)]
# [1]  199  236  313  689  826  832  888  915  917 1369 1723 1833 1844 1944 1974 1975 1982 2000
# [19] 2012 2211 2290 2386 2391 2945 3046 3422 3423

derived_var <- cbind(enquiry_customer_no, count_enquiry_recency_365)
colnames(derived_var) <- c("customer_no", "count_enquiry_recency_365")



####################################################################################################
####################### mean_diff_open_enquiry_dt ##################################################

# average difference between enquiry dt_opened date and enquiry date


fun_mean_diff <- function(x) ifelse(sum(is.na(x)) == length(x), NA, sum(na.omit(x))/length(na.omit(x))) 

tmp_mean_diff_open_enquiry_dt <- tapply(diff_open_enquiry_dt, raw_enquiry_training$customer_no, fun_mean_diff)

mean_diff_open_enquiry_dt <- as.matrix(tmp_mean_diff_open_enquiry_dt)


orig_derived_var <- derived_var

derived_var <- cbind(derived_var, mean_diff_open_enquiry_dt)
colnames(derived_var)[3] <- "mean_diff_open_enquiry_dt"



###########################################################################################
####################### max_freq_enquiry ##################################################

# most frequent enquiry purpose


enq_purpose <- raw_enquiry_training$enq_purpose

fun_max_freq_enquiry <- function(x) ifelse(sum(!is.na(x))==0, NA, 
                                           sort(x[!duplicated(x)])[which(table(x) == max(table(x)))])

tmp_max_freq_enquiry <- tapply(enq_purpose, raw_enquiry_training$customer_no, fun_max_freq_enquiry)


max_freq_enquiry <- as.matrix(tmp_max_freq_enquiry)


orig_derived_var <- derived_var

derived_var <- cbind(derived_var, max_freq_enquiry)
colnames(derived_var)[4] <- "max_freq_enquiry"



###########################################################################################
####################### count_enquiry_recency_90 #############################################

# number of enquiry made in past 90 days


fun_count_enquiry_recency_90 <- function(x) ifelse(sum(!is.na(x))==0, NA, sum(na.omit(x) <= 90))

tmp_count_enquiry_recency_90 <- tapply(diff_open_enquiry_dt, raw_enquiry_training$customer_no, fun_count_enquiry_recency_90)

count_enquiry_recency_90 <- as.matrix(tmp_count_enquiry_recency_90)

# head(count_enquiry_recency_90)
# [,1]
# 1    0
# 2    2
# 3    0
# 4    0
# 5    0
# 6    0
# 
# diff_open_enquiry_dt[which(raw_enquiry_training$customer_no == 2)]
# [1]   69   75   92  120  131  162  221  230  232  241  250  251  253  255  259  278  319  328
# [19]  335  395  398  405  413  453  454  474  498  510  540  573  579  580  595  602  610  616
# [37]  623  630  672  721  801  805  824  839  839  868  922  946  949  977  985 1006 1019 1117
# [55] 1128 1231 1260 1362 1524 1620 1693 1887 1954 1978 2090 3060 3060 3063

orig_derived_var <- derived_var

derived_var <- cbind(derived_var, count_enquiry_recency_90)
colnames(derived_var)[5] <- "count_enquiry_recency_90"



###########################################################################################
####################### perc_unsecured_others #############################################

# ratio of secured loan type enquiry purpose to total enquiry purpose made

mapping <- read.csv("./loan_type_mapping.csv", header = T)
mapping <- mapping[,1:3]

colnames(mapping)
# [1] "code"           "Loan.Type"      "Logi.Loan.Type"


fun_secured_loan_type_enquiry <- function(x) sum((mapping[,1] %in% x) * mapping[,3])

tmp_number_of_secured_loan_type_enquiry <- tapply(enq_purpose, raw_enquiry_training$customer_no, fun_secured_loan_type_enquiry)

number_of_secured_loan_type_enquiry <- as.matrix(tmp_number_of_secured_loan_type_enquiry)
# number_of_secured_loan_type_enquiry[which(is.na(number_of_secured_loan_type_enquiry))]
# integer(0)

fun_total_enquiry <- function(x) ifelse(sum(!is.na(x))==0, NA, length(na.omit(x)))

tmp_total_enquiry <- tapply(enq_purpose, raw_enquiry_training$customer_no, fun_total_enquiry)

total_enquiry <- as.matrix(tmp_total_enquiry)

perc_unsecured_others <- number_of_secured_loan_type_enquiry / total_enquiry
  

orig_derived_var <- derived_var

derived_var <- cbind(derived_var, perc_unsecured_others)
colnames(derived_var)[6] <- "perc_unsecured_others" 
  
  
################################################################################################################
####################### derived variable export ################################################################

t <- derived_var

t[is.na(t)] <- ""

write.csv(t,"./enquiry_70_derived_var.csv") 
  
  
  
  
  
  