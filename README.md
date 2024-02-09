# parsimony
#setwd("C:/Sue/PhD/CMP9701-DS/Code/AutoScore-Imbalance-main (1)/AutoScore-Imbalance-main/")
#load("Train082123.Rdata")

#RERUN START LINE 287

library(car)
library(scorecard)
library(stats) #stepwise feature selection
library(MASS) #other stepwise feature selection
library ("plyr")
library(dplyr)
library(tidyr)
library(randomForestSRC)
library(caret)
library(purrr)
library(caret)
library(utils)
library(pROC)
library(AutoScore)
library(vctrs)
library(InformationValue)
library(pastecs)
library(openxlsx)
library(data.table)

# FEB'24 added
#install.packages("dummies")
#library(dummies)

aucRFarr <- c()
AutoScorenumvar <- c()
AUCkeep <- c() #AutoScore
AUCarr <- c() #AutoSCore
bestparams <- c()

#FEB24 add F1
f1arr <- c()

#12/17/2023
cmarr <- c()
fparr <- c()
fnarr <- c()
tparr <- c()
tnarr <- c()

sensarr <- c()
specarr <- c()

ppvarr <- c()
npvarr <- c()

fnrarr <- c()

aucRFci <- c()

##################################################################################
#ONE-TIME
#import raw file
train_transaction <- read.csv("C:/Sue/PhD/CMP9701-DS/Code/train_transaction.csv/train_transaction.csv", 
                              stringsAsFactors=TRUE)

#save imported data to file for reloading later
setwd("C:/Sue/PhD/CMP9701-DS/Code/AutoScore-Imbalance-main (1)/AutoScore-Imbalance-main/")
save(list = c("train_transaction"), file = "Train082123.Rdata")

#load previously imported data
setwd("C:/Sue/PhD/CMP9701-DS/Code/AutoScore-Imbalance-main (1)/AutoScore-Imbalance-main/")
load("Train082123.Rdata") #contains renamed vars, e.g., C01 instead of C1, C02 instead of C2, etc.
#to ensure unique var names

str(train_transaction, list.len=ncol(train_transaction))

#count levels
#ProductCD (no missings or blanks); card4 (no missings, 1577 blanks); 
  #card6 (no missings, 1571 blanks); P_emaildomain (no missings, 94456 blanks);
  #R_emaildomain (no missings, 453249 blanks), M04 (no missings, 281444 blanks)

#https://stackoverflow.com/questions/52807921/number-of-missing-values-in-each-column-in-r
sapply(train_transaction, function(x) sum(is.na(x)))

#https://stackoverflow.com/questions/54611619/how-to-determine-the-number-of-rows-containing-empty-strings-in-each-column
setDT(train_transaction)[, lapply(.SD, function(x) sum(x == ""))]

#REFERENCE only
#https://rstats101.com/find-levels-of-a-factor-in-r/
str(train_transaction$ProductCD)
levels(train_transaction$ProductCD)

str(train_transaction$card4)
levels(train_transaction$card4)

str(train_transaction$card6)
levels(train_transaction$card6)

str(train_transaction$P_emaildomain)
levels(train_transaction$P_emaildomain)

str(train_transaction$R_emaildomain)
levels(train_transaction$R_emaildomain)

str(train_transaction$M04)
levels(train_transaction$M04)

for (r in 1:1) {

#r = 1
  
shuffleseed = 42 + r
#https://tyagi-sudarshini.medium.com/how-to-shuffle-a-dataframe-in-r-by-rows-e7971cd7949e
set.seed(shuffleseed) #iteration 1: 42, iteration 2 + 10, etc.
rows <- sample(nrow(train_transaction))
train_transaction2 <- train_transaction[rows, ]

#remove TransactionID as that is a system-generated field
train_transaction2 <- train_transaction2[,-1]  #393 vars left

#prep target column for proper name for splitting later (using AutoScore library)
names(train_transaction2)[names(train_transaction2 )=="isFraud"] <- "label"

##########################################################################
#UPDATE Feb24: split dataset
str(train_transaction2, list.len=ncol(train_transaction2))
#factor vars: ProductCD, card4, card6, P_emaildomain, R_emaildomain, M04

#stratified datasets by categorical var
#https://stackoverflow.com/questions/54566428/caret-creating-stratified-data-sets-based-on-several-variables
set.seed(42 + r)
train_transaction2 <- train_transaction2  %>%
  mutate(n = row_number()) %>% #create row number if you dont have one
  select(n, everything()) # put 'n' at the front of the dataset
train <- train_transaction2 %>%
  group_by(ProductCD, card4, card6, P_emaildomain, R_emaildomain, M04) %>% #any number of variables you wish to partition by proportionally
  sample_frac(.8) # '.7' is the proportion of the original df you wish to sample

#117780, 394 vars 
test <- anti_join(train_transaction2 , train) # creates test dataframe with those observations not in 'train.'

#add validation set
set.seed(42)

#58670 obs, 394 vars
validate <- train %>%
    group_by(ProductCD, card4, card6, P_emaildomain, R_emaildomain, M04) %>% 
    sample_frac(.125) #to get 10% validation set, i.e., 0.1 (desired final portion of validation set) / 0.7 (denominator = original portion for train set)

#414090 obs, 394 vars
train2 <- anti_join(train, validate)

#################################################################################
#UPDATE Feb24: add factors for missing values
#colnames(X)[2] 

#https://stackoverflow.com/questions/44332262/using-loop-to-add-columns-in-r
#colnames(train2)

#exclude row_number n added earlier above and label (target var), hence start from col 3
#for (i in 3:394){

#  colnm <- paste(colnames(train2)[i], 'na', sep='')

  #train2[[colnm]] <- ''
  #train2[[colnm]] <- ifelse(is.na(train2[[i]]), 1, 0)
  
  #train2[[paste(colnames(train2)[200], 'na', sep='')]]  <- ''
  #train2[[paste(colnames(train2)[i], 'na', sep='')]]  <- with(train2, ifelse(is.na(train2[[i]]), 1, 0))

#  train2[ , paste(colnames(train2)[i], 'na', sep='')] <- with(train2, ifelse(is.na(train2[[i]]), 1, 0))
  
  #train2
  #train2 <- train2 %>% 
   # mutate(colnm = colnames(train2)[i])
  
  #https://stackoverflow.com/questions/70758157/mutate-columns-with-dynamic-names-using-for-loop-in-r
  ## get the values. ifelse() keeps the dimensions
  #results = ifelse(is.na(train2[[i]]), 1, 0)
  ## fix the names, replacing "b" with "k_et"
  #colnames(results) = (colnames(results), 'na', sep = '')
  ## stick them together
  #train2 = cbind(train2, results)
  
  #train2[, ncol(train2) + 1] <- ''
  #names(train2)[ncol(train2)] <- with(train2, ifelse(is.na(train2[[i]]), 1, 0))
  
  #df[[varname]] <- with(df, Petal.Width * i)
  #mutate(train2, {{colnm}} := ifelse(is.na(train2[[i]]), 1, 0))
  
  #dummy.data.frame(train2)
#}
warnings()

str(train2, list.len=ncol(train2))
colnames(train2)


#UPDATE Feb24: change data frame argument in var_filter from train_transaction2 to train

# filter variable via missing rate, iv, identical value rate
# https://rdocumentation.org/packages/scorecard/versions/0.4.3/topics/var_filter
dt_f_ = var_filter(train2, y="label")

ncol(dt_f_) #525

#with dummy missing vars (786 vars)
# ✔ 188 variables are removed via identical_rate
# ✔ 199 variables are removed via info_value
# ✔ Variable filtering on 414090 rows and 785 columns in 00:02:01
# ✔ 261 variables are removed in total

#no dummy missing vars (394 vars orig --> 314 vars after)
# ℹ Filtering variables via missing_rate, identical_rate, info_value ...
# ✔ 75 variables are removed via identical_rate
# ✔ 18 variables are removed via info_value
# ✔ Variable filtering on 414090 rows and 393 columns in 00:00:25
# ✔ 80 variables are removed in total
###########################################################################

#RESUME HERE
#filtered set after scorecard initial filtering above
setwd("C:/Sue/PhD/CMP9701-DS/Code/")
save(list = c("dt_f_"), file = "dt_f_nonleak.Rdata") #this was originally using the train_setsample but changed to entire dataset

setwd("C:/Sue/PhD/CMP9701-DS/Code/")
load("dt_f_nonleak.Rdata") #to resume in a later session by retrieving saved data frame above

# binning of the fraud dataset
bins_fraud2 = woebin(dt_f_, y = "label", check_cate_num= FALSE)

# Warning message:
#   In rep_blank_na(dt) :
#   The blank values are replaced with NAs in the following columns:
#   card6, P_emaildomain, R_emaildomain, M04

#, var_skip = c("P_emaildomain", "R_emaildomain"))
#https://shichen.name/scorecard/reference/woebin.html#:~:text=Check%20whether%20the%20number%20of%20unique%20values%20in,slow%20if%20there%20are%20too%20many%20unique%20categories

bins_fraud_df2 = data.table::rbindlist(bins_fraud2)

save(list = c("bins_fraud_df2"), file = "bins_fraud_df2_nonleak.Rdata")

#END ONE-TIME
#######################################################################################

#already has variable filtering and bins created
setwd("C:/Sue/PhD/CMP9701-DS/Code/")
load("dt_f_nonleak.Rdata") #was shuffled up above initially
load("bins_fraud_df2_nonleak.Rdata")
# binning of the fraud dataset


# Error in err_metric(cm) : could not find function "err_metric"
# In addition: Warning messages:
#   1: In rep_blank_na(dt) :
#   The blank values are replaced with NAs in the following columns:
#   card6, P_emaildomain, R_emaildomain, M04
# 2: In rep_blank_na(dt) :
#   The blank values are replaced with NAs in the following columns:
#   card6, P_emaildomain, R_emaildomain, M04
# 3: In rep_blank_na(dt) :
#   The blank values are replaced with NAs in the following columns:
#   card6, P_emaildomain, R_emaildomain, M04
# 4: In rep_blank_na(dt) :
#   The blank values are replaced with NAs in the following columns:
#   card6, P_emaildomain, R_emaildomain, M04

#rm(list = c('bins_fraud2', 'bins_fraud_df2'))

#bins_fraud2 = woebin(dt_f_, y = "label", check_cate_num= FALSE)
#bins_fraud_df2 = data.table::rbindlist(bins_fraud2)

#FEB24 loop has to start earlier with initial split
#for (r in 1:55) { 
  
  #start the log file

  #FEB24 temporarily comment out next 3 lines
  #setwd("C:/Sue/PhD/_Ch4/FINAL2/")
  #filename = paste('Log', r, '.txt', col = '', sep = '')
  #sink(file=filename) #'myoutput.txt'
  
  #r = 1
  
  shuffleseed = 3 + (r*10)
  #https://tyagi-sudarshini.medium.com/how-to-shuffle-a-dataframe-in-r-by-rows-e7971cd7949e
  set.seed(shuffleseed) #iteration 1: 42, iteration 2 + 10, etc.
  rows <- sample(nrow(dt_f_))
  dt_f_ <- dt_f_[rows, ]  
  
  #FEB'24 SL - commented out since split was done earlier above
    #and replaced with redefined sets
  
  #out_splitsample <- split_data(data = dt_f_, ratio = c(0.7, 0.1, 0.2), #0.7, 0.1, 0.2 or 0.6, 0.2, 0.2 or 0.7, 0, 0.3
   #                             strat_by_label = FALSE, cross_validation = FALSE)
  
  #train_setsample <- out_splitsample$train_set
  #validation_setsample <- out_splitsample$validation_set
  #test_setsample <- out_splitsample$test_set
  
  #FEB'24 SL apply the same one-hot encoding for missing values to validation and test sets
  # for (i in 3:394){
  #   colnm <- paste(colnames(validate)[i], 'na', sep='')
  #   #train2[[colnm]] <- ''
  #   #train2[[colnm]] <- ifelse(is.na(train2[[i]]), 1, 0)
  #   
  #   #train2[[paste(colnames(train2)[200], 'na', sep='')]]  <- ''
  #   #train2[[paste(colnames(train2)[i], 'na', sep='')]]  <- with(train2, ifelse(is.na(train2[[i]]), 1, 0))
  #   
  #   validate[ , paste(colnames(validate)[i], 'na', sep='')] <- with(validate, ifelse(is.na(validate[[i]]), 1, 0))
  # 
  # }
  
  #FEB'24 SL : same as above for test set
  # for (i in 3:394){
  #   colnm <- paste(colnames(test)[i], 'na', sep='')
  #   #train2[[colnm]] <- ''
  #   #train2[[colnm]] <- ifelse(is.na(train2[[i]]), 1, 0)
  #   
  #   #train2[[paste(colnames(train2)[200], 'na', sep='')]]  <- ''
  #   #train2[[paste(colnames(train2)[i], 'na', sep='')]]  <- with(train2, ifelse(is.na(train2[[i]]), 1, 0))
  #   
  #   test[ , paste(colnames(test)[i], 'na', sep='')] <- with(test, ifelse(is.na(test[[i]]), 1, 0))
  #   
  # }  
  
  #FEB'24 SL: redefined sets (NO missings dummy encoded)
  train_setsample <- dt_f_  #414090, 314 vars  #NIX 525 vars
  validation_setsample <- validate #58670, 394 vars; needs var filter applied
  test_setsample <- test #117780, 314 vars; needs var filter applied
    
  #r = 1
  
  #remove cols in validation and test sets NOT in train set (intersect)
  
  #58670 obs, 316 vars after step below
  validation_setsample <- validation_setsample[, colnames(train_setsample)]
  
  #117780 obs, 316 vars after step below
  test_setsample <- test_setsample[, colnames(train_setsample)]
  
  #save split sets
  setwd("C:/Sue/PhD/_Ch4/FINAL2/")
  filename = paste("splitsets2_round_nonleak", r, ".Rdata" , col='', sep = '')
  save(list = c("train_setsample", "validation_setsample", "test_setsample"), file = filename)
    
  #r = 1
  
  #load updated train, validation, test sets
  #setwd("C:/Sue/PhD/CMP9701-DS/Code/")
  setwd("C:/Sue/PhD/_Ch4/FINAL2/")
  load(paste("splitsets2_round_nonleak", r, ".Rdata" , col='', sep = ''))
  
  #FEB'24 SL: changed input dataset argument from out_splitsample to dt_f_ based on var_filter step above
  #dset <- list = c("train_setsample", "validation_setsample", "test_setsample")
  out_splitsample <- list(train_setsample, validation_setsample, test_setsample)

  #NO ADJ DONE TO BINS; bin split sets
  dt_woe_list_ = lapply(out_splitsample, function(x) woebin_ply(x, bins_fraud_df2)) #315 columns, 472432 rows
  
  #check dummy missing cols
  #new_df = select(train_setsample, "V200", "V200na")
  
  # Warning messages:
  #   1: In rep_blank_na(dt) :
  #   The blank values are replaced with NAs in the following columns:
  #   card6, P_emaildomain, R_emaildomain, M04
  # 2: In rep_blank_na(dt) :
  #   The blank values are replaced with NAs in the following columns:
  #   card6, P_emaildomain, R_emaildomain, M04
  # 3: In rep_blank_na(dt) :
  #   The blank values are replaced with NAs in the following columns:
  #   card6, P_emaildomain, R_emaildomain, M04
  
  # save train set with woe bins applied; indexed by [[1]] = train, [[2]] = validation, [[3]] = test
  setwd("C:/Sue/PhD/_Ch4/FINAL2/")
  filename = paste("dt_woe_listCV2_round_nonleak", r, ".Rdata", col = '', sep = '')
  save(list = c("dt_woe_list_"), file = filename)
  #save(list = c("bins_fraud"), file = "bins_fraud.Rdata") #entire dataset
  #load("dt_woe_listCV2_.Rdata")
  
  # FEB24: modified set name to [[1]] for train, [[2]] for validation, [[3]] for test
            #former nomenclature: $train_set
  
  #load train set with woe bins applied
  setwd("C:/Sue/PhD/_Ch4/FINAL2/")
  filename = paste("dt_woe_listCV2_round_nonleak", r, ".Rdata", col = '', sep = '')
  load(filename)
  
  train_set <- as.data.frame(dt_woe_list_[1])
#m1 = glm(train_set$label ~ ., family = binomial(link = "logit"), data = train_set) 
  
  # Warning messages:
  #   1: In glm.fit(x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  :
  #                         non-finite coefficients at iteration 12
  #                       2: glm.fit: algorithm did not converge 
  #                       3: glm.fit: fitted probabilities numerically 0 or 1 occurred 
  #                       
#vif(m1, merge_coef = TRUE) # summary(m1)
  # Error in vif(m1, merge_coef = TRUE) : 
  #   There are aliased coefficients in the model
  
#fit <-lm(label ~ ., data =  train_set) 
  #dt_woe_list_$train_set
  
#the linearly dependent variables
# ld.vars <- attributes(alias(fit)$Complete)$dimnames[[1]]
  # [1] "C007_woe"    "V044_woe"    "V093_woe"    "V140_woe"    "V144_woe"    "V145_woe"    "V146_woe"    "V147_woe"   
  # [9] "V148_woe"    "V149_woe"    "V150_woe"    "V151_woe"    "V152_woe"    "V153_woe"    "V154_woe"    "V155_woe"   
  # [17] "V156_woe"    "V157_woe"    "V158_woe"    "V159_woe"    "V160_woe"    "V164_woe"    "V165_woe"    "V166_woe"   
  # [25] "V170_woe"    "V174_woe"    "V175_woe"    "V176_woe"    "V177_woe"    "V178_woe"    "V179_woe"    "V180_woe"   
  # [33] "V181_woe"    "V182_woe"    "V183_woe"    "V184_woe"    "V185_woe"    "V186_woe"    "V187_woe"    "V188_woe"   
  # [41] "V189_woe"    "V190_woe"    "V194_woe"    "V195_woe"    "V197_woe"    "V198_woe"    "V199_woe"    "V200_woe"   
  # [49] "V201_woe"    "V202_woe"    "V205_woe"    "V207_woe"    "V208_woe"    "V209_woe"    "V210_woe"    "V211_woe"   
  # [57] "V212_woe"    "V213_woe"    "V214_woe"    "V215_woe"    "V216_woe"    "V224_woe"    "V225_woe"    "V226_woe"   
  # [65] "V227_woe"    "V228_woe"    "V229_woe"    "V230_woe"    "V231_woe"    "V232_woe"    "V233_woe"    "V234_woe"   
  # [73] "V235_woe"    "V236_woe"    "V237_woe"    "V238_woe"    "V239_woe"    "V242_woe"    "V243_woe"    "V244_woe"   
  # [81] "V245_woe"    "V246_woe"    "V250_woe"    "V251_woe"    "V255_woe"    "V256_woe"    "V257_woe"    "V258_woe"   
  # [89] "V259_woe"    "V260_woe"    "V261_woe"    "V262_woe"    "V263_woe"    "V266_woe"    "V267_woe"    "V268_woe"   
  # [97] "V270_woe"    "V271_woe"    "V272_woe"    "V273_woe"    "V274_woe"    "V275_woe"    "V276_woe"    "V277_woe"   
  # [105] "V278_woe"    "V323_woe"    "V324_woe"    "V326_woe"    "V327_woe"    "V328_woe"    "V329_woe"    "V330_woe"   
  # [113] "V331_woe"    "V332_woe"    "V333_woe"    "V335_woe"    "V336_woe"    "V337_woe"    "V338_woe"    "V339_woe"   
  # [121] "addr2na_woe" "dist2na_woe" "D06na_woe"   "D07na_woe"   "D09na_woe"   "D11na_woe"   "D12na_woe"   "D13na_woe"  
  # [129] "D14na_woe"   "M02na_woe"   "M03na_woe"   "M09na_woe"   "V001na_woe"  "V002na_woe"  "V003na_woe"  "V004na_woe" 
  # [137] "V005na_woe"  "V006na_woe"  "C007na_woe"  "V008na_woe"  "V009na_woe"  "V010na_woe"  "V011na_woe"  "V012na_woe" 
  # [145] "V013na_woe"  "V014na_woe"  "V015na_woe"  "V016na_woe"  "V017na_woe"  "V018na_woe"  "V019na_woe"  "V020na_woe" 
  # [153] "V021na_woe"  "V022na_woe"  "V023na_woe"  "V024na_woe"  "V025na_woe"  "V026na_woe"  "V027na_woe"  "V028na_woe" 
  # [161] "V029na_woe"  "V030na_woe"  "V031na_woe"  "V032na_woe"  "V033na_woe"  "V034na_woe"  "V053na_woe"  "V054na_woe" 
  # [169] "V055na_woe"  "V056na_woe"  "V057na_woe"  "V058na_woe"  "V059na_woe"  "V060na_woe"  "V061na_woe"  "V062na_woe" 
  # [177] "V063na_woe"  "V064na_woe"  "V065na_woe"  "V066na_woe"  "V067na_woe"  "V068na_woe"  "V069na_woe"  "V070na_woe" 
  # [185] "V071na_woe"  "V072na_woe"  "V073na_woe"  "V074na_woe"  "V075na_woe"  "V076na_woe"  "V077na_woe"  "V078na_woe" 
  # [193] "V079na_woe"  "V080na_woe"  "V081na_woe"  "V082na_woe"  "V083na_woe"  "V084na_woe"  "V085na_woe"  "V086na_woe" 
  # [201] "V087na_woe"  "V088na_woe"  "V089na_woe"  "V090na_woe"  "V091na_woe"  "V092na_woe"  "V093na_woe"  "V094na_woe" 
  # [209] "V167na_woe"  "V168na_woe"  "V169na_woe"  "V170na_woe"  "V171na_woe"  "V172na_woe"  "V173na_woe"  "V174na_woe" 
  # [217] "V175na_woe"  "V176na_woe"  "V177na_woe"  "V178na_woe"  "V179na_woe"  "V180na_woe"  "V181na_woe"  "V182na_woe" 
  # [225] "V183na_woe"  "V184na_woe"  "V185na_woe"  "V186na_woe"  "V187na_woe"  "V188na_woe"  "V189na_woe"  "V190na_woe" 
  # [233] "V191na_woe"  "V192na_woe"  "V193na_woe"  "V194na_woe"  "V195na_woe"  "V196na_woe"  "V197na_woe"  "V198na_woe" 
  # [241] "V199na_woe"  "V200na_woe"  "V201na_woe"  "V202na_woe"  "V203na_woe"  "V204na_woe"  "V205na_woe"  "V206na_woe" 
  # [249] "V207na_woe"  "V208na_woe"  "V209na_woe"  "V210na_woe"  "V211na_woe"  "V212na_woe"  "V213na_woe"  "V214na_woe" 
  # [257] "V215na_woe"  "V216na_woe"  "V217na_woe"  "V218na_woe"  "V219na_woe"  "V220na_woe"  "V221na_woe"  "V222na_woe" 
  # [265] "V223na_woe"  "V224na_woe"  "V225na_woe"  "V226na_woe"  "V227na_woe"  "V228na_woe"  "V229na_woe"  "V230na_woe" 
  # [273] "V231na_woe"  "V232na_woe"  "V233na_woe"  "V234na_woe"  "V235na_woe"  "V236na_woe"  "V237na_woe"  "V238na_woe" 
  # [281] "V239na_woe"  "V240na_woe"  "V241na_woe"  "V242na_woe"  "V243na_woe"  "V244na_woe"  "V245na_woe"  "V246na_woe" 
  # [289] "V247na_woe"  "V248na_woe"  "V249na_woe"  "V250na_woe"  "V251na_woe"  "V252na_woe"  "V253na_woe"  "V254na_woe" 
  # [297] "V255na_woe"  "V256na_woe"  "V257na_woe"  "V258na_woe"  "V259na_woe"  "V260na_woe"  "V261na_woe"  "V262na_woe" 
  # [305] "V263na_woe"  "V264na_woe"  "V265na_woe"  "V266na_woe"  "V267na_woe"  "V268na_woe"  "V269na_woe"  "V270na_woe" 
  # [313] "V271na_woe"  "V272na_woe"  "V273na_woe"  "V274na_woe"  "V275na_woe"  "V276na_woe"  "V277na_woe"  "V278na_woe" 
  #remove linearly dependent vars
  #dt_woe_list_$train_set2 <- dt_woe_list_$train_set
  
  #copy train set for further filtering
  dt_woe_list_[[4]] <- dt_woe_list_[[1]]
  
  #Feb24: replace $trainset2 with [[4]] and $train_set with [[1]]
      #below from 525 to 205 vars (after removing linearly dep vars)
  
  # dt_woe_list_[[4]] <- subset(dt_woe_list_[[1]], select = -c(
  #   C007_woe,V044_woe,V093_woe,V140_woe,V144_woe,V145_woe,V146_woe,V147_woe,
  #   V148_woe,V149_woe,V150_woe,V151_woe,V152_woe,V153_woe,V154_woe,V155_woe,
  #   V156_woe,V157_woe,V158_woe,V159_woe,V160_woe,V164_woe,V165_woe,V166_woe,
  #   V170_woe,V174_woe,V175_woe,V176_woe,V177_woe,V178_woe,V179_woe,V180_woe,
  #   V181_woe,V182_woe,V183_woe,V184_woe,V185_woe,V186_woe,V187_woe,V188_woe,
  #   V189_woe,V190_woe,V194_woe,V195_woe,V197_woe,V198_woe,V199_woe,V200_woe,
  #   V201_woe,V202_woe,V205_woe,V207_woe,V208_woe,V209_woe,V210_woe,V211_woe,
  #   V212_woe,V213_woe,V214_woe,V215_woe,V216_woe,V224_woe,V225_woe,V226_woe,
  #   V227_woe,V228_woe,V229_woe,V230_woe,V231_woe,V232_woe,V233_woe,V234_woe,
  #   V235_woe,V236_woe,V237_woe,V238_woe,V239_woe,V242_woe,V243_woe,V244_woe,
  #   V245_woe,V246_woe,V250_woe,V251_woe,V255_woe,V256_woe,V257_woe,V258_woe,
  #   V259_woe,V260_woe,V261_woe,V262_woe,V263_woe,V266_woe,V267_woe,V268_woe,
  #   V270_woe,V271_woe,V272_woe,V273_woe,V274_woe,V275_woe,V276_woe,V277_woe,
  #   V278_woe,V323_woe,V324_woe,V326_woe,V327_woe,V328_woe,V329_woe,V330_woe,
  #   V331_woe,V332_woe,V333_woe,V335_woe,V336_woe,V337_woe,V338_woe,V339_woe))
  
    # addr2na_woe,dist2na_woe,D06na_woe,D07na_woe,D09na_woe,D11na_woe,D12na_woe,D13na_woe,
    # D14na_woe,M02na_woe,M03na_woe,M09na_woe,V001na_woe,V002na_woe,V003na_woe,V004na_woe,
    # V005na_woe,V006na_woe,C007na_woe,V008na_woe,V009na_woe,V010na_woe,V011na_woe,V012na_woe,
    # V013na_woe,V014na_woe,V015na_woe,V016na_woe,V017na_woe,V018na_woe,V019na_woe,V020na_woe,
    # V021na_woe,V022na_woe,V023na_woe,V024na_woe,V025na_woe,V026na_woe,V027na_woe,V028na_woe,
    # V029na_woe,V030na_woe,V031na_woe,V032na_woe,V033na_woe,V034na_woe,V053na_woe,V054na_woe,
    # V055na_woe,V056na_woe,V057na_woe,V058na_woe,V059na_woe,V060na_woe,V061na_woe,V062na_woe,
    # V063na_woe,V064na_woe,V065na_woe,V066na_woe,V067na_woe,V068na_woe,V069na_woe,V070na_woe,
    # V071na_woe,V072na_woe,V073na_woe,V074na_woe,V075na_woe,V076na_woe,V077na_woe,V078na_woe,
    # V079na_woe,V080na_woe,V081na_woe,V082na_woe,V083na_woe,V084na_woe,V085na_woe,V086na_woe,
    # V087na_woe,V088na_woe,V089na_woe,V090na_woe,V091na_woe,V092na_woe,V093na_woe,V094na_woe,
    # V167na_woe,V168na_woe,V169na_woe,V170na_woe,V171na_woe,V172na_woe,V173na_woe,V174na_woe,
    # V175na_woe,V176na_woe,V177na_woe,V178na_woe,V179na_woe,V180na_woe,V181na_woe,V182na_woe,
    # V183na_woe,V184na_woe,V185na_woe,V186na_woe,V187na_woe,V188na_woe,V189na_woe,V190na_woe,
    # V191na_woe,V192na_woe,V193na_woe,V194na_woe,V195na_woe,V196na_woe,V197na_woe,V198na_woe,
    # V199na_woe,V200na_woe,V201na_woe,V202na_woe,V203na_woe,V204na_woe,V205na_woe,V206na_woe,
    # V207na_woe,V208na_woe,V209na_woe,V210na_woe,V211na_woe,V212na_woe,V213na_woe,V214na_woe,
    # V215na_woe,V216na_woe,V217na_woe,V218na_woe,V219na_woe,V220na_woe,V221na_woe,V222na_woe,
    # V223na_woe,V224na_woe,V225na_woe,V226na_woe,V227na_woe,V228na_woe,V229na_woe,V230na_woe,
    # V231na_woe,V232na_woe,V233na_woe,V234na_woe,V235na_woe,V236na_woe,V237na_woe,V238na_woe,
    # V239na_woe,V240na_woe,V241na_woe,V242na_woe,V243na_woe,V244na_woe,V245na_woe,V246na_woe,
    # V247na_woe,V248na_woe,V249na_woe,V250na_woe,V251na_woe,V252na_woe,V253na_woe,V254na_woe,
    # V255na_woe,V256na_woe,V257na_woe,V258na_woe,V259na_woe,V260na_woe,V261na_woe,V262na_woe,
    # V263na_woe,V264na_woe,V265na_woe,V266na_woe,V267na_woe,V268na_woe,V269na_woe,V270na_woe,
    # V271na_woe,V272na_woe,V273na_woe,V274na_woe,V275na_woe,V276na_woe,V277na_woe,V278na_woe))
    
    # C007_woe ,V044_woe, V140_woe, V144_woe ,V145_woe, V146_woe, V147_woe ,V148_woe ,V149_woe, V150_woe,
    # V151_woe ,V152_woe ,V153_woe, V154_woe ,V155_woe ,V156_woe, V157_woe ,V158_woe ,V159_woe, V160_woe,
    # V164_woe ,V165_woe ,V166_woe ,V170_woe ,V174_woe, V175_woe ,V176_woe, V177_woe, V178_woe ,V179_woe,
    # V180_woe ,V181_woe, V182_woe ,V183_woe, V184_woe, V185_woe, V186_woe, V187_woe, V188_woe, V189_woe,
    # V190_woe ,V194_woe, V195_woe ,V197_woe ,V198_woe ,V199_woe ,V200_woe ,V201_woe, V202_woe, V205_woe,
    # V207_woe ,V208_woe, V209_woe ,V210_woe ,V211_woe ,V212_woe ,V213_woe, V214_woe, V215_woe, V216_woe,
    # V224_woe ,V225_woe ,V226_woe ,V227_woe ,V228_woe ,V229_woe ,V230_woe, V231_woe, V232_woe ,V233_woe,
    # V234_woe ,V235_woe, V236_woe ,V237_woe ,V238_woe, V239_woe ,V242_woe, V243_woe, V244_woe ,V245_woe,
    # V246_woe, V250_woe, V251_woe ,V255_woe, V256_woe, V257_woe, V258_woe, V259_woe, V260_woe, V261_woe,
    # V262_woe ,V266_woe ,V267_woe ,V268_woe, V270_woe ,V271_woe ,V272_woe, V273_woe, V274_woe, V275_woe,
    # V276_woe ,V277_woe ,V278_woe ,V323_woe, V324_woe, V326_woe ,V327_woe, V328_woe, V329_woe, V330_woe,
    # V331_woe ,V332_woe ,V333_woe ,V335_woe, V336_woe ,V337_woe ,V338_woe, V339_woe)) #118 vars removed
  
  #setwd("C:/Sue/PhD/_Ch4/FINAL2/")
  #save(list = c("dt_woe_list_"), file = filename) # with linear dep. filtering
  
  # no linear dep filtering
  filename = paste("dt_woe_listCV2_round_nonleak", r, ".Rdata", col = '', sep = '')
  save(list = c("dt_woe_list_"), file = filename) 
  
  #Feb24 SL: change dt_woe_list_$train_set2 with train_set2 (from dt_woe_list_[[4]]) 
  #train_set2 <- dt_woe_list_[[4]]
  
#m1 = glm( label ~ ., family = binomial(link = "logit"), data = train_set2) 
#options(max.print = 999999999)
#data.frame(vif(m1, merge_coef = TRUE)) 
  
  #Feb'24 SL: change $train_set3 with [[5]] to represent 3rd iteration of train set
          # :change $train_set2 with [[4]] to represent 2nd iteration of train set
  #this list (VIF > 5) was obtained by the filtered list above
  
  #below step not needed if not testing for multicollinearity or linear dep if non-continuous vars
  
  #30 cols after below step; 414,090 rows
  dt_woe_list_[[5]] <- subset(dt_woe_list_[[4]], select = -c(
    ProductCD_woe, card3_woe,
    card3_woe, addr1_woe,
    addr1_woe, dist1_woe,
    dist1_woe, R_emaildomain_woe,
    R_emaildomain_woe, C02_woe,
    C02_woe, C04_woe,
    C04_woe, C07_woe,
    C07_woe, C08_woe,
    C08_woe, C09_woe,
    C09_woe, C10_woe,
    C10_woe, C12_woe,
    C12_woe, C13_woe,
    C13_woe, C14_woe,
    C14_woe, D01_woe,
    D01_woe, D02_woe,
    D02_woe, D06_woe,
    D06_woe, D08_woe,
    D08_woe, D09_woe,
    D09_woe, D11_woe,
    D11_woe, D12_woe,
    D12_woe, D13_woe,
    D13_woe, D14_woe,
    D14_woe, M02_woe,
    M02_woe, M03_woe,
    M03_woe, M04_woe,
    M04_woe, M06_woe,
    M06_woe, M07_woe,
    M07_woe, M08_woe,
    M08_woe, M09_woe,
    M09_woe, V003_woe,
    V003_woe, V004_woe,
    V004_woe, V005_woe,
    V005_woe, V010_woe,
    V010_woe, V011_woe,
    V011_woe, V012_woe,
    V012_woe, V013_woe,
    V013_woe, V015_woe,
    V015_woe, V016_woe,
    V016_woe, V017_woe,
    V017_woe, V018_woe,
    V018_woe, V019_woe,
    V019_woe, V020_woe,
    V020_woe, V021_woe,
    V021_woe, V022_woe,
    V022_woe, V024_woe,
    V024_woe, V029_woe,
    V029_woe, V030_woe,
    V030_woe, V031_woe,
    V031_woe, V032_woe,
    V032_woe, V033_woe,
    V033_woe, V034_woe,
    V034_woe, V035_woe,
    V035_woe, V036_woe,
    V036_woe, V037_woe,
    V037_woe, V039_woe,
    V039_woe, V040_woe,
    V040_woe, V042_woe,
    V042_woe, V043_woe,
    V043_woe, V048_woe,
    V048_woe, V049_woe,
    V049_woe, V050_woe,
    V050_woe, V051_woe,
    V051_woe, V052_woe,
    V052_woe, V053_woe,
    V053_woe, V054_woe,
    V054_woe, V057_woe,
    V057_woe, V058_woe,
    V058_woe, V059_woe,
    V059_woe, V060_woe,
    V060_woe, V061_woe,
    V061_woe, V062_woe,
    V062_woe, V063_woe,
    V063_woe, V064_woe,
    V064_woe, V067_woe,
    V067_woe, V069_woe,
    V069_woe, V070_woe,
    V070_woe, V071_woe,
    V071_woe, V072_woe,
    V072_woe, V073_woe,
    V073_woe, V074_woe,
    V074_woe, V075_woe,
    V075_woe, V076_woe,
    V076_woe, V077_woe,
    V077_woe, V079_woe,
    V079_woe, V080_woe,
    V080_woe, V081_woe,
    V081_woe, V082_woe,
    V082_woe, V083_woe,
    V083_woe, V084_woe,
    V084_woe, V085_woe,
    V085_woe, V090_woe,
    V090_woe, V091_woe,
    V091_woe, V092_woe,
    V092_woe, V094_woe,
    V094_woe, V095_woe,
    V095_woe, V097_woe,
    V097_woe, V099_woe,
    V099_woe, V100_woe,
    V100_woe, V101_woe,
    V101_woe, V102_woe,
    V102_woe, V103_woe,
    V103_woe, V105_woe,
    V105_woe, V106_woe,
    V106_woe, V126_woe,
    V126_woe, V127_woe,
    V127_woe, V128_woe,
    V128_woe, V129_woe,
    V129_woe, V131_woe,
    V131_woe, V132_woe,
    V132_woe, V133_woe,
    V133_woe, V134_woe,
    V134_woe, V136_woe,
    V136_woe, V137_woe,
    V137_woe, V139_woe,
    V139_woe, V143_woe,
    V143_woe, V167_woe,
    V167_woe, V168_woe,
    V168_woe, V169_woe,
    V169_woe, V171_woe,
    V171_woe, V172_woe,
    V172_woe, V203_woe,
    V203_woe, V204_woe,
    V204_woe, V217_woe,
    V217_woe, V218_woe,
    V218_woe, V219_woe,
    V219_woe, V220_woe,
    V220_woe, V221_woe,
    V221_woe, V222_woe,
    V222_woe, V223_woe,
    V223_woe, V264_woe,
    V264_woe, V265_woe,
    V265_woe, V279_woe,
    V279_woe, V280_woe,
    V280_woe, V282_woe,
    V282_woe, V283_woe,
    V283_woe, V284_woe,
    V284_woe, V285_woe,
    V285_woe, V287_woe,
    V287_woe, V288_woe,
    V288_woe, V289_woe,
    V289_woe, V290_woe,
    V290_woe, V292_woe,
    V292_woe, V293_woe,
    V293_woe, V294_woe,
    V294_woe, V295_woe,
    V295_woe, V296_woe,
    V296_woe, V298_woe,
    V298_woe, V299_woe,
    V299_woe, V302_woe,
    V302_woe, V303_woe,
    V303_woe, V304_woe,
    V304_woe, V306_woe,
    V306_woe, V307_woe,
    V307_woe, V308_woe,
    V308_woe, V309_woe,
    V309_woe, V312_woe,
    V312_woe, V313_woe,
    V313_woe, V315_woe,
    V315_woe, V316_woe,
    V316_woe, V317_woe,
    V317_woe, V318_woe,
    V318_woe, V321_woe,
    V321_woe, V322_woe,
    V322_woe #, addr1na_woe,
    # addr1na_woe, dist1na_woe,
    # dist1na_woe, D02na_woe,
    # D02na_woe, D03na_woe,
    # D03na_woe, D08na_woe,
    # D08na_woe, D10na_woe,
    # D10na_woe, D15na_woe,
    # D15na_woe, M01na_woe,
    # M01na_woe, M06na_woe,
    # M06na_woe, M07na_woe,
    # M07na_woe, M08na_woe
  )) 
  
  #train, validation test sets with filtering applied
  setwd("C:/Sue/PhD/_Ch4/FINAL2/")
  save(list = c("dt_woe_list_"), file = filename)

  ################################################################################
  # FEB24 : skip stepwise regression (NOT USED- section within these #### borders)
##FURTHER FEATURE SELECTION
#https://www.statology.org/lasso-regression-in-r/
# library(glmnet)
# #define response variable
# y <- dt_woe_list_$train_set3$label
# 
# str(dt_woe_list_$train_set3) #get predictor vars
# #remaining vars after C:\Sue\PhD\_ROutput\WOEBinning_Scorecard\VIF_Remove.xlsx
# xmatrix <- (dt_woe_list_$train_set3[, c( 
# "TransactionAmt_woe",
# "card1_woe",
# "card2_woe",
# "card5_woe",
# "card6_woe",
# "dist1_woe",
# "dist2_woe",
# "P_emaildomain_woe",
# "C01_woe",
# "C05_woe",
# "C06_woe",
# "C11_woe",
# "D02_woe",
# "D03_woe",
# "D04_woe",
# "D05_woe",
# "D07_woe",
# "D10_woe",
# "D15_woe",
# "M05_woe",
# "V038_woe",
# "V045_woe",
# "V056_woe",
# "V078_woe",
# "V087_woe",
# "V096_woe",
# "V124_woe",
# "V130_woe",
# "V291_woe",
# "V307_woe",
# "V310_woe",
# "V313_woe",
# "V314_woe",
# "V320_woe"
# 
# ) ]) #34 vars excluding target var
# 
# x <- data.matrix(xmatrix)
# 
# set.seed(123) 
# cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
# 
# #install.packages("glmnet")
# #library(glmnet)
# 
# #perform k-fold cross-validation to find optimal lambda value
# cv_model <- cv.glmnet(x, y, alpha = 1)
# 
# #find optimal lambda value that minimizes test MSE
# best_lambda <- cv_model$lambda.min
# best_lambda
# #[1] 4.484981e-05
# 
# #find coefficients of best model
# best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
# coef(best_model)
# 
# #try various stepwise regressions
# fullmod = glm(label ~ .,data=dt_woe_list_$train_set3, family=binomial (link="logit"))
# summary(fullmod)
# 
# nothing <- glm(label ~ 1,data = dt_woe_list_$train_set3, family=binomial (link = "logit"))
# 
# forwards = step(nothing,
#             scope=list(lower=formula(nothing),upper=formula(fullmod)), direction="forward")
# 
# backwards = step(fullmod) 

#Other stepwise feature selection
# full.model <- glm(label ~., data = dt_woe_list_$train_set3, family = binomial (link = "logit"))
# step.model <- full.model %>% stepAIC(trace = FALSE)
# coef(step.model)
  
# summary(forwards)
# summary(backwards)
# summary(fullmod)
  
# END FEB24 not used SL
############################################################################
  
  #FEB24 SL - comment out
  #compare the entire list of train set vars to sig vars and remove insig vars
  #dt_woe_list_$train_set4 <- subset(dt_woe_list_$train_set3, select = -c(dist2_woe, D07_woe, V056_woe, V130_woe, V320_woe))
  #30 total vars including dependent var after removal
  
  #includes train, validation, test with filtering applied
  setwd("C:/Sue/PhD/_Ch4/FINAL2/")
  save(list = c("dt_woe_list_"), file = "filename") #this was the original filename
  
  filename = paste("dt_woe_listCV2_round_nonleak", r, ".Rdata", col = '', sep = '')
  
  load(filename)
  
## RERUN STARTS HERE ##

#for (r in 55:55) { 
    
  #FEB24 moved log file to earlier since split occurs earlier
  #start the log file
  #setwd("C:/Sue/PhD/_Ch4/FINAL2/")
  #filename = paste('Log', r, '.txt', col = '', sep = '')
  #sink(file=filename) #'myoutput.txt'
  
  #r = 1
  
  setwd("C:/Sue/PhD/_Ch4/FINAL2/")
  
  #FEB24 SL: added nonleak to filename; filtering NOT applied
  filename = paste("dt_woe_listCV2_round_nonleak", r, ".Rdata", col = '', sep = '')
  
  load(filename)
  
  #FEB24: from prior stepwise regression, which has been removed
  #dt_woe_list_$train_set4 <- subset(dt_woe_list_$train_set3, select = -c(dist2_woe, D07_woe, V056_woe, V130_woe, V320_woe))
  
  #variables to be transformed must have only positive values
  #find ranges
  
  #FEB24: INFO only; replaced $train_set4 with [[1]] as this is before any multicollearity/linear dependence removal
#  summary_df <- stat.desc( dt_woe_list_[[1]]) #this one was the best table
  
#  summary_df$row_names <- row.names(summary_df)
  
  #FEB24: "LR" and AS (AutoScore) based on [[5]]; RF based on [[1]]
#  write.xlsx(summary_df, 'C:/Sue/PhD/_Dataset/summarystat_woe_nonleakRF.xlsx')
  
  #boxTidwell(label~ ., data = dt_woe_list_$train_set4)
  
  #FEB24 SL - changed from $trainset4 to [[1]] before any multicollearity/linear dependence removal
  ### START TRAINING RANDOM FOREST MODEL ****
  set.seed(42 + (r*10)) #only needed once
  #downsample prior to fold creation (see citation Neunhoeffer further down)
  
  #FEB24 test linearity log odds
  #https://stackoverflow.com/questions/56350546/how-to-use-the-box-tidwell-function-with-a-logistic-regression-in-r
  #lreg <- glm(label ~ ., data=dt_woe_list_[[5]] , family = binomial(link="logit"))
  
  #Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
  # contrasts can be applied only to factors with 2 or more levels
  
  #lrset <- dt_woe_list_[[5]] 
  #http://sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/#linearity-assumption
  # Select only numeric predictors
  #mydata <- lrset %>%
   # dplyr::select_if(is.numeric) 
  #predictors <- colnames(mydata)
  # Bind the logit and tidying the data for plot
  
  # Fit the logistic regression model
  #model <- glm(label ~., data = lrset, 
   #            family = binomial)
  # Predict the probability (p) of diabete positivity
  #probabilities <- predict(model, type = "response")
  
  # mydata <- mydata %>%
  #   mutate(logit = log(probabilities/(1-probabilities))) %>%
  #   gather(key = "predictors", value = "predictor.value", -logit)
  
  # ggplot(mydata, aes(logit, predictor.value))+
  #   geom_point(size = 0.5, alpha = 0.5) +
  #   geom_smooth(method = "loess") + 
  #   theme_bw() + 
  #   facet_wrap(~predictors, scales = "free_y")
  
  #https://www.r-bloggers.com/2019/05/why-use-weight-of-evidence/
  #https://www.linkedin.com/pulse/understanding-weight-evidence-information-value-elshaddai-harris/
  #Weight-of-evidence 2.0 with shrinkage and spline-binning
    #THIS ONE ABOVE EXPLAINS WOE
  
  #https://stats.stackexchange.com/questions/189568/replacing-variables-by-woe-weight-of-evidence-in-logistic-regression
  #https://stats.stackexchange.com/questions/166816/why-should-one-do-a-woe-transformation-of-categorical-predictors-in-logistic-reg
  
  #Semi-Naive Bayesian Classifier "weight of evidence" "logistic regression"
  #De Smedt, F., Kayastha, P., & Dhital, M. R. (2023). Naïve and Semi-Naïve Bayesian Classification of Landslide Susceptibility Applied to the Kulekhani River Basin in Nepal as a Test Case. Geosciences, 13(10), 306.
  #Barddal, J. P., Loezer, L., Enembreck, F., & Lanzuolo, R. (2020). Lessons learned from data stream classification applied to credit scoring. Expert Systems with Applications, 162, 113899.
  
  #weight of evidence log odds
  #Dahal, R. K., Hasegawa, S., Nonomura, A., Yamanaka, M., Dhakal, S., & Paudyal, P. (2008). Predictive modelling of rainfall-induced landslide hazard in the Lesser Himalaya of Nepal based on weights-of-evidence. Geomorphology, 102(3-4), 496-510.
  
  # cols = colnames(lrset)
  # for (col in cols){
  #   if(is.factor(lrset[[col]])){
  #     cat(col, ' has ', length(levels(lrset[[col]])), '\n')
  #   }
  # }
  # 
  # logodds <- lreg$linear.predictors
  #boxTidwell(logodds ~ . -label, data=lrset)
    #variables to be transformed must have only positive values
  
  set.seed(42 + r)
  #FEB24: new dataset (traindown) - 29,058 observations, 314 vars     #NIX 526 vars
  traindown <- downSample(dt_woe_list_[[1]] , as.factor(dt_woe_list_[[1]]$label), 
                          list = FALSE, yname = "label")
  
  #drop extra target (factor) column generated from downsampling
  traindown2 <- subset(traindown, select = -c(31) )
  
  #12-17-2023
  traindown2$label <- as.factor(traindown2$label)
  
  str(traindown2, list.len=ncol(traindown2) )
  traindown2_ <- subset(traindown2, select = -c(label.1))
  
  #save off train set
  setwd("C:/Sue/PhD/_Ch4/FINAL2/")
  filename = paste("traindown2_", r, ".Rdata" , col='', sep = '')
  save(list = c("traindown2_"), file = filename)
  
  #27 combinations
  # Define the values which are going to be evaluated in the tuning process
  tuning_values <- list(ntrees=c(100, 200, 300),
                        mtry=c(80, 100, 120),
                        nodesize=c(3, 6, 9))
  
  # Get all possible combinations of the defined tuning values -(3 * 3 * 3)
  all_combinations <- cross(tuning_values) #tidyr::expand_grid(tuning_values)
  n_combinations <- length(all_combinations)

  n_inner_folds = 5
  best_params <- list(pccc=-Inf)
  inner_folds <-  createFolds(traindown2$label, k = 5, list = TRUE, returnTrain = FALSE)
  # for (j in 1:n_combinations) {
  #   cat("\tCombination:", j, "/", n_combinations, "\n")
  #   flags <- all_combinations[[j]]
  #   cat("\t\tInner folds: ")
  # 
  #   #hyperparameter tuning
  #   for (m in 1:n_inner_folds) {
  #     cat(m, ", ")
  #     inner_fold <- inner_folds[[m]]
  #     #DataInnerTraining <- DataTraining[inner_fold$training, ]
  #     DataInnerTraining <- dt_woe_list_$validation_set[-inner_fold,]
  #     #DataInnerTesting <- DataTraining[inner_fold$testing, ]
  #     DataInnerTesting <- dt_woe_list_$validation_set[inner_fold,]
  #     # Fit the model using the current combination of hyperparameters
  #     tuning_model <- rfsrc(label ~ ., data=DataInnerTraining, ntree=flags$ntree,
  #                           mtry=flags$mtry, nodesize=flags$nodesize)
  #     predictions <- predict(tuning_model, newdata=DataInnerTesting, type = 'class')#, type = "class") #, type = "class") #, type = "response")
  #     # Compute PCCC for the current combination of hyperparameters
  #     #current_pccc <- pccc(DataInnerTesting$isFraud, predictions)
  # 
  #     #roc(DataFrame[complete.cases(DataFrame),"CD3LR"],step_1$fitted.values,plot=FALSE)
  #     #https://stackoverflow.com/questions/55760669/roc-function-error-predictor-must-be-numeric-or-ordered
  # 
  #     #predict_qdatrain<-predict(fly_qda, newdata=ctrain)
  #     #roc_qda=roc(response=ctrain$diabetes, predictor= factor(predict_qdatrain$class,
  #     #                                                       ordered = TRUE), plot=TRUE)
  # 
  #     preddf <- as.data.frame(predictions$predicted)
  # 
  #     #DataInnerTesting$isFraud <- factor(DataInnerTesting$isFraud, levels=c(0, 1))
  # 
  #     rocRFall=roc(DataInnerTesting$label[complete.cases(DataInnerTesting$label)],
  #                  preddf[complete.cases(preddf), ])
  # 
  #     aucRF=pROC::auc(rocRFall)
  # 
  #     str(predictions$predicted) #5286
  #     str(DataInnerTesting$isFraud) #6608
  #     # If the current combination gives a greater PCCC, set it as new best_params
  #     if (aucRF > best_params$pccc) {   #current_pccc
  #       best_params <- flags
  #       best_params$pccc <- aucRF #current_pccc
  #     }
  #   }  #end folds
  # } #end hyperparameter combinations
  
  #https://stackoverflow.com/questions/13956435/setting-values-for-ntree-and-mtry-for-random-forest-regression-model
  #Also, somehow I forgot that the ranfomForest package itself has a tuneRF function that is specifically for searching for the "optimal" value for mtry.
  
#  bestparams[[r]] <- best_params
  # Using the best params combination, retrain the model but using the complete
  # training set
  
  model <- rfsrc(label ~ ., data=traindown2_, ntree=200, #best_params$ntree
                 mtry=100, nodesize=9)
    #best_params$mtry    best_params$nodesize
  
  #12-17-2023
  cm <- table(model$yvar, model$class.oob)
  
  #install.packages("caTools")
  #library(caTools)
  #err_metric(cm)
  
  # Create Data
  #actual <- factor(testset$label)
  #predicted <- factor(model$yvar)
  
  # create confusion matrix 
  #confusionMatrix(predicted, actual)
   #               mode = "everything"
    #              ,positive="1")
  
  # TP = 1,1
  # FP = 1,2
  # FN = 2,1
  # TN = 2,2
  
  tn <- cm[1,1]
  fn <- cm[1,2]
  fp <- cm[2,1]
  tp <- cm[2,2]
  
  #PPV
  
  #sensitivity
  
  #recall
  
  #F1 score
  
  
  #sensitivity = TP / (TP+FN) = recall
  sens <- tp / (tp + fn)
  
  #specificity = TN / (FP+TN) 
  spec <- tn / (fp + tn)
  
  #NPV = negative predictive value = TN / (TN + FN)
  npv <- tn / (tn + fn)
  
  #PPV = positive predictive value = precision
  ppv <- tp / (tp + fp )
  
  #FNR = 1−NPV
  fnr <- (1 - npv)
  
  cmarr[[r]] <- cm
  fparr[[r]] <- fp
  fnarr[[r]] <- fn
  tparr[[r]] <- tp
  tnarr[[r]] <- tn
  # 
  sensarr[[r]] <- sens
  specarr[[r]] <- spec
  
  fnrarr[[r]] <- fnr
  
  npvarr[[r]] <- npv
  ppvarr[[r]] <- ppv
  
  #remove cols in validation and test sets NOT in train set
  testset <- dt_woe_list_[[3]]
  #testset[, colnames(traindown2)]
  
  str(testset, list.len=ncol(testset))
  str(traindown2_, list.len=ncol(traindown2_) )
  
  #FEB24 SL: changed from $test_set to testset from above
  #new_df <- subset(testset, select = -c(D05_woe))
  
  #https://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
  #testset <- rbind(traindown2[1, ] , testset)
  #testset <- testset[-1,]
  
  #new_df <- testset
  #new_df <- new_df[, intersect(colnames(traindown2), colnames(dt_woe_list_[[3]]))]
  
  predictionstest <- predict(model, newdata=testset) #, type = 'prob') #'class'
  
  #plot(vimp(model, importance = "permute")$importance)
  
  f1 <- (2 * tp) / (2 * tp + fp + fn)
  f1arr[[r]] <- f1
  #calculate AUC on the test set using the above model
  preddftest <- as.data.frame(predictionstest$predicted)
  
  #F1        = performance.F1()
  
  #FEb24: replaced dt_woe_list_$test_set with testset
  #12-17-2023
  dt_woe_list_$testset$label <- as.factor(testset$label)
  
  #pROC::roc(dt_woe_list_$test_set$label, predictionstest[,2])
  
  #FEb24: replaced dt_woe_list_$test_set with testset
  rocRFall=roc(testset$label[complete.cases(testset$label)],
               preddftest[complete.cases(preddftest),2 ])
  
  #rocRFall=roc(dt_woe_list_$test_set$label[complete.cases(dt_woe_list_$test_set$label)],
   #            preddftest[complete.cases(preddftest), ])
  
  #roc_qda <- roc(response = dt_woe_list_$test_set$label[complete.cases(dt_woe_list_$test_set$label)],
   #              predictor = predictionstest$posterior[,"1"])
  
  #str(dt_woe_list_$test_set$label)
  #evalResult.rf[,     2]
  
  aucRF=pROC::auc(rocRFall) #0.8904 (test iteration 1)
  #save to array
  aucRFarr[[r]] <- aucRF

#######################################################################
###save F1
setwd("C:/Sue/PhD/_Ch4/FINAL2/RF/")
  
objnm <- paste('f1arr', r, '.Rdata', col = '', sep = '')
save(f1arr, file=objnm) #saves object as "f1arr.Rdata"

file_name <- paste('f1arr', r, '.txt', col='', sep='') #write contents of object to file

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(f1arr)

#close external connection to file 
sink()

#######################################################################
###save AUC
objnm <- paste('aucRFarr', r, '.Rdata', col = '', sep = '')
save(aucRFarr, file=objnm) #saves object as "aucRFarr.Rdata"

file_name <- paste('aucRFarr', r, '.txt', col='', sep='') #write contents of object to file

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(aucRFarr)

#close external connection to file 
sink()

#########################################################################

#https://rdocumentation.org/packages/randomForestSRC/versions/3.2.2/topics/vimp.rfsrc
#https://www.rdocumentation.org/packages/randomForestSRC/versions/3.2.2/topics/rfsrc
    #anti = default
    #default importance="anti" (equivalent to importance=TRUE) assigns cases to the anti (opposite) split.

plot(vimp(model)) # grab results from console log
impvar <- as.data.frame(vimp(model)$importance)

#add ID column
#https://statisticsglobe.com/convert-row-names-into-column-of-data-frame-in-r
impvar$row_names <- row.names(impvar)

#rename var imp column
#https://statisticsglobe.com/convert-row-names-into-column-of-data-frame-in-r
colnames(impvar)[1] <- 'varimp'

#sort by varimp
#https://stackoverflow.com/questions/51501989/how-to-sort-data-by-column-in-descending-order-in-r
impvar <- impvar[order(impvar$varimp, decreasing = TRUE),]  

#https://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r
options(scipen=999)

#add difference column
#https://stackoverflow.com/questions/15444133/how-to-create-a-column-that-is-the-difference-between-2-columns
impvar <- transform(impvar, new.col = varimp - lead(varimp)) #chg to next-lower var
impvar <- transform(impvar, new.col2 = new.col / lead(varimp)) #% chg

file_name <- paste('C:/Sue/PhD/_Ch4/FINAL2/RF/impvar_round', r, '.xlsx', col='', sep='')

library(openxlsx)

write.xlsx(impvar, file_name)

#############################################################
#save bestparams

#objnm <- paste('bestparams', r, '.Rdata', col = '', sep = '')
#save(bestparams, file=objnm) #saves object as "aucRFarr.Rdata"

file_name <- paste('bestparams', r, '.txt', col='', sep='') #write contents of object to file

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(best_params)

#close external connection to file 
sink()

#############################################################

###save confusion matrix metrics
# cmarr <- c()
# fparr <- c()
# fnarr <- c()
# tparr <- c()
# tnarr <- c()
# 
# sensarr <- c()
# specarr <- c()

#objnm <- paste('cmarr', r, '.Rdata', col = '', sep = '')
#save(cmarr, file=objnm) #saves object as "aucRFarr.Rdata"

file_name <- paste('cmarr', r, '.txt', col='', sep='') #write contents of object to file

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(cmarr)

#close external connection to file 
sink()

##########################################################
#sensitivity
#objnm <- paste('sensarr', r, '.Rdata', col = '', sep = '')
#save(sensarr, file=objnm) #saves object as "aucRFarr.Rdata"

file_name <- paste('sensarr', r, '.txt', col='', sep='') #write contents of object to file

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(sensarr)

#close external connection to file 
sink()

############################################################
#specificity
#objnm <- paste('specarr', r, '.Rdata', col = '', sep = '')
#save(cmarr, file=objnm) #saves object as "aucRFarr.Rdata"

file_name <- paste('specarr', r, '.txt', col='', sep='') #write contents of object to file

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(specarr)

#close external connection to file 
sink()

##############################################################

#cmarr[[r]] <- cm
# fparr[[r]] <- fp
# fnarr[[r]] <- fn
# tparr[[r]] <- tp
# tnarr[[r]] <- tn
# 
#sensarr[[r]] <- sens
#specarr[[r]] <- spec

fnrarr[[r]] <- fnr
#################################################################

#false negative rate (fnr)
#objnm <- paste('fnrarr', r, '.Rdata', col = '', sep = '')
#save(fnrarr, file=objnm) #saves object as "aucRFarr.Rdata"

file_name <- paste('fnrarr', r, '.txt', col='', sep='') #write contents of object to file

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(fnrarr)

#close external connection to file 
sink()

####################################################################

#npv
#objnm <- paste('npvarr', r, '.Rdata', col = '', sep = '')
#save(npvarr, file=objnm) #saves object as "aucRFarr.Rdata"

file_name <- paste('npvarr', r, '.txt', col='', sep='') #write contents of object to file

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(npvarr)

#close external connection to file 
sink()

##################################################################

#ppv
#objnm <- paste('ppvarr', r, '.Rdata', col = '', sep = '')
#save(ppvarr, file=objnm) #saves object as "aucRFarr.Rdata"

file_name <- paste('ppvarr', r, '.txt', col='', sep='') #write contents of object to file

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(ppvarr)

#close external connection to file 
sink()

##################################################################

#save plots
wdir <- paste('C:/Sue/PhD/_Ch4/FINAL2/RF/Round', r, '/', col = '', sep = '')
setwd(wdir)

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
#    
file.copy(from=plots.png.paths, to=wdir) #"C:/Sue/PhD/CMP9701-DS/Code/"

#clear all plots
dev.off(dev.list()["RStudioGD"]) 

##############################################################
### START TRAINING AUTOSCORE

#assumes "traindown2" dataset created above

#FEB24
#r = 1

#get slimmed down dataset
setwd("C:/Sue/PhD/_Ch4/FINAL2/")

#FEB24 SL: added nonleak to filename
filename = paste("dt_woe_listCV2_round_nonleak", r, ".Rdata", col = '', sep = '')

load(filename)

setwd("C:/Sue/PhD/_Ch4/FINAL2/")
filename = paste("traindown2_", r, ".Rdata" , col='', sep = '')

#FEB24: new dataset (traindown) - 29,058 observations, 314 vars     #NIX 526 vars
#traindown <- downSample(dt_woe_list_[[5]] , as.factor(dt_woe_list_[[5]]$label), 
 #                       list = FALSE, yname = "label")

#drop extra target (factor) column generated from downsampling
#traindown2 <- subset(traindown, select = -c(31) )

#12-17-2023
#traindown2$label <- as.factor(traindown2$label)
#traindown2_ <- subset(traindown2, select = -c(label.1))

#setwd("C:/Sue/PhD/_Ch4/FINAL2/")
#filename = paste("traindown2_", r, ".Rdata" , col='', sep = '')
#load(filename)

#FEB24 replaced traindown2_ with traindown2
ranking <- AutoScore_rank(train_set = traindown2_, method = "rf", ntree = 100)

# [[5]] new train = 
    # [[6]] new validation =
#CV validation fold = separate validation set

#Feb24: replaced dt_woe_list_$validation_set with validation_set

validation_set <- dt_woe_list_[[2]]

cols_to_keep <- intersect(colnames(validation_set),colnames(traindown2_))

new_df = subset(validation_set, select = c(cols_to_keep))

#test set with reduced columns
test_set <- dt_woe_list_[[3]]

cols_to_keeptest <- intersect(colnames(test_set),colnames(traindown2_))

new_testdf = subset(validation_set, select = c(cols_to_keeptest))

AUC <- AutoScore_parsimony(
  train_set = traindown2, validation_set = new_df,
  rank = ranking, max_score = 100, n_min = 1, n_max = 29,
  categorize = "quantile", quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),
  auc_lim_min = 0.5, auc_lim_max = "adaptive"
)

warnings()

# 20: In compute_score_table(train_set_2, max_score, variable_list) :
#   WARNING: GLM output contains NULL, Replace NULL with 1

AUC2 <- AUC

AUC2 <- as.data.frame(AUC2)

AUC2 <- tibble::rownames_to_column(AUC2, "VALUE")

#https://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r
options(scipen=999)

#https://stackoverflow.com/questions/15444133/how-to-create-a-column-that-is-the-difference-between-2-columns
AUC2 <- transform(AUC2, new.col = AUC2 - lag(AUC2)) #sorted ascending 

#https://www.datasciencemadesimple.com/generate-row-number-to-the-dataframe-in-r-2/#google_vignette
#add an autoID
AUC2$row_num <- seq.int(nrow(AUC2)) 

order(AUC2$new.col)
# 1  2  3  4  5  6  7  8  9 10

fltr <- tail(AUC2[order(AUC2$new.col),],15) #order top 15 vars by AUC

fltr2 <- fltr[fltr$new.col >= 0.005,] #sig vars --> change in AUC >= 0.005

numvar <- max(as.numeric(fltr2$row_num[!is.na(fltr2$row_num)])) #number of sig vars as defined above

#save AUC data
AutoScorenumvar[[r]] <- numvar

setwd('C:/Sue/PhD/_Ch4/FINAL2/AutoScore/')
file_name <- paste('AUCAutoScore_round_', r,  '.csv', col='', sep='')
write.csv(data.frame(AUC), file = file_name)

num_var <- numvar
final_variables <- names(ranking[1:num_var])

#Feb'24: changed from dt_woe_list_$validation_set to newdf
cut_vec <- AutoScore_weighting( 
  train_set = traindown2, validation_set = new_df,
  final_variables = final_variables, max_score = 100,
  categorize = "quantile", quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1))

scoring_table <- AutoScore_fine_tuning(
  train_set = traindown2, validation_set = new_df , 
  final_variables = final_variables, cut_vec = cut_vec, max_score = 100
)
#filter only for selected vars
#take the list of vars


#filter
AUCkeep[[r]] <- subset(AUC2$VALUE, AUC2$row_num <= numvar)
AUCarr[[r]] <- AUC2

#autoscoretest <- dt_woe_list_$test_set
#names(autoscoretest)[names(autoscoretest)=="isFraud"] <- "label"

#test_set <- subset (dt_woe_list$test_set, select = c("label", varskeep))

#Feb24: changed test_set from autoscoretest to new_testdf
pred_score <- AutoScore_testing(
  test_set = new_testdf, final_variables = final_variables, cut_vec = cut_vec,
  scoring_table = scoring_table, threshold = "best", with_label = TRUE
)

# ***Performance using AutoScore:
#   AUC:  0.8278   95% CI: 0.8178-0.8378 (DeLong)
# Best score threshold: >= 44 
# Other performance indicators based on this score threshold: 
#   Sensitivity: 0.7081 95% CI: 0.688-0.7283
# Specificity: 0.8217 95% CI: 0.8184-0.825
# PPV:         0.1275 95% CI: 0.124-0.1313
# NPV:         0.9871 95% CI: 0.9862-0.988

#F1 score
#2 * (sensitivity * precision a.k.a PPV / (sensitivity + precision a.k.a. PPV))
  #e.g., 2 * (( 0.7388 * 0.0942) / (0.7388 + 0.0942))) = 0.167

#low PPV ("low positive predictive value")
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5799952/
#https://www.researchgate.net/profile/Ari-Joffe-2/publication/358718524_Critical_Care_Randomized_Trials_Demonstrate_Power_Failure_A_Low_Positive_Predictive_Value_of_Findings_in_the_Critical_Care_Research_Field/links/647f992779a722376514e582/Critical-Care-Randomized-Trials-Demonstrate-Power-Failure-A-Low-Positive-Predictive-Value-of-Findings-in-the-Critical-Care-Research-Field.pdf
#confusionMatrix(pred, actual, mode = "everything", positive="1")

# ***Performance using AutoScore:
#   AUC:  0.823   95% CI: 0.8135-0.8325 (DeLong)
# Best score threshold: >= 61 
# Other performance indicators based on this score threshold: 
#   Sensitivity: 0.7388 95% CI: 0.7186-0.757
# Specificity: 0.7453 95% CI: 0.7417-0.749
# PPV:         0.0942 95% CI: 0.0917-0.0966
# NPV:         0.9876 95% CI: 0.9867-0.9885

#result <- print_roc_performance(pred_score$Label, pred_score$pred_score)
file_name <- paste('AutoScorenumvar', r, '.txt', col='', sep='')

#capture.output(summary(summarytest), file = file_name)

sink(file_name)

#print my_list to file
print(AutoScorenumvar)

#close external connection to file 
sink()

######################################################
file_name <- paste('AutoScoreAUCkeep', r, '.txt', col='', sep='')

#capture.output(summary(summarytest), file = file_name)

sink(file_name)

#print my_list to file
print(AUCkeep)

#close external connection to file 
sink()

####################################################
file_name <- paste('AutoScoreAUCall', r, '.txt', col='', sep='')

#capture.output(summary(summarytest), file = file_name)

sink(file_name)

#print my_list to file
print(AUCarr)

#close external connection to file 
sink()

#########################################################################
wdir = paste('C:/Sue/PhD/_Ch4/FINAL2/AutoScore/Round', r, '/', col = '', sep = '')

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
#    
file.copy(from=plots.png.paths, to=wdir) #C:/Sue/PhD/CMP9701-DS/Code/"

#clear all plots
dev.off(dev.list()["RStudioGD"]) 

#####################################################################
### START TRAINING LOGISTIC REGRESSION
# Initialize a vector to store the results 

# r = 1

#load the file if not done so already
#get slimmed down dataset
setwd("C:/Sue/PhD/_Ch4/FINAL2/")

#FEB24 SL: added nonleak to filename
filename = paste("dt_woe_listCV2_round_nonleak", r, ".Rdata", col = '', sep = '')

load(filename)

results <- c() 
#resultsrf <- c()
summary <- c()
#summaryrf <- c()
summarytest <- c()
#summarytestrf <- c()
numvararr <- c()
resultstest <- c()

#traindata <- c()
#validationdata <- c()
modelarr <- c()
#modelRFarr <- c()
sigvar <- c()
#sigvarRF <- c()
cvmean <- c()
#cvmeanrf <- c()
acc <- c()
#accrf <- c()
#rfimp <- c()
#rfimpranger <- c()
arr <- 0

senslrarr <- c()
speclrarr <- c()
ppvlrarr <- c()
npvlrarr <- c()

#false neg rate
fnrlrarr <- c()

cmlrarr <- c()

#Feb24
#F1 array logistic regression
f1rarrlr <- c()

model <- glm(label ~ ., traindown2_, family = binomial())

#dt_woe_list_$test_set
preds <- predict(model, new_testdf, type = "response") #dt_woe_list$test_set

options(scipen=999)

preds[preds>0.5] <- 1
preds[preds <=0.5] <- 0

#optimal <- optimalCutoff(dt_woe_list_$test_set$label, preds)[1]

#dt_woe_list_$test_set
#12-17-2023 confusion matrix
#confusionMatrix(data = as.numeric(preds>0.5), reference = dt_woe_list_$test_set$label)
cmlr <- confusionMatrix(new_testdf$label, preds)

#dt_woe_list_$test_set
cfdflr <- as.data.frame(confusionMatrix(new_testdf$label, preds))

tn <- cmlr[1,1]
fn <- cmlr[1,2]
fp <- cmlr[2,1]
tp <- cmlr[2,2]

ppv <- tp / (tp + fp)

npv <- tn / (tn + fn)

#FEb24 F1 score
f1 <- (2 * tp) / (2 * tp + fp + fn)

#dt_woe_list_$test_set
cmlrarr[[r]] <- cmlr
senslrarr[[r]] <- sensitivity(new_testdf$label, preds)
speclrarr[[r]] <- specificity(new_testdf$label, preds)
ppvlrarr[[r]] <- ppv
npvlrarr[[r]] <- npv
f1rarrlr[[r]] <- f1

fnrlrarr[[r]] <- (1 - senslrarr[[r]])
  
#misClassError(dt_woe_list_$test_set$label, preds, threshold=optimal)

#end 12-17-2023 confusion matrix

summary(model)
#other performance metrics - GLM significant variables
toselect.x <- summary(model)$coeff[-1,4] < 0.05 #Bonferroni correction (0.025) = not needed

# select sig. variables
relevant.x <- names(toselect.x)[toselect.x == TRUE] 
sigvar[[r]] <- relevant.x

#count vars
numvar <- vec_count(relevant.x)
numvararr[[r]] <- numvar

#validate
#results[[r]] <- preds #predict(model, test, type = "response") 
#resultsrf[[i]] <- predsrf #predict(rf, test, type = "prob")

#VALIDATION RESULTS
# prediction[[i]] <- twoClassSummary(testdata)
#https://www.statology.org/auc-in-r/
#summary[[r]] <- auc(traindown2$label, results[[r]])

#dt_woe_list_$test_set
#test on TEST set
resultstest[[r]] <- predict(model, new_testdf, type = "response") 

summarytest[[r]] <- auc(new_testdf$label, resultstest[[r]])


##########################################
#save(numvararr, file="numvararr.Rdata")

setwd('C:/Sue/PhD/_Ch4/FINAL2/LR/')

file_name <- paste('numvararr', r, '.txt', col='', sep='')

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(numvararr)

#close external connection to file 
sink()
###########################################

# saveRDS(summary, file="summaryvalidation.rds")
# 
# file_name <- paste('summaryvalidation', r, '.txt', col='', sep='')
# 
# #capture.output(summary(summary), file = file_name)
# 
# sink(file_name)
# 
# #print my_list to file
# print(summary)
# 
# #close external connection to file 
# sink()   

#################################################
#saveRDS(summarytest, file="summarytest.rds")

file_name <- paste('summarytest', r,  '.txt', col='', sep='')

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(summarytest)

#close external connection to file 
sink()   

################################################
#saveRDS(sigvar, file="sigvar.rds")

file_name <- paste('sigvar', r, '.txt', col='', sep='')

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(sigvar)

#close external connection to file 
sink() 

#####################################################

# cmlrarr[[r]] <- cmlr
# senslrarr[[r]] <- sensitivity(dt_woe_list_$test_set$label, preds)
# speclrarr[[r]] <- specificity(dt_woe_list_$test_set$label, preds)
# ppvlrarr[[r]] <- ppv
# npvlrarr[[r] <- npv
#          
# fnrlrarr[[r]] <- (1 - senslrarr[[r]])
         
file_name <- paste('cmlrarr', r,  '.txt', col='', sep='')

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(cmlrarr)

#close external connection to file 
sink()   

################################################

file_name <- paste('senslrarr', r,  '.txt', col='', sep='')

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(senslrarr)

#close external connection to file 
sink()   

################################################

file_name <- paste('speclrarr', r,  '.txt', col='', sep='')

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(speclrarr)

#close external connection to file 
sink()   

################################################

file_name <- paste('ppvlrarr', r,  '.txt', col='', sep='')

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(ppvlrarr)

#close external connection to file 
sink()   

################################################

file_name <- paste('npvlrarr', r,  '.txt', col='', sep='')

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(npvlrarr)

#close external connection to file 
sink()   

################################################

file_name <- paste('fnrlrarr', r,  '.txt', col='', sep='')

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(fnrlrarr)

#close external connection to file 
sink()   

################################################

file_name <- paste('f1rarrlr', r,  '.txt', col='', sep='')

#capture.output(summary(summary), file = file_name)

sink(file_name)

#print my_list to file
print(f1rarrlr )

#close external connection to file 
sink()   

################################################

} #end outer-most loop repeating random holdout cross-validation

