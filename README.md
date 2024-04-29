This code applies the AutoScore model from https://github.com/nliulab/AutoScore to a dataset for fraud detection, https://www.kaggle.com/competitions/ieee-fraud-detection/.

Other notes regarding parsimony.md:
1. "Train082123.Rdata" reflects modified column names to ensure no duplicate partial field names detected by AutoScore. The modified column names affected any column ending in and containing a single digit and starting with C, D, M, or V. E.g., column C1 was renamed to C01, D1 to D01, etc.

2. Once the columns were renamed per the rule above, the following code was run to import the above Kaggle dataset (downloaded as a .csv file):

  #ONE-TIME
  #import raw file
  train_transaction <- read.csv("C:/Sue/PhD/CMP9701-DS/Code/train_transaction.csv/train_transaction.csv", 
                              stringsAsFactors=TRUE)

  #save imported data to file for reloading later
  setwd("C:/Sue/PhD/CMP9701-DS/Code/AutoScore-Imbalance-main (1)/AutoScore-Imbalance-main/")
  save(list = c("train_transaction"), file = "Train082123.Rdata")

3. Replace library() with require() calls as needed to ensure requisite packages are installed
Replace C:\ directories with an appropriate directory

4. Steps are included in the code to save intermediate data frames to facilitate rerunning the code from various points. In some cases, the step of saving such a data frame may be commented out from the code, as that needed executed only once, specifically the lines below for data frame "dt_woe_listCV2_round_nonleak_Rmarkdown." Comment the below lines back in when running the code for the first time:

   # setwd("C:/Sue/PhD/_Ch4/FINAL2/")
   
   #filename = paste("dt_woe_listCV2_round_nonleak", r, ".Rdata", col = '', sep = '')
   
   #save(list = c("dt_woe_list_"), file = filename)

5. Various parameters are specified based on the model's optimization; these AutoScore parameters may change if other parts of the code/process are changed:

  numvar <- 37 #obtained by visually inspecting parsimony plot for where curve flattens out
  
  #n_max set to max number of predictor variables
  
  n_max = 153
  
  #per log: best score threshold >= 46
  test_set_3$pred <-  ifelse(test_set_3$total_score >= 46, 1, 0)

6. LASSO insignificant variables excluded from logistic regression identified by the comment below in the code. This list of variables may need modified if other changes are done in the code.
   #limit to variables from LASSO
