#================================================================= ==================================================
# Set working directory
Setwd('~/model sub-screening 3/show 1')
# required package: sltool (self-built), data.table, reshape2, ggplot2
Library(sltool)
Library(data.table)
Library(ggplot2)
# Import function
Source('model sub-screening 3_function 1.R')
#================================================================= ==================================================
# Import model_table exported from model partition filter 3_base (preprocessed)
Load('~/model sub-screening 3/model_table1.RData')
#================================================================= ==================================================
#测试
# ------------------------
# Run the function get_new_table1
New_table <- get_new_table1(model_table)
#Run function get_result
Result <- get_result(new_table)
# if return List of 7 elements then tested successfully
Result
# ------------------------
# Run the function get_new_table2
New_table <- get_new_table2(model_table)
#Run function get_result
Result <- get_result(new_table)
# if return List of 7 elements then tested successfully
Result
#================================================================= ==================================================
# The candidate model is as follows:
# "bt3_m","cashl_m","posjxl20_m", "cup_m" ,"zhima_m"
# need     need      no need      no need   for reference
#================================================================= ==================================================
# main
# new_table <- get_new_table1(model_table,models=c('cup_m','posjxl20_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
# pbocmodel1_th=c(75,42,12),nopbocmodel1_th=c(80,50,12))
# ----------
New_table <- get_new_table2(model_table,models=c('cup_m','cashl_m','posjxl20_m'),pbocno1=1,pbocno2=2,nopbocno=3,sdate='2018-01-22',
                            Pbocmodel1_th=c(63,30,8),pbocmodel2_th=c(65,33,7),nopbocmodel1_th=c(65,35,10))
# ----------
Result <- get_result(new_table,ce_ar_pred = 0.6, continue_ar_pred = 0.99,
                     Cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5)
# Output result:
# result
# Output core results
Result_core <- list(result[['cnt_transfer']][rn=='ce_reject',.(pass,continue)],
                    Result[["path_change"]][path %in% c('ce','continue','model')],
                    Result[["ar_change"]][path == 'total'],
                    Result[["fpd10_change"]][path == 'pass'])
Names(result_core) <- c('ce_reject->model_pass number', 'ce, continueue, model change', 'total pass rate change', 'total fpd10 change')

Result_core