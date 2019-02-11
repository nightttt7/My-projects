#==========================================================================================
# function get_new_table3
#Parameter:
# model_table: Total table
# models: The selected model, the default is c ('bt3_m', 'cup_m', 'cup_m', 'posjxl20_m'), the function is renamed to 'model1', 'model2', 'model3', 'model4'
# where model1 is the main model when there is credit data, model2 is the sub-model when there is credit data, model3 is the main model when there is no credit data, and model3 is the sub-model when there is no credit data.
# sdate,edate: the start and end date of the time window. The default is sdate='2018-01-22', edate='2018-04-16'
# th1-th4: Decision threshold for different models, the three values ​​are (pass/continue/ce above this percentage) (reject below the third percentage), the default is c(70,40,10)
# For th4, the three values ​​are respectively (below this percentage, when ce/model3 has no value, reject/model3 has a value when reject) (higher than the first percentage)
# passstyle: different styles of pass, 'value is both': model1pass and model2pass, 'either': model1pass or model2pass, default is 'both'
# Add column:
# haspboc: Is there any credit data (whether or not it is judged by model1(bt3), if necessary, the judgment method can be updated), the value is 1 (yes), 0 (none)
# newwf: new workflow with values ​​'none', 'pass', 'continue', 'ce', 'reject'
# key: The model used for decision making, the values ​​are 'model1', 'model2', 'model3', 'model4', 'none', and the case of the main model is counted as the main model.
# Output:
# new_table: Get new_table from model_table by time window and required column, and add new column
# Strategy:
# with credit report (model1+model2), the strategy table is as follows
# |---------------------------------------------------------------|   
# |model1\2 | pass      | continue | ce      | reject | NA        |
# |---------------------------------------------------------------|  
# |pass     | pass      | continue | ce      | reject | pass      |    
# |continue | continue  | continue | ce      | reject | continue  |    
# |ce       | ce        | ce       | ce      | reject | ce        |
# |reject   | reject    | reject   | reject  | reject | reject    |    
# |---------------------------------------------------------------|    
# without credit report(model3+model4), the strategy table is as follows
# |-------------------------------|
# |model3\4 | notreject  | reject |
# |-------------------------------|
# |pass     | pass       | reject |
# |continue | continue   | reject |
# |ce       | ce         | reject |
# |reject   | reject     | reject |
# |NA       | ce/continue| reject |
# |-------------------------------|
# No credit user and model3 has no value: ce
# test---
# setwd('~/Model Split Screen 3/Show 2')
# load('model_table1.RData')
# models=c('bt3_m','cup_m','cup_m','posjxl20_m')
# sdate='2018-01-22'
# edate='2018-04-16'
# passstyle='both'
# th1=c(70,40,10)
# th2=c(70,40,10)
# th3=c(70,40,10)
# th4=c(70,40,10)
# test---
# ------------------------------------------------- ---
Get_new_table3 <- function(model_table,models=c('bt3_m','cup_m','cup_m','posjxl20_m'),
                           Sdate='2018-01-22',edate='2018-04-16',passstyle='both',
                           Th1=c(70,40,10), th2=c(70,40,10),th3=c(70,40,10),th4=c(70,40,10)){
  # Import the required package: data.table
  Library(data.table)
  # Get new_table from model_table by time window and required column
  New_table <- model_table[appl_date >= sdate & appl_date < edate,
                           c(models, 'prod_cd', 'appl_date', 'appl_lim', 'biz_city_cd', 'path', 'status', 'fpd10', 'maxdpd10'), with=F]
  # Named 'model1', 'model2', 'model3', 'model4', easy to reuse
  Names(new_table)[1:4] <- c('model1','model2','model3','model4')
  # By bt3 whether there is a value to determine whether there is a credit user
  New_table[!is.na(model1), haspboc := 1]
  New_table[is.na(model1), haspboc := 0]
  # newwf='none', a list that won't change
  # unknow and customercancel are equivalent to missing values
  New_table[path == 'unknow' | path == 'customercancel', newwf := 'none']
  #hc and opscancel time before the model
  New_table[path == 'hc' | path == 'opscancel', newwf := 'none']
  # Theoretically, these cancel lists will also be affected, but for the convenience of calculation, they can be ignored and have little effect on the results.
  New_table[status == 'cancel', newwf := 'none']
  # The rest are all need to calculate the new workflow, the temporary assignment value newwf is 'blank'
  New_table[is.na(newwf),newwf := 'blank']
  # Get 100 equal parts of model score
  # [1] is the minimum value, [101] is the maximum value, 100 equal parts, from small to large
  Cut1 <- quantile(new_table[,model1],0:100/100,na.rm = T)
  Cut2 <- quantile(new_table[,model2],0:100/100,na.rm = T)
  Cut3 <- quantile(new_table[,model3],0:100/100,na.rm = T)
  Cut4 <- quantile(new_table[,model4],0:100/100,na.rm = T)
  #---------------
  # According to the strategy, model score, threshold (th1~th4), passstyle calculates newwf and key, in order to reduce the amount of code, this segment is more complicated.
  New_table[newwf == 'blank' & haspboc, key := 'model1']
  New_table[key == 'model1' & model1 >= cut1[th1[1]][[1]], newwf := 'pass']
  New_table[key == 'model1' & model1 < cut1[th1[1]][[1]], newwf := 'continue']
  New_table[key == 'model1' & model1 < cut1[th1[2]][[1]], newwf := 'ce']
  New_table[key == 'model1' & model1 < cut1[th1[3]][[1]], newwf := 'reject']
  #---
  New_table[key == 'model1' & newwf != 'reject' & model2 < cut2[th2[3]][[1]], key := 'model2']
  New_table[key == 'model2', newwf := 'reject']
  New_table[key == 'model1' & newwf != 'reject' & newwf != 'ce' & model2 < cut2[th2[2]][[1]], key := 'model2']
  New_table[key == 'model2' & newwf != 'reject', newwf := 'ce']
  #---
  If (passstyle == 'both') {
    New_table[key == 'model1' & newwf == 'pass' & model2 < cut2[th2[1]][[1]], key := 'model2']
    New_table[key == 'model2' & newwf != 'reject' & newwf != 'ce', newwf := 'continue']
  }
  If (passstyle == 'either') {
    New_table[key == 'model1' & newwf == 'continue' & model2 >= cut2[th2[1]][[1]], key := 'model2']
    New_table[key == 'model2' & newwf != 'reject' & newwf != 'ce', newwf := 'pass']
  }
  #---
  New_table[newwf == 'blank' & !haspboc, key := 'model3']
  New_table[key == 'model3' & model3 >= cut3[th3[1]][[1]], newwf := 'pass']
  New_table[key == 'model3' & model3 < cut3[th3[1]][[1]], newwf := 'continue']
  New_table[key == 'model3' & model3 < cut3[th3[2]][[1]], newwf := 'ce']
  New_table[key == 'model3' & model3 < cut3[th3[3]][[1]], newwf := 'reject']
  #---
  New_table[key == 'model3' & model4 < cut4[th4[3]][[1]] & newwf != 'reject', key := 'model4']
  New_table[key == 'model4', newwf := 'reject']
  #---
  New_table[key == 'model3' & is.na(model3), key := 'model4']
  New_table[key == 'model4' & model4 < cut4[th4[2]][[1]], newwf := 'reject']
  New_table[key == 'model4' & newwf != 'reject' & model4 < cut4[th4[1]][[1]], newwf := 'ce']
  New_table[key == 'model4' & newwf != 'reject' & newwf != 'ce' & model4 >= cut4[th4[1]][[1]], newwf := 'continue']
  #---
  New_table[key == 'model4' & is.na(model4), key := 'none']
  New_table[key == 'none', newwf := 'ce']
  #---
  Return(new_table)
}
#================================================================= ==================================================
# function get_result3: Output various indicators according to new_table, including the number of workflow / pass rate / fpd10 transfer table,
# Overall path number and proportion, pass rate, fpd10 change table, and selected core indicators.
#Default parameter
# Important estimate:
# CE pass rate remains unchanged at 60%
# continue Cancellation rate is about 5%
# Specific CE pass rate and ops cancel rate based on actual time window historical data
# CE pass rate = 60%
# continue cancel rate=1%
# ce_ar_pred <- 0.6
# continue_ar_pred <- 0.99
#Parameter: Estimate the ratio of pd10 of ce_rejectf is 2.5 times that of ce_pass
# cerejectpassfpd10 <- 2.5
#Parameter: The estimated pd10 ratio of model_rejectf before is 2.5 times that of model_pass
# oldmodelrejectpassfpd10 <- 2.5
# test---
# ce_ar_pred = 0.6
# continue_ar_pred = 0.99
# cerejectpassfpd10 = 2.5
# oldmodelrejectpassfpd10 = 2.5
# test---
# ------------------------------------------------- ---
Get_result3 <- function(new_table,ce_ar_pred = 0.6, continue_ar_pred = 0.99,
                       Cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5){
  Library(data.table)
  # function xy: transpose data.table
  Xy <- function(dt) {
    t <- data.table(t(dt[,2:ncol(dt)]),keep.rownames = T)
    Names(t) <- c(names(dt[,1]),c(dt[,1])[[1]])
    Return(t)
  }
  #-------------------------------------#
  #---1.transfer: Workflow Transfer Form-----#
  #-------------------------------------#
  # cnt_transfer: Transfer of quantity
  Cnt_transfer <- data.table(reshape2::acast(data = new_table[newwf != 'none',.N,by=.(path,status,newwf)],
                                             Path + status ~ newwf, value.var = 'N'), keep.rownames = T)[order(rn)]
  # Missing value converted to 0
  Cnt_transfer <- cnt_transfer[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # split ce for cepass and cereject
  Cnt_transfer[,cepass := round(ce*ce_ar_pred)]
  Cnt_transfer[rn == 'ce_pass',cepass := ce]
  Cnt_transfer[rn == 'ce_reject',cepass := 0]
  Cnt_transfer[,cereject := ce - cepass]
  Cnt_transfer <- cnt_transfer[,.(rn,cepass,cereject,continue,pass,reject)]
  # Calculate the sum of each row/column
  Cnt_transfer[,sum := cepass+cereject+continue+pass+reject]
  Cnt_transfer <- xy(cnt_transfer)
  Cnt_transfer[,sum := ce_pass+ce_reject+continue_pass+model_pass+model_reject]
  Cnt_transfer <- xy(cnt_transfer)
  Cnt_transfer
  # -----------------------
  # fpd_transfer: Transfer of fpd10
  # ce_reject, model_reject has no fpd10 performance, needs to be estimated, and the predicted value of fpd10 is transferred in fpd_transfer_predict
  Fpd_transfer <- data.table(reshape2::acast(data = new_table[newwf != 'none' & fpd10 == 'bad',.N,by=.(path,status,newwf)],
                                             Path + status ~ newwf, value.var = 'N'), keep.rownames = T)
  # Missing value converted to 0
  Fpd_transfer <- fpd_transfer[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # convert to double and add two lines
  Fpd_transfer <- cbind(fpd_transfer[,1],fpd_transfer[,2:5]*1.0)
  Fpd_transfer <- rbind(fpd_transfer, data.table(rn=c('ce_reject','model_reject')), fill = T)[order(rn)]
  # split ce for cepass and cereject
  Fpd_transfer[rn == 'ce_pass',cepass := ce]
  Fpd_transfer[rn == 'ce_pass',cereject := 0]
  Fpd_transfer[rn == 'continue_pass',cepass := ce*(1/(1+2.5*(1-ce_ar_pred)/ce_ar_pred))]
  Fpd_transfer[rn == 'continue_pass',cereject := ce - cepass]
  Fpd_transfer[rn == 'model_pass',cepass := ce*(1/(1+2.5*(1-ce_ar_pred)/ce_ar_pred))]
  Fpd_transfer[rn == 'model_pass',cereject := ce - cepass]
  Fpd_transfer <- fpd_transfer[,.(rn,cepass,cereject,continue,pass,reject)]
  Fpd_transfer[,sum := cepass+cereject+continue+pass+reject]
  Fpd_transfer
  # -----------------------
  # fpd_transfer_pct: Transfer of fpd scale
  Fpd_transfer_pct <- cbind(fpd_transfer[,1],fpd_transfer[,2:7]/cnt_transfer[1:5,2:7])
  Fpd_transfer_pct[rn == 'ce_pass',cereject := 0]
  Fpd_transfer_pct
  # -----------------------
  # forecast fpd performance---!!!!---The estimated way is rough, pending improvement in the next version
  Fpd_transfer_predict <- copy(fpd_transfer)
  # -----------------------
  Fpd_transfer_predict[rn == 'ce_reject',cepass := 0]
  Fpd_transfer_predict[rn == 'ce_reject',cereject := 0]
  Fpd_transfer_predict[rn == 'ce_reject',reject := 0]
  Fpd_transfer_predict[rn == 'model_reject',cereject := 0]
  Fpd_transfer_predict[rn == 'model_reject',reject := 0]
  # -----------------------
  # rn=model_reject,newpath=ce, (It is assumed here that the proportion of fpd10 through ce is constant, and this assumption is basically true)
  Fpd_transfer_predict[rn == 'model_reject',cepass := cnt_transfer[rn == 'model_reject',cepass]*fpd_transfer_pct[rn == 'ce_pass',sum]]
  # -----------------------
  # rn=ce_reject,newpath=continue
  Fpd_transfer_predict[rn == 'ce_reject', continue := cnt_transfer[rn == 'ce_reject', continueue]*fpd_transfer_pct[rn == 'ce_pass',sum]*cerejectpassfpd10]
  # rn=ce_reject,newpath=pass
  Fpd_transfer_predict[rn == 'ce_reject',pass := cnt_transfer[rn == 'ce_reject',pass]*fpd_transfer_pct[rn == 'ce_pass',sum]*cerejectpassfpd10]
  # -----------------------
  # rn=model_reject,newpath=continue
  Fpd_transfer_predict[rn == 'model_reject', continue := cnt_transfer[rn == 'model_reject', continueue]*fpd_transfer_pct[rn == 'model_pass',sum]*oldmodelrejectpassfpd10]
  # rn=model_reject,newpath=pass
  Fpd_transfer_predict[rn == 'model_reject', pass := cnt_transfer[rn == 'model_reject',pass]*fpd_transfer_pct[rn == 'model_pass',sum]*oldmodelrejectpassfpd10]
  # -----------------------
  Fpd_transfer_predict[, sum := cepass+cereject+continue+pass+reject]
  Fpd_transfer_predict <- xy(fpd_transfer_predict)
  Fpd_transfer_predict[,sum := ce_pass+ce_reject+continue_pass+model_pass+model_reject]
  Fpd_transfer_predict <- xy(fpd_transfer_predict)
  Fpd_transfer_predict
  # -----------------------
  # Calculate the transfer of fpd_transfer_pct_predict:fpd ratio based on estimated fpd performance (estimated)
  # 1.fpd_transfer_pct_predict_sum: The sum value contains all
  Fpd_transfer_pct_predict_sum <- cbind(cnt_transfer[,1],fpd_transfer_predict[,2:7]/cnt_transfer[,2:7])
  Fpd_transfer_pct_predict_sum
  # 2.fpd_transfer_pct_predict_passsum: The passsum value does not contain the rejected part
  Cnt_transfer_passsum <- cnt_transfer[1:5,1:6]
  Cnt_transfer_passsum[,passsum := cepass + continue +pass]
  Cnt_transfer_passsum <- xy(cnt_transfer_passsum)
  Cnt_transfer_passsum[,passsum := ce_pass+continue_pass+model_pass]
  cnt_transfer_passsum <- xy(cnt_transfer_passsum)
  # ---
  fpd_transfer_predict_passsum <- fpd_transfer_predict[1:5,1:6]
  fpd_transfer_predict_passsum[,passsum  := cepass + continue +pass]
  fpd_transfer_predict_passsum <- xy(fpd_transfer_predict_passsum)
  fpd_transfer_predict_passsum[,passsum := ce_pass+continue_pass+model_pass]
  fpd_transfer_predict_passsum <- xy(fpd_transfer_predict_passsum)
  # ---
  fpd_transfer_pct_predict_passsum <- cbind(cnt_transfer_passsum[,1],fpd_transfer_predict_passsum[,2:7]/cnt_transfer_passsum[,2:7])
  fpd_transfer_pct_predict_passsum
  #------------------------------------------------------#
  #---2.change: the overall path number and proportion, pass rate, fpd10 changes -----#
  #------------------------------------------------- -----#
  Change_table <- new_table[,.(path,status,fpd10,maxdpd10,newwf,key)]
  #新新path
  Change_table[newwf != 'none',newpath := newwf]
  Change_table[newpath %in% c('pass','reject'),newpath := 'model']
  Change_table[newwf == 'none',newpath := path]
  # change:Change table
  # path_change: New and old model path number and scale change table
  
  Path_change <- cbind(change_table[,.N,by=path][order(path)],
                       Change_table[,.N,by=newpath][order(newpath)])[,c(1,2,4)]
  Names(path_change) <- c('path','oldcnt','newcnt')
  Path_change <- xy(path_change)
  Path_change[,autonohc := continue+model+opscancel+unknow]
  Path_change[,auto := continue+model+hc+opscancel+unknow]
  Path_change <- xy(path_change)
  Path_change[,changecnt := newcnt-oldcnt]
  Total_count <- new_table[,.N]
  Path_change[,oldpct := oldcnt/total_count]
  Path_change[,newpct := newcnt/total_count]
  Path_change[,changepct := newpct-oldpct]
  Path_change
  # ar_change: New and old model pass rate change table
  Ce_pass_old <- change_table[path == 'ce'][status == 'pass'][,.N]
  Model_pass_old <- change_table[path == 'model'][status == 'pass'][,.N]
  Continue_pass_old <- change_table[path == 'continue'][status == 'pass'][,.N]
  Autonohc_pass_old <- model_pass_old + continue_pass_old
  Total_pass_old <- autonohc_pass_old + ce_pass_old
  #------
  Ce_old <- change_table[path == 'ce'][,.N]
  Model_old <- change_table[path == 'model'][,.N]
  Continue_old <- change_table[path == 'continue'][,.N]
  Autonohc_old <- model_old + continue_old
  #------
  Ce_ar_old <- ce_pass_old/ce_old
  Model_ar_old <- model_pass_old/model_old
  Continue_ar_old <- continue_pass_old/continue_old
  Autonohc_ar_old <- autonohc_pass_old/autonohc_old
  Total_ar_old <- total_pass_old/total_count
  #------
  #ce_pass_new is different from cnt_transfer in cnt_transfer according to the new ce pass state and the old one, here ce_ar_pred is calculated according to the ratio
  Ce_pass_new <- change_table[newpath == 'ce'][,.N]*ce_ar_pred
  Model_pass_new <- change_table[newwf == 'pass'][,.N]
  Continue_pass_new <- change_table[newpath == 'continue'][,.N]*continue_ar_pred
  Autonohc_pass_new <- model_pass_new + continue_pass_new
  Total_pass_new <- autonohc_pass_new + ce_pass_new
  #------
  Ce_new <- change_table[newpath == 'ce'][,.N]
  Model_new <- change_table[newpath == 'model'][,.N]
  Continue_new <- change_table[newpath == 'continue'][,.N]
  Autonohc_new <- model_new + continue_new
  #------
  Ce_ar_new <- ce_pass_new/ce_new
  Model_ar_new <- model_pass_new/model_new
  Continue_ar_new <- continue_pass_new/continue_new
  Autonohc_ar_new <- autonohc_pass_new/autonohc_new
  Total_ar_new <- total_pass_new/total_count
  #------
  Ar_change <- data.table(path = c('ce','model','continue','autonohc','total'),
                          Oldar = c(ce_ar_old, model_ar_old, continue_ar_old, autonohc_ar_old, total_ar_old),
                          Newar = c(ce_ar_new, model_ar_new, continue_ar_new, autonohc_ar_new, total_ar_new))
  Ar_change
  # fpd10_change: New and old model fpd10 change table
  Fpd10_old <- fpd_transfer_pct_predict_sum[rn %in% c('ce_pass','continue_pass','model_pass'),.(rn,sum)]
  Fpd10_old_all <- fpd_transfer_predict[rn %in% c('ce_pass','continue_pass','model_pass'),sum(sum)]/
    Cnt_transfer[rn %in% c('ce_pass', 'continue_pass', 'model_pass'), sum(sum)]
  Fpd10_new <- xy(fpd_transfer_pct_predict_sum)[rn %in% c('cepass','continue','pass'),.(rn,sum)]
  Fpd10_new_all <- xy(fpd_transfer_predict)[rn %in% c('cepass','continue','pass'),sum(sum)]/
    Xy(cnt_transfer)[rn %in% c('cepass','continue','pass'),sum(sum)]
  Fpd10_change <- cbind(fpd10_old,fpd10_new[,2])
  Names(fpd10_change) <- c('path','fpd10_old','fpd10_new')
  Fpd10_change <- rbind(fpd10_change,data.table(path='pass',fpd10_old=fpd10_old_all,fpd10_new=fpd10_new_all))
  Fpd10_change
  #------------------------------------------------- -----#
  #---3.model: The number and proportion of each model decision---------------------#
  #------------------------------------------------------#
  model_cnt <- data.table(reshape2::acast(data = change_table[newwf != 'none',.N,by=.(newwf,key)], newwf ~ key, value.var = 'N'), keep.rownames = T)
  names(model_cnt)[1] <- 'newwf'
  model_fpd10_cnt <- data.table(reshape2::acast(data = change_table[newwf != 'none' & fpd10 == 'bad',.N,by=.(newwf,key)], newwf ~ key, value.var = 'N'), keep.rownames = T)
  names(model_fpd10_cnt)[1] <- 'newwf'
  model_fpd10_pct <- cbind(model_cnt[,1],model_fpd10_cnt[,2:ncol(model_fpd10_cnt)]/model_cnt[,2:ncol(model_cnt)])
  model_cnt
  model_fpd10_cnt
  model_fpd10_pct
  #------------------------------------------------------#
  #---4.result: The main results of the above three analysis --------------------#
  #------------------------------------------------- -----#
  Result <- list(cnt_transfer,fpd_transfer_predict,fpd_transfer_pct_predict_sum,
                 Fpd_transfer_pct_predict_passsum, path_change, ar_change, fpd10_change,
                 Model_cnt, model_fpd10_cnt, model_fpd10_pct)
  Names(result) <- c('cnt_transfer','fpd_transfer_predict','fpd_transfer_pct_predict_sum',
                     'fpd_transfer_pct_predict_passsum', 'path_change', 'ar_change', 'fpd10_change',
                     'model_cnt', 'model_fpd10_cnt', 'model_fpd10_pct')
  Return(result)
}
#================================================================= ==================================================
# function get_result3_nopboc: get_result3 unlicensed data version, deleted a lot of code to run properly
# test---
# ce_ar_pred = 0.6
# continue_ar_pred = 0.99
# cerejectpassfpd10 = 2.5
# oldmodelrejectpassfpd10 = 2.5
# new_table <- new_table[haspboc == 0]
# test---
# ------------------------------------------------- ---
Get_result3_nopboc <- function(new_table,ce_ar_pred = 0.6, continue_ar_pred = 0.99,
                        Cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5){
  Library(data.table)
  #Default parameter
  # Important estimate:
  # CE pass rate remains unchanged at 60%
  # continue Cancellation rate is about 5%
  # Specific CE pass rate and ops cancel rate based on actual time window historical data
  # CE pass rate = 60%
  # continue cancel rate=1%
  # ce_ar_pred <- 0.6
  # continue_ar_pred <- 0.99
  #Parameter: Estimate the ratio of pd10 of ce_rejectf is 2.5 times that of ce_pass
  # cerejectpassfpd10 <- 2.5
  #Parameter: The estimated pd10 ratio of model_rejectf before is 2.5 times that of model_pass
  # oldmodelrejectpassfpd10 <- 2.5
  # function xy: transpose data.table
  Xy <- function(dt) {
    t <- data.table(t(dt[,2:ncol(dt)]),keep.rownames = T)
    Names(t) <- c(names(dt[,1]),c(dt[,1])[[1]])
    Return(t)
  }
  #-------------------------------------#
  #---1.transfer: Workflow Transfer Form-----#
  #-------------------------------------#
  # cnt_transfer: Transfer of quantity
  Cnt_transfer <- data.table(reshape2::acast(data = new_table[newwf != 'none',.N,by=.(path,status,newwf)],
                                             Path + status ~ newwf, value.var = 'N'), keep.rownames = T)[order(rn)]
  # Missing value converted to 0
  Cnt_transfer <- cnt_transfer[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # split ce for cepass and cereject
  Cnt_transfer[,cepass := round(ce*ce_ar_pred)]
  Cnt_transfer[rn == 'ce_pass',cepass := ce]
  Cnt_transfer[rn == 'ce_reject',cepass := 0]
  Cnt_transfer[,cereject := ce - cepass]
  Cnt_transfer <- cnt_transfer[,.(rn,cepass,cereject,continue,pass,reject)]
  # Calculate the sum of each row/column
  Cnt_transfer[,sum := cepass+cereject+continue+pass+reject]
  Cnt_transfer <- xy(cnt_transfer)
  Cnt_transfer[,sum := ce_pass+ce_reject+continue_pass]
  Cnt_transfer <- xy(cnt_transfer)
  Cnt_transfer
  # -----------------------
  # fpd_transfer: Transfer of fpd10
  # ce_reject, model_reject has no fpd10 performance, needs to be estimated, and the predicted value of fpd10 is transferred in fpd_transfer_predict
  Fpd_transfer <- data.table(reshape2::acast(data = new_table[newwf != 'none' & fpd10 == 'bad',.N,by=.(path,status,newwf)],
                                             Path + status ~ newwf, value.var = 'N'), keep.rownames = T)
  # Missing value converted to 0
  Fpd_transfer <- fpd_transfer[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # convert to double and add two lines
  Fpd_transfer <- cbind(fpd_transfer[,1],fpd_transfer[,2:5]*1.0)
  Fpd_transfer <- rbind(fpd_transfer, data.table(rn=c('ce_reject')), fill = T)[order(rn)]
  # split ce for cepass and cereject
  Fpd_transfer[rn == 'ce_pass',cepass := ce]
  Fpd_transfer[rn == 'ce_pass',cereject := 0]
  Fpd_transfer[rn == 'continue_pass',cepass := ce*(1/(1+2.5*(1-ce_ar_pred)/ce_ar_pred))]
  Fpd_transfer[rn == 'continue_pass',cereject := ce - cepass]
  Fpd_transfer <- fpd_transfer[,.(rn,cepass,cereject,continue,pass,reject)]
  Fpd_transfer[,sum := cepass+cereject+continue+pass+reject]
  Fpd_transfer
  # -----------------------
  # fpd_transfer_pct: Transfer of fpd scale
  Fpd_transfer_pct <- cbind(fpd_transfer[,1],fpd_transfer[,2:7]/cnt_transfer[1:3,2:7])
  Fpd_transfer_pct[rn == 'ce_pass',cereject := 0]
  fpd_transfer_pct
  # -----------------------
  #估计fpd performance---!!!!---The estimated way is rough, pending improvement in the next version
  Fpd_transfer_predict <- copy(fpd_transfer)
  # -----------------------
  Fpd_transfer_predict[rn == 'ce_reject',cepass := 0]
  Fpd_transfer_predict[rn == 'ce_reject',cereject := 0]
  Fpd_transfer_predict[rn == 'ce_reject',reject := 0]
  # -----------------------
  # rn=ce_reject,newpath=continue
  Fpd_transfer_predict[rn == 'ce_reject', continue := cnt_transfer[rn == 'ce_reject', continueue]*fpd_transfer_pct[rn == 'ce_pass',sum]*cerejectpassfpd10]
  # rn=ce_reject,newpath=pass
  Fpd_transfer_predict[rn == 'ce_reject',pass := cnt_transfer[rn == 'ce_reject',pass]*fpd_transfer_pct[rn == 'ce_pass',sum]*cerejectpassfpd10]
  # -----------------------
  Fpd_transfer_predict[, sum := cepass+cereject+continue+pass+reject]
  Fpd_transfer_predict <- xy(fpd_transfer_predict)
  Fpd_transfer_predict[,sum := ce_pass+ce_reject+continue_pass]
  Fpd_transfer_predict <- xy(fpd_transfer_predict)
  Fpd_transfer_predict
  # -----------------------
  # Calculate the transfer of fpd_transfer_pct_predict:fpd ratio based on estimated fpd performance (estimated)
  # 1.fpd_transfer_pct_predict_sum: The sum value contains all
  Fpd_transfer_pct_predict_sum <- cbind(cnt_transfer[,1],fpd_transfer_predict[,2:7]/cnt_transfer[,2:7])
  Fpd_transfer_pct_predict_sum
  # 2.fpd_transfer_pct_predict_passsum: The passsum value does not contain the rejected part
  Cnt_transfer_passsum <- cnt_transfer[1:3,1:6]
  Cnt_transfer_passsum[,passsum := cepass + continue +pass]
  Cnt_transfer_passsum <- xy(cnt_transfer_passsum)
  Cnt_transfer_passsum[,passsum := ce_pass+continue_pass]
  Cnt_transfer_passsum <- xy(cnt_transfer_passsum)
  # ---
  Fpd_transfer_predict_passsum <- fpd_transfer_predict[1:3,1:6]
  Fpd_transfer_predict_passsum[,passsum := cepass + continue +pass]
  Fpd_transfer_predict_passsum <- xy(fpd_transfer_predict_passsum)
  Fpd_transfer_predict_passsum[,passsum := ce_pass+continue_pass]
  Fpd_transfer_predict_passsum <- xy(fpd_transfer_predict_passsum)
  # ---
  Fpd_transfer_pct_predict_passsum <- cbind(cnt_transfer_passsum[,1],fpd_transfer_predict_passsum[,2:7]/cnt_transfer_passsum[,2:7])
  Fpd_transfer_pct_predict_passsum
  #------------------------------------------------- -----#
  #---2.change: the overall path number and proportion, pass rate, fpd10 changes -----#
  #------------------------------------------------- -----#
  Change_table <- new_table[,.(path,status,fpd10,maxdpd10,newwf,key)]
  # new path
  Change_table[newwf != 'none',newpath := newwf]
  Change_table[newpath %in% c('pass','reject'),newpath := 'model']
  Change_table[newwf == 'none',newpath := path]
  # change:Change table
  # path_change: New and old model path number and scale change table
  Path_change <- cbind(rbind(change_table[,.N,by=path],data.table(path='model',N=0))[order(path)],
                       Change_table[,.N,by=newpath][order(newpath)])[,c(1,2,4)]
  Names(path_change) <- c('path','oldcnt','newcnt')
  Path_change <- xy(path_change)
  Path_change[,autonohc := continue+model]
  Path_change[,auto := continue+model+hc]
  Path_change <- xy(path_change)
  Path_change[,changecnt := newcnt-oldcnt]
  Total_count <- new_table[,.N]
  Path_change[,oldpct := oldcnt/total_count]
  Path_change[,newpct := newcnt/total_count]
  Path_change[,changepct := newpct-oldpct]
  Path_change
  # ar_change: New and old model pass rate change table
  Ce_pass_old <- change_table[path == 'ce'][status == 'pass'][,.N]
  Model_pass_old <- change_table[path == 'model'][status == 'pass'][,.N]
  Continue_pass_old <- change_table[path == 'continue'][status == 'pass'][,.N]
  Autonohc_pass_old <- model_pass_old + continue_pass_old
  Total_pass_old <- autonohc_pass_old + ce_pass_old
  #------
  Ce_old <- change_table[path == 'ce'][,.N]
  Model_old <- change_table[path == 'model'][,.N]
  Continue_old <- change_table[path == 'continue'][,.N]
  Autonohc_old <- model_old + continue_old
  #------
  Ce_ar_old <- ce_pass_old/ce_old
  Model_ar_old <- model_pass_old/model_old
  Continue_ar_old <- continue_pass_old/continue_old
  Autonohc_ar_old <- autonohc_pass_old/autonohc_old
  Total_ar_old <- total_pass_old/total_count
  #------
  #ce_pass_new is different from cnt_transfer in cnt_transfer according to the new ce pass state and the old one, here ce_ar_pred is calculated according to the ratio
  Ce_pass_new <- change_table[newpath == 'ce'][,.N]*ce_ar_pred
  Model_pass_new <- change_table[newwf == 'pass'][,.N]
  Continue_pass_new <- change_table[newpath == 'continue'][,.N]*continue_ar_pred
  Autonohc_pass_new <- model_pass_new + continue_pass_new
  Total_pass_new <- autonohc_pass_new + ce_pass_new
  #------
  Ce_new <- change_table[newpath == 'ce'][,.N]
  Model_new <- change_table[newpath == 'model'][,.N]
  Continue_new <- change_table[newpath == 'continue'][,.N]
  Autonohc_new <- model_new + continue_new
  #------
  Ce_ar_new <- ce_pass_new/ce_new
  Model_ar_new <- model_pass_new/model_new
  Continue_ar_new <- continue_pass_new/continue_new
  Autonohc_ar_new <- autonohc_pass_new/autonohc_new
  Total_ar_new <- total_pass_new/total_count
  #------
  Ar_change <- data.table(path = c('ce','model','continue','autonohc','total'),
                          oldar = c(ce_ar_old,model_ar_old,continue_ar_old,autonohc_ar_old,total_ar_old),
                          newar = c(ce_ar_new,model_ar_new,continue_ar_new,autonohc_ar_new,total_ar_new))
  ar_change
  # fpd10_change:New and old model fpd10 change table
  Fpd10_old <- fpd_transfer_pct_predict_sum[rn %in% c('ce_pass','continue_pass','model_pass'),.(rn,sum)]
  Fpd10_old_all <- fpd_transfer_predict[rn %in% c('ce_pass','continue_pass','model_pass'),sum(sum)]/
    Cnt_transfer[rn %in% c('ce_pass', 'continue_pass', 'model_pass'), sum(sum)]
  Fpd10_new <- xy(fpd_transfer_pct_predict_sum)[rn %in% c('cepass','continue','pass'),.(rn,sum)]
  Fpd10_new_all <- xy(fpd_transfer_predict)[rn %in% c('cepass','continue','pass'),sum(sum)]/
    Xy(cnt_transfer)[rn %in% c('cepass','continue','pass'),sum(sum)]
  Fpd10_change <- cbind(rbind(fpd10_old,data.table(rn='model_pass',sum=0)),fpd10_new[,2])
  Names(fpd10_change) <- c('path','fpd10_old','fpd10_new')
  Fpd10_change <- rbind(fpd10_change,data.table(path='pass',fpd10_old=fpd10_old_all,fpd10_new=fpd10_new_all))
  Fpd10_change
  #------------------------------------------------- -----#
  #---3.model: The number and proportion of each model decision---------------------#
  #------------------------------------------------- -----#
  Model_cnt <- data.table(reshape2::acast(data = change_table[newwf != 'none',.N,by=.(newwf,key)], newwf ~ key, value.var = 'N'), keep .rownames = T)
  Names(model_cnt)[1] <- 'newwf'
  Model_fpd10_cnt <- data.table(reshape2::acast(data = change_table[newwf != 'none' & fpd10 == 'bad',.N,by=.(newwf,key)], newwf ~ key, value.var = 'N'), keep.rownames = T)
  Names(model_fpd10_cnt)[1] <- 'newwf'
  Model_fpd10_pct <- cbind(model_cnt[,1],model_fpd10_cnt[,2:ncol(model_fpd10_cnt)]/model_cnt[,2:ncol(model_cnt)])
  Model_cnt
  Model_fpd10_cnt
  Model_fpd10_pct
  #------------------------------------------------- -----#
  #---4.result: The main results of the above three analysis --------------------#
  #------------------------------------------------- -----#
  Result <- list(cnt_transfer,fpd_transfer_predict,fpd_transfer_pct_predict_sum,
                 Fpd_transfer_pct_predict_passsum, path_change, ar_change, fpd10_change,
                 Model_cnt, model_fpd10_cnt, model_fpd10_pct)
  Names(result) <- c('cnt_transfer','fpd_transfer_predict','fpd_transfer_pct_predict_sum',
                     'fpd_transfer_pct_predict_passsum', 'path_change', 'ar_change', 'fpd10_change',
                     'model_cnt', 'model_fpd10_cnt', 'model_fpd10_pct')
  Return(result)
}
#================================================================= ==================================================
# function get_core: Get the core indicator from result, put it into a single row data.table
# 2:to
# rem:remain, retention rate
# pct:% of total applications
# ch:change, rate of change
# cr:ce_reject
# cp:ce_pass
# con:continue
# pc:model_pass or continue
# mp:model_pass
# mj:model_reject
# sub:subsidiary percent, additional model ratio
Get_core <- function(result) {
  Return(data.table(cr2mp = result[['cnt_transfer']][rn=='ce_reject',pass],
                    Cr2pc = result[['cnt_transfer']][rn=='ce_reject',pass+continue],
                    Cp_rem = result[['cnt_transfer']][rn=='ce_pass',cepass/sum],
                    Con_rem = result[['cnt_transfer']][rn=='continue_pass', continue/sum],
                    Mp_rem = result[['cnt_transfer']][rn=='model_pass',pass/sum],
                    Ce_pct_ch = result[["path_change"]][path=='ce', changepct],
                    Mocel_pct_ch = result[["path_change"]][path=='model', changepct],
                    Model_ar = result[["ar_change"]][path == 'model',newar],
                    Autonohc_ar = result[["ar_change"]][path == 'autonohc',newar],
                    Total_ar = result[["ar_change"]][path == 'total',newar],
                    Mp_fpd10 = result[["fpd10_change"]][path == 'model_pass',fpd10_new],
                    Con_fpd10 = result[["fpd10_change"]][path == 'continue_pass',fpd10_new],
                    Pass_fpd10 = result[["fpd10_change"]][path == 'pass',fpd10_new]
  ))}
#================================================================= ==================================================
# function get_core1: unreported data version of get_core
Get_core1 <- function(result) {
  Return(data.table(cr2mp = result[['cnt_transfer']][rn=='ce_reject',pass],
                    Cr2pc = result[['cnt_transfer']][rn=='ce_reject',pass+continue],
                    Cp_rem = result[['cnt_transfer']][rn=='ce_pass',cepass/sum],
                    Con_rem = result[['cnt_transfer']][rn=='continue_pass', continue/sum],
                    Ce_pct_ch = result[["path_change"]][path=='ce', changepct],
                    Mocel_pct_ch = result[["path_change"]][path=='model', changepct],
                    Model_ar = result[["ar_change"]][path == 'model',newar],
                    Autonohc_ar = result[["ar_change"]][path == 'autonohc',newar],
                    Total_ar = result[["ar_change"]][path == 'total',newar],
                    Mp_fpd10 = result[["fpd10_change"]][path == 'model_pass',fpd10_new],
                    Con_fpd10 = result[["fpd10_change"]][path == 'continue_pass',fpd10_new],
                    Pass_fpd10 = result[["fpd10_change"]][path == 'pass',fpd10_new]
  ))}
#================================================================= ==================================================