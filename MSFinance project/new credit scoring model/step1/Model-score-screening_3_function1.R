#================================================================= ==================================================
# function get_new_table1: Calculate the new workflow and put it in new_table
# Strategy: first press pbocmodel1 to split, no data (generally because there is no credit) and then press nopbocmodel1
Get_new_table1 <- function(model_table,models=c('cashl_m','cup_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
                          Pbocmodel1_th=c(70,40,10), nopbocmodel1_th=c(70,40,10)){
  Library(data.table)
  # Calculate new workflows based on selected models and rules
  # New variable named newwf, value 'none', 'pass', 'continue', 'ce', 'reject'
  # Example parameters
  # models <- c('cashl_m','cup_m')
  # pbocno <- 1
  # nopbocno <- 2
  # sdate <- '2018-01-22'
  # pbocmodel1_th <-c(70,40,10)
  # nopbocmodel1_th <-c(70,40,10)
  # Calculate the new workflow
  New_table <- model_table[appl_date > sdate,c(models,'prod_cd','appl_date','appl_lim','biz_city_cd','path','status','fpd10','maxdpd10'),with=F]
  Names(new_table)[[pbocno]] <- 'pbocmodel1'
  Names(new_table)[[nopbocno]] <- 'nopbocmodel1'
  # View the singular classification by status, path
  # new_table[,.N,by=.(status,path)]
  # newwf='none', a list that won't change
  # unknow and customercancel are equivalent to missing values
  New_table[path == 'unknow' | path == 'customercancel', newwf := 'none']
  #hc and opscancel time before the model
  New_table[path == 'hc' | path == 'opscancel', newwf := 'none']
  # Theoretically, these cancel lists will also be affected, but for the convenience of calculation, they can be ignored and have little effect on the results.
  New_table[status == 'cancel', newwf := 'none']
  # View the singular classification by status, path
  # new_table[is.na(newwf),.N,by=.(status,path)]
  # The rest are all need to calculate the new workflow, the temporary assignment value newwf is 'blank'
  New_table[is.na(newwf),newwf := 'blank']
  # Get 100 equal parts of model score
  Pbocmodel1_cut <- quantile(new_table[,pbocmodel1],0:100/100,na.rm = T)
  Nopbocmodel1_cut <- quantile(new_table[,nopbocmodel1],0:100/100,na.rm = T)
  # [1] is the minimum value, [101] is the maximum value, 100 equal parts, from small to large
  #set threshold
  #---------------
  #greater then pass
  Pbocmodel1_thpass <- pbocmodel1_cut[pbocmodel1_th[1]][[1]]
  # ...and greater than this value continue
  Pbocmodel1_thcontinue <- pbocmodel1_cut[pbocmodel1_th[2]][[1]]
  # ...and greater than this value ce
  Pbocmodel1_threject <- pbocmodel1_cut[pbocmodel1_th[3]][[1]]
  #----------------
  #greater then pass
  Nopbocmodel1_thpass <- nopbocmodel1_cut[nopbocmodel1_th[1]][[1]]
  # ...and greater than this value continue
  Nopbocmodel1_thcontinue <- nopbocmodel1_cut[nopbocmodel1_th[2]][[1]]
  # ...and greater than this value ce
  Nopbocmodel1_threject <- nopbocmodel1_cut[nopbocmodel1_th[3]][[1]]
  #---------------
  # Calculate the new workflow, the order can not be messed up
  # Rule: There is a credit pbocmodel1, no credit nopbocmodel1, no transfer ce
  New_table[newwf == 'blank' & is.na(pbocmodel1) & is.na(nopbocmodel1), newwf := 'ce']
  New_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 >= pbocmodel1_thpass, newwf := 'pass']
  New_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 >= pbocmodel1_thcontinue, newwf := 'continue']
  New_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 >= pbocmodel1_threject, newwf := 'ce']
  New_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 < pbocmodel1_threject, newwf := 'reject']
  New_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 >= nopbocmodel1_thpass, newwf := 'pass']
  New_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 >= nopbocmodel1_thcontinue, newwf := 'continue']
  New_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 >= nopbocmodel1_threject, newwf := 'ce']
  New_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 < nopbocmodel1_threject, newwf := 'reject']
  Return(new_table)
}
#================================================================= ==================================================
# function get_new_table2: Calculate the new workflow and put it in new_table
# strategy: first press pbocmodel1+pbocmodel2 to divert
# If pbocmodel1 has data and pbocmodel2 has no data, press pbocmodel1 result
# pbocmodel1No data (generally because there is no credit), then press nopbocmodel1
# pbocmodel1+pbocmodel2's strategy table is as follows
# ------------------------------------------------- ---
# 1\2      | pass     | continue | ce     | reject |
# -------------------------------------------------|
# pass     | pass     | continue | ce     | reject |
# continue | continue | continue | ce     | reject |
# ce       | ce       | ce       | ce     | reject |
# reject   | reject   | reject   | reject | reject |
# ------------------------------------------------- ---

Get_new_table2 <- function(model_table,models=c('cashl_m','posjxl20_m','cup_m'), pbocno1=1, pbocno2=2, nopbocno=3,sdate='2018-01-22',
                          Pbocmodel1_th=c(70,40,10),pbocmodel2_th=c(70,40,10),nopbocmodel1_th=c(70,40,10)){
  Library(data.table)
  New_table <- model_table[appl_date > sdate,c(models,'prod_cd','appl_date','appl_lim','biz_city_cd','path','status','fpd10','maxdpd10'),with=F]
  Names(new_table)[[pbocno1]] <- 'pbocmodel1'
  Names(new_table)[[pbocno2]] <- 'pbocmodel2'
  Names(new_table)[[nopbocno]] <- 'nopbocmodel1'
  # View the singular classification by status, path
  # new_table[,.N,by=.(status,path)]
  # newwf='none', a list that won't change
  # unknow and customercancel are equivalent to missing values
  New_table[path == 'unknow' | path == 'customercancel', newwf := 'none']
  #hc and opscancel time before the model
  New_table[path == 'hc' | path == 'opscancel', newwf := 'none']
  # Theoretically, these cancel lists will also be affected, but for the convenience of calculation, they can be ignored and have little effect on the results.
  New_table[status == 'cancel', newwf := 'none']
  # View the singular classification by status, path
  # new_table[is.na(newwf),.N,by=.(status,path)]
  # The rest are all need to calculate the new workflow, the temporary assignment value newwf is 'blank'
  New_table[is.na(newwf),newwf := 'blank']
  # Get 100 equal parts of model score
  Pbocmodel1_cut <- quantile(new_table[,pbocmodel1],0:100/100,na.rm = T)
  Pbocmodel2_cut <- quantile(new_table[,pbocmodel2],0:100/100,na.rm = T)
  Nopbocmodel1_cut <- quantile(new_table[,nopbocmodel1],0:100/100,na.rm = T)
  # [1] is the minimum value, [101] is the maximum value, 100 equal parts, from small to large
  #set threshold
  #---------------
  #greater then pass
  Pbocmodel1_thpass <- pbocmodel1_cut[pbocmodel1_th[1]][[1]]
  # ...and greater than this value continue
  Pbocmodel1_thcontinue <- pbocmodel1_cut[pbocmodel1_th[2]][[1]]
  # ...and greater than this value ce
  Pbocmodel1_threject <- pbocmodel1_cut[pbocmodel1_th[3]][[1]]
  #---------------
  #greater then pass
  Pbocmodel2_thpass <- pbocmodel2_cut[pbocmodel2_th[1]][[1]]
  # ...and greater than this value continue
  Pbocmodel2_thcontinue <- pbocmodel2_cut[pbocmodel2_th[2]][[1]]
  # ...and greater than this value ce
  Pbocmodel2_threject <- pbocmodel2_cut[pbocmodel2_th[3]][[1]]
  #----------------
  #greater then pass
  Nopbocmodel1_thpass <- nopbocmodel1_cut[nopbocmodel1_th[1]][[1]]
  # ...and greater than this value continue
  Nopbocmodel1_thcontinue <- nopbocmodel1_cut[nopbocmodel1_th[2]][[1]]
  # ...and greater than this value ce
  Nopbocmodel1_threject <- nopbocmodel1_cut[nopbocmodel1_th[3]][[1]]
  #---------------
  # Calculate the new workflow, the order can not be messed up, in order to reduce the amount of code, the code here may need to be carefully interpreted
  New_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 >= pbocmodel1_thpass, newwf := 'pass' ]
  New_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 >= pbocmodel1_thcontinue, newwf := 'continue']
  New_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 >= pbocmodel1_threject, newwf := 'ce' ]
  New_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 < pbocmodel1_threject, newwf := 'reject']
  New_table[newwf %in% c('pass','continue','ce') & !is.na(pbocmodel2) & pbocmodel2 < pbocmodel2_threject, newwf := 'reject' ]
  New_table[newwf %in% c('pass','continue') & !is.na(pbocmodel2) & pbocmodel2 < pbocmodel2_thcontinue, newwf := 'ce' ]
  New_table[newwf %in% c('pass') & !is.na(pbocmodel2) & pbocmodel2 < pbocmodel2_thpass, newwf := 'continue']
  New_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 >= nopbocmodel1_thpass, newwf := 'pass']
  New_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 >= nopbocmodel1_thcontinue, newwf := 'continue']
  New_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 >= nopbocmodel1_threject, newwf := 'ce']
  New_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 < nopbocmodel1_threject, newwf := 'reject']
  New_table[newwf == 'blank', newwf := 'ce']
  Return(new_table)
}
#================================================================= ==================================================
#function get_result: Get new workflow changes and results
Get_result <- function(new_table,ce_ar_pred = 0.6, continue_ar_pred = 0.99,
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
  # forcast fpd performance---!!!!---The estimated way is rough, pending improvement in the next version
  Fpd_transfer_predict <- copy(fpd_transfer)
  # -----------------------
  fpd_transfer_predict[rn == 'ce_reject',cepass := 0]
  fpd_transfer_predict[rn == 'ce_reject',cereject := 0]
  fpd_transfer_predict[rn == 'ce_reject',reject := 0]
  fpd_transfer_predict[rn == 'model_reject',cereject := 0]
  fpd_transfer_predict[rn == 'model_reject',reject := 0]
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
  Cnt_transfer_passsum <- xy(cnt_transfer_passsum)
  # ---
  Fpd_transfer_predict_passsum <- fpd_transfer_predict[1:5,1:6]
  Fpd_transfer_predict_passsum[,passsum := cepass + continue +pass]
  Fpd_transfer_predict_passsum <- xy(fpd_transfer_predict_passsum)
  Fpd_transfer_predict_passsum[,passsum := ce_pass+continue_pass+model_pass]
  Fpd_transfer_predict_passsum <- xy(fpd_transfer_predict_passsum)
  # ---
  Fpd_transfer_pct_predict_passsum <- cbind(cnt_transfer_passsum[,1],fpd_transfer_predict_passsum[,2:7]/cnt_transfer_passsum[,2:7])
  Fpd_transfer_pct_predict_passsum
  #------------------------------------------------- -----#
  #---2.change: the overall path number and proportion, pass rate, fpd10 changes -----#
  #------------------------------------------------- -----#
  Change_table <- new_table[,.(path,status,fpd10,maxdpd10,newwf)]
  #新新path
  Change_table[newwf != 'none',newpath := newwf]
  Change_table[newpath %in% c('pass','reject'),newpath := 'model']
  Change_table[newwf == 'none',newpath := path]
  # change:Change table
  # path_change: New and old model path number and scale change table
  
  path_change <- cbind(change_table[,.N,by=path][order(path)],
                       change_table[,.N,by=newpath][order(newpath)])[,c(1,2,4)]
  names(path_change) <- c('path','oldcnt','newcnt')
  path_change <- xy(path_change)
  path_change[,autonohc := continue+model]
  path_change[,auto := continue+model+hc]
  path_change <- xy(path_change)
  path_change[,changecnt := newcnt-oldcnt]
  total_count <- new_table[,.N]
  path_change[,oldpct := oldcnt/total_count]
  path_change[,newpct := newcnt/total_count]
  path_change[,changepct := newpct-oldpct]
  path_change
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
  #------------------------------------------------- -----#
  #--------3.result: The main results of the above two analysis ---------------#
  #------------------------------------------------- -----#
  Result <- list(cnt_transfer,fpd_transfer_predict,fpd_transfer_pct_predict_sum,
                 Fpd_transfer_pct_predict_passsum, path_change, ar_change, fpd10_change)
  Names(result) <- c('cnt_transfer','fpd_transfer_predict','fpd_transfer_pct_predict_sum',
                     'fpd_transfer_pct_predict_passsum', 'path_change', 'ar_change', 'fpd10_change')
  Return(result)
}
#================================================================= ==================================================