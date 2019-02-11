#================================================================= ==========#
#=========Set Working Directory & Import Module & Get Data ====================#
#================================================================= ==========#
# Set working directory
Setwd('~/model sub-screening 3')
# required package: sltool (self-built), data.table, reshape2, ggplot2
Library(sltool)
Library(data.table)
Library(ggplot2)
# Run and take
Model_table <- as.data.table(run.sql(exe = 1:4, get = 5, uid = "qingqi.shi", pwd = "sqq513"))
# check data
Model_table[,.N,by=bt3_m]
Model_table[,.N,by=zhima_m]
Model_table[,.N,by=zhimamsxf_m]
Model_table[,.N,by=max_dpd]
Model_table[,.N,by=fpd]
# only get data
# model_table <- run.sql(get = 5, uid = "qingqi.shi", pwd = "sqq513")
# *************Data backup point 0*************
# save(model_table, file = 'model_table.RData')
# load('model_table.RData')
#================================================================= ==========#
#========= The variable appl_no is not unique and needs to: ============================#
#=========Compressing round_par,workflow_cd,rule_content,rule_id====#
#================================================================= ==========#
# hc Strong rejection (0/1)
Model_table[, hc := 0]
Model_table[grepl('HC',rule_id) | rule_content == 'TempPatch_beauty_different_province', hc := 1]
#------------------------------------------------- ----------
# ce (0/1)
Model_table[, ce := 0]
Model_table[grepl("3",round_par) & grepl("WF_CECheck",workflow_cd), ce := 1]
#------------------------------------------------- ----------
# Blaze2Pass, EnterBlaze3: used to calculate opscancel: operation cancellation
Model_table[, Blaze2Pass := 0]
Model_table[grepl("2",round_par) & grepl("WF_Path",workflow_cd) & workflow_cd != "WF_PathGo", Blaze2Pass := 1]
Model_table[, EnterBlaze3 := 0]
Model_table[grepl("3",round_par), EnterBlaze3 := 1]
#------------------------------------------------- ----------
# model(0/1:reject/2:go)
Model_table[, model := 0]
Model_table[grepl('2',round_par) & grepl("WF_Reject",workflow_cd) & grepl("SLBE",rule_id) & hc == 0, model := 1]
Model_table[(grepl("2",round_par)==1 & grepl("WF_PathGo",workflow_cd)), model := 2]
# continueModel Operation (0/1: Model + Operation continue)
Model_table[, continue := 0]
Model_table[(grepl("3",round_par) & grepl("WF_Continue",workflow_cd)), continue := 1]
#------------------------------------------------- ----------
# round_par,workflow_cd,rule_content,rule_id and hc,ce,op,mo
# names(model_table)
# test <-
Model_table <- model_table[, lapply(.SD, sum),
                           By= .(appl_no,prod_cd,contra_no,mer_no,appl_tm,appl_lim,appl_loan_term,appl_status,biz_city_cd,
                                 Max_dpd,fpd,zhima_m,zhimamsxf_m,bt3_m,pbjxl_m,cashl_m,posjxl20_m,posjxl21_m,ins_m,jxl_m,cup_m),
                           .SDcols = c("hc","ce","model","continue","Blaze2Pass","EnterBlaze3")]
# a <- test[1,1][[1]]
# model_table[appl_no == a,.(round_par,workflow_cd,rule_content,rule_id,hc,ce,mo,co,Blaze2Pass,EnterBlaze3)]
#================================================================= ==========#
#=================== Data Preprocessing & Visualization ========================#
#================================================================= ==========#
# opscancel Operation Cancellation (0/1)
Model_table[, opscancel := 0]
Model_table[Blaze2Pass > 0 & EnterBlaze3 == 0, opscancel := 1]
# "hc","ce","model","continue" value specification is 0/1
Model_table[hc > 0, hc := 1]
Model_table[ce > 0, ce := 1]
Model_table[model > 0, model := 1]
Model_table[continue > 0, continue := 1]
# model_table[, .N , by = c("hc","ce","opscancel","model","continue")]
# path Strong refusal hc / letter review ce / operation cancel opscancel / model model / model operation continue
# Sequence is very important, hc must be placed last to cover
Model_table[,path := 'unknow']
Model_table[ce == 1,path := 'ce']
Model_table[opscancel == 1,path := 'opscancel']
Model_table[model == 1,path := 'model']
Model_table[continue == 1,path := 'continue']
Model_table[hc == 1,path := 'hc']
#------------------------------------------------- ----------
# appl_date Application date (type is Date)
# appl_month Application month (type is Character)
Model_table[,appl_date := as.Date(appl_tm)]
Model_table[,appl_month := format(appl_date, "%Y%m")]
#------------------------------------------------- ----------
#stre status Pass Status (by Pass/Cancel Cancel/Reject Reject/Unknown)
Model_table [,status:='unknow']
Model_table [appl_status =='R', status: = 'reject']
Model_table [appl_status =='N',status:='pass']
Model_table [appl_status =='C'| appl_status =='A'| appl_status =='J', status: = 'cancel']
#------------------------------------------------- ----------
#fpd10,maxdpd10(good/bad)
Model_table [,fpd:= as.numeric(fpd)]
Model_table [appl_status =='N',fpd10:='good']
Model_table [appl_status =='N'&fpd> 10,fpd10:='bad']
Model_table [,max_dpd:= as.numeric(max_dpd)]
Model_table [appl_status =='N',maxdpd10:='good']
Model_table [appl_status == 'N' and max_dpd> 10, maxdpd10:= 'bad']
#------------------------------------------------- ----------
#Model score missing value processing
Model_table [pbjxl_m == -9,pbjxl_m:= NA]
Model_table [pbjxl_m == -2,pbjxl_m:= NA]
Model_table [cashl_m == -2,cashl_m:= NA]
Model_table [cashl_m == -1,cashl_m:= NA]
Model_table [posjxl20_m == -1,posjxl20_m:= NA]
Model_table [posjxl21_m == -1,posjxl21_m:= NA]
Model_table [ins_m == -2,ins_m:= NA]
Model_table [ins_m == -1,ins_m:= NA]
Model_table [jxl_m == -1,jxl_m:= NA]
Model_table [cashl_m == 1,cashl_m:= NA]
Model_table [cup_m> 1, cup_m:= NA]
Model_table [zhima_m> 1000, zhima_m:= NA]
Model_table [zhima_m <100, zhima_m:= NA]
#zhimamsxf_m: No need to deal with
#------------------------------------------------- ----------
#path Unreasonable data adjustment
#----------
#model_table [,. N,by =. (status,path)]
#model_table [appl_date>'2018-01-01',. N , by =. (status,path)]
#----------
#18年, path is unknow and rejected, are rejected in blaze3, should be rejected by hc
Model_table [appl_date>'2018-01-01'&status =='reject'&path =='unknow',path:='hc']
#18 years ago, the path was unknow and was rejected, still unknow
#----------
#path is unknow and canceled, it should be canceled by the customer, add customercancel to path
Model_table [status =='cancel'&path =='unknow',path:='customercancel']
#----------
#path is hc and passed, all are fake hc, delete hc effect, divided into ce, mo, co
Model_table [status =='pass'&path =='hc'&ce == 1,path:='ce']
Model_table [status =='pass'&path =='hc'&model == 1,path:='model']
Model_table [status =='pass'&path =='hc'&continue == 1,path:='continue']
#----------
#The remaining unreasonable data are all classified as unknow, so as not to affect the analysis later.
Model_table [status =='cancel'&path =='hc',path:='unknow']
Model_table [status =='reject'&path =='continue',path:='unknow']
Model_table [status =='reject'&path =='opscancel',path:='unknow']
#----------
#model_table [,. N,by =. (status,path)]
#model_table [appl_date>'2018-01-01',. N , by =. (status,path)]
#path Unreasonable data adjustment completed
#-----------------------------------------------------------
# pathVisualization
Ggplot(data=model_table[,.N,by = .(path,appl_month)], aes(appl_month, N, fill=path))+geom_bar(stat='identity')
# ggplot(data=model_table[,.N,by = .(path,appl_date)], aes(appl_date, N, fill=path))+geom_bar(stat='identity')
#------------------------------------------------- ----------
#Model sub-missing value visualization
# ggplot(data=model_table[,.N,by = .(!is.na(bt3_m),appl_date)], aes(appl_date, N, fill=is.na))+geom_bar(stat='identity')
# ggplot(data=model_table[,.N,by = .(!is.na(pbjxl_m),appl_date)], aes(appl_date, N, fill=is.na))+geom_bar(stat='identity')
# ggplot(data=model_table[,.N,by = .(!is.na(cashl_m),appl_date)], aes(appl_date, N, fill=is.na))+geom_bar(stat='identity')
# ggplot(data=model_table[,.N,by = .(!is.na(ins_m),appl_date)], aes(appl_date, N, fill=is.na))+geom_bar(stat='identity')
# ggplot(data=model_table[,.N,by = .(!is.na(posjxl20_m),appl_date)], aes(appl_date, N, fill=is.na))+geom_bar(stat='identity')
# ggplot(data=model_table[,.N,by = .(!is.na(posjxl21_m),appl_date)], aes(appl_date, N, fill=is.na))+geom_bar(stat='identity')
# ggplot(data=model_table[,.N,by = .(!is.na(jxl_m),appl_date)], aes(appl_date, N, fill=is.na))+geom_bar(stat='identity')
# ggplot(data=model_table[,.N,by = .(!is.na(cup_m),appl_date)], aes(appl_date, N, fill=is.na))+geom_bar(stat='identity')
# ggplot(data=model_table[,.N,by = .(!is.na(zhima_m),appl_date)], aes(appl_date, N, fill=is.na))+geom_bar(stat='identity')
# ggplot(data=model_table[,.N,by = .(!is.na(zhimamsxf_m),appl_date)], aes(appl_date, N, fill=is.na))+geom_bar(stat='identity')
#------------------------------------------------- ----------
# "bt3_m","pbjxl_m","cashl_m","ins_m"
# "posjxl20_m","posjxl21_m","jxl_m","cup_m" Do not credit
# According to the model classification, 1, pbjxl_m, ins_m, posjxl21_m is not enough to distinguish PFD10, is eliminated
# cashl_m Although the performance is average, only cashl_m and bt3_m are left in the model to be credited, leaving for the time being.
# jxl_m need to use sesame seeds, no longer used
# The candidate model is as follows:
# "bt3_m","cashl_m","posjxl20_m", "cup_m" ,"Zhima_m"
#要信信 Want to collect letters Do not solicit letters Do not send letters for reference
# bt3_m, cup_m score is the smaller the better, so take the opposite
Model_table[, bt3_m := -bt3_m]
Model_table[, cup_m := -cup_m]
# *************Data backup point 1***********************
Save(model_table, file = 'model_table1.RData')
# ***********************************************
# If you have new data, run here.
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# #The following code is the development version
# #or Please use the model-score-filter-3_function-version 1.R
# #or Please use the model-score-filter-3_function-version 2.R
# #Update to 18/06/20
# # load('model_table1.RData')
# #================================================================ ===========#
# #=========== Calculate the new workflow and put it in new_table=================#
# #================================================================ ===========#
# # Calculate new workflows based on selected models and rules
# # New variable named newwf with values ​​'none', 'pass', 'continue', 'ce', 'reject'
# # para.
# models <- c('bt3_m','cup_m')
# pbocno <- 1
# nopbocno <- 2
# sdate <- '2018-01-22'
# nopbocmodel1_th <-c(75,40,5)
# pbocmodel1_th <-c(70,40,10)
# # Calculate a new workflow
# new_table <- model_table[appl_date > sdate,c(models,'prod_cd','appl_date','appl_lim','biz_city_cd','path','status','fpd10','maxdpd10'),with=F ]
# names(new_table)[[pbocno]] <- 'pbocmodel1'
# names(new_table)[[nopbocno]] <- 'nopbocmodel1'
# # View the singular classification by status, path
# # new_table[,.N,by=.(status,path)]
# # newwf='none', a list that won't change
# # unknow and customercancel are equivalent to missing values
# new_table[path == 'unknow' | path == 'customercancel', newwf := 'none']
# # hc and opscancel time before the model
# new_table[path == 'hc' | path == 'opscancel', newwf := 'none']
# # Theoretically, these cancel lists will also be affected, but for the convenience of calculation, they can be ignored and have little effect on the results.
# new_table[status == 'cancel', newwf := 'none']
# # View the singular classification by status, path
# # new_table[is.na(newwf),.N,by=.(status,path)]
# # The rest are all need to calculate the new workflow, the temporary assignment value newwf is 'blank'
# new_table[is.na(newwf),newwf := 'blank']
# # Get 100 aliquots of model scores
# pbocmodel1_cut <- quantile(new_table[,pbocmodel1],0:100/100,na.rm = T)
# nopbocmodel1_cut <- quantile(new_table[,nopbocmodel1],0:100/100,na.rm = T)
# # [1] is the minimum value, [101] is the maximum value, 100 equal parts, from small to large
# # Set threshold threshold
# #---------------
# #greater then pass
# pbocmodel1_thpass <- pbocmodel1_cut[pbocmodel1_th[1]][[1]]
# # ...and greater than this value continue
# pbocmodel1_thcontinue <- pbocmodel1_cut[pbocmodel1_th[2]][[1]]
# # ... and greater than this value ce
# pbocmodel1_threject <- pbocmodel1_cut[pbocmodel1_th[3]][[1]]
# #----------------
# #greater then pass
# nopbocmodel1_thpass <- nopbocmodel1_cut[nopbocmodel1_th[1]][[1]]
# # ...and greater than this value continue
# nopbocmodel1_thcontinue <- nopbocmodel1_cut[nopbocmodel1_th[2]][[1]]
# # ... and greater than this value ce
# nopbocmodel1_threject <- nopbocmodel1_cut[nopbocmodel1_th[3]][[1]]
# #---------------
# # Calculate the new workflow, the order can not be messed up
# # Rule: There is a credit pbocmodel1, no credit nopbocmodel1, no transfer ce
# new_table[newwf == 'blank'& is.na(pbocmodel1) & is.na(nopbocmodel1), newwf := 'ce']
# new_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 >= pbocmodel1_thpass, newwf := 'pass']
# new_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 >= pbocmodel1_thcontinue, newwf := 'continue']
# new_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 >= pbocmodel1_threject, newwf := 'ce']
# new_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 < pbocmodel1_threject, newwf := 'reject']
# new_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 >= nopbocmodel1_thpass, newwf := 'pass']
# new_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 >= nopbocmodel1_thcontinue, newwf := 'continue']
# new_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 >= nopbocmodel1_threject, newwf := 'ce']
# new_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 <  nopbocmodel1_threject, newwf := 'reject']
# # View newwf situation
# # new_table[,.N,by=newwf]
# #================================================================ ===========#
# #======================================================================================
# #================================================================ ===========#
# # Important estimate:
# # CE pass rate remains unchanged at 60%
# # continue Cancellation rate is about 5%
# # Specific CE pass rate and ops cancel rate based on actual time window historical data
# # CE pass rate = 60%
# # continue cancel rate=1%
# ce_ar_pred <- 0.6
# continue_ar_pred <- 0.99
# #Parameter: Estimate the ratio of pd10 of ce_rejectf is 2.5 times that of ce_pass
# cerejectpassfpd10 <- 2.5
# #Parameter: Predict the pd10 ratio of model_rejectf before the model_pass is 2.5 times
# oldmodelrejectpassfpd10 <- 2.5
# # function xy: transpose data.table
# xy <- function(dt) {
# t <- data.table(t(dt[,2:ncol(dt)]),keep.rownames = T)
# names(t) <- c(names(dt[,1]),c(dt[,1])[[1]])
# return(t)
# }
# #-------------------------------------#
# #---1.transfer: Workflow Transfer Form-----#
# #-------------------------------------#
# # cnt_transfer: Transfer of quantity
# cnt_transfer <- data.table(reshape2::acast(data = new_table[newwf != 'none',.N,by=.(path,status,newwf)],
# path + status ~ newwf, value.var = 'N'), keep.rownames = T)[order(rn)]
# # Missing value converted to 0
# cnt_transfer <- cnt_transfer[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
# # split ce for cepass and cereject
# cnt_transfer[,cepass := round(ce*ce_ar_pred)]
# cnt_transfer[rn == 'ce_pass',cepass := ce]
# cnt_transfer[rn == 'ce_reject',cepass := 0]
# cnt_transfer[,cereject := ce - cepass]
# cnt_transfer <- cnt_transfer[,.(rn,cepass,cereject,continue,pass,reject)]
# # Calculate the sum of each row/column
# cnt_transfer[,sum := cepass+cereject+continue+pass+reject]
# cnt_transfer <- xy(cnt_transfer)
# cnt_transfer[,sum := ce_pass+ce_reject+continue_pass+model_pass+model_reject]
# cnt_transfer <- xy(cnt_transfer)
# cnt_transfer
# # -----------------------
# # fpd_transfer: Transfer of fpd10
# # ce_reject, model_reject has no fpd10 performance, needs to be estimated, and the predicted value of fpd10 is transferred in fpd_transfer_predict
# fpd_transfer <- data.table(reshape2::acast(data = new_table[newwf != 'none' & fpd10 == 'bad',.N,by=.(path,status,newwf)],
# path + status ~ newwf, value.var = 'N'), keep.rownames = T)
# # Missing value converted to 0
# fpd_transfer <- fpd_transfer[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
# # Convert to double and add two行
# fpd_transfer <- cbind(fpd_transfer[,1],fpd_transfer[,2:5]*1.0)
# fpd_transfer <- rbind(fpd_transfer, data.table(rn=c('ce_reject','model_reject')), fill = T)[order(rn)]
# # let ce to be cepass or cereject
# fpd_transfer[rn == 'ce_pass',cepass := ce]
# fpd_transfer[rn == 'ce_pass',cereject := 0]
# fpd_transfer[rn == 'continue_pass',cepass := ce*(1/(1+2.5*(1-ce_ar_pred)/ce_ar_pred))]
# fpd_transfer[rn == 'continue_pass',cereject := ce - cepass]
# fpd_transfer[rn == 'model_pass',cepass := ce*(1/(1+2.5*(1-ce_ar_pred)/ce_ar_pred))]
# fpd_transfer[rn == 'model_pass',cereject := ce - cepass]
# fpd_transfer <- fpd_transfer[,.(rn,cepass,cereject,continue,pass,reject)]
# fpd_transfer[,sum := cepass+cereject+continue+pass+reject]
# fpd_transfer
# # -----------------------
# # fpd_transfer_pct: Transfer of fpd scale
# fpd_transfer_pct <- cbind(fpd_transfer[,1],fpd_transfer[,2:7]/cnt_transfer[1:5,2:7])
# fpd_transfer_pct[rn == 'ce_pass',cereject := 0]
# fpd_transfer_pct
# # -----------------------
# # Estimate fpd performance ---!!!!--- Estimated way is rough, pending improvement in the next version
# fpd_transfer_predict <- copy(fpd_transfer)
# # -----------------------
# fpd_transfer_predict[rn == 'ce_reject',cepass := 0]
# fpd_transfer_predict[rn == 'ce_reject',cereject := 0]
# fpd_transfer_predict[rn == 'ce_reject',reject := 0]
# fpd_transfer_predict[rn == 'model_reject',cereject := 0]
# fpd_transfer_predict[rn == 'model_reject',reject := 0]
# # -----------------------
# # rn=model_reject,newpath=ce, (It is assumed here that the proportion of fpd10 through ce is constant, and this assumption is basically true)
# fpd_transfer_predict[rn == 'model_reject',cepass := cnt_transfer[rn == 'model_reject',cepass]*fpd_transfer_pct[rn == 'ce_pass',sum]]
# # -----------------------
# # rn=ce_reject,newpath=continue
# fpd_transfer_predict[rn == 'ce_reject',continue := cnt_transfer[rn == 'ce_reject',continue]*fpd_transfer_pct[rn == 'ce_pass',sum]*cerejectpassfpd10]
# # rn=ce_reject,newpath=pass
# fpd_transfer_predict[rn == 'ce_reject',pass := cnt_transfer[rn == 'ce_reject',pass]*fpd_transfer_pct[rn == 'ce_pass',sum]*cerejectpassfpd10]
# # -----------------------
# # rn=model_reject,newpath=continue
# fpd_transfer_predict[rn == 'model_reject', continue := cnt_transfer[rn == 'model_reject', continueue]*fpd_transfer_pct[rn == 'model_pass',sum]*oldmodelrejectpassfpd10]
# # rn=model_reject,newpath=pass
# fpd_transfer_predict[rn == 'model_reject', pass := cnt_transfer[rn == 'model_reject',pass]*fpd_transfer_pct[rn == 'model_pass',sum]*oldmodelrejectpassfpd10]
# # -----------------------
# fpd_transfer_predict[, sum := cepass+cereject+continue+pass+reject]
# fpd_transfer_predict <- xy(fpd_transfer_predict)
# fpd_transfer_predict[,sum := ce_pass+ce_reject+continue_pass+model_pass+model_reject]
# fpd_transfer_predict <- xy(fpd_transfer_predict)
# fpd_transfer_predict
# # -----------------------
# # Calculate the transfer of fpd_transfer_pct_predict:fpd ratio based on estimated fpd performance (estimated)
# # 1.fpd_transfer_pct_predict_sum: The sum value contains all
# fpd_transfer_pct_predict_sum <- cbind(cnt_transfer[,1],fpd_transfer_predict[,2:7]/cnt_transfer[,2:7])
# fpd_transfer_pct_predict_sum
# # 2.fpd_transfer_pct_predict_passsum: The passsum value does not contain the rejected part
# cnt_transfer_passsum <- cnt_transfer[1:5,1:6]
# cnt_transfer_passsum[,passsum := cepass + continue +pass]
# cnt_transfer_passsum <- xy(cnt_transfer_passsum)
# cnt_transfer_passsum[,passsum := ce_pass+continue_pass+model_pass]
# cnt_transfer_passsum <- xy(cnt_transfer_passsum)
# # ---
# fpd_transfer_predict_passsum <- fpd_transfer_predict[1:5,1:6]
# fpd_transfer_predict_passsum[,passsum := cepass + continue +pass]
# fpd_transfer_predict_passsum <- xy(fpd_transfer_predict_passsum)
# fpd_transfer_predict_passsum[,passsum := ce_pass+continue_pass+model_pass]
# fpd_transfer_predict_passsum <- xy(fpd_transfer_predict_passsum)
# # ---
# fpd_transfer_pct_predict_passsum <- cbind(cnt_transfer_passsum[,1],fpd_transfer_predict_passsum[,2:7]/cnt_transfer_passsum[,2:7])
# fpd_transfer_pct_predict_passsum
# #|------------------------------------------ ------#
# #---2.change: The overall path number and proportion, pass rate, fpd10 change -----#
# #|------------------------------------------ ------#
# change_table <- new_table[,.(path,status,fpd10,maxdpd10,newwf)]
# # Add newpath
# change_table[newwf != 'none',newpath := newwf]
# change_table[newpath %in% c('pass','reject'),newpath := 'model']
# change_table[newwf == 'none',newpath := path]
# # change:Change table
# # path_change: New and old model path number and scale change table
#
# path_change <- cbind(change_table[,.N,by=path][order(path)],
# change_table[,.N,by=newpath][order(newpath)])[,c(1,2,4)]
# names(path_change) <- c('path','oldcnt','newcnt')
# path_change <- xy(path_change)
# path_change[,autonohc := continue+model]
# path_change[,auto := continue+model+hc]
# path_change <- xy(path_change)
# path_change[,changecnt := newcnt-oldcnt]
# total_count <- new_table[,.N]
# path_change[,oldpct := oldcnt/total_count]
# path_change[,newpct := newcnt/total_count]
# path_change[,changepct := newpct-oldpct]
# path_change
# # ar_change: New and old model pass rate change table
# ce_pass_old <- change_table[path == 'ce'][status == 'pass'][,.N]
# model_pass_old <- change_table[path == 'model'][status == 'pass'][,.N]
# continue_pass_old <- change_table[path == 'continue'][status == 'pass'][,.N]
# autonohc_pass_old <- model_pass_old + continue_pass_old
# total_pass_old <- autonohc_pass_old + ce_pass_old
# #------
# ce_old <- change_table[path == 'ce'][,.N]
# model_old <- change_table[path == 'model'][,.N]
# continue_old <- change_table[path == 'continue'][,.N]
# autonohc_old <- model_old + continue_old
# #------
# ce_ar_old <- ce_pass_old/ce_old
# model_ar_old <- model_pass_old/model_old
# continue_ar_old <- continue_pass_old/continue_old
# autonohc_ar_old <- autonohc_pass_old/autonohc_old
# total_ar_old <- total_pass_old/total_count
# #------
# # ce_pass_new is different from cnt_transfer in that cnt_transfer is consistent with the old ce passing state, which is calculated according to the ratio of ce_ar_pred
# ce_pass_new <- change_table[newpath == 'ce'][,.N]*ce_ar_pred
# model_pass_new <- change_table[newwf == 'pass'][,.N]
# continue_pass_new <- change_table[newpath == 'continue'][,.N]*continue_ar_pred
# autonohc_pass_new <- model_pass_new + continue_pass_new
# total_pass_new <- autonohc_pass_new + ce_pass_new
# #------
# ce_new <- change_table[newpath == 'ce'][,.N]
# model_new <- change_table[newpath == 'model'][,.N]
# continue_new <- change_table[newpath == 'continue'][,.N]
# autonohc_new <- model_new + continue_new
# #------
# ce_ar_new <- ce_pass_new/ce_new
# model_ar_new <- model_pass_new/model_new
# continue_ar_new <- continue_pass_new/continue_new
# autonohc_ar_new <- autonohc_pass_new/autonohc_new
# total_ar_new <- total_pass_new/total_count
# #------
# ar_change <- data.table(path = c('ce','model','continue','autonohc','total'),
# oldar = c(ce_ar_old, model_ar_old, continue_ar_old, autonohc_ar_old, total_ar_old),
# newar = c(ce_ar_new, model_ar_new, continue_ar_new, autonohc_ar_new, total_ar_new))
# ar_change
# # fpd10_change: New and old model fpd10 change table
# fpd10_old <- fpd_transfer_pct_predict_sum[rn %in% c('ce_pass','continue_pass','model_pass'),.(rn,sum)]
# fpd10_old_all <- fpd_transfer_predict[rn %in% c('ce_pass','continue_pass','model_pass'),sum(sum)]/
# cnt_transfer[rn %in% c('ce_pass','continue_pass','model_pass'),sum(sum)]
# fpd10_new <- xy(fpd_transfer_pct_predict_sum)[rn %in% c('cepass','continue','pass'),.(rn,sum)]
# fpd10_new_all <- xy(fpd_transfer_predict)[rn %in% c('cepass','continue','pass'),sum(sum)]/
# xy(cnt_transfer)[rn %in% c('cepass','continue','pass'),sum(sum)]
# fpd10_change <- cbind(fpd10_old,fpd10_new[,2])
# names(fpd10_change) <- c('path','fpd10_old','fpd10_new')
# fpd10_change <- rbind(fpd10_change,data.table(path='pass',fpd10_old=fpd10_old_all,fpd10_new=fpd10_new_all))
# #|------------------------------------------ ------#
# #--------3.result: The main results of the above two analysis ---------------#
# #|------------------------------------------ ------#
# result <- list(cnt_transfer,fpd_transfer_predict,fpd_transfer_pct_predict_sum,
# fpd_transfer_pct_predict_passsum, path_change, ar_change, fpd10_change)
# names(result) <- c('cnt_transfer','fpd_transfer_predict','fpd_transfer_pct_predict_sum',
# 'fpd_transfer_pct_predict_passsum', 'path_change', 'ar_change', 'fpd10_change')
# result