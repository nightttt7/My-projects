# Set working directory
Setwd('~/model sub-screening 3/show 2')
# required package: sltool (self-built), data.table, reshape2, ggplot2
Library(sltool)
Library(data.table)
Library(ggplot2)
# Import model_table exported from model partition filter 3_base (preprocessed)
Load('~/model sub-screening 3/model_table1.RData')
# candidate model is as follows
# bt3_m : To be credited, the main model
# cashl_m : To be credited, it can be used to reject
# cup_m : Do not credit, the main model
# posjxl20_m : Do not ask for credit, can be used to refuse
#================================================================= ==================================================
# time wondow start date (cashl_m access date)
Sdate <- '2018-01-22'
# Get new_table
New_table <- model_table[appl_date > sdate,.(bt3_m,cashl_m,cup_m,posjxl20_m,path,status,fpd10)]
# ----
# earlier time window
# sdate <- '2017-11-07'
# edate <- '2017-12-31'
# Get new_table
# new_table <- model_table[appl_date > sdate & appl_date <= edate ,.(bt3_m,cashl_m,cup_m,posjxl20_m,path,status,fpd10)]
# ----
# Delete irrelevant data
New_table <- new_table[path != 'unknow' & path != 'customercancel' & path != 'hc' &
                         Path != 'opscancel' & status != 'cancel']
# Get 100 equal parts of model score
Bt3_cut <- quantile(new_table[,bt3_m],0:100/100,na.rm = T)
Cashl_cut <- quantile(new_table[,cashl_m],0:100/100,na.rm = T)
Cup_cut <- quantile(new_table[,cup_m],0:100/100,na.rm = T)
Posjxl20_cut <- quantile(new_table[,posjxl20_m],0:100/100,na.rm = T)
#================================================================= ==================================================
# 1. Have a credit customer (bt3_m has value)
New_table1 <- new_table[!is.na(bt3_m)]
#====================================================
# Scheme 1, using only one model, the result see reject_result1
# ! Non-automated code, need manual operation!
# This line only runs once
Reject_result1 <- data.table()
# ------begin
# ------Adjust the last column of cut, choosen and reject_result1
Cut <- posjxl20_cut
New_table1[,chosen := posjxl20_m]
For (th in 2:50) {
  New_table1[,rej := 0]
  New_table1[!is.na(chosen) & chosen < cut[th],rej := 1]
  N_table <- new_table1[,.(.N,sum(rej)),by=.(status,fpd10)]
  N_table1 <- new_table1[,.(.N,sum(rej)),by=.(path,status,fpd10)]
  Names(N_table)[4] <- c('reject')
  Names(N_table1)[5] <- c('reject')
  # rejectfpd: The number of rejected fpd10
  Op0 <- N_table[fpd10 == 'bad',reject]
  # cerej2rej: the proportion of the original ce_reject rejected
  Op1 <- N_table1[path == 'ce' & status=='reject',reject/N]
  # pass2rej: the proportion of the original pass rejected
  Op2 <- N_table[status=='pass',sum(reject)/sum(N)]
  # fpd10den: The ratio of the original pass rejected fpd10 / the ratio of the original pass fpd10 (fpd10 multiple after the rejection, concentration)
  Op3 <- N_table[status=='pass' & fpd10 == 'bad',reject/N]/op2
  Reject_result1 <- rbind(reject_result1,data.table(op0,op1,op2,op3,'posjxl20_m'))
}
# ------end
Names(reject_result1) <- c('rejectfpd','cerej2rej','pass2rej','fpd10den','model')
P1.0 <- ggplot(data=reject_result1, aes(pass2rej, rejectfpd, color = model))+geom_line(stat='identity')
P1.1 <- ggplot(data=reject_result1, aes(pass2rej, fpd10den, color = model))+geom_line(stat='identity')
P1.2 <- ggplot(data=reject_result1, aes(pass2rej, cerej2rej, color = model))+geom_line(stat='identity')
Multiplot(p1.0, p1.1, p1.2)
# Note that the effect of bt3_m is disturbed because the original model is based on bt3_m, so the bt3_m model has not yet been selected.
# The data when it was officially launched got bt3_m_before for comparison
# It can be seen that in the ability to identify risky customers, bt3_m_before, cashl_m, cup_m is relatively strong, posjxl20_m is relatively poor
# Meanwhile, in rejecting the ability of CE as a risky customer, bt3_m_before, cashl_m is relatively strong, cup_m, posjxl20_m is relatively poor
# Do not recommend using the posjxl20_m model
# ---------------------------
# Scheme 2, using two + in bt3_m/cup_m/cashl_m (same satisfaction/any satisfaction), the result of filtering is shown in reject_result2
# ! Non-automated code, need manual operation!
reject_result2 <- data.table()
for (th1 in 2:15) {
  for (th2 in 2:15) {
    new_table1[,rej := 0]
    new_table1[bt3_m < bt3_cut[th1] | cashl_m < cashl_cut[th2],rej := 1]
    N_table <- new_table1[,.(.N,sum(rej)),by=.(status,fpd10)]
    N_table1 <- new_table1[,.(.N,sum(rej)),by=.(path,status,fpd10)]
    names(N_table)[4] <- c('reject')
    names(N_table1)[5] <- c('reject')
    # rejectfpd: The number of rejected fpd10
    Op0 <- N_table[fpd10 == 'bad',reject]
    # cerej2rej: the proportion of the original ce_reject rejected
    Op1 <- N_table1[path == 'ce' & status=='reject',reject/N]
    # pass2rej: the proportion of the original pass rejected
    Op2 <- N_table[status=='pass',sum(reject)/sum(N)]
    # fpd10den: The ratio of the original pass rejected fpd10 / the ratio of the original pass fpd10 (fpd10 multiple after the rejection, concentration)
    Op3 <- N_table[status=='pass' & fpd10 == 'bad',reject/N]/op2
    Reject_result2 <- rbind(reject_result2,data.table(th1,th2,op0,op1,op2,op3,'bt3_or_cashl'))
  }
}
Names(reject_result2) <- c('th1','th2','rejectfpd','cerej2rej','pass2rej','fpd10den','model')
P2.0 <- ggplot(data=reject_result2, aes(pass2rej, rejectfpd, color = model))+geom_line(stat='identity')
P2.1 <- ggplot(data=reject_result2, aes(pass2rej, fpd10den, color = model))+geom_line(stat='identity')
P2.2 <- ggplot(data=reject_result2, aes(pass2rej, cerej2rej, color = model))+geom_line(stat='identity')
Multiplot (p2.0, p2.1, p2.2)
# ----
# When the strategy is satisfied at the same time, the thresholds of the two models must be adjusted to more than 20% to have a pass2rej of about 5%, which is logically unsuitable.
# and fpd10den is not as good as scenario 1, so it does not consider the strategy of simultaneous satisfaction.
# Note that this result was disturbed by the bt3 model, and some applications with a low bt3_m have been rejected.
# However, this result is still informative. When the strategy is satisfied, the performance is obviously better, so the strategy that is satisfied at the same time is abandoned.
# cup_m+cashl_m performs well, but the strategy of bt3 is underestimated due to interference, and the performance of the US model has been excellent since its launch.
# Therefore do not consider using cup_m+cashl_m
# ----
# strategy is almost the same, using cup_m / cashl_m and bt3 match almost the same effect
# cup_m+cashl_m performs well, but the strategy of bt3 is underestimated due to interference, and the performance of the US model has been excellent since its launch.
# Therefore do not consider using cup_m+cashl_m
# ---------------------------
# Scheme 3, using bt3_m+cup_m+cashl_m (any one is satisfied), the results of the screening are shown in reject_result3
# ! Non-automated code, need manual operation!
Reject_result3 <- data.table()
For (th1 in 2:10) {
  For (th2 in 2:5) {
    For (th3 in 2:5) {
      New_table1[,rej := 0]
      New_table1[bt3_m < bt3_cut[th1] | cashl_m < cashl_cut[th2] | cup_m < cup_cut[th3],rej := 1]
      N_table <- new_table1[,.(.N,sum(rej)),by=.(status,fpd10)]
      N_table1 <- new_table1[,.(.N,sum(rej)),by=.(path,status,fpd10)]
      Names(N_table)[4] <- c('reject')
      Names(N_table1)[5] <- c('reject')
      # rejectfpd: The number of rejected fpd10
      Op0 <- N_table[fpd10 == 'bad',reject]
      # cerej2rej: the proportion of the original ce_reject rejected
      Op1 <- N_table1[path == 'ce' & status=='reject',reject/N]
      # pass2rej: the proportion of the original pass rejected
      Op2 <- N_table[status=='pass',sum(reject)/sum(N)]
      # fpd10den: The ratio of the original pass rejected fpd10 / the ratio of the original pass fpd10 (fpd10 multiple after the rejection, concentration)
      Op3 <- N_table[status=='pass' & fpd10 == 'bad',reject/N]/op2
      Reject_result3 <- rbind(reject_result3,data.table(th1,th2,th3,op0,op1,op2,op3,'bt3_or_cashl_or_cup'))
    }
  }
}
Names(reject_result3) <- c('th1','th2','th3','rejectfpd','cerej2rej','pass2rej','fpd10den','model')
P3.0 <- ggplot(data=reject_result3, aes(pass2rej, rejectfpd, color = model))+geom_line(stat='identity')
P3.1 <- ggplot(data=reject_result3, aes(pass2rej, fpd10den, color = model))+geom_line(stat='identity')
P3.2 <- ggplot(data=reject_result3, aes(pass2rej, cerej2rej, color = model))+geom_line(stat='identity')
Multiplot (p3.0, p3.1, p3.2)
# Performance is not as good as option 2

# Conclusion: Use bt3 with cup or cashl (any one is satisfied)

#====================================================
# 2. No credit customer (bt3_m no value)
New_table2 <- new_table[is.na(bt3_m)]
#====================================================
# Scheme 1, only use cup_m, the result see reject_result4
Reject_result4 <- data.table()
For (th1 in 2:30) {
  New_table2[,rej := 0]
  New_table2[!is.na(cup_m) & cup_m < cup_cut[th1],rej := 1]
  N_table <- new_table2[,.(.N,sum(rej)),by=.(status,fpd10)]
  N_table1 <- new_table2[,.(.N,sum(rej)),by=.(path,status,fpd10)]
  Names(N_table)[4] <- c('reject')
  Names(N_table1)[5] <- c('reject')
  # rejectfpd: The number of rejected fpd10
  Op0 <- N_table[fpd10 == 'bad',reject]
  # cerej2rej: the proportion of the original ce_reject rejected
  Op1 <- N_table1[path == 'ce' & status=='reject',reject/N]
  # pass2rej: the proportion of the original pass rejected
  Op2 <- N_table[status=='pass',sum(reject)/sum(N)]
  # fpd10den: The ratio of the original pass rejected fpd10 / the ratio of the original pass fpd10 (fpd10 multiple after the rejection, concentration)
  Op3 <- N_table[status=='pass' & fpd10 == 'bad',reject/N]/op2
  Reject_result4 <- rbind(reject_result4,data.table(th1,op0,op1,op2,op3,'cup'))
}
Names(reject_result4) <- c('th1','rejectfpd','cerej2rej','pass2rej','fpd10den','model')
P4.0 <- ggplot(data=reject_result4, aes(pass2rej, rejectfpd, color = model))+geom_line(stat='identity')
P4.1 <- ggplot(data=reject_result4, aes(pass2rej, fpd10den, color = model))+geom_line(stat='identity')
P4.2 <- ggplot(data=reject_result4, aes(pass2rej, cerej2rej, color = model))+geom_line(stat='identity')
Multiplot (p4.0, p4.1, p4.2)
# ------
# Scheme 2, use cup_m+posjxl20 (any one is satisfied), the result see reject_result5
Reject_result5 <- data.table()
For (th1 in 2:15) {
  For (th2 in 2:15) {
    New_table2[,rej := 0]
    New_table2[cup_m < cup_cut[th1] | posjxl20_m < posjxl20_cut[th2],rej := 1]
    N_table <- new_table2[,.(.N,sum(rej)),by=.(status,fpd10)]
    N_table1 <- new_table2[,.(.N,sum(rej)),by=.(path,status,fpd10)]
    Names(N_table)[4] <- c('reject')
    Names(N_table1)[5] <- c('reject')
    # rejectfpd: The number of rejected fpd10
    Op0 <- N_table[fpd10 == 'bad',reject]
    # cerej2rej: the proportion of the original ce_reject rejected
    Op1 <- N_table1[path == 'ce' & status=='reject',reject/N]
    # pass2rej: the proportion of the original pass rejected
    Op2 <- N_table[status=='pass',sum(reject)/sum(N)]
    # fpd10den: The ratio of the original pass rejected fpd10 / the ratio of the original pass fpd10 (fpd10 multiple after the rejection, concentration)
    Op3 <- N_table[status=='pass' & fpd10 == 'bad',reject/N]/op2
    Reject_result5 <- rbind(reject_result5,data.table(th1,th2,op0,op1,op2,op3,'bt3_or_cup'))
  }
}
Names(reject_result5) <- c('th1','th2','rejectfpd','cerej2rej','pass2rej','fpd10den','model')
P5.0 <- ggplot(data=reject_result5, aes(pass2rej, rejectfpd, color = model))+geom_line(stat='identity')
P5.1 <- ggplot(data=reject_result5, aes(pass2rej, fpd10den, color = model))+geom_line(stat='identity')
P5.2 <- ggplot(data=reject_result5, aes(pass2rej, cerej2rej, color = model))+geom_line(stat='identity')
Multiplot (p5.0, p5.1, p5.2)

# Conclusion: Option 2 performs well, it is recommended that white households use cup+posjxl (any one is satisfied)