# 设定工作目录
setwd('~/模型分筛选3/展示2')
# 需要的package:sltool(自建),data.table,reshape2,ggplot2
library(sltool)
library(data.table)
library(ggplot2)
# 导入从模型分筛选3_base中导出的model_table(经过预处理)
load('~/模型分筛选3/model_table1.RData')
# 待选模型如下
# bt3_m : 要征信,主模型
# cashl_m : 要征信,可用于拒绝
# cup_m : 不要征信,主模型
# posjxl20_m : 不要征信,可用于拒绝
#==========================================================================================
# 时间窗口开始日期(cashl_m接入日期)
sdate <- '2018-01-22'
# 获取new_table
new_table <- model_table[appl_date > sdate,.(bt3_m,cashl_m,cup_m,posjxl20_m,path,status,fpd10)]
# ----
# 较早的时间窗口
# sdate <- '2017-11-07'
# edate <- '2017-12-31'
# 获取new_table
# new_table <- model_table[appl_date > sdate & appl_date <= edate ,.(bt3_m,cashl_m,cup_m,posjxl20_m,path,status,fpd10)]
# ----
# 删除无关数据
new_table <- new_table[path != 'unknow' & path != 'customercancel' & path != 'hc' & 
                         path != 'opscancel' & status != 'cancel']
# 取得模型分数的100等分
bt3_cut <- quantile(new_table[,bt3_m],0:100/100,na.rm = T)
cashl_cut <- quantile(new_table[,cashl_m],0:100/100,na.rm = T)
cup_cut <- quantile(new_table[,cup_m],0:100/100,na.rm = T)
posjxl20_cut <- quantile(new_table[,posjxl20_m],0:100/100,na.rm = T)
#==========================================================================================
# 1.有征信客户(bt3_m有值)
new_table1 <- new_table[!is.na(bt3_m)]
#============================================
# 方案1,仅使用一个模型,结果见reject_result1
# !非自动化代码,需要手动操作!
# 这行仅运行一次
reject_result1 <- data.table()
# ------begin
# ------调整cut,chosen和reject_result1最后一列
cut <- posjxl20_cut
new_table1[,chosen := posjxl20_m]
for (th in 2:50) {
  new_table1[,rej := 0]
  new_table1[!is.na(chosen) & chosen < cut[th],rej := 1]
  N_table <- new_table1[,.(.N,sum(rej)),by=.(status,fpd10)]
  N_table1 <- new_table1[,.(.N,sum(rej)),by=.(path,status,fpd10)]
  names(N_table)[4] <- c('reject')
  names(N_table1)[5] <- c('reject')
  # rejectfpd:被拒绝的fpd10个数 
  op0 <- N_table[fpd10 == 'bad',reject]
  # cerej2rej:原有ce_reject被reject的比例
  op1 <- N_table1[path == 'ce' & status=='reject',reject/N]
  # pass2rej:原有pass被reject的比例
  op2 <- N_table[status=='pass',sum(reject)/sum(N)]
  # fpd10den:原有pass被reject的fpd10的比例/原有pass的fpd10的比例(被reject后的fpd10倍数,浓度)
  op3 <- N_table[status=='pass' & fpd10 == 'bad',reject/N]/op2
  reject_result1 <- rbind(reject_result1,data.table(op0,op1,op2,op3,'posjxl20_m'))
}
# ------end
names(reject_result1) <- c('rejectfpd','cerej2rej','pass2rej','fpd10den','model')
p1.0 <- ggplot(data=reject_result1, aes(pass2rej, rejectfpd, color = model))+geom_line(stat='identity')
p1.1 <- ggplot(data=reject_result1, aes(pass2rej, fpd10den, color = model))+geom_line(stat='identity')
p1.2 <- ggplot(data=reject_result1, aes(pass2rej, cerej2rej, color = model))+geom_line(stat='identity')
multiplot(p1.0,p1.1,p1.2)
# 注意,bt3_m的效果是受到了干扰的,因为原有模型就是以bt3_m为主的,因此选择bt3_m模型尚未
# 正式上线的时候的数据得出了bt3_m_before,用于比较
# 可以看出,在识别风险客户的能力上,bt3_m_before,cashl_m,cup_m相对较强,posjxl20_m相对较差
# 同时,在拒绝掉CE认为的风险客户的能力上,bt3_m_before,cashl_m相对较强,cup_m,posjxl20_m相对较差
# 不建议使用posjxl20_m模型
# ---------------------------
# 方案2,使用bt3_m/cup_m/cashl_m中的两个+(同时满足/任一满足),筛选出的结果见reject_result2
# !非自动化代码,需要手动操作!
reject_result2 <- data.table()
for (th1 in 2:15) {
  for (th2 in 2:15) {
    new_table1[,rej := 0]
    new_table1[bt3_m < bt3_cut[th1] | cashl_m < cashl_cut[th2],rej := 1]
    N_table <- new_table1[,.(.N,sum(rej)),by=.(status,fpd10)]
    N_table1 <- new_table1[,.(.N,sum(rej)),by=.(path,status,fpd10)]
    names(N_table)[4] <- c('reject')
    names(N_table1)[5] <- c('reject')
    # rejectfpd:被拒绝的fpd10个数 
    op0 <- N_table[fpd10 == 'bad',reject]
    # cerej2rej:原有ce_reject被reject的比例
    op1 <- N_table1[path == 'ce' & status=='reject',reject/N]
    # pass2rej:原有pass被reject的比例
    op2 <- N_table[status=='pass',sum(reject)/sum(N)]
    # fpd10den:原有pass被reject的fpd10的比例/原有pass的fpd10的比例(被reject后的fpd10倍数,浓度)
    op3 <- N_table[status=='pass' & fpd10 == 'bad',reject/N]/op2
    reject_result2 <- rbind(reject_result2,data.table(th1,th2,op0,op1,op2,op3,'bt3_or_cashl'))
  }
}
names(reject_result2) <- c('th1','th2','rejectfpd','cerej2rej','pass2rej','fpd10den','model')
p2.0 <- ggplot(data=reject_result2, aes(pass2rej, rejectfpd, color = model))+geom_line(stat='identity')
p2.1 <- ggplot(data=reject_result2, aes(pass2rej, fpd10den, color = model))+geom_line(stat='identity')
p2.2 <- ggplot(data=reject_result2, aes(pass2rej, cerej2rej, color = model))+geom_line(stat='identity')
multiplot(p2.0,p2.1,p2.2)
# ----
# 策略为同时满足时,两个模型的阈值都要调整到20%以上才能有5%左右的pass2rej,在逻辑上已经不适合了
# 并且fpd10den表现也不如方案1,所以不考虑同时满足的策略
# 注意,这个结果是被bt3模型 干扰了的,已经有一部分bt3_m低的申请被拒绝了
# 但这个结果依然有参考性, 策略为任一满足时,表现明显更优,因此同时满足的策略被放弃
# cup_m+cashl_m表现较好,但是有bt3的策略由于收到干扰是被低估了的,而美业模型自上线以来的表现比较优异
# 因此不考虑使用cup_m+cashl_m
# ----
# 策略为任一满足时,使用cup_m/cashl_m与bt3配合效果几乎一致
# cup_m+cashl_m表现较好,但是有bt3的策略由于收到干扰是被低估了的,而美业模型自上线以来的表现比较优异
# 因此不考虑使用cup_m+cashl_m
# ---------------------------
# 方案3,使用bt3_m+cup_m+cashl_m(任一满足),筛选出的结果见reject_result3
# !非自动化代码,需要手动操作!
reject_result3 <- data.table()
for (th1 in 2:10) {
  for (th2 in 2:5) {
    for (th3 in 2:5) {
      new_table1[,rej := 0]
      new_table1[bt3_m < bt3_cut[th1] | cashl_m < cashl_cut[th2] | cup_m < cup_cut[th3],rej := 1]
      N_table <- new_table1[,.(.N,sum(rej)),by=.(status,fpd10)]
      N_table1 <- new_table1[,.(.N,sum(rej)),by=.(path,status,fpd10)]
      names(N_table)[4] <- c('reject')
      names(N_table1)[5] <- c('reject')
      # rejectfpd:被拒绝的fpd10个数 
      op0 <- N_table[fpd10 == 'bad',reject]
      # cerej2rej:原有ce_reject被reject的比例
      op1 <- N_table1[path == 'ce' & status=='reject',reject/N]
      # pass2rej:原有pass被reject的比例
      op2 <- N_table[status=='pass',sum(reject)/sum(N)]
      # fpd10den:原有pass被reject的fpd10的比例/原有pass的fpd10的比例(被reject后的fpd10倍数,浓度)
      op3 <- N_table[status=='pass' & fpd10 == 'bad',reject/N]/op2
      reject_result3 <- rbind(reject_result3,data.table(th1,th2,th3,op0,op1,op2,op3,'bt3_or_cashl_or_cup'))
    }
  }
}
names(reject_result3) <- c('th1','th2','th3','rejectfpd','cerej2rej','pass2rej','fpd10den','model')
p3.0 <- ggplot(data=reject_result3, aes(pass2rej, rejectfpd, color = model))+geom_line(stat='identity')
p3.1 <- ggplot(data=reject_result3, aes(pass2rej, fpd10den, color = model))+geom_line(stat='identity')
p3.2 <- ggplot(data=reject_result3, aes(pass2rej, cerej2rej, color = model))+geom_line(stat='identity')
multiplot(p3.0,p3.1,p3.2)
# 表现不如方案2

# 结论:使用bt3与cup或cashl(任一满足) 效果较好

#============================================
# 2.无征信客户(bt3_m无值)
new_table2 <- new_table[is.na(bt3_m)]
#============================================
# 方案1,仅使用cup_m,结果见reject_result4
reject_result4 <- data.table()
for (th1 in 2:30) {
  new_table2[,rej := 0]
  new_table2[!is.na(cup_m) & cup_m < cup_cut[th1],rej := 1]
  N_table <- new_table2[,.(.N,sum(rej)),by=.(status,fpd10)]
  N_table1 <- new_table2[,.(.N,sum(rej)),by=.(path,status,fpd10)]
  names(N_table)[4] <- c('reject')
  names(N_table1)[5] <- c('reject')
  # rejectfpd:被拒绝的fpd10个数 
  op0 <- N_table[fpd10 == 'bad',reject]
  # cerej2rej:原有ce_reject被reject的比例
  op1 <- N_table1[path == 'ce' & status=='reject',reject/N]
  # pass2rej:原有pass被reject的比例
  op2 <- N_table[status=='pass',sum(reject)/sum(N)]
  # fpd10den:原有pass被reject的fpd10的比例/原有pass的fpd10的比例(被reject后的fpd10倍数,浓度)
  op3 <- N_table[status=='pass' & fpd10 == 'bad',reject/N]/op2
  reject_result4 <- rbind(reject_result4,data.table(th1,op0,op1,op2,op3,'cup'))
}
names(reject_result4) <- c('th1','rejectfpd','cerej2rej','pass2rej','fpd10den','model')
p4.0 <- ggplot(data=reject_result4, aes(pass2rej, rejectfpd, color = model))+geom_line(stat='identity')
p4.1 <- ggplot(data=reject_result4, aes(pass2rej, fpd10den, color = model))+geom_line(stat='identity')
p4.2 <- ggplot(data=reject_result4, aes(pass2rej, cerej2rej, color = model))+geom_line(stat='identity')
multiplot(p4.0,p4.1,p4.2)
# ------
# 方案2,使用cup_m+posjxl20(任一满足),结果见reject_result5
reject_result5 <- data.table()
for (th1 in 2:15) {
  for (th2 in 2:15) {
    new_table2[,rej := 0]
    new_table2[cup_m < cup_cut[th1] | posjxl20_m < posjxl20_cut[th2],rej := 1]
    N_table <- new_table2[,.(.N,sum(rej)),by=.(status,fpd10)]
    N_table1 <- new_table2[,.(.N,sum(rej)),by=.(path,status,fpd10)]
    names(N_table)[4] <- c('reject')
    names(N_table1)[5] <- c('reject')
    # rejectfpd:被拒绝的fpd10个数 
    op0 <- N_table[fpd10 == 'bad',reject]
    # cerej2rej:原有ce_reject被reject的比例
    op1 <- N_table1[path == 'ce' & status=='reject',reject/N]
    # pass2rej:原有pass被reject的比例
    op2 <- N_table[status=='pass',sum(reject)/sum(N)]
    # fpd10den:原有pass被reject的fpd10的比例/原有pass的fpd10的比例(被reject后的fpd10倍数,浓度)
    op3 <- N_table[status=='pass' & fpd10 == 'bad',reject/N]/op2
    reject_result5 <- rbind(reject_result5,data.table(th1,th2,op0,op1,op2,op3,'bt3_or_cup'))
  }
}
names(reject_result5) <- c('th1','th2','rejectfpd','cerej2rej','pass2rej','fpd10den','model')
p5.0 <- ggplot(data=reject_result5, aes(pass2rej, rejectfpd, color = model))+geom_line(stat='identity')
p5.1 <- ggplot(data=reject_result5, aes(pass2rej, fpd10den, color = model))+geom_line(stat='identity')
p5.2 <- ggplot(data=reject_result5, aes(pass2rej, cerej2rej, color = model))+geom_line(stat='identity')
multiplot(p5.0,p5.1,p5.2)

# 结论: 方案2表现较好,建议白户使用cup+posjxl(任一满足)
