#==========================================================================================
# 函数get_new_table3
# 参数:
# model_table:总表
# models:被选择的模型,默认是c('bt3_m','cup_m','cup_m','posjxl20_m'),函数中被改名为'model1','model2','model3','model4'
#        其中model1是有征信数据时主模型,model2是有征信数据时副模型,model3是无征信数据时主模型,model3是有无征信数据时副模型
# sdate,edate:时间窗口的开始和结束日期,默认是sdate='2018-01-22',edate='2018-04-16'
# th1-th4:不同模型的决策阈值,三个值分别是(超过此百分比则pass/continue/ce)(低于第三个百分比则reject),默认为c(70,40,10)
#         对于th4,三个值分别是(低于此百分比则ce/model3无值时reject/model3有值时reject)(高于第一个百分比则continue)
# passstyle:pass的不同风格,'值为both':model1pass且model2pass,'either':model1pass或model2pass,默认为'both'
# 新增列:
# haspboc:有无征信数据(通过model1(bt3)是否有值判断,如有必要可以更新判断方式),值为1(有),0(无)
# newwf:新的workflow,值为'none','pass','continue','ce','reject'
# key:用于决策的模型,值为'model1','model2','model3','model4','none',符合主模型的情况都算作主模型的
# 输出:
# new_table:按时间窗口和所需列从model_table中获取new_table,并添加新列
# 策略:
# 有征信用户(model1+model2),策略表如下
# |---------------------------------------------------------------|   
# |model1\2 | pass      | continue | ce      | reject | NA        |
# |---------------------------------------------------------------|  
# |pass     | pass      | continue | ce      | reject | pass      |    
# |continue | continue  | continue | ce      | reject | continue  |    
# |ce       | ce        | ce       | ce      | reject | ce        |
# |reject   | reject    | reject   | reject  | reject | reject    |    
# |---------------------------------------------------------------|    
# 无征信用户(model3+model4),策略表如下
# |-------------------------------|
# |model3\4 | notreject  | reject |
# |-------------------------------|
# |pass     | pass       | reject |
# |continue | continue   | reject |
# |ce       | ce         | reject |
# |reject   | reject     | reject |
# |NA       | ce/continue| reject |
# |-------------------------------|
# 无征信用户且model3无值:ce
# test---
#     setwd('~/模型分筛选3/展示2')
#     load('model_table1.RData')
#     models=c('bt3_m','cup_m','cup_m','posjxl20_m')
#     sdate='2018-01-22'
#     edate='2018-04-16'
#     passstyle='both'
#     th1=c(70,40,10)
#     th2=c(70,40,10)
#     th3=c(70,40,10)
#     th4=c(70,40,10)
# test---
# ----------------------------------------------------
get_new_table3 <- function(model_table,models=c('bt3_m','cup_m','cup_m','posjxl20_m'),
                           sdate='2018-01-22',edate='2018-04-16',passstyle='both',
                           th1=c(70,40,10),th2=c(70,40,10),th3=c(70,40,10),th4=c(70,40,10)){
  # 导入所需包:data.table
  library(data.table)
  # 按时间窗口和所需列从model_table中获取new_table
  new_table <- model_table[appl_date >= sdate & appl_date < edate,
                           c(models,'prod_cd','appl_date','appl_lim','biz_city_cd','path','status','fpd10','maxdpd10'),with=F]
  # 命名为'model1','model2','model3','model4',方便复用
  names(new_table)[1:4] <- c('model1','model2','model3','model4')
  # 通过bt3是否有值判断是否为有征信用户
  new_table[!is.na(model1), haspboc := 1]
  new_table[is.na(model1), haspboc := 0]
  # newwf='none', 不会发生变化的单子
  # unknow和customercancel相当于缺失值
  new_table[path == 'unknow' | path == 'customercancel', newwf := 'none']
  # hc和opscancel时间在模型之前
  new_table[path == 'hc' | path == 'opscancel', newwf := 'none']
  # 理论上这些cancel的单子也会收到影响,但为了方便计算,可以忽略,对结果几乎没影响
  new_table[status == 'cancel', newwf := 'none']
  # 剩余都是需要计算新的workflow的,暂赋值newwf为'blank'
  new_table[is.na(newwf),newwf := 'blank']
  # 取得模型分数的100等分
  # [1]是最小值,[101]是最大值,100等分,从小到大
  cut1 <- quantile(new_table[,model1],0:100/100,na.rm = T)
  cut2 <- quantile(new_table[,model2],0:100/100,na.rm = T)
  cut3 <- quantile(new_table[,model3],0:100/100,na.rm = T)
  cut4 <- quantile(new_table[,model4],0:100/100,na.rm = T)
  #---------------
  # 根据策略,模型分数,阈值(th1~th4),passstyle计算newwf和key,为减少代码量,此段处理较复杂
  new_table[newwf == 'blank' & haspboc, key := 'model1']
  new_table[key == 'model1' & model1 >= cut1[th1[1]][[1]], newwf := 'pass']
  new_table[key == 'model1' & model1 <  cut1[th1[1]][[1]], newwf := 'continue']
  new_table[key == 'model1' & model1 <  cut1[th1[2]][[1]], newwf := 'ce']
  new_table[key == 'model1' & model1 <  cut1[th1[3]][[1]], newwf := 'reject']
  #---
  new_table[key == 'model1' & newwf != 'reject' & model2 <  cut2[th2[3]][[1]], key := 'model2']
  new_table[key == 'model2', newwf := 'reject']
  new_table[key == 'model1' & newwf != 'reject' & newwf != 'ce' & model2 < cut2[th2[2]][[1]], key := 'model2']
  new_table[key == 'model2' & newwf != 'reject', newwf := 'ce']
  #---
  if (passstyle == 'both') {
    new_table[key == 'model1' & newwf == 'pass' & model2 < cut2[th2[1]][[1]], key := 'model2']
    new_table[key == 'model2' & newwf != 'reject' & newwf != 'ce', newwf := 'continue']
  }
  if (passstyle == 'either') {
    new_table[key == 'model1' & newwf == 'continue' & model2 >= cut2[th2[1]][[1]], key := 'model2']
    new_table[key == 'model2' & newwf != 'reject' & newwf != 'ce', newwf := 'pass']
  }
  #---
  new_table[newwf == 'blank' & !haspboc, key := 'model3']
  new_table[key == 'model3' & model3 >= cut3[th3[1]][[1]], newwf := 'pass']
  new_table[key == 'model3' & model3 <  cut3[th3[1]][[1]], newwf := 'continue']
  new_table[key == 'model3' & model3 <  cut3[th3[2]][[1]], newwf := 'ce']
  new_table[key == 'model3' & model3 <  cut3[th3[3]][[1]], newwf := 'reject']
  #---
  new_table[key == 'model3' & model4 <  cut4[th4[3]][[1]] & newwf != 'reject', key := 'model4']
  new_table[key == 'model4', newwf := 'reject']
  #---
  new_table[key == 'model3' & is.na(model3), key := 'model4']
  new_table[key == 'model4' & model4 <  cut4[th4[2]][[1]], newwf := 'reject']
  new_table[key == 'model4' & newwf != 'reject' & model4 <  cut4[th4[1]][[1]], newwf := 'ce']
  new_table[key == 'model4' & newwf != 'reject' & newwf != 'ce' & model4 >= cut4[th4[1]][[1]], newwf := 'continue']
  #---
  new_table[key == 'model4' & is.na(model4), key := 'none']
  new_table[key == 'none', newwf := 'ce']  
  #---
  return(new_table)
}
#==========================================================================================
# 函数get_result3:根据new_table输出各项指标,包含workflow的数量/通过率/fpd10转移表格,
# 整体path数量和比例,通过率,fpd10的变化表格,以及筛选出来的核心指标.
# 默认参数
# 重要估计:
# CE通过率保持60%不变
# continue取消率大约在5%
# 具体CE通过率和ops取消率根据实际时间窗口历史数据
# CE通过率=60%
# continue取消率=1%
# ce_ar_pred <- 0.6
# continue_ar_pred <- 0.99
# 参数:预估ce_rejectf的pd10比例是ce_pass的2.5倍
# cerejectpassfpd10 <- 2.5
# 参数:预估之前的model_rejectf的pd10比例是model_pass的2.5倍
# oldmodelrejectpassfpd10 <- 2.5
# test---
#     ce_ar_pred = 0.6
#     continue_ar_pred = 0.99
#     cerejectpassfpd10 = 2.5
#     oldmodelrejectpassfpd10 = 2.5
# test---
# ----------------------------------------------------
get_result3 <- function(new_table,ce_ar_pred = 0.6, continue_ar_pred = 0.99, 
                       cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5){
  library(data.table)
  # 函数xy:转置data.table
  xy <- function(dt) {
    t <- data.table(t(dt[,2:ncol(dt)]),keep.rownames = T)
    names(t) <- c(names(dt[,1]),c(dt[,1])[[1]])
    return(t)
  }
  #-------------------------------------#
  #---1.transfer:workflow的转移表格-----#
  #-------------------------------------#
  # cnt_transfer:数量的转移
  cnt_transfer <- data.table(reshape2::acast(data = new_table[newwf != 'none',.N,by=.(path,status,newwf)], 
                                             path + status ~ newwf, value.var = 'N'), keep.rownames = T)[order(rn)]
  # 缺失值转化为0
  cnt_transfer <- cnt_transfer[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # 拆分ce为cepass和cereject
  cnt_transfer[,cepass := round(ce*ce_ar_pred)]
  cnt_transfer[rn == 'ce_pass',cepass := ce]
  cnt_transfer[rn == 'ce_reject',cepass := 0]
  cnt_transfer[,cereject := ce - cepass]
  cnt_transfer <- cnt_transfer[,.(rn,cepass,cereject,continue,pass,reject)]
  # 计算每行/列的和
  cnt_transfer[,sum := cepass+cereject+continue+pass+reject]
  cnt_transfer <- xy(cnt_transfer)
  cnt_transfer[,sum := ce_pass+ce_reject+continue_pass+model_pass+model_reject]
  cnt_transfer <- xy(cnt_transfer)
  cnt_transfer
  # -----------------------
  # fpd_transfer:fpd10的转移 
  # ce_reject,model_reject无fpd10表现,需要预估,有预估值的fpd10的转移在fpd_transfer_predict呈现
  fpd_transfer <- data.table(reshape2::acast(data = new_table[newwf != 'none' & fpd10 == 'bad',.N,by=.(path,status,newwf)], 
                                             path + status ~ newwf, value.var = 'N'), keep.rownames = T)
  # 缺失值转化为0
  fpd_transfer <- fpd_transfer[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # 转化为double并添加两行
  fpd_transfer <- cbind(fpd_transfer[,1],fpd_transfer[,2:5]*1.0)
  fpd_transfer <- rbind(fpd_transfer, data.table(rn=c('ce_reject','model_reject')), fill = T)[order(rn)]
  # 拆分ce为cepass和cereject
  fpd_transfer[rn == 'ce_pass',cepass := ce]
  fpd_transfer[rn == 'ce_pass',cereject := 0]
  fpd_transfer[rn == 'continue_pass',cepass := ce*(1/(1+2.5*(1-ce_ar_pred)/ce_ar_pred))]
  fpd_transfer[rn == 'continue_pass',cereject := ce - cepass]
  fpd_transfer[rn == 'model_pass',cepass := ce*(1/(1+2.5*(1-ce_ar_pred)/ce_ar_pred))]
  fpd_transfer[rn == 'model_pass',cereject := ce - cepass]
  fpd_transfer <- fpd_transfer[,.(rn,cepass,cereject,continue,pass,reject)]
  fpd_transfer[,sum := cepass+cereject+continue+pass+reject]
  fpd_transfer
  # -----------------------
  # fpd_transfer_pct:fpd比例的转移
  fpd_transfer_pct <- cbind(fpd_transfer[,1],fpd_transfer[,2:7]/cnt_transfer[1:5,2:7])
  fpd_transfer_pct[rn == 'ce_pass',cereject := 0]
  fpd_transfer_pct
  # -----------------------
  # 预估fpd表现---!!!!---估计的方式较粗糙,有待在下一版改进
  fpd_transfer_predict <- copy(fpd_transfer)
  # -----------------------
  fpd_transfer_predict[rn == 'ce_reject',cepass := 0]
  fpd_transfer_predict[rn == 'ce_reject',cereject := 0]
  fpd_transfer_predict[rn == 'ce_reject',reject := 0]
  fpd_transfer_predict[rn == 'model_reject',cereject := 0]
  fpd_transfer_predict[rn == 'model_reject',reject := 0]
  # -----------------------
  # rn=model_reject,newpath=ce,(这里需要假设ce通过单子的fpd10比例是恒定的,而这个假设基本上是成立的)
  fpd_transfer_predict[rn == 'model_reject',cepass := cnt_transfer[rn == 'model_reject',cepass]*fpd_transfer_pct[rn == 'ce_pass',sum]]
  # -----------------------
  # rn=ce_reject,newpath=continue
  fpd_transfer_predict[rn == 'ce_reject',continue := cnt_transfer[rn == 'ce_reject',continue]*fpd_transfer_pct[rn == 'ce_pass',sum]*cerejectpassfpd10]
  # rn=ce_reject,newpath=pass
  fpd_transfer_predict[rn == 'ce_reject',pass := cnt_transfer[rn == 'ce_reject',pass]*fpd_transfer_pct[rn == 'ce_pass',sum]*cerejectpassfpd10]
  # -----------------------
  # rn=model_reject,newpath=continue
  fpd_transfer_predict[rn == 'model_reject', continue := cnt_transfer[rn == 'model_reject',continue]*fpd_transfer_pct[rn == 'model_pass',sum]*oldmodelrejectpassfpd10]
  # rn=model_reject,newpath=pass
  fpd_transfer_predict[rn == 'model_reject', pass := cnt_transfer[rn == 'model_reject',pass]*fpd_transfer_pct[rn == 'model_pass',sum]*oldmodelrejectpassfpd10]
  # -----------------------
  fpd_transfer_predict[, sum := cepass+cereject+continue+pass+reject]
  fpd_transfer_predict <- xy(fpd_transfer_predict)
  fpd_transfer_predict[,sum := ce_pass+ce_reject+continue_pass+model_pass+model_reject]
  fpd_transfer_predict <- xy(fpd_transfer_predict)
  fpd_transfer_predict
  # -----------------------
  # 根据预估fpd表现计算fpd_transfer_pct_predict:fpd比例的转移(预估)
  # 1.fpd_transfer_pct_predict_sum: sum值包含所有
  fpd_transfer_pct_predict_sum <- cbind(cnt_transfer[,1],fpd_transfer_predict[,2:7]/cnt_transfer[,2:7])
  fpd_transfer_pct_predict_sum
  # 2.fpd_transfer_pct_predict_passsum: passsum值不包含被拒绝的部分
  cnt_transfer_passsum <- cnt_transfer[1:5,1:6]
  cnt_transfer_passsum[,passsum  := cepass + continue +pass]
  cnt_transfer_passsum <- xy(cnt_transfer_passsum)
  cnt_transfer_passsum[,passsum := ce_pass+continue_pass+model_pass]
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
  #---2.change:整体path数量和比例,通过率,fpd10的变化-----#
  #------------------------------------------------------#
  change_table <- new_table[,.(path,status,fpd10,maxdpd10,newwf,key)]
  # 新增newpath
  change_table[newwf != 'none',newpath := newwf]
  change_table[newpath %in% c('pass','reject'),newpath := 'model']
  change_table[newwf == 'none',newpath := path]
  # change:变化表
  # path_change:新旧模型path数量及比例变化表
  
  path_change <- cbind(change_table[,.N,by=path][order(path)],
                       change_table[,.N,by=newpath][order(newpath)])[,c(1,2,4)]
  names(path_change) <- c('path','oldcnt','newcnt')
  path_change <- xy(path_change)
  path_change[,autonohc := continue+model+opscancel+unknow]
  path_change[,auto := continue+model+hc+opscancel+unknow]
  path_change <- xy(path_change)
  path_change[,changecnt := newcnt-oldcnt]
  total_count <- new_table[,.N]
  path_change[,oldpct := oldcnt/total_count]
  path_change[,newpct := newcnt/total_count]
  path_change[,changepct := newpct-oldpct]
  path_change
  # ar_change:新旧模型通过率变化表
  ce_pass_old       <-  change_table[path == 'ce'][status == 'pass'][,.N]
  model_pass_old    <-  change_table[path == 'model'][status == 'pass'][,.N]
  continue_pass_old <-  change_table[path == 'continue'][status == 'pass'][,.N]
  autonohc_pass_old <-  model_pass_old + continue_pass_old
  total_pass_old    <-  autonohc_pass_old + ce_pass_old
  #------
  ce_old            <-  change_table[path == 'ce'][,.N]
  model_old         <-  change_table[path == 'model'][,.N]
  continue_old      <-  change_table[path == 'continue'][,.N]
  autonohc_old      <-  model_old + continue_old
  #------
  ce_ar_old       <-  ce_pass_old/ce_old
  model_ar_old    <-  model_pass_old/model_old
  continue_ar_old <-  continue_pass_old/continue_old
  autonohc_ar_old <-  autonohc_pass_old/autonohc_old
  total_ar_old    <-  total_pass_old/total_count
  #------
  # ce_pass_new的口径与cnt_transfer中不同,cnt_transfer中按照新的ce通过状态与旧的一致,这里按ce_ar_pred根据比例计算
  ce_pass_new       <-  change_table[newpath == 'ce'][,.N]*ce_ar_pred
  model_pass_new    <-  change_table[newwf == 'pass'][,.N]
  continue_pass_new <-  change_table[newpath == 'continue'][,.N]*continue_ar_pred
  autonohc_pass_new <-  model_pass_new + continue_pass_new
  total_pass_new    <-  autonohc_pass_new + ce_pass_new
  #------
  ce_new            <-  change_table[newpath == 'ce'][,.N]
  model_new         <-  change_table[newpath == 'model'][,.N]
  continue_new      <-  change_table[newpath == 'continue'][,.N]
  autonohc_new      <-  model_new + continue_new
  #------
  ce_ar_new       <-  ce_pass_new/ce_new
  model_ar_new    <-  model_pass_new/model_new
  continue_ar_new <-  continue_pass_new/continue_new
  autonohc_ar_new <-  autonohc_pass_new/autonohc_new
  total_ar_new    <-  total_pass_new/total_count
  #------
  ar_change <- data.table(path = c('ce','model','continue','autonohc','total'),
                          oldar = c(ce_ar_old,model_ar_old,continue_ar_old,autonohc_ar_old,total_ar_old),
                          newar = c(ce_ar_new,model_ar_new,continue_ar_new,autonohc_ar_new,total_ar_new))
  ar_change
  # fpd10_change:新旧模型fpd10变化表
  fpd10_old <- fpd_transfer_pct_predict_sum[rn %in% c('ce_pass','continue_pass','model_pass'),.(rn,sum)]
  fpd10_old_all <- fpd_transfer_predict[rn %in% c('ce_pass','continue_pass','model_pass'),sum(sum)]/
    cnt_transfer[rn %in% c('ce_pass','continue_pass','model_pass'),sum(sum)]
  fpd10_new <- xy(fpd_transfer_pct_predict_sum)[rn %in% c('cepass','continue','pass'),.(rn,sum)]
  fpd10_new_all <- xy(fpd_transfer_predict)[rn %in% c('cepass','continue','pass'),sum(sum)]/
    xy(cnt_transfer)[rn %in% c('cepass','continue','pass'),sum(sum)]
  fpd10_change <- cbind(fpd10_old,fpd10_new[,2])
  names(fpd10_change) <- c('path','fpd10_old','fpd10_new')
  fpd10_change <- rbind(fpd10_change,data.table(path='pass',fpd10_old=fpd10_old_all,fpd10_new=fpd10_new_all))
  fpd10_change
  #------------------------------------------------------#
  #---3.model:各模型决策的数量和占比---------------------#
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
  #---4.result:以上三项分析的主要结果--------------------#
  #------------------------------------------------------#
  result <- list(cnt_transfer,fpd_transfer_predict,fpd_transfer_pct_predict_sum,
                 fpd_transfer_pct_predict_passsum,path_change,ar_change,fpd10_change,
                 model_cnt,model_fpd10_cnt,model_fpd10_pct)
  names(result) <- c('cnt_transfer','fpd_transfer_predict','fpd_transfer_pct_predict_sum',
                     'fpd_transfer_pct_predict_passsum','path_change','ar_change','fpd10_change',
                     'model_cnt','model_fpd10_cnt','model_fpd10_pct')
  return(result)
}
#==========================================================================================
# 函数get_result3_nopboc:get_result3的无征信数据版本,删掉了很多代码以能正常运行
# test---
#     ce_ar_pred = 0.6
#     continue_ar_pred = 0.99
#     cerejectpassfpd10 = 2.5
#     oldmodelrejectpassfpd10 = 2.5
#     new_table <- new_table[haspboc == 0]
# test---
# ----------------------------------------------------
get_result3_nopboc <- function(new_table,ce_ar_pred = 0.6, continue_ar_pred = 0.99, 
                        cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5){
  library(data.table)
  # 默认参数
  # 重要估计:
  # CE通过率保持60%不变
  # continue取消率大约在5%
  # 具体CE通过率和ops取消率根据实际时间窗口历史数据
  # CE通过率=60%
  # continue取消率=1%
  # ce_ar_pred <- 0.6
  # continue_ar_pred <- 0.99
  # 参数:预估ce_rejectf的pd10比例是ce_pass的2.5倍
  # cerejectpassfpd10 <- 2.5
  # 参数:预估之前的model_rejectf的pd10比例是model_pass的2.5倍
  # oldmodelrejectpassfpd10 <- 2.5
  # 函数xy:转置data.table
  xy <- function(dt) {
    t <- data.table(t(dt[,2:ncol(dt)]),keep.rownames = T)
    names(t) <- c(names(dt[,1]),c(dt[,1])[[1]])
    return(t)
  }
  #-------------------------------------#
  #---1.transfer:workflow的转移表格-----#
  #-------------------------------------#
  # cnt_transfer:数量的转移
  cnt_transfer <- data.table(reshape2::acast(data = new_table[newwf != 'none',.N,by=.(path,status,newwf)], 
                                             path + status ~ newwf, value.var = 'N'), keep.rownames = T)[order(rn)]
  # 缺失值转化为0
  cnt_transfer <- cnt_transfer[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # 拆分ce为cepass和cereject
  cnt_transfer[,cepass := round(ce*ce_ar_pred)]
  cnt_transfer[rn == 'ce_pass',cepass := ce]
  cnt_transfer[rn == 'ce_reject',cepass := 0]
  cnt_transfer[,cereject := ce - cepass]
  cnt_transfer <- cnt_transfer[,.(rn,cepass,cereject,continue,pass,reject)]
  # 计算每行/列的和
  cnt_transfer[,sum := cepass+cereject+continue+pass+reject]
  cnt_transfer <- xy(cnt_transfer)
  cnt_transfer[,sum := ce_pass+ce_reject+continue_pass]
  cnt_transfer <- xy(cnt_transfer)
  cnt_transfer
  # -----------------------
  # fpd_transfer:fpd10的转移 
  # ce_reject,model_reject无fpd10表现,需要预估,有预估值的fpd10的转移在fpd_transfer_predict呈现
  fpd_transfer <- data.table(reshape2::acast(data = new_table[newwf != 'none' & fpd10 == 'bad',.N,by=.(path,status,newwf)], 
                                             path + status ~ newwf, value.var = 'N'), keep.rownames = T)
  # 缺失值转化为0
  fpd_transfer <- fpd_transfer[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # 转化为double并添加两行
  fpd_transfer <- cbind(fpd_transfer[,1],fpd_transfer[,2:5]*1.0)
  fpd_transfer <- rbind(fpd_transfer, data.table(rn=c('ce_reject')), fill = T)[order(rn)]
  # 拆分ce为cepass和cereject
  fpd_transfer[rn == 'ce_pass',cepass := ce]
  fpd_transfer[rn == 'ce_pass',cereject := 0]
  fpd_transfer[rn == 'continue_pass',cepass := ce*(1/(1+2.5*(1-ce_ar_pred)/ce_ar_pred))]
  fpd_transfer[rn == 'continue_pass',cereject := ce - cepass]
  fpd_transfer <- fpd_transfer[,.(rn,cepass,cereject,continue,pass,reject)]
  fpd_transfer[,sum := cepass+cereject+continue+pass+reject]
  fpd_transfer
  # -----------------------
  # fpd_transfer_pct:fpd比例的转移
  fpd_transfer_pct <- cbind(fpd_transfer[,1],fpd_transfer[,2:7]/cnt_transfer[1:3,2:7])
  fpd_transfer_pct[rn == 'ce_pass',cereject := 0]
  fpd_transfer_pct
  # -----------------------
  # 预估fpd表现---!!!!---估计的方式较粗糙,有待在下一版改进
  fpd_transfer_predict <- copy(fpd_transfer)
  # -----------------------
  fpd_transfer_predict[rn == 'ce_reject',cepass := 0]
  fpd_transfer_predict[rn == 'ce_reject',cereject := 0]
  fpd_transfer_predict[rn == 'ce_reject',reject := 0]
  # -----------------------
  # rn=ce_reject,newpath=continue
  fpd_transfer_predict[rn == 'ce_reject',continue := cnt_transfer[rn == 'ce_reject',continue]*fpd_transfer_pct[rn == 'ce_pass',sum]*cerejectpassfpd10]
  # rn=ce_reject,newpath=pass
  fpd_transfer_predict[rn == 'ce_reject',pass := cnt_transfer[rn == 'ce_reject',pass]*fpd_transfer_pct[rn == 'ce_pass',sum]*cerejectpassfpd10]
  # -----------------------
  fpd_transfer_predict[, sum := cepass+cereject+continue+pass+reject]
  fpd_transfer_predict <- xy(fpd_transfer_predict)
  fpd_transfer_predict[,sum := ce_pass+ce_reject+continue_pass]
  fpd_transfer_predict <- xy(fpd_transfer_predict)
  fpd_transfer_predict
  # -----------------------
  # 根据预估fpd表现计算fpd_transfer_pct_predict:fpd比例的转移(预估)
  # 1.fpd_transfer_pct_predict_sum: sum值包含所有
  fpd_transfer_pct_predict_sum <- cbind(cnt_transfer[,1],fpd_transfer_predict[,2:7]/cnt_transfer[,2:7])
  fpd_transfer_pct_predict_sum
  # 2.fpd_transfer_pct_predict_passsum: passsum值不包含被拒绝的部分
  cnt_transfer_passsum <- cnt_transfer[1:3,1:6]
  cnt_transfer_passsum[,passsum  := cepass + continue +pass]
  cnt_transfer_passsum <- xy(cnt_transfer_passsum)
  cnt_transfer_passsum[,passsum := ce_pass+continue_pass]
  cnt_transfer_passsum <- xy(cnt_transfer_passsum)
  # ---
  fpd_transfer_predict_passsum <- fpd_transfer_predict[1:3,1:6]
  fpd_transfer_predict_passsum[,passsum  := cepass + continue +pass]
  fpd_transfer_predict_passsum <- xy(fpd_transfer_predict_passsum)
  fpd_transfer_predict_passsum[,passsum := ce_pass+continue_pass]
  fpd_transfer_predict_passsum <- xy(fpd_transfer_predict_passsum)
  # ---
  fpd_transfer_pct_predict_passsum <- cbind(cnt_transfer_passsum[,1],fpd_transfer_predict_passsum[,2:7]/cnt_transfer_passsum[,2:7])
  fpd_transfer_pct_predict_passsum
  #------------------------------------------------------#
  #---2.change:整体path数量和比例,通过率,fpd10的变化-----#
  #------------------------------------------------------#
  change_table <- new_table[,.(path,status,fpd10,maxdpd10,newwf,key)]
  # 新增newpath
  change_table[newwf != 'none',newpath := newwf]
  change_table[newpath %in% c('pass','reject'),newpath := 'model']
  change_table[newwf == 'none',newpath := path]
  # change:变化表
  # path_change:新旧模型path数量及比例变化表
  path_change <- cbind(rbind(change_table[,.N,by=path],data.table(path='model',N=0))[order(path)],
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
  # ar_change:新旧模型通过率变化表
  ce_pass_old       <-  change_table[path == 'ce'][status == 'pass'][,.N]
  model_pass_old    <-  change_table[path == 'model'][status == 'pass'][,.N]
  continue_pass_old <-  change_table[path == 'continue'][status == 'pass'][,.N]
  autonohc_pass_old <-  model_pass_old + continue_pass_old
  total_pass_old    <-  autonohc_pass_old + ce_pass_old
  #------
  ce_old            <-  change_table[path == 'ce'][,.N]
  model_old         <-  change_table[path == 'model'][,.N]
  continue_old      <-  change_table[path == 'continue'][,.N]
  autonohc_old      <-  model_old + continue_old
  #------
  ce_ar_old       <-  ce_pass_old/ce_old
  model_ar_old    <-  model_pass_old/model_old
  continue_ar_old <-  continue_pass_old/continue_old
  autonohc_ar_old <-  autonohc_pass_old/autonohc_old
  total_ar_old    <-  total_pass_old/total_count
  #------
  # ce_pass_new的口径与cnt_transfer中不同,cnt_transfer中按照新的ce通过状态与旧的一致,这里按ce_ar_pred根据比例计算
  ce_pass_new       <-  change_table[newpath == 'ce'][,.N]*ce_ar_pred
  model_pass_new    <-  change_table[newwf == 'pass'][,.N]
  continue_pass_new <-  change_table[newpath == 'continue'][,.N]*continue_ar_pred
  autonohc_pass_new <-  model_pass_new + continue_pass_new
  total_pass_new    <-  autonohc_pass_new + ce_pass_new
  #------
  ce_new            <-  change_table[newpath == 'ce'][,.N]
  model_new         <-  change_table[newpath == 'model'][,.N]
  continue_new      <-  change_table[newpath == 'continue'][,.N]
  autonohc_new      <-  model_new + continue_new
  #------
  ce_ar_new       <-  ce_pass_new/ce_new
  model_ar_new    <-  model_pass_new/model_new
  continue_ar_new <-  continue_pass_new/continue_new
  autonohc_ar_new <-  autonohc_pass_new/autonohc_new
  total_ar_new    <-  total_pass_new/total_count
  #------
  ar_change <- data.table(path = c('ce','model','continue','autonohc','total'),
                          oldar = c(ce_ar_old,model_ar_old,continue_ar_old,autonohc_ar_old,total_ar_old),
                          newar = c(ce_ar_new,model_ar_new,continue_ar_new,autonohc_ar_new,total_ar_new))
  ar_change
  # fpd10_change:新旧模型fpd10变化表
  fpd10_old <- fpd_transfer_pct_predict_sum[rn %in% c('ce_pass','continue_pass','model_pass'),.(rn,sum)]
  fpd10_old_all <- fpd_transfer_predict[rn %in% c('ce_pass','continue_pass','model_pass'),sum(sum)]/
    cnt_transfer[rn %in% c('ce_pass','continue_pass','model_pass'),sum(sum)]
  fpd10_new <- xy(fpd_transfer_pct_predict_sum)[rn %in% c('cepass','continue','pass'),.(rn,sum)]
  fpd10_new_all <- xy(fpd_transfer_predict)[rn %in% c('cepass','continue','pass'),sum(sum)]/
    xy(cnt_transfer)[rn %in% c('cepass','continue','pass'),sum(sum)]
  fpd10_change <- cbind(rbind(fpd10_old,data.table(rn='model_pass',sum=0)),fpd10_new[,2])
  names(fpd10_change) <- c('path','fpd10_old','fpd10_new')
  fpd10_change <- rbind(fpd10_change,data.table(path='pass',fpd10_old=fpd10_old_all,fpd10_new=fpd10_new_all))
  fpd10_change
  #------------------------------------------------------#
  #---3.model:各模型决策的数量和占比---------------------#
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
  #---4.result:以上三项分析的主要结果--------------------#
  #------------------------------------------------------#
  result <- list(cnt_transfer,fpd_transfer_predict,fpd_transfer_pct_predict_sum,
                 fpd_transfer_pct_predict_passsum,path_change,ar_change,fpd10_change,
                 model_cnt,model_fpd10_cnt,model_fpd10_pct)
  names(result) <- c('cnt_transfer','fpd_transfer_predict','fpd_transfer_pct_predict_sum',
                     'fpd_transfer_pct_predict_passsum','path_change','ar_change','fpd10_change',
                     'model_cnt','model_fpd10_cnt','model_fpd10_pct')
  return(result)
}
#==========================================================================================
# 函数get_core:从result中获取核心指标,放入一个单行data.table中
# 缩略词表
# 2:to
# rem:remain,保持率
# pct:占总申请量的百分比
# ch:change,变化率
# cr:ce_reject
# cp:ce_pass
# con:continue
# pc:model_pass or continue
# mp:model_pass
# mj:model_reject
# sub:subsidiary percent,附加模型占比
get_core <- function(result) {
  return(data.table(cr2mp = result[['cnt_transfer']][rn=='ce_reject',pass],
                    cr2pc = result[['cnt_transfer']][rn=='ce_reject',pass+continue],
                    cp_rem = result[['cnt_transfer']][rn=='ce_pass',cepass/sum],
                    con_rem = result[['cnt_transfer']][rn=='continue_pass',continue/sum],
                    mp_rem = result[['cnt_transfer']][rn=='model_pass',pass/sum],
                    ce_pct_ch = result[["path_change"]][path=='ce', changepct],
                    mocel_pct_ch = result[["path_change"]][path=='model', changepct],
                    model_ar = result[["ar_change"]][path == 'model',newar],
                    autonohc_ar = result[["ar_change"]][path == 'autonohc',newar],
                    total_ar = result[["ar_change"]][path == 'total',newar],
                    mp_fpd10 = result[["fpd10_change"]][path == 'model_pass',fpd10_new],
                    con_fpd10 = result[["fpd10_change"]][path == 'continue_pass',fpd10_new],
                    pass_fpd10 = result[["fpd10_change"]][path == 'pass',fpd10_new]
  ))}
#==========================================================================================
# 函数get_core1:get_core的无征信数据版本
get_core1 <- function(result) {
  return(data.table(cr2mp = result[['cnt_transfer']][rn=='ce_reject',pass],
                    cr2pc = result[['cnt_transfer']][rn=='ce_reject',pass+continue],
                    cp_rem = result[['cnt_transfer']][rn=='ce_pass',cepass/sum],
                    con_rem = result[['cnt_transfer']][rn=='continue_pass',continue/sum],
                    ce_pct_ch = result[["path_change"]][path=='ce', changepct],
                    mocel_pct_ch = result[["path_change"]][path=='model', changepct],
                    model_ar = result[["ar_change"]][path == 'model',newar],
                    autonohc_ar = result[["ar_change"]][path == 'autonohc',newar],
                    total_ar = result[["ar_change"]][path == 'total',newar],
                    mp_fpd10 = result[["fpd10_change"]][path == 'model_pass',fpd10_new],
                    con_fpd10 = result[["fpd10_change"]][path == 'continue_pass',fpd10_new],
                    pass_fpd10 = result[["fpd10_change"]][path == 'pass',fpd10_new]
  ))}
#==========================================================================================