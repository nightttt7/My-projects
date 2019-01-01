#===========================================================#
#=========设定工作目录&导入模块&取得数据====================#
#===========================================================#
# 设定工作目录
setwd('~/模型分筛选3')
# 需要的package:sltool(自建),data.table,reshape2,ggplot2
library(sltool)
library(data.table)
library(ggplot2)
# 运行并取数
model_table <- as.data.table(run.sql(exe = 1:4, get = 5, uid = "qingqi.shi", pwd = "sqq513"))
# 数据完整检查
model_table[,.N,by=bt3_m]
model_table[,.N,by=zhima_m]
model_table[,.N,by=zhimamsxf_m]
model_table[,.N,by=max_dpd]
model_table[,.N,by=fpd]
# 仅取数
# model_table <- run.sql(get = 5, uid = "qingqi.shi", pwd = "sqq513")
# *************数据备份点0*************
# save(model_table, file = 'model_table.RData')
# load('model_table.RData')
#===========================================================#
#=========变量appl_no不唯一,需要:===========================#
#=========压缩round_par,workflow_cd,rule_content,rule_id====#
#===========================================================#
# hc 强拒绝(0/1)
model_table[, hc := 0]
model_table[grepl('HC',rule_id) | rule_content == 'TempPatch_beauty_different_province', hc := 1]
#-----------------------------------------------------------
# ce 信审(0/1)
model_table[, ce := 0]
model_table[grepl("3",round_par) & grepl("WF_CECheck",workflow_cd), ce := 1]
#-----------------------------------------------------------
# Blaze2Pass,EnterBlaze3:用于计算opscancel:运营取消
model_table[, Blaze2Pass := 0]
model_table[grepl("2",round_par) & grepl("WF_Path",workflow_cd) & workflow_cd != "WF_PathGo", Blaze2Pass := 1]
model_table[, EnterBlaze3 := 0]
model_table[grepl("3",round_par), EnterBlaze3 := 1]
#-----------------------------------------------------------
# model 模型(0/1:reject/2:go)
model_table[, model := 0]
model_table[grepl('2',round_par) & grepl("WF_Reject",workflow_cd) & grepl("SLBE",rule_id) & hc == 0, model := 1]
model_table[(grepl("2",round_par)==1 & grepl("WF_PathGo",workflow_cd)), model := 2]
# continue 模型运营(0/1:模型+运营continue通过)
model_table[, continue := 0]
model_table[(grepl("3",round_par) &  grepl("WF_Continue",workflow_cd)), continue := 1]
#-----------------------------------------------------------
# 压缩round_par,workflow_cd,rule_content,rule_id以及hc,ce,op,mo
# names(model_table)
# test <- 
model_table <- model_table[, lapply(.SD, sum), 
                           by= .(appl_no,prod_cd,contra_no,mer_no,appl_tm,appl_lim,appl_loan_term,appl_status,biz_city_cd,
                                 max_dpd,fpd,zhima_m,zhimamsxf_m,bt3_m,pbjxl_m,cashl_m,posjxl20_m,posjxl21_m,ins_m,jxl_m,cup_m),
                           .SDcols = c("hc","ce","model","continue","Blaze2Pass","EnterBlaze3")] 
# a <- test[1,1][[1]]
# model_table[appl_no == a,.(round_par,workflow_cd,rule_content,rule_id,hc,ce,mo,co,Blaze2Pass,EnterBlaze3)]
#===========================================================#
#===================数据预处理&可视化=======================#
#===========================================================#
# opscancel 运营取消(0/1)
model_table[, opscancel := 0]
model_table[Blaze2Pass > 0 & EnterBlaze3 == 0, opscancel := 1]
# "hc","ce","model","continue"值规范为0/1
model_table[hc > 0, hc := 1]
model_table[ce > 0, ce := 1]
model_table[model > 0, model := 1]
model_table[continue > 0, continue := 1]
# model_table[, .N ,by = c("hc","ce","opscancel","model","continue")]
# path 强拒绝hc/信审ce/运营取消opscancel/模型model/模型运营continue
# 顺序很重要,hc必须放最后去覆盖
model_table[,path := 'unknow']
model_table[ce == 1,path := 'ce']
model_table[opscancel == 1,path := 'opscancel']
model_table[model == 1,path := 'model']
model_table[continue == 1,path := 'continue']
model_table[hc == 1,path := 'hc']
#-----------------------------------------------------------
# appl_date 申请日期(类型为Date)
# appl_month 申请月份(类型为Character)
model_table[,appl_date := as.Date(appl_tm)]
model_table[,appl_month := format(appl_date, "%Y%m")]
#-----------------------------------------------------------
# status 通过状态(通过pass/取消cancel/拒绝reject/unknow)
model_table[,status := 'unknow']
model_table[appl_status == 'R' , status := 'reject']
model_table[appl_status == 'N' , status := 'pass']
model_table[appl_status == 'C' | appl_status == 'A' | appl_status == 'J', status := 'cancel']
#-----------------------------------------------------------
# fpd10,maxdpd10(good/bad)
model_table[,fpd := as.numeric(fpd)]
model_table[appl_status == 'N',fpd10 := 'good']
model_table[appl_status == 'N' & fpd > 10, fpd10 := 'bad']
model_table[,max_dpd := as.numeric(max_dpd)]
model_table[appl_status == 'N',maxdpd10 := 'good']
model_table[appl_status == 'N' & max_dpd > 10, maxdpd10 := 'bad']
#-----------------------------------------------------------
# 模型分数缺失值处理
model_table[pbjxl_m == -9,pbjxl_m := NA]
model_table[pbjxl_m == -2,pbjxl_m := NA]
model_table[cashl_m == -2,cashl_m := NA]
model_table[cashl_m == -1,cashl_m := NA]
model_table[posjxl20_m == -1,posjxl20_m := NA]
model_table[posjxl21_m == -1,posjxl21_m := NA]
model_table[ins_m == -2,ins_m := NA]
model_table[ins_m == -1,ins_m := NA]
model_table[jxl_m == -1,jxl_m := NA]
model_table[cashl_m == 1,cashl_m := NA]
model_table[cup_m > 1,cup_m := NA]
model_table[zhima_m > 1000,zhima_m := NA]
model_table[zhima_m < 100,zhima_m := NA]
# zhimamsxf_m:无需处理
#-----------------------------------------------------------
# path不合理数据调整
#----------
# model_table[,.N,by = .(status,path)]
# model_table[appl_date > '2018-01-01',.N,by = .(status,path)]
#----------
# 18年以来,path为unknow且被拒绝的,都是在blaze3被拒绝的,应该都是hc拒绝
model_table[appl_date > '2018-01-01' & status == 'reject' & path == 'unknow', path := 'hc']
# 18年以前 ,path为unknow且被拒绝的,依然unknow
#----------
# path为unknow且被取消的,应该都是客户自己取消的,新增customercancel到path
model_table[status == 'cancel' & path == 'unknow', path := 'customercancel']
#----------
# path为hc且通过的,都是假hc,删除hc影响,分为ce,mo,co
model_table[status == 'pass' & path == 'hc' & ce == 1, path := 'ce']
model_table[status == 'pass' & path == 'hc' & model == 1, path := 'model']
model_table[status == 'pass' & path == 'hc' & continue == 1, path := 'continue']
#----------
# 剩余的不合理数据全部划为unknow,以免影响后面的分析 
model_table[status == 'cancel' & path == 'hc', path := 'unknow']
model_table[status == 'reject' & path == 'continue', path := 'unknow']
model_table[status == 'reject' & path == 'opscancel', path := 'unknow']
#----------
# model_table[,.N,by = .(status,path)]
# model_table[appl_date > '2018-01-01',.N,by = .(status,path)]
# path不合理数据调整完成
#-----------------------------------------------------------
# path可视化
ggplot(data=model_table[,.N,by = .(path,appl_month)], aes(appl_month, N, fill=path))+geom_bar(stat='identity')
# ggplot(data=model_table[,.N,by = .(path,appl_date)], aes(appl_date, N, fill=path))+geom_bar(stat='identity')
#-----------------------------------------------------------
# 模型分缺失值可视化
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
#-----------------------------------------------------------
# "bt3_m","pbjxl_m","cashl_m","ins_m" 要征信
# "posjxl20_m","posjxl21_m","jxl_m","cup_m" 不要征信
# 根据模型分筛选1,pbjxl_m,ins_m,posjxl21_m对PFD10的区分度不够,被淘汰
# cashl_m虽然表现一般,但是要征信的模型中只剩cashl_m和bt3_m,暂时留下
# jxl_m需要使用芝麻分,不再使用
# 待选模型如下:
# "bt3_m","cashl_m","posjxl20_m",  "cup_m"   ,"Zhima_m"
#  要征信  要征信    不要征信       不要征信   供参考
# bt3_m,cup_m分数是越小越优质,故将其取相反数
model_table[, bt3_m := -bt3_m]
model_table[, cup_m := -cup_m]
# *************数据备份点1***********************
save(model_table, file = 'model_table1.RData')
# ***********************************************
# 如有新数据,运行到此即可 
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
# #以下代码为开发版
# #正式版请使用模型分筛选3_函数版1.R
# #正式版请使用模型分筛选3_函数版2.R
# #开发版更新至18/06/20
# # load('model_table1.RData')
# #===========================================================#
# #===========计算新的workflow并放入new_table=================#
# #===========================================================#
# # 根据选择的模型(models)和规则计算新的workflow
# # 新变量名为newwf,值为'none','pass','continue','ce','reject'
# # 参数
# models <- c('bt3_m','cup_m')
# pbocno <- 1
# nopbocno <- 2
# sdate <- '2018-01-22'
# nopbocmodel1_th <-c(75,40,5)
# pbocmodel1_th <-c(70,40,10)
# # 计算新的workflow
# new_table <- model_table[appl_date > sdate,c(models,'prod_cd','appl_date','appl_lim','biz_city_cd','path','status','fpd10','maxdpd10'),with=F]
# names(new_table)[[pbocno]] <- 'pbocmodel1'
# names(new_table)[[nopbocno]] <- 'nopbocmodel1'
# # 查看按status,path分类的单数
# # new_table[,.N,by=.(status,path)]
# # newwf='none', 不会发生变化的单子
# # unknow和customercancel相当于缺失值
# new_table[path == 'unknow' | path == 'customercancel', newwf := 'none']
# # hc和opscancel时间在模型之前
# new_table[path == 'hc' | path == 'opscancel', newwf := 'none']
# # 理论上这些cancel的单子也会收到影响,但为了方便计算,可以忽略,对结果几乎没影响
# new_table[status == 'cancel', newwf := 'none']
# # 查看按status,path分类的单数
# # new_table[is.na(newwf),.N,by=.(status,path)]
# # 剩余都是需要计算新的workflow的,暂赋值newwf为'blank'
# new_table[is.na(newwf),newwf := 'blank']
# # 取得模型分数的100等分
# pbocmodel1_cut <- quantile(new_table[,pbocmodel1],0:100/100,na.rm = T)
# nopbocmodel1_cut <- quantile(new_table[,nopbocmodel1],0:100/100,na.rm = T)
# # [1]是最小值,[101]是最大值,100等分,从小到大
# # 设置阈值threshold
# #---------------
# # 大于此值通过
# pbocmodel1_thpass <- pbocmodel1_cut[pbocmodel1_th[1]][[1]]
# # ...且大于此值continue
# pbocmodel1_thcontinue <- pbocmodel1_cut[pbocmodel1_th[2]][[1]]
# # ...且大于此值ce
# pbocmodel1_threject <- pbocmodel1_cut[pbocmodel1_th[3]][[1]]
# #----------------
# # 大于此值通过
# nopbocmodel1_thpass <- nopbocmodel1_cut[nopbocmodel1_th[1]][[1]]
# # ...且大于此值continue
# nopbocmodel1_thcontinue <- nopbocmodel1_cut[nopbocmodel1_th[2]][[1]]
# # ...且大于此值ce
# nopbocmodel1_threject <- nopbocmodel1_cut[nopbocmodel1_th[3]][[1]]
# #---------------
# # 计算新的workflow,顺序不能乱
# # 规则为:有征信用pbocmodel1,无征信用nopbocmodel1,都无转ce
# new_table[newwf == 'blank' & is.na(pbocmodel1) & is.na(nopbocmodel1), newwf := 'ce']
# new_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 >= pbocmodel1_thpass, newwf := 'pass']
# new_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 >= pbocmodel1_thcontinue, newwf := 'continue']
# new_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 >= pbocmodel1_threject, newwf := 'ce']
# new_table[newwf == 'blank' & !is.na(pbocmodel1) & pbocmodel1 < pbocmodel1_threject, newwf := 'reject']
# new_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 >= nopbocmodel1_thpass, newwf := 'pass']
# new_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 >= nopbocmodel1_thcontinue, newwf := 'continue']
# new_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 >= nopbocmodel1_threject, newwf := 'ce']
# new_table[newwf == 'blank' & !is.na(nopbocmodel1) & nopbocmodel1 <  nopbocmodel1_threject, newwf := 'reject']
# # 查看newwf情况
# # new_table[,.N,by=newwf]
# #===========================================================#
# #=================新workflow变化情况与结果 =================#
# #===========================================================#
# # 重要估计:
# # CE通过率保持60%不变
# # continue取消率大约在5%
# # 具体CE通过率和ops取消率根据实际时间窗口历史数据
# # CE通过率=60%
# # continue取消率=1%
# ce_ar_pred <- 0.6
# continue_ar_pred <- 0.99
# # 参数:预估ce_rejectf的pd10比例是ce_pass的2.5倍
# cerejectpassfpd10 <- 2.5
# # 参数:预估之前的model_rejectf的pd10比例是model_pass的2.5倍
# oldmodelrejectpassfpd10 <- 2.5
# # 函数xy:转置data.table
# xy <- function(dt) {
#   t <- data.table(t(dt[,2:ncol(dt)]),keep.rownames = T)
#   names(t) <- c(names(dt[,1]),c(dt[,1])[[1]])
#   return(t)
# }
# #-------------------------------------#
# #---1.transfer:workflow的转移表格-----#
# #-------------------------------------#
# # cnt_transfer:数量的转移
# cnt_transfer <- data.table(reshape2::acast(data = new_table[newwf != 'none',.N,by=.(path,status,newwf)], 
#                                       path + status ~ newwf, value.var = 'N'), keep.rownames = T)[order(rn)]
# # 缺失值转化为0
# cnt_transfer <- cnt_transfer[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
# # 拆分ce为cepass和cereject
# cnt_transfer[,cepass := round(ce*ce_ar_pred)]
# cnt_transfer[rn == 'ce_pass',cepass := ce]
# cnt_transfer[rn == 'ce_reject',cepass := 0]
# cnt_transfer[,cereject := ce - cepass]
# cnt_transfer <- cnt_transfer[,.(rn,cepass,cereject,continue,pass,reject)]
# # 计算每行/列的和
# cnt_transfer[,sum := cepass+cereject+continue+pass+reject]
# cnt_transfer <- xy(cnt_transfer)
# cnt_transfer[,sum := ce_pass+ce_reject+continue_pass+model_pass+model_reject]
# cnt_transfer <- xy(cnt_transfer)
# cnt_transfer
# # -----------------------
# # fpd_transfer:fpd10的转移 
# # ce_reject,model_reject无fpd10表现,需要预估,有预估值的fpd10的转移在fpd_transfer_predict呈现
# fpd_transfer <- data.table(reshape2::acast(data = new_table[newwf != 'none' & fpd10 == 'bad',.N,by=.(path,status,newwf)], 
#                                            path + status ~ newwf, value.var = 'N'), keep.rownames = T)
# # 缺失值转化为0
# fpd_transfer <- fpd_transfer[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
# # 转化为double并添加两行
# fpd_transfer <- cbind(fpd_transfer[,1],fpd_transfer[,2:5]*1.0)
# fpd_transfer <- rbind(fpd_transfer, data.table(rn=c('ce_reject','model_reject')), fill = T)[order(rn)]
# # 拆分ce为cepass和cereject
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
# # fpd_transfer_pct:fpd比例的转移
# fpd_transfer_pct <- cbind(fpd_transfer[,1],fpd_transfer[,2:7]/cnt_transfer[1:5,2:7])
# fpd_transfer_pct[rn == 'ce_pass',cereject := 0]
# fpd_transfer_pct
# # -----------------------
# # 预估fpd表现---!!!!---估计的方式较粗糙,有待在下一版改进
# fpd_transfer_predict <- copy(fpd_transfer)
# # -----------------------
# fpd_transfer_predict[rn == 'ce_reject',cepass := 0]
# fpd_transfer_predict[rn == 'ce_reject',cereject := 0]
# fpd_transfer_predict[rn == 'ce_reject',reject := 0]
# fpd_transfer_predict[rn == 'model_reject',cereject := 0]
# fpd_transfer_predict[rn == 'model_reject',reject := 0]
# # -----------------------
# # rn=model_reject,newpath=ce,(这里需要假设ce通过单子的fpd10比例是恒定的,而这个假设基本上是成立的)
# fpd_transfer_predict[rn == 'model_reject',cepass := cnt_transfer[rn == 'model_reject',cepass]*fpd_transfer_pct[rn == 'ce_pass',sum]]
# # -----------------------
# # rn=ce_reject,newpath=continue
# fpd_transfer_predict[rn == 'ce_reject',continue := cnt_transfer[rn == 'ce_reject',continue]*fpd_transfer_pct[rn == 'ce_pass',sum]*cerejectpassfpd10]
# # rn=ce_reject,newpath=pass
# fpd_transfer_predict[rn == 'ce_reject',pass := cnt_transfer[rn == 'ce_reject',pass]*fpd_transfer_pct[rn == 'ce_pass',sum]*cerejectpassfpd10]
# # -----------------------
# # rn=model_reject,newpath=continue
# fpd_transfer_predict[rn == 'model_reject', continue := cnt_transfer[rn == 'model_reject',continue]*fpd_transfer_pct[rn == 'model_pass',sum]*oldmodelrejectpassfpd10]
# # rn=model_reject,newpath=pass
# fpd_transfer_predict[rn == 'model_reject', pass := cnt_transfer[rn == 'model_reject',pass]*fpd_transfer_pct[rn == 'model_pass',sum]*oldmodelrejectpassfpd10]
# # -----------------------
# fpd_transfer_predict[, sum := cepass+cereject+continue+pass+reject]
# fpd_transfer_predict <- xy(fpd_transfer_predict)
# fpd_transfer_predict[,sum := ce_pass+ce_reject+continue_pass+model_pass+model_reject]
# fpd_transfer_predict <- xy(fpd_transfer_predict)
# fpd_transfer_predict
# # -----------------------
# # 根据预估fpd表现计算fpd_transfer_pct_predict:fpd比例的转移(预估)
# # 1.fpd_transfer_pct_predict_sum: sum值包含所有
# fpd_transfer_pct_predict_sum <- cbind(cnt_transfer[,1],fpd_transfer_predict[,2:7]/cnt_transfer[,2:7])
# fpd_transfer_pct_predict_sum
# # 2.fpd_transfer_pct_predict_passsum: passsum值不包含被拒绝的部分
# cnt_transfer_passsum <- cnt_transfer[1:5,1:6]
# cnt_transfer_passsum[,passsum  := cepass + continue +pass]
# cnt_transfer_passsum <- xy(cnt_transfer_passsum)
# cnt_transfer_passsum[,passsum := ce_pass+continue_pass+model_pass]
# cnt_transfer_passsum <- xy(cnt_transfer_passsum)
# # ---
# fpd_transfer_predict_passsum <- fpd_transfer_predict[1:5,1:6]
# fpd_transfer_predict_passsum[,passsum  := cepass + continue +pass]
# fpd_transfer_predict_passsum <- xy(fpd_transfer_predict_passsum)
# fpd_transfer_predict_passsum[,passsum := ce_pass+continue_pass+model_pass]
# fpd_transfer_predict_passsum <- xy(fpd_transfer_predict_passsum)
# # ---
# fpd_transfer_pct_predict_passsum <- cbind(cnt_transfer_passsum[,1],fpd_transfer_predict_passsum[,2:7]/cnt_transfer_passsum[,2:7])
# fpd_transfer_pct_predict_passsum
# #------------------------------------------------------#
# #---2.change:整体path数量和比例,通过率,fpd10的变化-----#
# #------------------------------------------------------#
# change_table <- new_table[,.(path,status,fpd10,maxdpd10,newwf)]
# # 新增newpath
# change_table[newwf != 'none',newpath := newwf]
# change_table[newpath %in% c('pass','reject'),newpath := 'model']
# change_table[newwf == 'none',newpath := path]
# # change:变化表
# # path_change:新旧模型path数量及比例变化表
# 
# path_change <- cbind(change_table[,.N,by=path][order(path)],
#                      change_table[,.N,by=newpath][order(newpath)])[,c(1,2,4)]
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
# # ar_change:新旧模型通过率变化表
# ce_pass_old       <-  change_table[path == 'ce'][status == 'pass'][,.N]
# model_pass_old    <-  change_table[path == 'model'][status == 'pass'][,.N]
# continue_pass_old <-  change_table[path == 'continue'][status == 'pass'][,.N]
# autonohc_pass_old <-  model_pass_old + continue_pass_old
# total_pass_old    <-  autonohc_pass_old + ce_pass_old
# #------
# ce_old            <-  change_table[path == 'ce'][,.N]
# model_old         <-  change_table[path == 'model'][,.N]
# continue_old      <-  change_table[path == 'continue'][,.N]
# autonohc_old      <-  model_old + continue_old
# #------
# ce_ar_old       <-  ce_pass_old/ce_old
# model_ar_old    <-  model_pass_old/model_old
# continue_ar_old <-  continue_pass_old/continue_old
# autonohc_ar_old <-  autonohc_pass_old/autonohc_old
# total_ar_old    <-  total_pass_old/total_count
# #------
# # ce_pass_new的口径与cnt_transfer中不同,cnt_transfer中按照新的ce通过状态与旧的一致,这里按ce_ar_pred根据比例计算
# ce_pass_new       <-  change_table[newpath == 'ce'][,.N]*ce_ar_pred
# model_pass_new    <-  change_table[newwf == 'pass'][,.N]
# continue_pass_new <-  change_table[newpath == 'continue'][,.N]*continue_ar_pred
# autonohc_pass_new <-  model_pass_new + continue_pass_new
# total_pass_new    <-  autonohc_pass_new + ce_pass_new
# #------
# ce_new            <-  change_table[newpath == 'ce'][,.N]
# model_new         <-  change_table[newpath == 'model'][,.N]
# continue_new      <-  change_table[newpath == 'continue'][,.N]
# autonohc_new      <-  model_new + continue_new
# #------
# ce_ar_new       <-  ce_pass_new/ce_new
# model_ar_new    <-  model_pass_new/model_new
# continue_ar_new <-  continue_pass_new/continue_new
# autonohc_ar_new <-  autonohc_pass_new/autonohc_new
# total_ar_new    <-  total_pass_new/total_count
# #------
# ar_change <- data.table(path = c('ce','model','continue','autonohc','total'),
#                         oldar = c(ce_ar_old,model_ar_old,continue_ar_old,autonohc_ar_old,total_ar_old),
#                         newar = c(ce_ar_new,model_ar_new,continue_ar_new,autonohc_ar_new,total_ar_new))
# ar_change
# # fpd10_change:新旧模型fpd10变化表
# fpd10_old <- fpd_transfer_pct_predict_sum[rn %in% c('ce_pass','continue_pass','model_pass'),.(rn,sum)]
# fpd10_old_all <- fpd_transfer_predict[rn %in% c('ce_pass','continue_pass','model_pass'),sum(sum)]/
#   cnt_transfer[rn %in% c('ce_pass','continue_pass','model_pass'),sum(sum)]
# fpd10_new <- xy(fpd_transfer_pct_predict_sum)[rn %in% c('cepass','continue','pass'),.(rn,sum)]
# fpd10_new_all <- xy(fpd_transfer_predict)[rn %in% c('cepass','continue','pass'),sum(sum)]/
#   xy(cnt_transfer)[rn %in% c('cepass','continue','pass'),sum(sum)]
# fpd10_change <- cbind(fpd10_old,fpd10_new[,2])
# names(fpd10_change) <- c('path','fpd10_old','fpd10_new')
# fpd10_change <- rbind(fpd10_change,data.table(path='pass',fpd10_old=fpd10_old_all,fpd10_new=fpd10_new_all))
# #------------------------------------------------------#
# #--------3.result:以上两项分析的主要结果---------------#
# #------------------------------------------------------#
# result <- list(cnt_transfer,fpd_transfer_predict,fpd_transfer_pct_predict_sum,
#                fpd_transfer_pct_predict_passsum,path_change,ar_change,fpd10_change)
# names(result) <- c('cnt_transfer','fpd_transfer_predict','fpd_transfer_pct_predict_sum',
#                    'fpd_transfer_pct_predict_passsum','path_change','ar_change','fpd10_change')
# result