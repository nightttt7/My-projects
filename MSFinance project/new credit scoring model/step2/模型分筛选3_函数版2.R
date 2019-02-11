#==========================================================================================
# 设定工作目录
setwd('~/模型分筛选3/展示2')
# 导入package
# 需要的package:sltool(自建),data.table,reshape2,ggplot2
library(sltool)
library(data.table)
library(ggplot2)
# 导入函数
source('模型分筛选3_函数2.R')
# 导入从模型分筛选3_base中导出的model_table(经过预处理)
load('~/模型分筛选3/model_table1.RData')
#==========================================================================================
# 测试:测试通过
# 运行函数get_new_table1
# new_table <- get_new_table3(model_table)
# 运行函数get_result
# result <- get_result3(new_table)
# 返回完整的list则测试成功
# result
#==========================================================================================
new_table <- get_new_table3(model_table,models=c('bt3_m','cup_m','cup_m','posjxl20_m','zhimamsxf_m'),
                            sdate='2018-01-22',edate='2018-04-16',passstyle='both',
                            th1=c(47,28,5),th2=c(38,12,3),th3=c(72,46,5),th4=c(65,15,3))
# ----all------
result <- get_result3(new_table,ce_ar_pred = 0.6, continue_ar_pred = 0.99, cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5)
result_core <- get_core(result)
result_core
result
# ----haspboc=1------
result1 <- get_result3(new_table[haspboc == 1],ce_ar_pred = 0.6, continue_ar_pred = 0.99, cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5)
result_core1 <- get_core(result1)
result_core1
result1
# result1[['model_cnt']]
# result1[['model_fpd10_pct']]
# ----haspboc=0------
result0 <- get_result3_nopboc(new_table[haspboc == 0],ce_ar_pred = 0.6, continue_ar_pred = 0.99, cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5)
result_core0 <- get_core1(result0)
result_core0
result0
# result0[['model_cnt']]
# result0[['model_fpd10_pct']]
#==========================================================================================
# 利用马上芝麻分测试新策略的有效性
# 旧策略有效性
temp_table <- new_table[newwf != 'none' & !is.na(zhimamsxf_m),.(path,status,zhimamsxf_m)]
temp_table[path=='model' & status == 'pass', path := 'pass']
temp_table[path=='model' & status == 'reject', path := 'reject']
p1 <- ggplot(data = temp_table[,.(path,zhimamsxf_m)], aes(x=path, y=zhimamsxf_m))+geom_violin(trim = FALSE)
p2 <- ggplot(data = temp_table[,.(path,zhimamsxf_m)], aes(x=path, y=zhimamsxf_m))+geom_boxplot(width=0.2)
rm(temp_table)
gc()
# 新策略有效性
p3 <- ggplot(data = new_table[newwf != 'none' & !is.na(zhimamsxf_m),.(newwf,zhimamsxf_m)], aes(x=newwf, y=zhimamsxf_m))+geom_violin(trim = FALSE)
p4 <- ggplot(data = new_table[newwf != 'none' & !is.na(zhimamsxf_m),.(newwf,zhimamsxf_m)], aes(x=newwf, y=zhimamsxf_m))+geom_boxplot(width=0.2)
# 画图
multiplot(p1,p3,p2,p4,cols = 2)
# ----------------------
# (生美/医美)新策略有效性
new_table[prod_cd %in% c('3119','3120','3325','3326','3328','3338','3383','3384'),prod_kd := '生美']
new_table[prod_cd %in% c('3112','3113','3121','3123','3131','3132','3329','3333','3334','3335','3336','3339','3340','3385','3386','3387','3594','3595','3596'),prod_kd := '医美']
p5 <- ggplot(data = new_table[newwf != 'none' & !is.na(zhimamsxf_m) & prod_kd == '医美', .(newwf,zhimamsxf_m)], aes(x=newwf, y=zhimamsxf_m))+geom_boxplot(width=0.2)
p6 <- ggplot(data = new_table[newwf != 'none' & !is.na(zhimamsxf_m) & prod_kd == '生美', .(newwf,zhimamsxf_m)], aes(x=newwf, y=zhimamsxf_m))+geom_boxplot(width=0.2)
# (高金额/低金额)新策略有效性
new_table[,amount := 'low']
new_table[(prod_kd == '医美' & appl_lim >= 30000) | (prod_kd == '生美' & appl_lim >= 15000), amount := 'high']
p7 <- ggplot(data = new_table[newwf != 'none' & !is.na(zhimamsxf_m) & amount == 'low',.(newwf,zhimamsxf_m)], aes(x=newwf, y=zhimamsxf_m))+geom_boxplot(width=0.2)
p8 <- ggplot(data = new_table[newwf != 'none' & !is.na(zhimamsxf_m) & amount == 'high',.(newwf,zhimamsxf_m)], aes(x=newwf, y=zhimamsxf_m))+geom_boxplot(width=0.2)
# 画图
multiplot(p4,p5,p6,cols = 3)
multiplot(p4,p7,p8,cols = 3)















