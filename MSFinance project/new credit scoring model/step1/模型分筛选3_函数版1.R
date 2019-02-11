#==========================================================================================
# 设定工作目录
setwd('~/模型分筛选3/展示1')
# 需要的package:sltool(自建),data.table,reshape2,ggplot2
library(sltool)
library(data.table)
library(ggplot2)
# 导入函数
source('模型分筛选3_函数1.R')
#==========================================================================================
# 导入从模型分筛选3_base中导出的model_table(经过预处理)
load('~/模型分筛选3/model_table1.RData')
#==========================================================================================
# 测试
# ------------------------
# 运行函数get_new_table1
new_table <- get_new_table1(model_table)
# 运行函数get_result
result <- get_result(new_table)
# 返回完整的包含7个元素的list则测试成功
result
# ------------------------
# 运行函数get_new_table2
new_table <- get_new_table2(model_table)
# 运行函数get_result
result <- get_result(new_table)
# 返回完整的包含7个元素的list则测试成功
result
#==========================================================================================
# 待选模型如下:
# "bt3_m","cashl_m","posjxl20_m",  "cup_m"   ,"zhima_m"
#  要征信  要征信    不要征信       不要征信   供参考
#==========================================================================================
# main
# new_table <- get_new_table1(model_table,models=c('cup_m','posjxl20_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
#                             pbocmodel1_th=c(75,42,12),nopbocmodel1_th=c(80,50,12))
# ----------
new_table <- get_new_table2(model_table,models=c('cup_m','cashl_m','posjxl20_m'),pbocno1=1,pbocno2=2,nopbocno=3,sdate='2018-01-22',
                            pbocmodel1_th=c(63,30,8),pbocmodel2_th=c(65,33,7),nopbocmodel1_th=c(65,35,10))
# ----------
result <- get_result(new_table,ce_ar_pred = 0.6, continue_ar_pred = 0.99, 
                     cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5)
# 输出结果:
# result
# 输出核心结果
result_core <- list(result[['cnt_transfer']][rn=='ce_reject',.(pass,continue)],
                    result[["path_change"]][path %in% c('ce','continue','model')],
                    result[["ar_change"]][path == 'total'],
                    result[["fpd10_change"]][path == 'pass'])
names(result_core) <- c('ce_reject->model_pass数量','ce,continue,model的变动','总通过率的变动','总fpd10的变动')

result_core
