# 设定工作目录
setwd('~/模型分筛选3/展示1')
# 1+1
# model_table,models=c('bt3_m','posjxl20_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
# pbocmodel1_th=c(68,43,11),nopbocmodel1_th=c(74,46,8)
# result_1 <- result
# result_core_1 <- result_core
# save(result_1,file = 'result_1')
# save(result_core_1,file = 'result_core_1')
load('result_1')
load('result_core_1')
result_1
result_core_1

# 1+1
# model_table,models=c('cashl_m','posjxl20_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
# pbocmodel1_th=c(85,50,9),nopbocmodel1_th=c(78,50,9)
# result_2 <- result
# result_core_2 <- result_core
# save(result_2,file = 'result_2')
# save(result_core_2,file = 'result_core_2')
load('result_2')
load('result_core_2')
result_2
result_core_2

# 1+1
# model_table,models=c('bt3_m','cup_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
# pbocmodel1_th=c(67,40,7),nopbocmodel1_th=c(68,40,7)
# result_3 <- result
# result_core_3 <- result_core
# save(result_3,file = 'result_3')
# save(result_core_3,file = 'result_core_3')
load('result_3')
load('result_core_3')
result_3
result_core_3

# 1+1
# model_table,models=c('posjxl20_m','cup_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
# pbocmodel1_th=c(75,43,10),nopbocmodel1_th=c(80,47,10)
# result_4 <- result
# result_core_4 <- result_core
# save(result_4,file = 'result_4')
# save(result_core_4,file = 'result_core_4')
load('result_4')
load('result_core_4')
result_4
result_core_4

# 1+1
# model_table,models=c('cup_m','posjxl20_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
# pbocmodel1_th=c(75,42,12),nopbocmodel1_th=c(80,50,12)
# result_5 <- result
# result_core_5 <- result_core
# save(result_5,file = 'result_5')
# save(result_core_5,file = 'result_core_5')
load('result_5')
load('result_core_5')
result_5
result_core_5

# 2+1
# model_table,models=c('bt3_m','cup_m','posjxl20_m'),pbocno1=1,pbocno2=2,nopbocno=3,sdate='2018-01-22',
# pbocmodel1_th=c(62,35,7),pbocmodel2_th=c(60,31,7),nopbocmodel1_th=c(75,40,10))
# result_6 <- result
# result_core_6 <- result_core
# save(result_6,file = 'result_6')
# save(result_core_6,file = 'result_core_6')
load('result_6')
load('result_core_6')
result_6
result_core_6

# 2+1
# model_table,models=c('bt3_m','cup_m','cup_m'),pbocno1=1,pbocno2=2,nopbocno=3,sdate='2018-01-22',
# pbocmodel1_th=c(62,30,7),pbocmodel2_th=c(60,30,7),nopbocmodel1_th=c(60,30,7)
# result_7 <- result
# result_core_7 <- result_core
# save(result_7,file = 'result_7')
# save(result_core_7,file = 'result_core_7')
load('result_7')
load('result_core_7')
result_7
result_core_7

# 2+1
# model_table,models=c('bt3_m','posjxl20_m','cup_m'),pbocno1=1,pbocno2=2,nopbocno=3,sdate='2018-01-22',
# pbocmodel1_th=c(59,26,7),pbocmodel2_th=c(59,27,7),nopbocmodel1_th=c(62,30,7)
# result_8 <- result
# result_core_8 <- result_core
# save(result_8,file = 'result_8')
# save(result_core_8,file = 'result_core_8')
load('result_8')
load('result_core_8')
result_8
result_core_8

# 2+1
# model_table,models=c('cup_m','cashl_m','posjxl20_m'),pbocno1=1,pbocno2=2,nopbocno=3,sdate='2018-01-22',
# pbocmodel1_th=c(63,30,8),pbocmodel2_th=c(65,33,7),nopbocmodel1_th=c(65,35,10)
# result_9 <- result
# result_core_9 <- result_core
# save(result_9,file = 'result_9')
# save(result_core_9,file = 'result_core_9')
load('result_9')
load('result_core_9')
result_9
result_core_9

# 结论:
# bt3_m 和 cup_m 整体表现较好
# posjxl20_m 和 cashl_m 可以适当辅助
