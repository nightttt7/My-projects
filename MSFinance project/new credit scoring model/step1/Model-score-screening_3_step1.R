# Set working directory
Setwd('~/model sub-screening 3/show 1')
# 1+1
# model_table,models=c('bt3_m','posjxl20_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
# pbocmodel1_th=c(68,43,11),nopbocmodel1_th=c(74,46,8)
# result_1 <- result
# result_core_1 <- result_core
# save(result_1,file = 'result_1')
# save(result_core_1,file = 'result_core_1')
Load('result_1')
Load('result_core_1')
Result_1
Result_core_1

# 1+1
# model_table,models=c('cashl_m','posjxl20_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
# pbocmodel1_th=c(85,50,9),nopbocmodel1_th=c(78,50,9)
# result_2 <- result
# result_core_2 <- result_core
# save(result_2,file = 'result_2')
# save(result_core_2,file = 'result_core_2')
Load('result_2')
Load('result_core_2')
Result_2
Result_core_2

# 1+1
# model_table,models=c('bt3_m','cup_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
# pbocmodel1_th=c(67,40,7),nopbocmodel1_th=c(68,40,7)
# result_3 <- result
# result_core_3 <- result_core
# save(result_3,file = 'result_3')
# save(result_core_3,file = 'result_core_3')
Load('result_3')
Load('result_core_3')
Result_3
Result_core_3

# 1+1
# model_table,models=c('posjxl20_m','cup_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
# pbocmodel1_th=c(75,43,10),nopbocmodel1_th=c(80,47,10)
# result_4 <- result
# result_core_4 <- result_core
# save(result_4,file = 'result_4')
# save(result_core_4,file = 'result_core_4')
Load('result_4')
Load('result_core_4')
Result_4
Result_core_4

# 1+1
# model_table,models=c('cup_m','posjxl20_m'),pbocno=1,nopbocno=2,sdate='2018-01-22',
# pbocmodel1_th=c(75,42,12),nopbocmodel1_th=c(80,50,12)
# result_5 <- result
# result_core_5 <- result_core
# save(result_5,file = 'result_5')
# save(result_core_5,file = 'result_core_5')
Load('result_5')
Load('result_core_5')
Result_5
Result_core_5

# 2+1
# model_table,models=c('bt3_m','cup_m','posjxl20_m'),pbocno1=1,pbocno2=2,nopbocno=3,sdate='2018-01-22',
# pbocmodel1_th=c(62,35,7),pbocmodel2_th=c(60,31,7),nopbocmodel1_th=c(75,40,10))
# result_6 <- result
# result_core_6 <- result_core
# save(result_6,file = 'result_6')
# save(result_core_6,file = 'result_core_6')
Load('result_6')
Load('result_core_6')
Result_6
Result_core_6

# 2+1
# model_table,models=c('bt3_m','cup_m','cup_m'), pbocno1=1, pbocno2=2, nopbocno=3,sdate='2018-01-22',
# pbocmodel1_th=c(62,30,7),pbocmodel2_th=c(60,30,7),nopbocmodel1_th=c(60,30,7)
# result_7 <- result
# result_core_7 <- result_core
# save(result_7,file = 'result_7')
# save(result_core_7,file = 'result_core_7')
Load('result_7')
Load('result_core_7')
Result_7
Result_core_7

# 2+1
# model_table,models=c('bt3_m','posjxl20_m','cup_m'), pbocno1=1, pbocno2=2, nopbocno=3,sdate='2018-01-22',
# pbocmodel1_th=c(59,26,7),pbocmodel2_th=c(59,27,7),nopbocmodel1_th=c(62,30,7)
# result_8 <- result
# result_core_8 <- result_core
# save(result_8,file = 'result_8')
# save(result_core_8,file = 'result_core_8')
Load('result_8')
Load('result_core_8')
Result_8
Result_core_8

# 2+1
# model_table,models=c('cup_m','cashl_m','posjxl20_m'),pbocno1=1,pbocno2=2,nopbocno=3,sdate='2018-01-22',
# pbocmodel1_th=c(63,30,8),pbocmodel2_th=c(65,33,7),nopbocmodel1_th=c(65,35,10)
# result_9 <- result
# result_core_9 <- result_core
# save(result_9,file = 'result_9')
# save(result_core_9,file = 'result_core_9')
Load('result_9')
Load('result_core_9')
Result_9
Result_core_9

# in conclusion:
# bt3_m and cup_m have better overall performance
# posjxl20_m and cashl_m can be appropriately assisted