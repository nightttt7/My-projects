ch_newwfkey <- function(th,table){
  table[newwf == 'blank' & haspboc, key := 'model1']
  table[key == 'model1' & bt3_m >= -th[[1]], newwf := 'pass']
  table[key == 'model1' & bt3_m <  -th[[1]], newwf := 'continue']
  table[key == 'model1' & bt3_m <  -th[[2]], newwf := 'ce']
  table[key == 'model1' & bt3_m <  -th[[3]], newwf := 'reject']
  table[key == 'model1' & newwf != 'reject' & cup_m <  -th[[6]], key := 'model2']
  table[key == 'model2', newwf := 'reject']
  table[key == 'model1' & newwf != 'reject' & newwf != 'ce' & cup_m < -th[[5]], key := 'model2']
  table[key == 'model2' & newwf != 'reject', newwf := 'ce']
  table[key == 'model1' & newwf == 'pass' & cup_m < -th[[4]], key := 'model2']
  table[key == 'model2' & newwf != 'reject' & newwf != 'ce', newwf := 'continue']
  table[newwf == 'blank' & !haspboc, key := 'model3']
  table[key == 'model3' & cup_m >= -th[[7]], newwf := 'pass']
  table[key == 'model3' & cup_m <  -th[[7]], newwf := 'continue']
  table[key == 'model3' & cup_m <  -th[[8]], newwf := 'ce']
  table[key == 'model3' & cup_m <  -th[[9]], newwf := 'reject']
  table[key == 'model3' & posjxl20_m <  th[[12]] & newwf != 'reject', newwf := 'reject']
  table[key == 'model3' & is.na(cup_m), key := 'model4']
  table[key == 'model4' & posjxl20_m < th[[11]], newwf := 'reject']
  table[key == 'model4' & newwf != 'reject' & posjxl20_m <  th[[10]], newwf := 'ce']
  table[key == 'model4' & newwf != 'reject' & newwf != 'ce' & posjxl20_m >= th[[10]], newwf := 'continue']
  table[key == 'model4' & is.na(posjxl20_m), key := 'none']
  table[key == 'none', newwf := 'ce']
  return(table)
}
get_arpath <- function(table) {
  path_go <- table[newwf == 'pass',.N]
  path_1 <- table[newwf == 'continue',.N]
  path_2 <- table[newwf == 'ce',.N]
  path_reject <- table[newwf == 'reject',.N]
  path_all <- table[,.N]
  ar <- (path_go + path_1 + 0.6 * path_2)/path_all
  path_go <- path_go/path_all
  path_1 <- path_1/path_all
  path_2 <- path_2/path_all
  path_reject <- path_reject/path_all
  
  pboc_path_go <- table[key %in% c('model1','model2') & newwf == 'pass',.N]
  pboc_path_1 <- table[key %in% c('model1','model2') & newwf == 'continue',.N]
  pboc_path_2 <- table[key %in% c('model1','model2') & newwf == 'ce',.N]
  pboc_path_reject <- table[key %in% c('model1','model2') & newwf == 'reject',.N]
  pboc_path_all <- table[!is.na(bt3_m),.N]
  pboc_ar <- (pboc_path_go + pboc_path_1 + 0.6 * pboc_path_2)/pboc_path_all
  pboc_path_go <- pboc_path_go/pboc_path_all
  pboc_path_1 <- pboc_path_1/pboc_path_all
  pboc_path_2 <- pboc_path_2/pboc_path_all
  pboc_path_reject <- pboc_path_reject/pboc_path_all
  
  nopboc_path_go <- table[key %in% c('model3') & newwf == 'pass',.N]
  nopboc_path_1 <- table[key %in% c('model3') & newwf == 'continue',.N]
  nopboc_path_2 <- table[key %in% c('model3') & newwf == 'ce',.N]
  nopboc_path_reject <- table[key %in% c('model3') & newwf == 'reject',.N]
  nopboc_path_all <- table[is.na(bt3_m) & !is.na(cup_m),.N]
  nopboc_ar <- (nopboc_path_go + nopboc_path_1 + 0.6 * nopboc_path_2)/nopboc_path_all
  nopboc_path_go <- nopboc_path_go/nopboc_path_all
  nopboc_path_1 <- nopboc_path_1/nopboc_path_all
  nopboc_path_2 <- nopboc_path_2/nopboc_path_all
  nopboc_path_reject <- nopboc_path_reject/nopboc_path_all
  
  nocup_path_go <- table[key %in% c('model4') & newwf == 'pass',.N]
  nocup_path_1 <- table[key %in% c('model4') & newwf == 'continue',.N]
  nocup_path_2 <- table[key %in% c('model4') & newwf == 'ce',.N]
  nocup_path_reject <- table[key %in% c('model4') & newwf == 'reject',.N]
  nocup_path_all <- table[is.na(bt3_m) & is.na(cup_m) & !is.na(posjxl20_m),.N]
  nocup_ar <- (nocup_path_go + nocup_path_1 + 0.6 * nocup_path_2)/nocup_path_all
  nocup_path_go <- nocup_path_go/nocup_path_all
  nocup_path_1 <- nocup_path_1/nocup_path_all
  nocup_path_2 <- nocup_path_2/nocup_path_all
  nocup_path_reject <- nocup_path_reject/nocup_path_all  
  
  output <- c(ar,pboc_ar,nopboc_ar,nocup_ar,path_go,path_1,path_2,path_reject,
              pboc_path_go,pboc_path_1,pboc_path_2,pboc_path_reject,
              nopboc_path_go,nopboc_path_1,nopboc_path_2,nopboc_path_reject,
              nocup_path_go,nocup_path_1,nocup_path_2,nocup_path_reject)
  return(output)
}
#==========================================================================================
# 设定工作目录
setwd('~/模型分筛选3/差异化策略')
# 导入package
# 需要的package:sltool(自建),data.table,reshape2,ggplot2
library(sltool)
library(data.table)
library(ggplot2)
library(XLConnect)
#==========================================================================================
# 导入从模型分筛选3/cuts中导出的cuts
load('~/模型分筛选3/cuts/model_score_cuts.RData')
# 导入pos_level.csv(门店风险等级表)
# 导入sa_level.csv(sa风险等级表)
# 导入city_level.csv(城市风险等级表)
pos_level_t <- read.csv('pos_level.csv',row.names = 1)
sa_level_t <- read.csv('sa_level.csv',row.names = 1)
city_level_t <- read.csv('city_level.csv',row.names = 1)
#==========================================================================================
# 导入从模型分筛选3_base中导出的model_table(经过预处理)
load('~/模型分筛选3/model_table1.RData')
# 对model_table进行进一步处理
# 限定时间窗口
model_table <- model_table[appl_date >= '2018-01-22' & appl_date <= '2018-04-16']
# 增加prod_kd
model_table[prod_cd %in% c('3119','3120','3325','3326','3328','3338','3383','3384'),prod_kd := '生美']
model_table[prod_cd %in% c('3112','3113','3121','3123','3131','3132','3329','3333','3334','3335','3336','3339','3340','3385','3386','3387','3594','3595','3596'),prod_kd := '医美']
# 增加pos_level
model_table[,pos_level := pos_level_t[as.character(mer_no),]]
# 增加sa_level,city_level(尚无数据,全部填充0)
# model_table[,sa_level := pos_level_t[as.character(sa编号的变量名,暂无),]]
# model_table[,city_level := pos_level_t[as.character(biz_city_cd ),]]
model_table[,sa_level := 0]
model_table[,city_level := 0]
# 增加lim_level
model_table[,lim_level := 'low']
model_table[prod_kd == '医美' & appl_lim >= 30000,lim_level := 'high']
model_table[prod_kd == '生美' & appl_lim >= 15000,lim_level := 'high']
# 通过bt3是否有值判断是否为有征信用户
model_table[!is.na(bt3_m), haspboc := 1]
model_table[is.na(bt3_m), haspboc := 0]
# 增加newwf
# newwf='none', 不会发生变化的单子
# unknow和customercancel相当于缺失值
model_table[path == 'unknow' | path == 'customercancel', newwf := 'none']
# hc和opscancel时间在模型之前
model_table[path == 'hc' | path == 'opscancel', newwf := 'none']
# 理论上这些cancel的单子也会收到影响,但为了方便计算,可以忽略,对结果几乎没影响
model_table[status == 'cancel', newwf := 'none']
# 剩余都是需要计算新的workflow的,暂赋值newwf为'blank'
model_table[is.na(newwf),newwf := 'blank']
#==========================================================================================
# 导入美业自动化策略表(input)
wb <- loadWorkbook('美业自动化策略.xlsx')
input <- wb$readWorksheet(sheet = '主策略', startRow = 2, endRow = 12)
input <- data.table(input)
#==========================================================================================
# 参数:模型得分形式
input[,th1_1.1 := cuts[th1_1,bt3]]
input[,th1_2.1 := cuts[th1_2,bt3]]
input[,th1_3.1 := cuts[th1_3,bt3]]
input[,th2_1.1 := cuts[th2_1,cup]]
input[,th2_2.1 := cuts[th2_2,cup]]
input[,th2_3.1 := cuts[th2_3,cup]]
input[,th3_1.1 := cuts[th3_1,cup]]
input[,th3_2.1 := cuts[th3_2,cup]]
input[,th3_3.1 := cuts[th3_3,cup]]
input[,th4_1.1 := cuts[th4_1,posjxl]]
input[,th4_2.1 := cuts[th4_2,posjxl]]
input[,th4_3.1 := cuts[th4_3,posjxl]]
#==========================================================================================
# 差异化调整前与差异化调整后
input <- data.frame(input)
th <- input[1,42:53]
model_table <- ch_newwfkey(th,model_table)

# no = 1
arpath <- get_arpath(model_table)
arpath -> input[1,7:26]

# no = 2
table2 <- model_table[prod_kd == '医美' & pos_level == -1 & lim_level == 'high']
arpath <- get_arpath(table2)
arpath -> input[2,7:26]
table2[newwf != 'none', newwf := 'blank']
table2[,key := 'NA']
th <- input[2,42:53]
table2 <- ch_newwfkey(th,table2)
arpath <- get_arpath(table2)
arpath -> input[2,54:73]

# no = 3
table3 <- model_table[prod_kd == '医美' & pos_level == -1 & lim_level == 'low']
arpath <- get_arpath(table3)
arpath -> input[3,7:26]
table3[newwf != 'none', newwf := 'blank']
table3[,key := 'NA']
th <- input[3,42:53]
table3 <- ch_newwfkey(th,table3)
arpath <- get_arpath(table3)
arpath -> input[3,54:73]

# no = 4
table4 <- model_table[prod_kd == '医美' & pos_level == 0 & lim_level == 'high']
arpath <- get_arpath(table4)
arpath -> input[4,7:26]
table4[newwf != 'none', newwf := 'blank']
table4[,key := 'NA']
th <- input[4,42:53]
table4 <- ch_newwfkey(th,table4)
arpath <- get_arpath(table4)
arpath -> input[4,54:73]

# no = 5
table5 <- model_table[prod_kd == '医美' & pos_level == 0 & lim_level == 'low']
arpath <- get_arpath(table5)
arpath -> input[5,7:26]
table5[newwf != 'none', newwf := 'blank']
table5[,key := 'NA']
th <- input[5,42:53]
table5 <- ch_newwfkey(th,table5)
arpath <- get_arpath(table5)
arpath -> input[5,54:73]

# no = 6
table6 <- model_table[prod_kd == '生美' & pos_level == 0 & lim_level == 'high']
arpath <- get_arpath(table6)
arpath -> input[6,7:26]
table6[newwf != 'none', newwf := 'blank']
table6[,key := 'NA']
th <- input[6,42:53]
table6 <- ch_newwfkey(th,table6)
arpath <- get_arpath(table6)
arpath -> input[6,54:73]

# no = 7
table7 <- model_table[prod_kd == '生美' & pos_level == 0 & lim_level == 'low']
arpath <- get_arpath(table7)
arpath -> input[7,7:26]
table7[newwf != 'none', newwf := 'blank']
table7[,key := 'NA']
th <- input[7,42:53]
table7 <- ch_newwfkey(th,table7)
arpath <- get_arpath(table7)
arpath -> input[7,54:73]

# no = 8
table8 <- model_table[pos_level == 1 | pos_level == 2]
arpath <- get_arpath(table8)
arpath -> input[8,7:26]
table8[newwf != 'none', newwf := 'blank']
table8[,key := 'NA']
th <- input[8,42:53]
table8 <- ch_newwfkey(th,table8)
arpath <- get_arpath(table8)
arpath -> input[8,54:73]

# no = 9
table9 <- model_table[pos_level == 3 | pos_level == 4]
arpath <- get_arpath(table9)
arpath -> input[9,7:26]
table9[newwf != 'none', newwf := 'blank']
table9[,key := 'NA']
th <- input[9,42:53]
table9 <- ch_newwfkey(th,table9)
arpath <- get_arpath(table9)
arpath -> input[9,54:73]

# no = 10
table10 <- model_table[pos_level == 5]
arpath <- get_arpath(table10)
arpath -> input[10,7:26]
table10[newwf != 'none', newwf := 'blank']
table10[,key := 'NA']
th <- input[10,42:53]
table10 <- ch_newwfkey(th,table10)
arpath <- get_arpath(table10)
arpath -> input[10,54:73]

# no = 1 
model_table_new <- rbind(table2,table3,table4,table5,table6,table7,table8,table9,table10)
arpath <- get_arpath(model_table_new)
arpath -> input[1,54:73]

#==========================================================================================
setStyleAction(wb, XLC$"STYLE_ACTION.PREDEFINED")
writeWorksheet(wb, input, sheet = '主策略', startRow = 2)
saveWorkbook(wb)














