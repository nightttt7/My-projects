#===========================================================#
#====================说明&参数设置==========================#
#=========设定工作目录&导入模块&从数据库取得数据============#
#===========================================================#
# 运行此文件前需要先在HUE修改"xxx模型监控Hive"的三个日期参数至上周日并运行,此文件每周一运行
# 如果不在周一运行请调整2_main中的"""AND to_date(date_sub(now(),interval 1 day))"""
# data.frame和data.table混用是因为to_excel函数是针对data.frame编写
# 需要的package:sltool(自建),data.table,reshape2
# 参数1:需要展示的周数(从上周往前数)
week_show <- 6
setwd('~/xxx模型监控')
library(sltool)
library(data.table)
table <- as.data.table(run.sql(exe = 1, get = 2, uid = "xxxxxxxxxxxxx", pwd = "xxxxxxxxx"))
#===========================================================#
#=====================数据预处理============================#
#===========================================================#
## 增加auto,model,status,date,week列
# auto 自动状态(自动auto/仅运营ops_only/信审ce)(仅运营+信审=运营)
table[,auto := 'auto']
table[!is.na(final_checker) == TRUE, auto := 'ce']
table[!is.na(basic_checker) == TRUE & is.na(final_checker) == TRUE, auto := 'ops_only']
# model 模型状态(hc/bt3/jxl)
table[hc != 0,model := 'hc']
table[hc == 0 & is.na(bt3_score) == FALSE,model := 'bt3']
table[hc == 0 & is.na(bt3_score) == TRUE & is.na(jxlposscorev3) == FALSE, model := 'jxl']
# status 通过状态(通过pass/取消cancel/拒绝reject)
table[,status := 'reject']
table[appl_status == 'N' | appl_status == 'A' | appl_status == 'J', status := 'pass']
table[appl_status == 'C', status := 'cancel']
# data 申请日期
table[,date := as.Date(appl_tm)]
# week 申请所在周的周一,2018-04-09是xxx自动模型上线第一周的周一 
week_first <- as.Date('2018-04-09')
week_today <- floor(as.numeric(Sys.Date()-as.Date('2018-04-09'))/7)
table[, week := floor((date-week_first)/7)*7+week_first]
# 不在需要展示的范围内的订单,week列变为NA. week_show见说明&参数设置
table[week_today-floor((date-week_first)/7) > week_show, week := NA]
#===========================================================#
#======================结果计算 ============================#
#===========================================================#
# add_index:增加index列到最左,用于不同表格的对齐
add_index <- function(x) {
  index <- rownames(x)
  x <- cbind(index,x)
}

# delete_last: 删除最后一列,用于有NA列的情况
delete_last <- function(x) {
  x <- x[,-ncol(x)]
  return(x)
}

# 1 计算申请量,自动化率

# 总申请量/每周申请量
# 计数,total_appl_count将在后面被使用
total_appl_count <- table[,.N]
# 计数,并转化为data.frame
total_appl <- data.frame(table[,.N])
names(total_appl) <- c('合计申请量')
# 按week计数
week_appl <- table[,.N,by=week]
# 将week列转置为列名
week_appl <- reshape2::acast(week_appl, 1 ~ week,value.var = 'N')
# 合并total_appl和week_appl
output_appl <- cbind(total_appl,week_appl)
# 对齐不同表格
output_appl <- add_index(output_appl)
# 删除NA列
output_appl <- delete_last(output_appl)

# 初审比例 
total_ops_ratio <- data.frame(table[auto!='auto',.N]/total_appl_count)
names(total_ops_ratio) <- c('合计初审比例')
week_ops_ratio <- table[auto!='auto',.N,by=week]
week_ops_ratio <- reshape2::acast(week_ops_ratio, 1 ~ week,value.var = 'N')
week_ops_ratio <- week_ops_ratio/week_appl
output_ops_ratio <- cbind(total_ops_ratio,week_ops_ratio)
output_ops_ratio <- add_index(output_ops_ratio)
output_ops_ratio <- delete_last(output_ops_ratio)

# 信审比例
total_ce_ratio <- data.frame(table[auto=='ce',.N]/table[,.N])
names(total_ce_ratio) <- c('合计信审比例')
week_ce_ratio <- table[auto=='ce',.N,by=week]
week_ce_ratio <- reshape2::acast(week_ce_ratio, 1 ~ week,value.var = 'N')
week_ce_ratio <- week_ce_ratio/week_appl
output_ce_ratio <- cbind(total_ce_ratio,week_ce_ratio)
output_ce_ratio <- add_index(output_ce_ratio)
output_ce_ratio <- delete_last(output_ce_ratio)

# 2 计算分数分布

# bt3:xxxxxxxxx
# f:按分割点(cut_p)将分数划分成段
f <- function(x) {
  cut_p <- c(0,0.0034,0.0055,0.0079,0.0108,0.0146,0.0197,0.0265,0.0382,0.0633,1)
  return(cut(x,cut_p,right=FALSE))
}
# 需要按分数段排序
total_bt3_region <- data.frame(table[model=='bt3', .N, by = f(bt3_score)][order(f)])
names(total_bt3_region) <- c('分数区间','合计数量')
week_bt3_region <- table[model=='bt3', .N, by = .(f(bt3_score),week)]
# reshape2::acast已经自动按分数段排序,故顺序和total_bt3_region一致
week_bt3_region <- reshape2::acast(week_bt3_region, f ~ week,value.var = 'N')
output_bt3_region <- cbind(total_bt3_region,week_bt3_region)
# NA值改为0
output_bt3_region[is.na(output_bt3_region)] <- 0 
# 计算每周合计
sum_t <- apply(output_bt3_region[,-1], 2, sum)
# 计算比例
output_bt3_region_prop <- output_bt3_region[,-1]/rbind(sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t)
output_bt3_region_prop <- cbind(output_bt3_region[,1],output_bt3_region_prop)
names(output_bt3_region_prop)[1:2] <- c('分数区间','合计比例')
bt3_score_mean <- table[model=='bt3', mean(bt3_score), by = week]
bt3_score_mean <- reshape2::acast(bt3_score_mean, 1 ~ week,value.var = 'V1')
output_bt3_score_mean <- cbind(data.frame(table[model=='bt3', mean(bt3_score)]),bt3_score_mean)
names(output_bt3_score_mean)[1] <- c('合计平均得分')
output_bt3_score_mean <- add_index(output_bt3_score_mean)
output_bt3_region <- delete_last(output_bt3_region)
output_bt3_region_prop <- delete_last(output_bt3_region_prop)
output_bt3_score_mean <- delete_last(output_bt3_score_mean)

# jxl:xxxxxxxxx 
f <- function(x) {
  cut_p <- c(0,0.918,0.946,0.964,0.974,0.982,0.988,0.991,0.993,0.995,1)
  return(cut(x,cut_p,right=FALSE))
}
total_jxl_region <- data.frame(table[model=='jxl', .N, by = f(jxlposscorev3)][order(f)])
total_jxl_region <- total_jxl_region[-11,]
names(total_jxl_region) <- c('分数区间','合计数量')
week_jxl_region <- table[model=='jxl', .N, by = .(f(jxlposscorev3),week)]
week_jxl_region <- reshape2::acast(week_jxl_region, f ~ week,value.var = 'N')
week_jxl_region <- week_jxl_region[-11,]
output_jxl_region <- cbind(total_jxl_region,week_jxl_region)
output_jxl_region[is.na(output_jxl_region)] <- 0 
sum_t <- apply(output_jxl_region[,-1], 2, sum)
output_jxl_region_prop <- output_jxl_region[,-1]/rbind(sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t)
output_jxl_region_prop <- cbind(output_jxl_region[,1],output_jxl_region_prop)
names(output_jxl_region_prop)[1:2] <- c('分数区间','合计比例')
jxl_score_mean <- table[model=='jxl', mean(jxlposscorev3), by = week]
jxl_score_mean <- reshape2::acast(jxl_score_mean, 1 ~ week,value.var = 'V1')
output_jxl_score_mean <- cbind(data.frame(table[model=='jxl', mean(jxlposscorev3)]),jxl_score_mean)
names(output_jxl_score_mean)[1] <- c('合计平均得分')
output_jxl_score_mean <- add_index(output_jxl_score_mean)
output_jxl_region <- delete_last(output_jxl_region)
output_jxl_region_prop <- delete_last(output_jxl_region_prop)
output_jxl_score_mean <- delete_last(output_jxl_score_mean)

# 3 计算决策路径

# bt3:xxxxxxxxx
total_bt3_wf <- data.frame(table[model=='bt3',.N,by = workflowcode][order(workflowcode)])
bt3_wf <- table[model=='bt3',.N,by = .(workflowcode,week)]
bt3_wf <- reshape2::acast(bt3_wf, workflowcode ~ week,value.var = 'N')
bt3_wf[is.na(bt3_wf)] <- 0
output_bt3_wf <- cbind(total_bt3_wf,bt3_wf)
names(output_bt3_wf)[1:2] <- c('路径','合计数量')
sum_t <- apply(output_bt3_region[,-1], 2, sum)
output_bt3_wf_ratio <- output_bt3_wf[,-1]/rbind(sum_t,sum_t,sum_t,sum_t)
output_bt3_wf_ratio <- cbind(output_bt3_wf[,1],output_bt3_wf_ratio)
names(output_bt3_wf_ratio)[1:2] <- c('路径','合计比例')
output_bt3_wf <- delete_last(output_bt3_wf)
output_bt3_wf_ratio <- delete_last(output_bt3_wf_ratio)

# jxl:xxxxxxx 
total_jxl_wf <- data.frame(table[model=='jxl',.N,by = workflowcode][order(workflowcode)])
jxl_wf <- table[model=='jxl',.N,by = .(workflowcode,week)]
jxl_wf <- reshape2::acast(jxl_wf, workflowcode ~ week,value.var = 'N')
jxl_wf[is.na(jxl_wf)] <- 0
output_jxl_wf <- cbind(total_jxl_wf,jxl_wf)
names(output_jxl_wf)[1:2] <- c('路径','合计数量')
sum_t <- apply(output_jxl_region[,-1], 2, sum)
output_jxl_wf_ratio <- output_jxl_wf[,-1]/rbind(sum_t,sum_t,sum_t,sum_t)
output_jxl_wf_ratio <- cbind(output_jxl_wf[,1],output_jxl_wf_ratio)
names(output_jxl_wf_ratio)[1:2] <- c('路径','合计比例')
output_jxl_wf <- delete_last(output_jxl_wf)
output_jxl_wf_ratio <- delete_last(output_jxl_wf_ratio)

# 4 计算贡献率
# xxx/xxxxxx的通过/拒绝数量
model_status_count <- table[(model=='bt3' | model=='jxl') & (status=='pass' | status=='reject'), .N, by = .(week,model,status)]
model_status_count <- model_status_count[, model_status := paste(model,status,sep = "_")]
model_status_count <- reshape2::acast(model_status_count, model_status ~ week,value.var = 'N')
kind <- rownames(model_status_count)
output_model_status_count <- data.frame(kind,model_status_count)
output_model_status_count <- add_index(output_model_status_count)
model_status_prop <- model_status_count/rbind(week_appl,week_appl,week_appl,week_appl)
kind <- rownames(model_status_prop)
output_model_status_prop <- data.frame(kind,model_status_prop)
output_model_status_prop <- add_index(output_model_status_prop)
output_model_status_count <- delete_last(output_model_status_count)
output_model_status_prop <- delete_last(output_model_status_prop)

# 5 NodeofEdu
table[model=='hc',node := 'hc_reject']
table[model!='hc' & workflowcode=='WF_Reject',node := 'model_reject']
table[workflowcode=='WF_Cancel',node := 'auto_cancel']
table[auto=='ops_only' & status=='pass',node := 'ops_pass']
table[auto=='ops_only' & status=='cancel',node := 'ops_cancel']
table[auto=='ce' & status=='pass',node := 'ce_pass']
table[auto=='ce' & status=='cancel',node := 'ce_cancel']
table[auto=='ce' & status=='reject',node := 'ce_reject']
# 有少量申请无法被分类,原因未知
output_node <- table[,.N,by=.(week,node)]
output_node <- reshape2::acast(output_node, node ~ week,value.var = 'N')
kind <- rownames(output_node)
output_node <- data.frame(kind,output_node)
output_node <- add_index(output_node)
output_node <- delete_last(output_node)
#===========================================================#
#=================输出结果到EXCEL===========================#
#===========================================================#
# 数据放入output
output <- list(
  '申请量',
  output_appl,
  '自动化率',
  output_ops_ratio,
  output_ce_ratio,
  'xxxxx模型打分分布',
  output_bt3_region,
  output_bt3_region_prop,
  'xxxxx平均分',
  output_bt3_score_mean,
  'xxxxxxxxxxxxx模型打分分布',
  output_jxl_region,
  output_jxl_region_prop,
  'xxxxxxxxxxxxx平均分',
  output_jxl_score_mean,
  'xxxxx模型决策路径分布',
  output_bt3_wf,
  output_bt3_wf_ratio,
  'xxxxxxxxxxxxx模型决策路径分布',
  output_jxl_wf,
  output_jxl_wf_ratio,
  '模型贡献',
  output_model_status_count,
  '模型贡献占总申请量比例',
  output_model_status_prop,
  'Node:分流情况',
  output_node
)
# excelpath,sheetname,hl_locs,pp_locs
today <- format(Sys.time(), "%b%d")
xlpath <- paste("xxxx模型监控", today, ".xlsx")
sheetname <- '模型监控'
hl_locs <- list(
  list(1,1),
  list(5,1),
  list(12,1),
  list(37,1),
  list(41,1),
  list(66,1),
  list(70,1),
  list(83,1),
  list(96,1),
  list(103,1),
  list(110,1)
  )
pp_locs <- list(
  list(7,1:8),
  list(10,1:8),
  list(26,2:8),
  list(27,2:8),
  list(28,2:8),
  list(29,2:8),
  list(30,2:8),
  list(31,2:8),
  list(32,2:8),
  list(33,2:8),
  list(34,2:8),
  list(35,2:8),
  list(55,2:8),
  list(56,2:8),
  list(57,2:8),
  list(58,2:8),
  list(59,2:8),
  list(60,2:8),
  list(61,2:8),
  list(62,2:8),
  list(63,2:8),
  list(64,2:8),
  list(78,2:8),
  list(79,2:8),
  list(80,2:8),
  list(81,2:8),
  list(91,2:8),
  list(92,2:8),
  list(93,2:8),
  list(94,2:8),
  list(105,2:8),
  list(106,2:8),
  list(107,2:8),
  list(108,2:8)
  )
# 输出excel
to_excel(output,xlpath,sheetname,hl_locs=hl_locs, pp_locs=pp_locs)
#===========================================================#
#=================cleancleancleanclean======================#
#===========================================================#
rm(list=ls())
gc()
