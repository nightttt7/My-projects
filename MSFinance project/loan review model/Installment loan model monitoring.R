#================================================================= ==========#
#==================== Description & parameter settings ========================== =#
#=========Setting the working directory & import module & getting data from the database ============#
#================================================================= ==========#
# Before running this file, you need to modify the three date parameters of "xxx model monitoring Hive" in HUE to run on Sunday. This file runs every Monday.
# If you are not running on Monday, please adjust """AND to_date(date_sub(now(),interval 1 day))""" in 2_main
#data.frame and data.table are mixed because the to_excel function is written for data.frame
# required package: sltool (self-built), data.table, reshape2
# Parameter 1: Number of weeks to display (from last week)
Week_show <- 6
Setwd('~/xxx model monitoring')
Library(sltool)
Library(data.table)
Table <- as.data.table(run.sql(exe = 1, get = 2, uid = "xxxxxxxxxxxxx", pwd = "xxxxxxxxx"))
#================================================================= ==========#
#============================== Data Preprocessing ========================== ===#
#================================================================= ==========#
## Add auto, model, status, date, week column
# auto Automatic status (automatic auto/operating only ops_only/trust review ce) (operation + credit review = operation only)
Table[,auto := 'auto']
Table[!is.na(final_checker) == TRUE, auto := 'ce']
Table[!is.na(basic_checker) == TRUE & is.na(final_checker) == TRUE, auto := 'ops_only']
# modelModel Status (hc/bt3/jxl)
Table[hc != 0,model := 'hc']
Table[hc == 0 & is.na(bt3_score) == FALSE,model := 'bt3']
Table[hc == 0 & is.na(bt3_score) == TRUE & is.na(jxlposscorev3) == FALSE, model := 'jxl']
# status Pass status (pass/cancel/reject)
Table[,status := 'reject']
Table[appl_status == 'N' | appl_status == 'A' | appl_status == 'J', status := 'pass']
Table[appl_status == 'C', status := 'cancel']
# data Application date
Table[,date := as.Date(appl_tm)]
# week Application Monday of the week, 2018-04-09 is the first week of the xxx automatic model online
Week_first <- as.Date('2018-04-09')
Week_today <- floor(as.numeric(Sys.Date()-as.Date('2018-04-09'))/7)
Table[, week := floor((date-week_first)/7)*7+week_first]
# Not in the order that needs to be displayed, the week column becomes NA. week_show see instructions & parameter settings
Table[week_today-floor((date-week_first)/7) > week_show, week := NA]
#================================================================= ==========#
#====================== Result Calculation ========================== ===#
#================================================================= ==========#
# add_index: Increase the index column to the left, for alignment of different tables
Add_index <- function(x) {
  Index <- rownames(x)
  x <- cbind(index,x)
}

# delete_last: Delete the last column for the case with the NA column
Delete_last <- function(x) {
  x <- x[,-ncol(x)]
  Return(x)
}

# 1 Calculate the application volume, automation rate

# Total application volume / weekly application volume
# Count, total_appl_count will be used later
Total_appl_count <- table[,.N]
# Count and convert to data.frame
Total_appl <- data.frame(table[,.N])
Names(total_appl) <- c('total application amount')
#per week count
Week_appl <- table[,.N,by=week]
# Transpose the week column to the column name
Week_appl <- reshape2::acast(week_appl, 1 ~ week, value.var = 'N')
# merge total_appl and week_appl
Output_appl <- cbind(total_appl,week_appl)
# Align different forms
Output_appl <- add_index(output_appl)
# Delete NA column
Output_appl <- delete_last(output_appl)

#ops_ratio
Total_ops_ratio <- data.frame(table[auto!='auto',.N]/total_appl_count)
Names(total_ops_ratio) <- c('total initial trial scale')
Week_ops_ratio <- table[auto!='auto',.N,by=week]
Week_ops_ratio <- reshape2::acast(week_ops_ratio, 1 ~ week,value.var = 'N')
Week_ops_ratio <- week_ops_ratio/week_appl
Output_ops_ratio <- cbind(total_ops_ratio,week_ops_ratio)
Output_ops_ratio <- add_index(output_ops_ratio)
Output_ops_ratio <- delete_last(output_ops_ratio)

#ce_ratio
Total_ce_ratio <- data.frame(table[auto=='ce',.N]/table[,.N])
Names(total_ce_ratio) <- c('total credit ratio')
Week_ce_ratio <- table[auto=='ce',.N,by=week]
Week_ce_ratio <- reshape2::acast(week_ce_ratio, 1 ~ week,value.var = 'N')
Week_ce_ratio <- week_ce_ratio/week_appl
Output_ce_ratio <- cbind(total_ce_ratio,week_ce_ratio)
Output_ce_ratio <- add_index(output_ce_ratio)
Output_ce_ratio <- delete_last(output_ce_ratio)

# 2 Calculate the score distribution

# bt3:xxxxxxxxx
# f: Split the score into segments by cut_p
f <- function(x) {
  Cut_p <- c (0, 0.0034, 0.0055, 0.0079, 0.0108, 0.0146, 0.0197, 0.0265, 0.0382, 0.0633, 1)
  Return(cut(x,cut_p,right=FALSE))
}
# Need to sort by score
Total_bt3_region <- data.frame(table[model=='bt3', .N, by = f(bt3_score)][order(f)])
Names(total_bt3_region) <- c('score interval', 'total amount')
Week_bt3_region <- table[model=='bt3', .N, by = .(f(bt3_score),week)]
# reshape2::acast has been automatically sorted by fractional segment, so the order is consistent with total_bt3_region
Week_bt3_region <- reshape2::acast(week_bt3_region, f ~ week, value.var = 'N')
Output_bt3_region <- cbind(total_bt3_region,week_bt3_region)
# NAValue changed to 0
Output_bt3_region[is.na(output_bt3_region)] <- 0
# Calculate weekly totals
Sum_t <- apply(output_bt3_region[,-1], 2, sum)
#get ratio
output_bt3_region_prop <- output_bt3_region[,-1]/rbind(sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t)
output_bt3_region_prop <- cbind(output_bt3_region[,1],output_bt3_region_prop)
Names(output_bt3_region_prop)[1:2] <- c('score interval', 'total scale')
Bt3_score_mean <- table[model=='bt3', mean(bt3_score), by = week]
Bt3_score_mean <- reshape2::acast(bt3_score_mean, 1 ~ week, value.var = 'V1')
Output_bt3_score_mean <- cbind(data.frame(table[model=='bt3', mean(bt3_score)]),bt3_score_mean)
Names(output_bt3_score_mean)[1] <- c('total average score')
Output_bt3_score_mean <- add_index(output_bt3_score_mean)
Output_bt3_region <- delete_last(output_bt3_region)
Output_bt3_region_prop <- delete_last(output_bt3_region_prop)
Output_bt3_score_mean <- delete_last(output_bt3_score_mean)

# jxl:xxxxxxxxx
f <- function(x) {
  Cut_p <- c(0,0.918,0.946,0.964,0.974,0.982,0.988,0.991,0.993,0.995,1)
  Return(cut(x,cut_p,right=FALSE))
}
Total_jxl_region <- data.frame(table[model=='jxl', .N, by = f(jxlposscorev3)][order(f)])
Total_jxl_region <- total_jxl_region[-11,]
Names(total_jxl_region) <- c('score interval', 'total amount')
Week_jxl_region <- table[model=='jxl', .N, by = .(f(jxlposscorev3),week)]
Week_jxl_region <- reshape2::acast(week_jxl_region, f ~ week, value.var = 'N')
Week_jxl_region <- week_jxl_region[-11,]
Output_jxl_region <- cbind(total_jxl_region,week_jxl_region)
Output_jxl_region[is.na(output_jxl_region)] <- 0
Sum_t <- apply(output_jxl_region[,-1], 2, sum)
Output_jxl_region_prop <- output_jxl_region[,-1]/rbind(sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t,sum_t)
Output_jxl_region_prop <- cbind(output_jxl_region[,1],output_jxl_region_prop)
Names(output_jxl_region_prop)[1:2] <- c('score interval', 'total scale')
Jxl_score_mean <- table[model=='jxl', mean(jxlposscorev3), by = week]
Jxl_score_mean <- reshape2::acast(jxl_score_mean, 1 ~ week, value.var = 'V1')
Output_jxl_score_mean <- cbind(data.frame(table[model=='jxl', mean(jxlposscorev3)]), jxl_score_mean)
Names(output_jxl_score_mean)[1] <- c('total average score')
Output_jxl_score_mean <- add_index(output_jxl_score_mean)
Output_jxl_region <- delete_last(output_jxl_region)
Output_jxl_region_prop <- delete_last(output_jxl_region_prop)
Output_jxl_score_mean <- delete_last(output_jxl_score_mean)

# 3 Calculation decision path

# bt3:xxxxxxxxx
Total_bt3_wf <- data.frame(table[model=='bt3',.N,by = workflowcode][order(workflowcode)])
Bt3_wf <- table[model=='bt3',.N,by = .(workflowcode,week)]
Bt3_wf <- reshape2::acast(bt3_wf, workflowcode ~ week, value.var = 'N')
Bt3_wf[is.na(bt3_wf)] <- 0
Output_bt3_wf <- cbind(total_bt3_wf,bt3_wf)
Names(output_bt3_wf)[1:2] <- c('path', 'total amount')
Sum_t <- apply(output_bt3_region[,-1], 2, sum)
Output_bt3_wf_ratio <- output_bt3_wf[,-1]/rbind(sum_t,sum_t,sum_t,sum_t)
Output_bt3_wf_ratio <- cbind(output_bt3_wf[,1],output_bt3_wf_ratio)
Names(output_bt3_wf_ratio)[1:2] <- c('path', 'total scale')
Output_bt3_wf <- delete_last(output_bt3_wf)
Output_bt3_wf_ratio <- delete_last(output_bt3_wf_ratio)

# jxl:xxxxxxx
Total_jxl_wf <- data.frame(table[model=='jxl',.N,by = workflowcode][order(workflowcode)])
Jxl_wf <- table[model=='jxl',.N,by = .(workflowcode,week)]
Jxl_wf <- reshape2::acast(jxl_wf, workflowcode ~ week,value.var = 'N')
Jxl_wf[is.na(jxl_wf)] <- 0
Output_jxl_wf <- cbind(total_jxl_wf,jxl_wf)
Names(output_jxl_wf)[1:2] <- c('path', 'total amount')
Sum_t <- apply(output_jxl_region[,-1], 2, sum)
Output_jxl_wf_ratio <- output_jxl_wf[,-1]/rbind(sum_t,sum_t,sum_t,sum_t)
Output_jxl_wf_ratio <- cbind(output_jxl_wf[,1],output_jxl_wf_ratio)
Names(output_jxl_wf_ratio)[1:2] <- c('path', 'total scale')
Output_jxl_wf <- delete_last(output_jxl_wf)
Output_jxl_wf_ratio <- delete_last(output_jxl_wf_ratio)

# 4 Calculate contribution rate
Pass/reject number of # xxx/xxxxxx
Model_status_count <- table[(model=='bt3' | model=='jxl') & (status=='pass' | status=='reject'), .N, by = .(week,model,status) ]
Model_status_count <- model_status_count[, model_status := paste(model,status,sep = "_")]
Model_status_count <- reshape2::acast(model_status_count, model_status ~ week, value.var = 'N')
Kind <- rownames(model_status_count)
Output_model_status_count <- data.frame(kind,model_status_count)
Output_model_status_count <- add_index(output_model_status_count)
Model_status_prop <- model_status_count/rbind(week_appl,week_appl,week_appl,week_appl)
Kind <- rownames(model_status_prop)
Output_model_status_prop <- data.frame(kind,model_status_prop)
Output_model_status_prop <- add_index(output_model_status_prop)
Output_model_status_count <- delete_last(output_model_status_count)
Output_model_status_prop <- delete_last(output_model_status_prop)

# 5 NodeofEdu
Table[model=='hc',node := 'hc_reject']
Table[model!='hc' & workflowcode=='WF_Reject',node := 'model_reject']
Table[workflowcode=='WF_Cancel',node := 'auto_cancel']
Table[auto=='ops_only' & status=='pass',node := 'ops_pass']
Table[auto=='ops_only' & status=='cancel',node := 'ops_cancel']
Table[auto=='ce' & status=='pass',node := 'ce_pass']
Table[auto=='ce' & status=='cancel',node := 'ce_cancel']
Table[auto=='ce' & status=='reject',node := 'ce_reject']
# A small number of applications cannot be classified, the reason is unknown
output_node <- table[,.N,by=.(week,node)]
output_node <- reshape2::acast(output_node, node ~ week,value.var = 'N')
kind <- rownames(output_node)
output_node <- data.frame(kind,output_node)
output_node <- add_index(output_node)
output_node <- delete_last(output_node)
#===========================================================#
#============================================================================================
#================================================================= ==========#
#数据入output
Output <- list(
  'Application amount',
  Output_appl,
  'Automation rate',
  Output_ops_ratio,
  Output_ce_ratio,
  'xxxxx model score distribution',
  Output_bt3_region,
  Output_bt3_region_prop,
  'xxxxx average score',
  Output_bt3_score_mean,
  'xxxxxxxxxxxxx model score distribution',
  Output_jxl_region,
  Output_jxl_region_prop,
  'xxxxxxxxxxxxx average score',
  Output_jxl_score_mean,
  'xxxxx model decision path distribution',
  Output_bt3_wf,
  Output_bt3_wf_ratio,
  'xxxxxxxxxxxxx model decision path distribution',
  Output_jxl_wf,
  Output_jxl_wf_ratio,
  'model contribution',
  Output_model_status_count,
  'Model contribution as a percentage of total applications',
  Output_model_status_prop,
  'Node: shunt situation',
  Output_node
)
# excelpath,sheetname,hl_locs,pp_locs
Today <- format(Sys.time(), "%b%d")
Xlpath <- paste("xxxx model monitoring", today, ".xlsx")
Sheetname <- 'model monitoring'
Hl_locs <- list(
  List(1,1),
  List(5,1),
  List(12,1),
  List(37,1),
  List(41,1),
  List(66,1),
  List(70,1),
  List(83,1),
  List(96,1),
  List(103,1),
  List(110,1)
  )
Pp_locs <- list(
  List(7,1:8),
  List(10,1:8),
  List(26,2:8),
  List(27,2:8),
  List(28,2:8),
  List(29,2:8),
  List(30,2:8),
  List(31,2:8),
  List(32,2:8),
  List(33,2:8),
  List(34,2:8),
  List(35,2:8),
  List(55,2:8),
  List(56,2:8),
  List(57,2:8),
  List(58,2:8),
  List(59,2:8),
  List(60,2:8),
  List(61,2:8),
  List(62,2:8),
  List(63,2:8),
  List(64,2:8),
  List(78,2:8),
  List(79,2:8),
  List(80,2:8),
  List(81,2:8),
  List(91,2:8),
  List(92,2:8),
  List(93,2:8),
  List(94,2:8),
  List(105,2:8),
  List(106,2:8),
  List(107,2:8),
  List(108,2:8)
  )
#输出excel
To_excel(output,xlpath,sheetname,hl_locs=hl_locs, pp_locs=pp_locs)
#================================================================= ==========#
#=================cleancleancleanclean=======================#
#================================================================= ==========#
Rm(list=ls())
Gc()
