#===========================================================#
#=========设定工作目录&导入模块&取得数据====================#
#===========================================================#
# 设定工作目录
setwd('~/客群分析')
# 需要的package:sltool(自建),data.table,reshape2,ggplot2
library(sltool)
library(data.table)
library(ggplot2)
# 运行并取数
# cust_table <- as.data.table(run.sql(exe = 1:6, get = 7, uid = "xxxxxxxxxxx", pwd = "xxxxxxxxxxxxxx"))
# 仅取数
# cust_table <- run.sql(get = 7, uid = "xxxxxxxxxxxx", pwd = "xxxxxxxxxxxxxxxxxxxx")
# 数据备份和重载
# save(cust_table, file = '1704-1804.RData')
# save(cust_table, file = '1704-1804.done.RData')
# load('1704-1804.RData')
# load('1704-1804.done.RData')
#===========================================================#
#===================数据预处理:申请情况=====================#
#===========================================================#
# 处理产品代码
kd1 <- c('3112','3113','3121','3123','3131','3132','3329','3333','3334','3335','3336','3339','3340','3385','3386','3387','3594','3595','3596')
kd2 <- c('3119','3120','3325','3326','3328','3338','3383','3384')
kd3 <- c('3124','3203','3204','3207','3208','3220','3221')
kd4 <- c('3209','3210','3388','3389','3390','3391','3392','3393','3394','3395','3396','3397','3398','3399','3507','3508','3509','3530','3531','3532')
kd5 <- c('3551')
kd6 <- c('3403','3404','3405')
kd7 <- c('5504','4104')
cust_table[prod_cd %in% kd1,prod_kd := '雪球医美']
cust_table[prod_cd %in% kd2,prod_kd := '雪球生美']
cust_table[prod_cd %in% kd3,prod_kd := '渠道美业']
cust_table[prod_cd %in% kd4,prod_kd := '教育']
cust_table[prod_cd %in% kd5,prod_kd := '车险']
cust_table[prod_cd %in% kd6,prod_kd := '租房']
cust_table[prod_cd %in% kd7,prod_kd := '其他']
#------------------------------------------------------------
# rule_id_cat处理
hc_pboc <- c('HC043','HC044','HC044','HC069','HC069','HC069','HC069','HC069','HC069','HC069','HC070','HC071','HC071','HC103','HC104','HC105')
hc_fr <- c('HC011','HC012','HC014','HC015','HC016','HC020','HC021','HC023','HC024','HC051','HC052','HC057','HC058','HC059 ','HC060','HC061','HC085','HC089','HC090','HC117','HC120','HC120','HC120')
hc_bl <- c('HC027','HC028','HC029','HC030','HC067','HC091')
hc_mbl <- c('HC005')
hc_po <- c('TempPatch_beauty_different_province','HC001','HC002','HC003','HC004','HC009','HC017','HC018','HC019','HC026','HC038','HC053','HC054','HC055','HC066','HC084','HC092','HC093','HC094','HC096','HC098','HC099','HC100','HC101','HC110','HC111','HC112','HC113','HC114','HC115','HC118','HC119','HC121')
model <- c('SLBE016','SLBE006','SLBE012','SLBE004','SLBE001','SLBE009','SLBE013')
hc_group = list(list(hc_pboc,'hc_pboc'),list(hc_fr,'hc_fr'),list(hc_bl,'hc_bl'),list(hc_mbl,'hc_mbl'),list(hc_po,'hc_po'),list(model,'model'))
# 命中HC则为1,不命中则为0
cust_table[,hc := 0]
cust_table[grepl('HC', rule_id_cat) == TRUE,hc := 1]
# 如果是HC的某一类/模型通过拒绝,则为1,否则为0
for (hc_kind in hc_group){
  f <- function(x) {
    for (hc in hc_kind[[1]]) {if (grepl(pattern = hc, x = x)) {return(1)}}
    return(0)}
  cust_table[,hc_kind[[2]] := lapply(cust_table[,rule_id_cat],f)]
}
#------------------------------------------------------------
# opstoce 运营转信审
cust_table[,opstoce := 0]
cust_table[!is.na(ops2ce),opstoce := 1]
# human 人工状态(信审ce/仅运营ops_only)
cust_table[!is.na(final_checker) == TRUE, human := 'ce']
cust_table[!is.na(basic_checker) == TRUE & is.na(final_checker) == TRUE, human := 'ops_only']
# auto 自动状态(/强拒绝hc/模型model)
cust_table[model == 1 & human != 'ce', auto := 'model']
cust_table[hc == 1, auto := 'hc']
# status 通过状态(通过pass/取消cancel/拒绝reject)
cust_table[,status := 'reject']
cust_table[appl_status == 'N' | appl_status == 'A' | appl_status == 'J', status := 'pass']
cust_table[appl_status == 'C', status := 'cancel']
# status1 通过状态01值(1:通过,0:未通过)
cust_table[,status1 := 0]
cust_table[status == 'pass',status1 := 1]
#------------------------------------------------------------
# appl_tm提取成日期,小时,年月
cust_table[,appl_date := as.Date(appl_tm)]
cust_table[,appl_hour := as.POSIXlt(appl_tm)$hour]
cust_table[,appl_month := format(appl_date, "%Y%m")]
#------------------------------------------------------------
# 删除已使用列
cust_table[, (c('appl_status','basic_checker','final_checker','rule_id_cat','appl_tm')) := NULL]
#------------------------------------------------------------
# 删除变量,清理内存
rm(kd1,kd2,kd3,kd4,kd5,kd6,kd7,hc_pboc,hc_fr,hc_bl,hc_mbl,hc_po,hc_group,hc_kind,model,f)
gc()
#===========================================================#
#===================数据预处理:客户情况=====================#
#===========================================================#
# gender_cd,edu_degree,soc_id,marg_status,house_cond,house_cond的处理
cust_table[,gender_cd := lapply(cust_table[,gender_cd],as.character)]
cust_table[,marg_status := lapply(cust_table[,marg_status],as.character)]
f <- function(x) csv_rep(x,csvf='gender_cd.csv')
cust_table[,gender_cd := lapply(cust_table[,gender_cd],f)]
f <- function(x) csv_rep(x,csvf='soc_id.csv')
cust_table[,soc_id := lapply(cust_table[,soc_id],f)]
f <- function(x) csv_rep(x,csvf='edu_degree.csv')
cust_table[, edu_degree := .(lapply(cust_table[,edu_degree],f))]
f <- function(x) csv_rep(x,csvf='marg_status.csv')
cust_table[,marg_status := lapply(cust_table[,marg_status],f)]
f <- function(x) csv_rep(x,csvf='house_cond.csv')
cust_table[,house_cond := lapply(cust_table[,house_cond],f)]
# !不这样处理会影响之后的操作,原因未知!
# 处理使用lapply处理过的数据
cust_table[,gender_cd := as.character(gender_cd)]
cust_table[,edu_degree := as.character(edu_degree)]
cust_table[,soc_id := as.character(soc_id)]
cust_table[,marg_status := as.character(marg_status)]
cust_table[,house_cond := as.character(house_cond)]
cust_table[,biz_city_cd := as.character(biz_city_cd)]
cust_table[,currt_city := as.character(currt_city)]
cust_table[,unit_addr_city := as.character(unit_addr_city)]
-----------------------------------------------------------
# 省份和城市处理
cust_table[,biz_city_cd := lapply(cust_table[,biz_city_cd],as.character)]
cust_table[,currt_city := lapply(cust_table[,currt_city],as.character)]
cust_table[,unit_addr_city := lapply(cust_table[,unit_addr_city],as.character)]
f <- function(x) csv_rep(x,csvf='citycode.csv')
cust_table[,biz_city_name := lapply(cust_table[,biz_city_cd],f)]
cust_table[,currt_city_name := lapply(cust_table[,currt_city],f)]
cust_table[,unit_addr_city_name := lapply(cust_table[,unit_addr_city],f)]
f <- function(x) csv_rep(x,csvf='provcode.csv')
cust_table[,currt_prov_name := lapply(cust_table[,substr(currt_city,0,2)],f)]
cust_table[,biz_prov_name := lapply(cust_table[,substr(biz_city_cd,0,2)],f)]
cust_table[,unit_addr_prov_name := lapply(cust_table[,substr(unit_addr_city,0,2)],f)]
# 有少量无法转换为城市名的citycode,系编码表未更新导致
# !不这样处理会影响之后的操作,原因未知!
# 处理使用lapply处理过的数据
cust_table[,biz_city_name := as.character(biz_city_name)]
cust_table[,biz_prov_name := as.character(biz_prov_name)]
cust_table[,currt_city_name := as.character(currt_city_name)]
cust_table[,currt_prov_name := as.character(currt_prov_name)]
cust_table[,unit_addr_city_name := as.character(unit_addr_city_name)]
cust_table[,unit_addr_prov_name := as.character(unit_addr_prov_name)]
#===========================================================#
#===================数据预处理:风险表现=====================#
#===========================================================#
# 处理maoth部分的内容
# mer_status
f <- function(x) csv_rep(x,csvf='mer_status.csv')
cust_table[,mer_status := lapply(cust_table[,mer_status],as.character)]
cust_table[,mer_status := lapply(cust_table[,mer_status],f)]
cust_table[,mer_status := as.character(mer_status)]
# 是否被交叉销售营销
cust_table[,isxsell := 0]
cust_table[xprod_cd == 1401,isxsell := 1]
# 是否在交叉合同中的基准日期
obdate <- as.Date('2018-05-01')
# 是否在交叉销售合同中
cust_table[,effc_start_dt := as.Date(effc_start_dt)]
cust_table[,effc_end_dt := as.Date(effc_end_dt)]
cust_table[,inxsell := 0]
cust_table[isxsell == 1 & obdate > effc_start_dt & obdate < effc_end_dt ,inxsell := 1]
cust_table[, (c('xprod_cd')) := NULL]
# curt_dpd,max_dpd缺失值
cust_table[is.na(curt_dpd),curt_dpd := 0]
cust_table[is.na(max_dpd),max_dpd := 0]
# 是否fpd10
cust_table[,fpd10 := 0]
cust_table[fpd > 10, fpd10 := 1]
# 是否在途
cust_table[,con_way := 0]
cust_table[contra_status %in% c('活动中未逾期','活动中逾期'),con_way := 1]
# 是否逾期
cust_table[,con_overdue := 0]
cust_table[contra_status %in% c('逾期自动终止未结清','活动中逾期','逾期自动终止结清'),con_overdue := 1]
# 是否未结清
cust_table[,con_notclear := 1]
cust_table[contra_status %in% c('到期结清','退货终止结清','提前还款结清'),con_notclear := 0]
cust_table[, (c('contra_status')) := NULL]
# 1-12期逾期天数
cust_table[,overdue_times := 0]
cust_table[,overdue_first := 0]
cust_table[twpd > 0 ,overdue_times := overdue_times+1]
cust_table[twpd > 0 ,overdue_first := 12]
cust_table[evpd> 0, overdue_times := overdue_times+1]
cust_table[evpd> 0, overdue_first := 11]
cust_table[tnpd> 0, overdue_times := overdue_times+1]
cust_table[tnpd> 0, overdue_first := 10]
cust_table[nnpd> 0, overdue_times := overdue_times+1]
cust_table[nnpd> 0, overdue_first := 9]
cust_table[etpd> 0, overdue_times := overdue_times+1]
cust_table[etpd> 0, overdue_first := 8]
cust_table[svpd> 0, overdue_times := overdue_times+1]
cust_table[svpd> 0, overdue_first := 7]
cust_table[sxpd> 0, overdue_times := overdue_times+1]
cust_table[sxpd> 0, overdue_first := 6]
cust_table[fvpd> 0, overdue_times := overdue_times+1]
cust_table[fvpd> 0, overdue_first := 5]
cust_table[qpd> 0, overdue_times := overdue_times+1]
cust_table[qpd> 0, overdue_first := 4]
cust_table[tpd> 0, overdue_times := overdue_times+1]
cust_table[tpd> 0, overdue_first := 3]
cust_table[spd> 0, overdue_times := overdue_times+1]
cust_table[spd> 0, overdue_first := 2]
cust_table[fpd> 0, overdue_times := overdue_times+1]
cust_table[fpd> 0, overdue_first := 1]
# 添加当前在途合同量,按union_id
con_way <- cust_table[,.(sum(con_way)),by=union_id]
names(con_way) <- c('union_id1','way_count')
cust_table <- data.table(dplyr::left_join(cust_table,con_way,c("union_id" = "union_id1")))
# 添加逾期合同量,按union_id
con_overdue <- cust_table[,.(sum(con_overdue)),by=union_id]
names(con_overdue) <- c('union_id1','overdue_count')
cust_table <- data.table(dplyr::left_join(cust_table,con_overdue,c("union_id" = "union_id1")))
# 添加未结清合同量,按union_id
con_notclear <- cust_table[,.(sum(con_notclear)),by=union_id]
names(con_notclear) <- c('union_id1','notclear_count')
cust_table <- data.table(dplyr::left_join(cust_table,con_notclear,c("union_id" = "union_id1")))
# 删除变量,清理内存
rm(f,obdate,con_notclear,con_overdue,con_way)
gc()
#===========================================================#
#===================数据预处理:离散化=======================#
#===========================================================#
# 离散化,值表示小于等于此数字,但是0表示刚好是0,99表示到无穷大,-1表示NA(data.table中还是NULL)
# appl_lim
f <- function(x) cut(x,c(0,10000,20000,30000,40000,50000),labels = c(1,2,3,4,5))
cust_table[, cut_appl_lim := lapply(cust_table[,appl_lim],f)]
# down_pay_amt
f <- function(x) cut(x,c(-1,0,5000,10000,20000,999999),labels = c(0,0.5,1,2,99))
cust_table[, cut_down_pay_amt := lapply(cust_table[,down_pay_amt],f)]
# down_pay_pct
f <- function(x) cut(x,c(-1,0,0.2,0.4,0.6,1),labels = c(0,0.2,0.4,0.6,1))
cust_table[, cut_down_pay_pct := lapply(cust_table[,down_pay_pct],f)]
# tempvector6_r
cust_table[,tempvector6_r := as.numeric(tempvector6_r)]
f <- function(x) cut(x,c(0,0.0034,0.0055,0.0079,0.0108,0.0146,0.0197,0.0265,0.0382,0.0633,1),
                     labels = c(0.0034,0.0055,0.0079,0.0108,0.0146,0.0197,0.0265,0.0382,0.0633,1))
cust_table[, cut_tempvector6_r := lapply(cust_table[,tempvector6_r],f)]
# pos_jxl_model3_jxlposscorev3
cust_table[,pos_jxl_model3_jxlposscorev3 := as.numeric(pos_jxl_model3_jxlposscorev3)]
f <- function(x) cut(x,c(0,0.918,0.946,0.964,0.974,0.982,0.988,0.991,0.993,0.995,1),
                     labels = c(0.918,0.946,0.964,0.974,0.982,0.988,0.991,0.993,0.995,1))
cust_table[, cut_pos_jxl_model3_jxlposscorev3 := lapply(cust_table[,pos_jxl_model3_jxlposscorev3],f)]
# mo_earn
f <- function(x) cut(x,c(0,5000,10000,20000,50000,9999999),labels = c(0.5,1,2,5,99))
cust_table[, cut_mo_earn := lapply(cust_table[,mo_earn],f)]
# oth_earn
f <- function(x) cut(x,c(-1,0,5000,10000,9999999),labels = c(0,0.5,1,99))
cust_table[, cut_oth_earn := lapply(cust_table[,oth_earn],f)]
# oth_loan
f <- function(x) cut(x,c(-1,0,5000,9999999),labels = c(0,0.5,99))
cust_table[, cut_oth_loan := lapply(cust_table[,oth_loan],f)]
# work_life
f <- function(x) cut(x,c(-1,0,2,5,10,99),labels = c(0,2,5,10,99))
cust_table[, cut_work_life := lapply(cust_table[,work_life],f)]
# 删除变量,清理内存
rm(f)
gc()
#------------------------------------------------------------
# !不这样处理会影响之后的操作,原因未知!
cust_table[,cut_appl_lim := unlist(cut_appl_lim)]
cust_table[,cut_down_pay_amt := unlist(cut_down_pay_amt)]
cust_table[,cut_down_pay_pct := unlist(cut_down_pay_pct)]
cust_table[,cut_tempvector6_r := unlist(cut_tempvector6_r)]
cust_table[,cut_pos_jxl_model3_jxlposscorev3 := unlist(cut_pos_jxl_model3_jxlposscorev3)]
cust_table[,cut_mo_earn := unlist(cut_mo_earn)]
cust_table[,cut_oth_earn := unlist(cut_oth_earn)]
cust_table[,cut_oth_loan := unlist(cut_oth_loan)]
cust_table[,cut_work_life := unlist(cut_work_life)]
#===========================================================#
#=========main:客群按日期和产品分类的分析和可视化===========#
#=========todo:title,均分的y轴调整,颜色...==-===============#
#===========================================================#
# t1-t4:按日查看指标,限制需要展示的数据范围(日期,产品类别)
# 可以调整sdate和edate的值,查看想要的时间段
sdate <- as.Date('2017-01-01')
edate <- Sys.Date()
prod_kds <- c('雪球医美','雪球生美','渠道美业','教育','车险','租房')
t0 <- cust_table[appl_date >= sdate & appl_date <= edate & (prod_kd %in% prod_kds),]
#--------------------------------------------------------------
# 每日取消/拒绝/通过的量,每日女性/男性申请量
t2.1 <- t0[, .N, by = .(appl_date,status)][,status :=(factor(status,levels = c('cancel','reject','pass')))]
t2.2 <- t0[, .N, by = .(appl_date,gender_cd)]
p1 <- ggplot(data=t2.1, aes(appl_date, N, fill=status))+geom_bar(stat='identity')
p2 <- ggplot(data=t2.2, aes(appl_date, N, fill=gender_cd))+geom_bar(stat='identity',position = "fill")
multiplot(p1,p2)
#-----------------------------------------------------------
t3.1 <- t0[auto != 'ce' & auto != 'ops_only' , .N, by = .(appl_date,status)][,status :=(factor(status,levels = c('cancel','reject','pass')))]
t3.2 <- t0[auto == 'ce', .N, by = .(appl_date,status)][,status :=(factor(status,levels = c('cancel','reject','pass')))]
t3.3 <- t0[auto == 'ops_only', .N, by = .(appl_date,status)][,status :=(factor(status,levels = c('cancel','reject','pass')))]
t3.4 <- t0[ops2ce == TRUE, .N, by = .(appl_date,status)][,status :=(factor(status,levels = c('cancel','reject','pass')))]
# 自动/信审/运营/运营转信审的比例
p1 <- ggplot(data=t3.1, aes(appl_date, N, fill=status))+geom_bar(stat='identity',position = "fill")
p2 <- ggplot(data=t3.2, aes(appl_date, N, fill=status))+geom_bar(stat='identity',position = "fill")
p3 <- ggplot(data=t3.3, aes(appl_date, N, fill=status))+geom_bar(stat='identity',position = "fill")
p4 <- ggplot(data=t3.4, aes(appl_date, N, fill=status))+geom_bar(stat='identity',position = "fill")
multiplot(p1,p2,p3,p4)
#----------------------------------------------------------------------
# t5:按月查看按soc_id,edu_degree,marg_status,house_cond,cut_mo_earn,cut_oth_earn,cut_oth_loan,cut_work_life分类的
# 申请量,通过率, 平均申请金额, 限制需要展示的数据范围(日期,产品类别)
sdate <- as.Date('2017-01-01')
edate <- Sys.Date()
prod_kds <- c('雪球医美','雪球生美','渠道美业','教育','车险','租房')
t5 <- cust_table[appl_date >= sdate & appl_date <= edate & (prod_kd %in% prod_kds),
                 .(appl_month,soc_id,edu_degree,marg_status,house_cond,cut_mo_earn,
                   cut_oth_earn,cut_oth_loan,cut_work_life,status1,appl_lim)]
t5.x <- list()
t5.x[[1]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,soc_id)]
t5.x[[2]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,edu_degree)]
t5.x[[3]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,marg_status)]
t5.x[[4]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,house_cond)]
t5.x[[5]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,cut_mo_earn)]
t5.x[[6]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,cut_oth_earn)]
t5.x[[7]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,cut_oth_loan)]
t5.x[[8]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,cut_work_life)]
names(t5.x[[1]])[2] <- 'var'
names(t5.x[[2]])[2] <- 'var'
names(t5.x[[3]])[2] <- 'var'
names(t5.x[[4]])[2] <- 'var'
names(t5.x[[5]])[2] <- 'var'
names(t5.x[[6]])[2] <- 'var'
names(t5.x[[7]])[2] <- 'var'
names(t5.x[[8]])[2] <- 'var'
f <- function(data) {
  p1 <- ggplot(data=data, aes(appl_month, N, fill=var))+geom_bar(stat='identity',position = "dodge")
  p2 <- ggplot(data=data, aes(appl_month, V2, fill=var))+geom_bar(stat='identity',position = "dodge")
  p3 <- ggplot(data=data, aes(appl_month, V3, fill=var))+geom_bar(stat='identity',position = "dodge")
  multiplot(p1, p2, p3)
}
f(t5.x[[1]])
f(t5.x[[2]])
f(t5.x[[3]])
f(t5.x[[4]])
f(t5.x[[5]])
f(t5.x[[6]])
f(t5.x[[7]])
f(t5.x[[8]])
#----------------------------------------------------------------------------------------------------
# t6:按日/月查看指标,限制需要展示的数据范围(日期,产品类别)
sdate <- as.Date('2017-01-01')
edate <- Sys.Date()
prod_kds <- c('雪球医美','雪球生美','渠道美业','教育','车险','租房')
t6 <- cust_table[appl_date >= sdate & appl_date <= edate & (prod_kd %in% prod_kds),
                 .(appl_date,appl_month,status1,cut_appl_lim,cut_down_pay_amt,cut_down_pay_pct,appl_hour)]
t6.1 <- t6[,mean(status1),by=appl_date]
t6.1_m <- t6[,mean(status1),by=appl_month]
t6.2 <- t6[,.(.N),by=list(appl_date,cut_appl_lim)]
t6.2_m <- t6[,.(.N),by=list(appl_month,cut_appl_lim)]
t6.3 <- t6[,.(.N),by=list(appl_date,cut_down_pay_amt)]
t6.3_m <- t6[,.(.N),by=list(appl_month,cut_down_pay_amt)]
t6.4 <- t6[,.(.N),by=list(appl_date,cut_down_pay_pct)]
t6.4_m <- t6[,.(.N),by=list(appl_month,cut_down_pay_pct)]
t6.5 <- t6[,.(.N),by=list(appl_date,appl_hour)]
t6.5_m <- t6[,.(.N),by=list(appl_month,appl_hour)]
names(t6.2  )[2] <- 'var'
names(t6.2_m)[2] <- 'var'
names(t6.3  )[2] <- 'var'
names(t6.3_m)[2] <- 'var'
names(t6.4  )[2] <- 'var'
names(t6.4_m)[2] <- 'var'
names(t6.5  )[2] <- 'var'
names(t6.5_m)[2] <- 'var'
p1 <- ggplot(data=t6.1, aes(appl_date, V1))+geom_bar(stat='identity')
p2 <- ggplot(data=t6.1_m, aes(appl_month, V1))+geom_bar(stat='identity')
multiplot(p1, p2)
p1 <- ggplot(data=t6.2, aes(appl_date, N, fill=var))+geom_bar(stat='identity',position = "dodge")
p2 <- ggplot(data=t6.2_m, aes(appl_month, N, fill=var))+geom_bar(stat='identity',position = "dodge")
multiplot(p1, p2)
p1 <- ggplot(data=t6.3, aes(appl_date, N, fill=var))+geom_bar(stat='identity',position = "dodge")
p2 <- ggplot(data=t6.3_m, aes(appl_month, N, fill=var))+geom_bar(stat='identity',position = "dodge")
multiplot(p1, p2)
p1 <- ggplot(data=t6.4, aes(appl_date, N, fill=var))+geom_bar(stat='identity',position = "dodge")
p2 <- ggplot(data=t6.4_m, aes(appl_month, N, fill=var))+geom_bar(stat='identity',position = "dodge")
multiplot(p1, p2)
p1 <- ggplot(data=t6.4, aes(appl_date, N, fill=var))+geom_bar(stat='identity',position = "dodge")
p2 <- ggplot(data=t6.4_m, aes(appl_month, N, fill=var))+geom_bar(stat='identity',position = "dodge")
multiplot(p1, p2)
# 清理内存
rm(edate,f,p1,p2,p3,p4,prod_kds,sdate,t0,t2.1,t2.2,t3.1,t3.2,t3.3,t3.4,t4.1,t4.2,t4.3,t5,t5.x,
   t6,t6.1,t6.1_m,t6.2,t6.2_m,t6.3,t6.3_m,t6.4,t6.4_m,t6.5,t6.5_m)
gc()