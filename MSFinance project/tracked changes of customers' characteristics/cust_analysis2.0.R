#================================================================= ==========#
#=========Set Working Directory & Import Module & Get Data ====================#
#================================================================= ==========#
# Set working directory
Setwd('~/custchar')
# required package: sltool (self-built), data.table, reshape2, ggplot2
Library(sltool)
Library(data.table)
Library(ggplot2)
#Run and take
# cust_table <- as.data.table(run.sql(exe = 1:6, get = 7, uid = "xxxxxxxxxxx", pwd = "xxxxxxxxxxxxxx"))
# get data
# cust_table <- run.sql(get = 7, uid = "xxxxxxxxxxxx", pwd = "xxxxxxxxxxxxxxxxxxxx")
#Data backup and overloading
# save(cust_table, file = '1704-1804.RData')
# save(cust_table, file = '1704-1804.done.RData')
# load('1704-1804.RData')
# load('1704-1804.done.RData')
#================================================================= ==========#
#==================== Data Processing: Application Status ======================#
#================================================================= ==========#
#Processing product code
Kd1 <- c('3112', '3113', '3121', '3123', '3131', '3132', '3329', '3333', '3334', '3335', ​​'3336', ' 3339', '3340', '3385', '3386', '3387', '3594', '3595', '3596')
Kd2 <- c('3119', '3120', '3325', '3326', '3328', '3338', '3383', '3384')
Kd3 <- c('3124', '3203', '3204', '3207', '3208', '3220', '3221')
Kd4 <- c('3209','3210','3388','3389','3390','3391','3392','3393','3394','3395','3396',' 3397', '3398', '3399', '3507', '3508', '3509', '3530', '3531', '3532')
Kd5 <- c('3551')
Kd6 <- c('3403','3404','3405')
Kd7 <- c('5504', '4104')
Cust_table[prod_cd %in% kd1,prod_kd := 'Snowball Beauty']
Cust_table[prod_cd %in% kd2,prod_kd := 'Snowball Beauty']
Cust_table[prod_cd %in% kd3,prod_kd := 'Channel America']
Cust_table[prod_cd %in% kd4,prod_kd := 'education']
Cust_table[prod_cd %in% kd5,prod_kd := 'car insurance']
Cust_table[prod_cd %in% kd6,prod_kd := 'renting']
Cust_table[prod_cd %in% kd7,prod_kd := 'other']
#------------------------------------------------- -----------
# rule_id_catProcessing
Hc_pboc <- c('HC043', 'HC044', 'HC044', 'HC069', 'HC069', 'HC069', 'HC069', 'HC069', 'HC069', 'HC069', 'HC070', ' HC071', 'HC071', 'HC103', 'HC104', 'HC105')
Hc_fr <- c('HC011','HC012', 'HC014', 'HC015', 'HC016', 'HC020', 'HC021', 'HC023', 'HC024', 'HC051', 'HC052', ' HC057', 'HC058', 'HC059 ', 'HC060', 'HC061', 'HC085', 'HC089', 'HC090', 'HC117', 'HC120', 'HC120', 'HC120')
Hc_bl <- c('HC027', 'HC028', 'HC029', 'HC030', 'HC067', 'HC091')
Hc_mbl <- c('HC005')
Hc_po <- c('TempPatch_beauty_different_province', 'HC001', 'HC002', 'HC003', 'HC004', 'HC009', 'HC017', 'HC018', 'HC019', 'HC026', 'HC038', ' HC053', 'HC054', 'HC055', 'HC066', 'HC084', 'HC092', 'HC093', 'HC094', 'HC096', 'HC098', 'HC099', 'HC100', 'HC101' , 'HC110', 'HC111', 'HC112', 'HC113', 'HC114', 'HC115', 'HC118', 'HC119', 'HC121')
Model <- c('SLBE016','SLBE006','SLBE012','SLBE004','SLBE001','SLBE009','SLBE013')
Hc_group = list(list(hc_pboc,'hc_pboc'),list(hc_fr,'hc_fr'),list(hc_bl,'hc_bl'),list(hc_mbl,'hc_mbl'),list(hc_po,'hc_po'),list (model, 'model'))
# hit HC is 1 and miss is 0
Cust_table[,hc := 0]
Cust_table[grepl('HC', rule_id_cat) == TRUE,hc := 1]
# If it is a certain class/model of HC, it is 1 if it is rejected, otherwise it is 0.
For (hc_kind in hc_group){
  f <- function(x) {
    For (hc in hc_kind[[1]]) {if (grepl(pattern = hc, x = x)) {return(1)}}
    Return(0)}
  Cust_table[,hc_kind[[2]] := lapply(cust_table[,rule_id_cat],f)]
}
#------------------------------------------------- -----------
# opstoce Operational Transfer Review
Cust_table[,opstoce := 0]
Cust_table[!is.na(ops2ce),opstoce := 1]
# human Artificial status (letter ce/operating ops_only)
Cust_table[!is.na(final_checker) == TRUE, human := 'ce']
Cust_table[!is.na(basic_checker) == TRUE & is.na(final_checker) == TRUE, human := 'ops_only']
# auto automatic state (/ strong rejection hc / model model)
Cust_table[model == 1 & human != 'ce', auto := 'model']
Cust_table[hc == 1, auto := 'hc']
# status Pass status (pass/cancel/reject)
Cust_table[,status := 'reject']
Cust_table[appl_status == 'N' | appl_status == 'A' | appl_status == 'J', status := 'pass']
Cust_table[appl_status == 'C', status := 'cancel']
# status1 Pass state 01 value (1: pass, 0: fail)
Cust_table[,status1 := 0]
Cust_table[status == 'pass',status1 := 1]
#------------------------------------------------- -----------
# appl_tm extracted into date, hour, year and month
Cust_table[,appl_date := as.Date(appl_tm)]
Cust_table[,appl_hour := as.POSIXlt(appl_tm)$hour]
Cust_table[,appl_month := format(appl_date, "%Y%m")]
#------------------------------------------------- -----------
# Delete used columns
Cust_table[, (c('appl_status','basic_checker','final_checker','rule_id_cat','appl_tm')) := NULL]
#------------------------------------------------- -----------
# Delete variables, clean up memory
Rm(kd1,kd2,kd3,kd4,kd5,kd6,kd7,hc_pboc,hc_fr,hc_bl,hc_mbl,hc_po,hc_group,hc_kind,model,f)
Gc()
#================================================================= ==========#
#=================== Data Pre-Processing: Customer Situation ======================#
#================================================================= ==========#
# gender_cd, edu_degree, soc_id, marg_status, house_cond, house_cond processing
Cust_table[,gender_cd := lapply(cust_table[,gender_cd],as.character)]
Cust_table[,marg_status := lapply(cust_table[,marg_status],as.character)]
f <- function(x) csv_rep(x,csvf='gender_cd.csv')
Cust_table[,gender_cd := lapply(cust_table[,gender_cd],f)]
f <- function(x) csv_rep(x,csvf='soc_id.csv')
Cust_table[,soc_id := lapply(cust_table[,soc_id],f)]
f <- function(x) csv_rep(x,csvf='edu_degree.csv')
Cust_table[, e
# ! Do not handle this will affect the subsequent operations, the reason is unknown!
#Processing data processed using lapply
cust_table[,gender_cd := as.character(gender_cd)]
cust_table[,edu_degree := as.character(edu_degree)]
cust_table[,soc_id := as.character(soc_id)]
cust_table[,marg_status := as.character(marg_status)]
cust_table[,house_cond := as.character(house_cond)]
cust_table[,biz_city_cd := as.character(biz_city_cd)]
cust_table[,currt_city := as.character(currt_city)]
cust_table[,unit_addr_city := as.character(unit_addr_city)]
-----------------------------------------------------------
# Province and city processing
Cust_table[, biz_city_cd := lapply(cust_table[,biz_city_cd],as.character)]
Cust_table[,currt_city := lapply(cust_table[,currt_city],as.character)]
Cust_table[,unit_addr_city := lapply(cust_table[,unit_addr_city],as.character)]
f <- function(x) csv_rep(x,csvf='citycode.csv')
Cust_table[, biz_city_name := lapply(cust_table[,biz_city_cd],f)]
Cust_table[,currt_city_name := lapply(cust_table[,currt_city],f)]
Cust_table[,unit_addr_city_name := lapply(cust_table[,unit_addr_city],f)]
f <- function(x) csv_rep(x,csvf='provcode.csv')
Cust_table[,currt_prov_name := lapply(cust_table[,substr(currt_city,0,2)],f)]
Cust_table[, biz_prov_name := lapply(cust_table[,substr(biz_city_cd,0,2)],f)]
Cust_table[,unit_addr_prov_name := lapply(cust_table[,substr(unit_addr_city,0,2)],f)]
# There are a small number of citycodes that cannot be converted to city names. The codelist is not updated.
# ! Do not handle this will affect the subsequent operations, the reason is unknown!
#Processing data processed using lapply
Cust_table[, biz_city_name := as.character(biz_city_name)]
Cust_table[, biz_prov_name := as.character(biz_prov_name)]
Cust_table[,currt_city_name := as.character(currt_city_name)]
Cust_table[,currt_prov_name := as.character(currt_prov_name)]
Cust_table[,unit_addr_city_name := as.character(unit_addr_city_name)]
Cust_table[,unit_addr_prov_name := as.character(unit_addr_prov_name)]
#================================================================= ==========#
#==================== Data Preprocessing: Risk Performance ======================#
#================================================================= ==========#
#Handling the contents of the month part
# mer_status
f <- function(x) csv_rep(x,csvf='mer_status.csv')
Cust_table[,mer_status := lapply(cust_table[,mer_status],as.character)]
Cust_table[,mer_status := lapply(cust_table[,mer_status],f)]
Cust_table[,mer_status := as.character(mer_status)]
# Whether it is cross-selling marketing
Cust_table[,isxsell := 0]
Cust_table[xprod_cd == 1401, isxsell := 1]
# Whether it is the base date in the cross contract
Obdate <- as.Date('2018-05-01')
# Whether in a cross-selling contract
Cust_table[,effc_start_dt := as.Date(effc_start_dt)]
Cust_table[,effc_end_dt := as.Date(effc_end_dt)]
Cust_table[,inxsell := 0]
Cust_table[isxsell == 1 & obdate > effc_start_dt & obdate < effc_end_dt ,inxsell := 1]
Cust_table[, (c('xprod_cd')) := NULL]
# curt_dpd, max_dpd missing value
Cust_table[is.na(curt_dpd),curt_dpd := 0]
Cust_table[is.na(max_dpd),max_dpd := 0]
#fpd10
Cust_table[,fpd10 := 0]
Cust_table[fpd > 10, fpd10 := 1]
# Is it in transit?
Cust_table[,con_way := 0]
Cust_table[contra_status %in% c ('Not overdue in activity', 'overdue in activity'), con_way := 1]
#Overdue
Cust_table[,con_overdue := 0]
Cust_table[contra_status %in% c ('overdue auto-terminate unresolved', 'overdue in activity', 'overdue auto-terminate settlement'), con_overdue := 1]
#settled
Cust_table[,con_notclear := 1]
Cust_table[contra_status %in% c('expiration settlement', 'return termination settlement', 'prepayment repayment'), con_notclear := 0]
Cust_table[, (c('contra_status')) := NULL]
# 1-12 period of overdue days
Cust_table[,overdue_times := 0]
Cust_table[,overdue_first := 0]
Cust_table[twpd > 0 ,overdue_times := overdue_times+1]
Cust_table[twpd > 0 ,overdue_first := 12]
Cust_table[evpd> 0, overdue_times := overdue_times+1]
Cust_table[evpd> 0, overdue_first := 11]
Cust_table[tnpd> 0, overdue_times := overdue_times+1]
Cust_table[tnpd> 0, overdue_first := 10]
Cust_table[nnpd> 0, overdue_times := overdue_times+1]
Cust_table[nnpd> 0, overdue_first := 9]
Cust_table[etpd> 0, overdue_times := overdue_times+1]
Cust_table[etpd> 0, overdue_first := 8]
Cust_table[svpd> 0, overdue_times := overdue_times+1]
Cust_table[svpd> 0, overdue_first := 7]
Cust_table[sxpd> 0, overdue_times := overdue_times+1]
Cust_table[sxpd> 0, overdue_first := 6]
Cust_table[fvpd> 0, overdue_times := overdue_times+1]
Cust_table[fvpd> 0, overdue_first := 5]
Cust_table[qpd> 0, overdue_times := overdue_times+1]
Cust_table[qpd> 0, overdue_first := 4]
Cust_table[tpd> 0, overdue_times := overdue_times+1]
Cust_table[tpd> 0, overdue_first := 3]
Cust_table[spd> 0, overdue_times := overdue_times+1]
Cust_table[spd> 0, overdue_first := 2]
Cust_table[fpd> 0, overdue_times := overdue_times+1]
Cust_table[fpd> 0, overdue_first := 1]
# Add the current in-transit contract amount, press union_id
Con_way <- cust_table[,.(sum(con_way)),by=union_id]
Names(con_way) <- c('union_id1','way_count')
Cust_table <- data.table(dplyr::left_join(cust_table,con_way,c("union_id" = "union_id1")))
# Add overdue contract amount, press union_id
Con_overdue <- cust_table[,.(sum(con_overdue)),by=union_id]
Names(con_overdue) <- c('union_id1','overdue_count')
Cust_table <- data.table(dplyr::left_join(cust_table,con_overdue,c("union_id" = "union_id1")))
# Add unresolved contract amount, press union_id
Con_notclear <- cust_table[,.(sum(con_notclear)),by=union_id]
Names(con_notclear) <- c('union_id1','notclear_count')
Cust_table <- data.table(dplyr::left_join(cust_table,con_notclear,c("union_id" = "union_id1")))
# Delete variables, clean up memory
Rm(f,obdate,con_notclear,con_overdue,con_way)
Gc()
#================================================================= ==========#
#==================== Data Preprocessing: Discretization ========================#
#================================================================= ==========#
# Discretization, the value means less than or equal to this number, but 0 means exactly 0, 99 means to infinity, -1 means NA (data.table or NULL)
# appl_lim
f <- function(x) cut(x,c(0,10000,20000,30000,40000,50000),labels = c(1,2,3,4,5))
Cust_table[, cut_appl_lim := lapply(cust_table[,appl_lim],f)]
# down_pay_amt
f <- function(x) cut(x,c(-1,0,5000,10000,20000,999999),labels = c(0,0.5,1,2,99))
Cust_table[, cut_down_pay_amt := lapply(cust_table[,down_pay_amt],f)]
# down_pay_pct
f <- function(x) cut(x,c(-1,0,0.2,0.4,0.6,1),labels = c(0,0.2,0.4,0.6,1))
Cust_table[, cut_down_pay_pct := lapply(cust_table[,down_pay_pct],f)]
# tempvector6_r
Cust_table[,tempvector6_r := as.numeric(tempvector6_r)]
f <- function(x) cut(x,c(0,0.0034,0.0055,0.0079,0.0108,0.0146,0.0197,0.0265,0.0382,0.0633,1),
                     Labels = c (0.0034, 0.0055, 0.0079, 0.0108, 0.0146, 0.0197, 0.0265, 0.0382, 0.0633, 1))
Cust_table[, cut_tempvector6_r := lapply(cust_table[,tempvector6_r],f)]
# pos_jxl_model3_jxlposscorev3
Cust_table[,pos_jxl_model3_jxlposscorev3 := as.numeric(pos_jxl_model3_jxlposscorev3)]
f <- function(x) cut(x,c(0,0.918,0.946,0.964,0.974,0.982,0.988,0.991,0.993,0.995,1),
                     Labels = c (0.918, 0.946, 0.964, 0.974, 0.982, 0.988, 0.991, 0.993, 0.995, 1)
Cust_table[, cut_pos_jxl_model3_jxlposscorev3 := lapply(cust_table[,pos_jxl_model3_jxlposscorev3],f)]
# mo_earn
f <- function(x) cut(x,c(0,5000,10000,20000,50000,9999999),labels = c(0.5,1,2,5,99))
Cust_table[, cut_mo_earn := lapply(cust_table[,mo_earn],f)]
# oth_earn
f <- function(x) cut(x,c(-1,0,5000,10000,9999999),labels = c(0,0.5,1,99))
Cust_table[, cut_oth_earn := lapply(cust_table[,oth_earn],f)]
# oth_loan
f <- function(x) cut(x,c(-1,0,5000,9999999),labels = c(0,0.5,99))
Cust_table[, cut_oth_loan := lapply(cust_table[,oth_loan],f)]
# work_life
f <- function(x) cut(x,c(-1,0,2,5,10,99),labels = c(0,2,5,10,99))
Cust_table[, cut_work_life := lapply(cust_table[,work_life],f)]
# Delete variables, clean up memory
Rm(f)
Gc()
#------------------------------------------------- -----------
# ! Do not handle this will affect the subsequent operations, the reason is unknown!
Cust_table[,cut_appl_lim := unlist(cut_appl_lim)]
Cust_table[,cut_down_pay_amt := unlist(cut_down_pay_amt)]
Cust_table[,cut_down_pay_pct := unlist(cut_down_pay_pct)]
Cust_table[,cut_tempvector6_r := unlist(cut_tempvector6_r)]
Cust_table[,cut_pos_jxl_model3_jxlposscorev3 := unlist(cut_pos_jxl_model3_jxlposscorev3)]
Cust_table[,cut_mo_earn := unlist(cut_mo_earn)]
Cust_table[,cut_oth_earn := unlist(cut_oth_earn)]
Cust_table[,cut_oth_loan := unlist(cut_oth_loan)]
Cust_table[,cut_work_life := unlist(cut_work_life)]
#================================================================= ==========#
#=========main: Analysis and visualization of customer groups by date and product classification ===========#
#=========todo:title, the y-axis adjustment of the average, color...==-===============#
#================================================================= ==========#
# t1-t4: View indicators by day, limit the range of data to be displayed (date, product category)
# You can adjust the values ​​of sdate and edate to see the desired time period.
Sdate <- as.Date('2017-01-01')
Edate <- Sys.Date()
Prod_kds <- c('snowball beauty', 'snowball beauty', 'channel beauty', 'education', 'car insurance', 'rental')
T0 <- cust_table[appl_date >= sdate & appl_date <= edate & (prod_kd %in% prod_kds),]
#------------------------------------------------- -------------
# Daily cancellation/rejection/passing amount, daily female/male application volume
T2.1 <- t0[, .N, by = .(appl_date,status)][,status :=(factor(status,levels = c('cancel','reject','pass')))]
T2.2 <- t0[, .N, by = .(appl_date,gender_cd)]
P1 <- ggplot(data=t2.1, aes(appl_date, N, fill=status))+geom_bar(stat='identity')
P2 <- ggplot(data=t2.2, aes(appl_date, N, fill=gender_cd))+geom_bar(stat='identity',position = "fill")
Multiplot(p1,p2)
#------------------------------------------------- ----------
T3.1 <- t0[auto != 'ce' & auto != 'ops_only' , .N, by = .(appl_date,status)][,status :=(factor(status,levels = c('cancel' , 'reject', 'pass')))]
T3.2 <- t0[auto == 'ce', .N, by = .(appl_date,status)][,status :=(factor(status,levels = c('cancel','reject','pass ')))]
T3.3 <- t0[auto == 'ops_only', .N, by = .(appl_date,status)][,status :=(factor(status,levels = c('cancel','reject','pass ')))]
T3.4 <- t0[ops2ce == TRUE, .N, by = .(appl_date,status)][,status :=(factor(status,levels = c('cancel','reject','pass') ))]
#Automatic/trust review/operation/operational transfer review ratio
P1 <- ggplot(data=t3.1, aes(appl_date, N, fill=status))+geom_bar(stat='identity',position = "fill")
P2 <- ggplot(data=t3.2, aes(appl_date, N, fill=status))+geom_bar(stat='identity',position = "fill")
P3 <- ggplot(data=t3.3, aes(appl_date, N, fill=status))+geom_bar(stat='identity',position = "fill")
P4 <- ggplot(data=t3.4, aes(appl_date, N, fill=status))+geom_bar(stat='identity',position = "fill")
Multiplot(p1,p2,p3,p4)
#------------------------------------------------- ---------------------
# t5: View by month according to soc_id, edu_degree, marg_status, house_cond, cut_mo_earn, cut_oth_earn, cut_oth_loan, cut_work_life
#Application volume, pass rate, average application amount, limit the range of data to be displayed (date, product category)
Sdate <- as.Date('2017-01-01')
Edate <- Sys.Date()
Prod_kds <- c('snowball beauty', 'snowball beauty', 'channel beauty', 'education', 'car insurance', 'rental')
T5 <- cust_table[appl_date >= sdate & appl_date <= edate & (prod_kd %in% prod_kds),
                 (appl_month, soc_id, edu_degree, marg_status, house_cond, cut_mo_earn,
                   Cut_oth_earn, cut_oth_loan, cut_work_life, status1, appl_lim)]
T5.x <- list()
T5.x[[1]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,soc_id)]
T5.x[[2]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,edu_degree)]
T5.x[[3]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,marg_status)]
T5.x[[4]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,house_cond)]
T5.x[[5]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,cut_mo_earn)]
T5.x[[6]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,cut_oth_earn)]
T5.x[[7]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,cut_oth_loan)]
T5.x[[8]] <- t5[, .(.N,mean(status1),mean(appl_lim)), by = list(appl_month,cut_work_life)]
Names(t5.x[[1]])[2] <- 'var'
Names(t5.x[[2]])[2] <- 'var'
Names(t5.x[[3]])[2] <- 'var'
Names(t5.x[[4]])[2] <- 'var'
Names(t5.x[[5]])[2] <- 'var'
Names(t5.x[[6]])[2] <- 'var'
Names(t5.x[[7]])[2] <- 'var'
Names(t5.x[[8]])[2] <- 'var'
f <- function(data) {
  P1 <- ggplot(data=data, aes(appl_month, N, fill=var))+geom_bar(stat='identity',position = "dodge")
  P2 <- ggplot(data=data, aes(appl_month, V2, fill=var))+geom_bar(stat='identity',position = "dodge")
  P3 <- ggplot(data=data, aes(appl_month, V3, fill=var))+geom_bar(stat='identity',position = "dodge")
  Multiplot(p1, p2, p3)
}
f(t5.x[[1]])
f(t5.x[[2]])
f(t5.x[[3]])
f(t5.x[[4]])
f(t5.x[[5]])
f(t5.x[[6]])
f(t5.x[[7]])
f(t5.x[[8]])
#------------------------------------------------- -------------------------------------------------- -
# t6: View metrics by day/month, limit the range of data to be displayed (date, product category)
Sdate <- as.Date('2017-01-01')
Edate <- Sys.Date()
Prod_kds <- c('snowball beauty', 'snowball beauty', 'channel beauty', 'education', 'car insurance', 'rental')
T6 <- cust_table[appl_date >= sdate & appl_date <= edate & (prod_kd %in% prod_kds),
                 (appl_date, appl_month, status1, cut_appl_lim, cut_down_pay_amt, cut_down_pay_pct, appl_hour)]
T6.1 <- t6[,mean(status1),by=appl_date]
T6.1_m <- t6[,mean(status1),by=appl_month]
T6.2 <- t6[,.(.N),by=list(appl_date,cut_appl_lim)]
T6.2_m <- t6[,.(.N),by=list(appl_month,cut_appl_lim)]
T6.3 <- t6[,.(.N),by=list(appl_date,cut_down_pay_amt)]
T6.3_m <- t6[,.(.N),by=list(appl_month,cut_down_pay_amt)]
T6.4 <- t6[,.(.N),by=list(appl_date,cut_down_pay_pct)]
T6.4_m <- t6[,.(.N),by=list(appl_month,cut_down_pay_pct)]
T6.5 <- t6[,.(.N),by=list(appl_date,appl_hour)]
T6.5_m <- t6[,.(.N),by=list(appl_month,appl_hour)]
Names(t6.2 )[2] <- 'var'
Names(t6.2_m)[2] <- 'var'
Names(t6.3 )[2] <- 'var'
Names(t6.3_m)[2] <- 'var'
Names(t6.4 )[2] <- 'var'
Names(t6.4_m)[2] <- 'var'
Names(t6.5 )[2] <- 'var'
Names(t6.5_m)[2] <- 'var'
P1 <- ggplot(data=t6.1, aes(appl_date, V1))+geom_bar(stat='identity')
P2 <- ggplot(data=t6.1_m, aes(appl_month, V1))+geom_bar(stat='identity')
Multiplot(p1, p2)
P1 <- ggplot(data=t6.2, aes(appl_date, N, fill=var))+geom_bar(stat='identity',position = "dodge")
P2 <- ggplot(data=t6.2_m, aes(appl_month, N, fill=var))+geom_bar(stat='identity',position = "dodge")
Multiplot(p1, p2)
P1 <- ggplot(data=t6.3, aes(appl_date, N, fill=var))+geom_bar(stat='identity',position = "dodge")
P2 <- ggplot(data=t6.3_m, aes(appl_month, N, fill=var))+geom_bar(stat='identity',position = "dodge")
Multiplot(p1, p2)
P1 <- ggplot(data=t6.4, aes(appl_date, N, fill=var))+geom_bar(stat='identity',position = "dodge")
P2 <- ggplot(data=t6.4_m, aes(appl_month, N, fill=var))+geom_bar(stat='identity',position = "dodge")
Multiplot(p1, p2)
P1 <- ggplot(data=t6.4, aes(appl_date, N, fill=var))+geom_bar(stat='identity',position = "dodge")
P2 <- ggplot(data=t6.4_m, aes(appl_month, N, fill=var))+geom_bar(stat='identity',position = "dodge")
Multiplot(p1, p2)
# Clean up memory
Rm(edate,f,p1,p2,p3,p4,prod_kds,sdate,t0,t2.1,t2.2,t3.1,t3.2,t3.3,t3.4,t4.1,t4. 2, t4.3, t5, t5.x,
   T6, t6.1, t6.1_m, t6.2, t6.2_m, t6.3, t6.3_m, t6.4, t6.4_m, t6.5, t6.5_m)
Gc()
