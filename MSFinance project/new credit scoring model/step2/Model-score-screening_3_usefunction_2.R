#================================================================= ==================================================
# Set working directory
Setwd('~/model sub-screening 3/show 2')
# Import package
# required package: sltool (self-built), data.table, reshape2, ggplot2
Library(sltool)
Library(data.table)
Library(ggplot2)
# Import function
Source('model sub-screening 3_function 2.R')
# Import model_table exported from model partition filter 3_base (preprocessed)
Load('~/model sub-screening 3/model_table1.RData')
#================================================================= ==================================================
# !!Test passed!!
# Run the function get_new_table1
# new_table <- get_new_table3(model_table)
# Run function get_result
# result <- get_result3(new_table)
# if return fully list, the test is successful
# result
#================================================================= ==================================================
New_table <- get_new_table3(model_table,models=c('bt3_m','cup_m','cup_m','posjxl20_m','zhimamsxf_m'),
                            Sdate='2018-01-22',edate='2018-04-16',passstyle='both',
                            Th1=c(47,28,5),th2=c(38,12,3), th3=c(72,46,5),th4=c(65,15,3))
# ----all------
Result <- get_result3(new_table,ce_ar_pred = 0.6, continue_ar_pred = 0.99, cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5)
Result_core <- get_core(result)
Result_core
Result
# ----haspboc=1------
Result1 <- get_result3(new_table[haspboc == 1],ce_ar_pred = 0.6, continue_ar_pred = 0.99, cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5)
Result_core1 <- get_core(result1)
Result_core1
Result1
# result1[['model_cnt']]
# result1[['model_fpd10_pct']]
# ----haspboc=0------
Result0 <- get_result3_nopboc(new_table[haspboc == 0],ce_ar_pred = 0.6, continue_ar_pred = 0.99, cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5)
Result_core0 <- get_core1(result0)
Result_core0
Result0
# result0[['model_cnt']]
# result0[['model_fpd10_pct']]
#================================================================= ==================================================
# Using the instant sesame test to test the effectiveness of the new strategy
# old strategy effectiveness
Temp_table <- new_table[newwf != 'none' & !is.na(zhimamsxf_m),.(path,status,zhimamsxf_m)]
Temp_table[path=='model' & status == 'pass', path := 'pass']
Temp_table[path=='model' & status == 'reject', path := 'reject']
P1 <- ggplot(data = temp_table[,.(path,zhimamsxf_m)], aes(x=path, y=zhimamsxf_m))+geom_violin(trim = FALSE)
P2 <- ggplot(data = temp_table[,.(path,zhimamsxf_m)], aes(x=path, y=zhimamsxf_m))+geom_boxplot(width=0.2)
Rm(temp_table)
Gc()
# new strategy effectiveness
P3 <- ggplot(data = new_table[newwf != 'none' & !is.na(zhimamsxf_m),.(newwf,zhimamsxf_m)], aes(x=newwf, y=zhimamsxf_m))+geom_violin(trim = FALSE)
P4 <- ggplot(data = new_table[newwf != 'none' & !is.na(zhimamsxf_m),.(newwf,zhimamsxf_m)], aes(x=newwf, y=zhimamsxf_m))+geom_boxplot(width=0.2)
# plot
Multiplot(p1,p3,p2,p4,cols = 2)
# ----------------------
# (lb/mb) New strategy effectiveness
New_table[prod_cd %in% c('3119','3120','3325','3326','3328','3338','3383','3384'),prod_kd := 'lb']
New_table[prod_cd %in% c('3112','3113','3121','3123','3131','3132','3329','3333','3334','3335','3336 ','3339', '3340', '3385', '3386', '3387', '3594', '3595', '3596'), prod_kd := 'mb']
P5 <- ggplot(data = new_table[newwf != 'none' & !is.na(zhimamsxf_m) & prod_kd == 'mb', .(newwf,zhimamsxf_m)], aes(x=newwf, y=zhimamsxf_m) )+geom_boxplot(width=0.2)
P6 <- ggplot(data = new_table[newwf != 'none' & !is.na(zhimamsxf_m) & prod_kd == 'lb', .(newwf,zhimamsxf_m)], aes(x=newwf, y=zhimamsxf_m) )+geom_boxplot(width=0.2)
# (high amount / low amount) new strategy effectiveness
New_table[,amount := 'low']
New_table[(prod_kd == 'mb' & appl_lim >= 30000) | (prod_kd == 'lb' & appl_lim >= 15000), amount := 'high']
P7 <- ggplot(data = new_table[newwf != 'none' & !is.na(zhimamsxf_m) & amount == 'low',.(newwf,zhimamsxf_m)], aes(x=newwf, y=zhimamsxf_m)) +geom_boxplot(width=0.2)
P8 <- ggplot(data = new_table[newwf != 'none' & !is.na(zhimamsxf_m) & amount == 'high',.(newwf,zhimamsxf_m)], aes(x=newwf, y=zhimamsxf_m)) +geom_boxplot(width=0.2)
# plot
Multiplot(p4,p5,p6,cols = 3)
Multiplot(p4,p7,p8,cols = 3)