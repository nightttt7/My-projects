cores0 <- data.table()
for (th3_1 in seq(from=65,to=75,by=2)) {
  for (th3_2 in seq(from=30,to=50,by=2)) {
    for (th3_3 in seq(from=3,to=6,by=2)) {
      new_table <- get_new_table3(model_table,models=c('bt3_m','cup_m','cup_m','posjxl20_m'),
                                  sdate='2018-01-22',edate='2018-04-16',passstyle='both',
                                  th1=c(46,20,5),th2=c(37,20,2),
                                  th3=c(th3_1,th3_2,th3_3),
                                  th4=c(57,10,2))
      result0 <- get_result3_nopboc(new_table[haspboc == 0],ce_ar_pred = 0.6, continue_ar_pred = 0.99, cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5)
      result_core0 <- get_core1(result0)
      result_core0 <- cbind(data.table(th3_1=th3_1,th3_2=th3_2),result_core0)
      cores0 <- rbind(cores0,result_core0)
    }
  }
}
write.csv(cores0,file='cores_0.csv')



