cores1 <- data.table()
for (th1_1 in seq(from=38,to=50,by=3)) {
  for (th1_2 in seq(from=14,to=30,by=4)) {
    for (th2_1 in seq(from=35,to=50,by=3)) {
      for (th2_2 in seq(from=14,to=30,by=4)) {
        new_table <- get_new_table3(model_table,models=c('bt3_m','cup_m','cup_m','posjxl20_m'),
                                    sdate='2018-01-22',edate='2018-04-16',passstyle='either',
                                    th1=c(th1_1,th1_2,5),
                                    th2=c(th2_1,th2_2,2),
                                    th3=c(55,28,7),th4=c(57,10,3))
        result1 <- get_result3(new_table[haspboc == 1],ce_ar_pred = 0.6, continue_ar_pred = 0.99, cerejectpassfpd10 = 2.5, oldmodelrejectpassfpd10 = 2.5)
        result_core1 <- get_core(result1)
        result_core1 <- cbind(data.table(th1_1=th1_1,th1_2=th1_2,th2_1=th2_1,th2_2=th2_2),result_core1)
        cores1 <- rbind(cores1,result_core1)
      }
    }
  }
}
write.csv(cores1,file='cores_1.csv')


