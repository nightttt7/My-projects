# jxl_table <- new_table[newwf == 'blank' & is.na(model1) & is.na(model2) & !is.na(model4),
# .(model4,appl_lim,biz_city_cd,path,status,fpd10,maxdpd10)]
# save(jxl_table,file='jxl_table.RData')
Load('jxl_table.RData')
# jxl_table
Cut <- quantile(new_table[,model4],0:100/100,na.rm = T)
Cut
Jxl_table[fpd10 == 'bad']
Jxl_table[maxdpd10 == 'bad']
# Only a single fpd10, and is in the high segment of posjxl20, so fpd10 can not refer to
# There are three single maxdpd10, respectively in posjxl20 71%, 55%, 40% segmentation, you can refer to
# Recommend more than 57% continue
# > 57% then ce
# < 10% then reject
# It is expected that the number of ce transferred every day is as follows
767*0.47/80
# 4.5 per day