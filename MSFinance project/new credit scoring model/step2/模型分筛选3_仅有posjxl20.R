# jxl_table <- new_table[newwf == 'blank' & is.na(model1) & is.na(model2) & !is.na(model4),
#                        .(model4,appl_lim,biz_city_cd,path,status,fpd10,maxdpd10)]
# save(jxl_table,file='jxl_table.RData')
load('jxl_table.RData')
# jxl_table
cut <- quantile(new_table[,model4],0:100/100,na.rm = T)
cut
jxl_table[fpd10 == 'bad']
jxl_table[maxdpd10 == 'bad']
# 仅有一单fpd10,且处于posjxl20的高分段,因此fpd10无法参考
# 有三单maxdpd10,分别处于posjxl20 71%,55%,40%分段,可以参考
# 建议57%以上continue
# 57%以下ce
# 10%以下reject
# 预计每天因此转ce的单数如下
767*0.47/80
# 每天4.5单