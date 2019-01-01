# - 涉及商业机密已删除具体数字/代码/具体描述,本代码已无法运行
#===========================================================#
#======================注意=================================#
#===========================================================#
# 运行前需要修改dt参数
# 因为有时候前一天的数据还没运行好,要用更早的的数据
# dt = xxxxxxxx
# 请使用ctrl+F替换
#===========================================================#
#===设定工作目录&导入模块&从excel导入数据&从数据库取得数据==#
#===========================================================#
setwd('~/关注门店')
# 导入模块
library(dplyr)
library(RODBC)
library(readxl)
# 导入excel
old_dt <- read_excel(file.choose())
# 处理excel中的合并单元格 
colnames(old_dt) <- old_dt[1,]
old_dt <- old_dt[-1,]
colnames(old_dt)[c(2,4)] <- c("category","mer_name")
# 加入一列合并的category,biz_prov and mer_name
old_dt$keymer <- paste0(old_dt$category,old_dt$biz_prov,old_dt$mer_name)
# 链接impala数据库
conn <- odbcConnect('xxx', uid = "xxx", pwd = "xxx",
                    believeNRows = FALSE, DBMSencoding = "UTF-8")
# SQL查询: 申请情况+逾期情况表
mer_overdue<-sqlQuery(conn,"WITH
                      -- t1.申请情况
                      t1 AS (
                      SELECT
                      -- 1.类别
                      CASE WHEN prod_cd IN ('0000','0000') THEN 'xx'
                      WHEN prod_cd IN ('0000','0000') THEN 'yy' 
                      END AS 'category',
                      -- 2.省份
                      biz_prov,
                      -- 3.商户名
                      mer_name,
                      -- 4.周1所在日期
                      CASE WHEN DAYOFWEEK(appl_tm) = 2 THEN to_date(appl_tm)
                      WHEN DAYOFWEEK(appl_tm) = 3 THEN to_date(DATE_SUB(CAST(appl_tm AS TIMESTAMP),INTERVAL 1 DAY))
                      WHEN DAYOFWEEK(appl_tm) = 4 THEN to_date(DATE_SUB(CAST(appl_tm AS TIMESTAMP),INTERVAL 2 DAY)) 
                      WHEN DAYOFWEEK(appl_tm) = 5 THEN to_date(DATE_SUB(CAST(appl_tm AS TIMESTAMP),INTERVAL 3 DAY)) 
                      WHEN DAYOFWEEK(appl_tm) = 6 THEN to_date(DATE_SUB(CAST(appl_tm AS TIMESTAMP),INTERVAL 4 DAY))
                      WHEN DAYOFWEEK(appl_tm) = 7 THEN to_date(DATE_SUB(CAST(appl_tm AS TIMESTAMP),INTERVAL 5 DAY))
                      WHEN DAYOFWEEK(appl_tm) = 1 THEN to_date(DATE_SUB(CAST(appl_tm AS TIMESTAMP),INTERVAL 6 DAY))
                      END AS 'week_bucket',
                      -- 申请量
                      COUNT(appl_no) AS 'applications',
                      -- 通过量
                      COUNT(case when appl_status in ('A','N','J') THEN appl_no else NULL end) AS 'approval_no',
                      -- 通过率
                      COUNT(case when appl_status in ('A','N','J') THEN appl_no else NULL end) / COUNT(appl_no) AS 'approve_rate',
                      -- 合同量
                      COUNT(case when contra_no is not null then contra_no else null end ) as 'sign_no',
                      -- 总金额
                      sum(case when contra_no is not null then crdt_lim else 0 end) as 'sum_loan_amount',
                      -- 平均金额
                      sum(case when contra_no is not null then crdt_lim else 0 end)/COUNT(case when contra_no is not null then contra_no else null end ) as 'aver_loan_amount'
                      FROM
                      -- 主表
                      dsst.gdl_aprvadt_det
                      WHERE 
                      dt='20100000'
                      AND
                      -- xxxx产品
                      prod_cd IN ('0000','0000')
                      AND
                      -- 申请日期在2010-00-00以后
                      to_date(appl_tm)>='2010-00-00'
                      -- 按商户和周期groupby
                      GROUP BY 1,2,3,4
                      ),
                      -- t2.逾期情况
                      t2 AS (
                      SELECT
                      -- 1.类别
                      CASE WHEN main.prod_cd IN('0000','0000') THEN 'xx'
                      WHEN main.prod_cd IN('0000','0000') THEN 'yy' 
                      END AS 'category',
                      -- 2.省份
                      main.biz_prov,
                      -- 3.商户名
                      main.mer_name,
                      -- 4.周1所在日期
                      CASE WHEN DAYOFWEEK(risk.appl_tm) = 2 THEN to_date(risk.appl_tm)
                      WHEN DAYOFWEEK(risk.appl_tm) = 3 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP),INTERVAL 1 DAY))
                      WHEN DAYOFWEEK(risk.appl_tm) = 4 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP),INTERVAL 2 DAY)) 
                      WHEN DAYOFWEEK(risk.appl_tm) = 5 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP),INTERVAL 3 DAY)) 
                      WHEN DAYOFWEEK(risk.appl_tm) = 6 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP),INTERVAL 4 DAY))
                      WHEN DAYOFWEEK(risk.appl_tm) = 7 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP),INTERVAL 5 DAY))
                      WHEN DAYOFWEEK(risk.appl_tm) = 1 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP),INTERVAL 6 DAY))
                      END AS 'week_bucket',
                      -- 5.合同数
                      COUNT(risk.contra_no) AS 'Due_contracts', 
                      -- 6789.四个逾期指标
                      COUNT(CASE WHEN 1st_loan_pmt_due_dt IS NOT NULL AND fpd>=10 THEN 1 ELSE NULL END) AS 'FPD10_n',
                      COUNT(CASE WHEN 1st_loan_pmt_due_dt IS NOT NULL AND fpd>=30 THEN 1 ELSE NULL END)AS 'FPD30_n',
                      COUNT(CASE WHEN 1st_loan_pmt_due_dt IS NOT NULL AND fpd<30 AND spd>=30 THEN 1 ELSE NULL END) AS 'SPD30_n',
                      COUNT(CASE WHEN 1st_loan_pmt_due_dt IS NOT NULL AND fpd<60 AND spd<30 AND tpd>=30 THEN 1 ELSE NULL END) AS 'TPD30_n'
                      FROM
                      -- 逾期表 
                      dsst.gdl_risk_triangle AS risk
                      LEFT JOIN 
                      -- 主表
                      dsst.gdl_aprvadt_det AS main
                      ON 
                      -- 合同编号相同
                      risk.contra_no=main.contra_no
                      WHERE 
                      -- xx产品
                      CAST(risk.prod_cd AS STRING) IN ('0000','0000')
                      AND 
                      risk.dt='20100000'
                      AND 
                      main.dt='20100000'
                      AND 
                      -- 第一期应还款日期不为空
                      -- Q:不能用risk.1st_loan_pmt_due_dt 指定表,原因不详
                      1st_loan_pmt_due_dt IS NOT NULL
                      AND
                      -- 第一期应还款日期在今天以前
                      1st_loan_pmt_due_dt<to_date(NOW()) 
                      AND 
                      -- 激活日期在30天以前
                      to_date(risk.active_dt)<DATE_SUB(to_date(NOW()),30)  
                      AND
                      -- 申请日期在2010-00-00以后
                      to_date(risk.appl_tm)>='2000-00-00'
                      -- 按商户和周期groupby
                      GROUP BY 1,2,3,4
                      )
                      SELECT 
                      -- t1全表 t1的四个逾期指标
                      t1.*,t2.FPD10_n,t2.FPD30_n,t2.SPD30_n,t2.TPD30_n
                      FROM 
                      t1
                      -- 合并t2到t1
                      LEFT JOIN 
                      t2 
                      ON
                      -- 按两个表重合的字段合并
                      t1.biz_prov=t2.biz_prov 
                      AND
                      t1.week_bucket=t2.week_bucket 
                      AND
                      t1.category=t2.category 
                      AND
                      t1.mer_name=t2.mer_name
                      -- 按商户和周期排序
                      ORDER BY 1,2,3,4
                      ;")
# SQL查询: 门店状态表 (xx)
mer_status<-sqlQuery(conn,
                     "SELECT
                     DISTINCT struc.mer_no,
                     struc.mer_name,
                     CASE 
                     WHEN struc.mer_status='09' THEN 'xx'
                     WHEN struc.mer_status='10' THEN 'xx' 
                     ELSE struc.mer_status 
                     END AS 'mer_status'
                     from 
                     dsst.gdl_mer_struc AS struc
                     JOIN
                     (
                     SELECT
                     * 
                     FROM 
                     dsst.gdl_aprvadt_det 
                     WHERE 
                     dt='20100000' 
                     AND
                     -- xxxx
                     prod_cd in ('0000','0000')
                     ) AS main
                     ON struc.mer_no=main.mer_no
                     WHERE struc.dt='20100000'
                     ;",stringsAsFactor=FALSE)
week_bucket<-sqlQuery(conn,
                     "Select  
                     *  
                     FROM  
                     risk_analysis.tmp_total_week_bucket_lisa
                     ORDER BY 
                     selectweek 
                     DESC;",stringsAsFactor=FALSE)
# 关闭数据库链接
odbcClose(conn)
#===========================================================#
#=====================数据处理==============================#
#===========================================================#
# 提取 week_bucket
week_bucket_fpd10 <- week_bucket[2,'week_bucket']
week_bucket_fpd30 <- week_bucket[3,'week_bucket']
week_bucket_spd30 <- week_bucket[4,'week_bucket']
week_bucket_tpd30 <- week_bucket[5,'week_bucket']

# 转变数据格式为字符
mer_overdue$category<-as.character(mer_overdue$category)
mer_overdue$biz_prov<-as.character(mer_overdue$biz_prov)
mer_overdue$mer_name<-as.character(mer_overdue$mer_name)
mer_overdue$week_bucket<-as.character(mer_overdue$week_bucket)

# mer_overdue增加列: tt:各门店合同量合计
# %>%:管道函数: 左件的值发送给右件的表达式,并作为右件表达式函数的第一个参数
mer_app_tt<-mer_overdue %>% group_by(category,biz_prov,mer_name) %>% summarise(tt=sum(sign_no))
mer_overdue<-left_join(mer_overdue,mer_app_tt,by=c('category','biz_prov','mer_name'))

# ta: 唯一值 category,biz_prov,mer_name,tt 即每家门店的合同量合计
ta<-unique(mer_overdue[,c(1:3,ncol(mer_overdue))])

# 日期参数在这里修改
zj_wk_fpd10<-mer_overdue[which(mer_overdue$week_bucket==week_bucket_fpd10),c(1:3,11)]
zj_wk_fpd30<-mer_overdue[which(mer_overdue$week_bucket==week_bucket_fpd30),c(1:3,12)]
zj_wk_spd30<-mer_overdue[which(mer_overdue$week_bucket==week_bucket_spd30),c(1:3,13)]
zj_wk_tpd30<-mer_overdue[which(mer_overdue$week_bucket==week_bucket_tpd30),c(1:3,14)]

# ta,zj_wk_fpd10,zj_wk_fpd30,zj_wk_spd30,zj_wk_tpd30 合并为 zj_wk_fstpd
zj_wk_fstpd<-ta %>% left_join(.,zj_wk_fpd10,by=c('category','biz_prov','mer_name')) %>% 
  left_join(.,zj_wk_fpd30,by=c('category','biz_prov','mer_name')) %>% 
  left_join(.,zj_wk_spd30,by=c('category','biz_prov','mer_name')) %>% 
  left_join(.,zj_wk_tpd30,by=c('category','biz_prov','mer_name'))

# zj_wk_fstpd增加列: keymer
# mutate(): 对已有列进行数据运算并添加为新列
zj_wk_fstpd<-mutate(zj_wk_fstpd,keymer=paste0(zj_wk_fstpd$category,zj_wk_fstpd$biz_prov,zj_wk_fstpd$mer_name))

# add_tips是zj_wk_fstpd中满足特定条件的行
add_tips<-zj_wk_fstpd[which(zj_wk_fstpd$fpd10_n>=2|zj_wk_fstpd$fpd30_n>=2|zj_wk_fstpd$spd30_n>=1|zj_wk_fstpd$tpd30_n>=1),]
#===========================================================#
#================将结果导出为csv============================#
#===========================================================#
# tips_add1.csv
# 删掉add_tips中曾在old_dt中出现过的xx所在行,删掉keymer列
if (which(add_tips$keymer %in% old_dt$keymer)==0){
  add1<-add_tips[,-ncol(add_tips)]
} else {
add1<-add_tips[-which(add_tips$keymer %in% old_dt$keymer),-ncol(add_tips)]
}
# 增加新列tab并赋值1(已结观察1周),移动到第一列
add1$tab<-1
add1<-add1[,c(ncol(add1),1:ncol(add1)-1)]
# 合并xx状态
add1 <- left_join(add1,mer_status[,c("mer_name","mer_status")],by='mer_name')
# 写入csv
write.csv(add1,'tips_add1.csv',fileEncoding = 'GBK')

# tips_add2.csv
# 更新观察2周的
old_tab1<-old_dt[which(old_dt$tab==1),c(1:9,ncol(old_dt))]
add_tab2<-zj_wk_fstpd[which(zj_wk_fstpd$keymer %in% old_tab1$keymer),]
old_tab1<-old_tab1[,-ncol(old_tab1)]
add_tab2<-add_tab2[,-ncol(add_tab2)]
add2<-left_join(old_tab1,add_tab2,by=c('category','biz_prov','mer_name'))
add2$total_sign<-add2$tt
add2$tt<-NULL
add2$tab<-2
# 合并xx状态
add2 <- left_join(add2,mer_status[,c("mer_name","mer_status")],by='mer_name')
# 写入csv
write.csv(add2,'tips_add2.csv',fileEncoding = 'GBK')

# tips_add3.csv
# 更新观察3周的
old_tab2<-old_dt[which(old_dt$tab==2),c(1:13,ncol(old_dt))]
add_tab3<-zj_wk_fstpd[which(zj_wk_fstpd$keymer %in% old_tab2$keymer),]
old_tab2<-old_tab2[,-ncol(old_tab2)]
add_tab3<-add_tab3[,-ncol(add_tab3)]
add3<-left_join(old_tab2,add_tab3,by=c('category','biz_prov','mer_name'))
add3$total_sign<-add3$tt
add3$tt<-NULL
add3$tab<-3
# 合并xx状态
add3 <- left_join(add3,mer_status[,c("mer_name","mer_status")],by='mer_name')
# 写入csv
write.csv(add3,'tips_add3.csv',fileEncoding = 'GBK')

# tips_add4.csv
# 更新观察4周的
old_tab3<-old_dt[which(old_dt$tab==3),c(1:17,ncol(old_dt))]
add_tab4<-zj_wk_fstpd[which(zj_wk_fstpd$keymer %in% old_tab3$keymer),]
old_tab3<-old_tab3[,-ncol(old_tab3)]
add_tab4<-add_tab4[,-ncol(add_tab4)]
add4<-left_join(old_tab3,add_tab4,by=c('category','biz_prov','mer_name'))
add4$total_sign<-add4$tt
add4$tt<-NULL
add4$tab<-4
# 合并xx状态
add4 <- left_join(add4,mer_status[,c("mer_name","mer_status")],by='mer_name')
# 写入csv
write.csv(add4,'tips_add4.csv',fileEncoding = 'GBK')