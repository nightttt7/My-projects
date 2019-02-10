# - Involved in trade secrets has been deleted specific numbers / codes / specific description, this code has been unable to run
#================================================================= ==========#
#=================================================================================================== =======#
#================================================================= ==========#
# Modify the dt parameter before running
# Because sometimes the previous day's data is not running well, use earlier data.
# dt = xxxxxxxx
# Please use ctrl+F to replace
#================================================================= ==========#
#===Setting the working directory & import module & importing data from excel & getting data from the database ==#
#================================================================= ==========#
Setwd('~/Follow the store')
# Import module
Library(dplyr)
Library(RODBC)
Library(readxl)
# Import excel
Old_dt <- read_excel(file.choose())
#Processing merged cells in excel
Colnames(old_dt) <- old_dt[1,]
Old_dt <- old_dt[-1,]
Colnames(old_dt)[c(2,4)] <- c("category","mer_name")
# Join a list of merged categories, biz_prov and mer_name
Old_dt$keymer <- paste0(old_dt$category,old_dt$biz_prov,old_dt$mer_name)
#connet to impala database
Conn <- odbcConnect('xxx', uid = "xxx", pwd = "xxx",
                    believeNRows = FALSE, DBMSencoding = "UTF-8")
# SQL Query: Application Status + Overdue Status Table
Mer_overdue<-sqlQuery(conn,"WITH
                      -- t1. Application status
                      T1 AS (
                      SELECT
                      -- 1. Category
                      CASE WHEN prod_cd IN ('0000', '0000') THEN 'xx'
                      WHEN prod_cd IN ('0000', '0000') THEN 'yy'
                      END AS 'category',
                      -- 2. Provinces
                      Biz_prov,
                      -- 3. Merchant name
                      Mer_name,
                      -- 4. The date of week 1
                      CASE WHEN DAYOFWEEK(appl_tm) = 2 THEN to_date(appl_tm)
                      WHEN DAYOFWEEK(appl_tm) = 3 THEN to_date(DATE_SUB(CAST(appl_tm AS TIMESTAMP), INTERVAL 1 DAY))
                      WHEN DAYOFWEEK(appl_tm) = 4 THEN to_date(DATE_SUB(CAST(appl_tm AS TIMESTAMP), INTERVAL 2 DAY))
                      WHEN DAYOFWEEK(appl_tm) = 5 THEN to_date(DATE_SUB(CAST(appl_tm AS TIMESTAMP), INTERVAL 3 DAY))
                      WHEN DAYOFWEEK(appl_tm) = 6 THEN to_date(DATE_SUB(CAST(appl_tm AS TIMESTAMP), INTERVAL 4 DAY))
                      WHEN DAYOFWEEK(appl_tm) = 7 THEN to_date(DATE_SUB(CAST(appl_tm AS TIMESTAMP), INTERVAL 5 DAY))
                      WHEN DAYOFWEEK(appl_tm) = 1 THEN to_date(DATE_SUB(CAST(appl_tm AS TIMESTAMP), INTERVAL 6 DAY))
                      END AS 'week_bucket',
                      -- Application volume
                      COUNT(appl_no) AS 'applications',
                      -- throughput
                      COUNT(case when appl_status in ('A','N','J') THEN appl_no else NULL end) AS 'approval_no',
                      -- Passing rate
                      COUNT(case when appl_status in ('A','N','J') THEN appl_no else NULL end) / COUNT(appl_no) AS 'approve_rate',
                      -- Contract amount
                      COUNT(case when contra_no is not null then contra_no else null end ) as 'sign_no',
                      -- total amount
                      Sum(case when contra_no is not null then crdt_lim else 0 end) as 'sum_loan_amount',
                      -- average amount
                      Sum(case when contra_no is not null then crdt_lim else 0 end)/COUNT(case when contra_no is not null then contra_no else null end ) as 'aver_loan_amount'
                      FROM
                      -- Primary table
                      Dsst.gdl_aprvadt_det
                      WHERE
                      Dt='20100000'
                      AND
                      -- xxxx products
                      Prod_cd IN ('0000', '0000')
                      AND
                      -- Application date is after 2010-00-00
                      To_date(appl_tm)>='2010-00-00'
                      -- Groupby by business and cycle
                      GROUP BY 1,2,3,4
                      ),
                      -- t2. Overdue situation
                      T2 AS (
                      SELECT
                      -- 1. Category
                      CASE WHEN main.prod_cd IN('0000','0000') THEN 'xx'
                      WHEN main.prod_cd IN('0000','0000') THEN 'yy'
                      END AS 'category',
                      -- 2. Provinces
                      Main.biz_prov,
                      -- 3. Merchant name
                      main.mer_name,
                      -- 4.date of monday
                      CASE WHEN DAYOFWEEK(risk.appl_tm) = 2 THEN to_date(risk.appl_tm)
                      WHEN DAYOFWEEK(risk.appl_tm) = 3 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP),INTERVAL 1 DAY))
                      WHEN DAYOFWEEK(risk.appl_tm) = 4 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP),INTERVAL 2 DAY)) 
                      WHEN DAYOFWEEK(risk.appl_tm) = 5 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP),INTERVAL 3 DAY)) 
                      WHEN DAYOFWEEK(risk.appl_tm) = 6 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP),INTERVAL 4 DAY))
                      WHEN DAYOFWEEK(risk.appl_tm) = 7 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP),INTERVAL 5 DAY))
                      WHEN DAYOFWEEK(risk.appl_tm) = 1 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP),INTERVAL 6 DAY))
                      END AS 'week_bucket',
                      -- 5. Number of contracts
                      COUNT(risk.contra_no) AS 'Due_contracts',
                      -- 6789. Four overdue indicators
                      COUNT(CASE WHEN 1st_loan_pmt_due_dt IS NOT NULL AND fpd>=10 THEN 1 ELSE NULL END) AS 'FPD10_n',
                      COUNT(CASE WHEN 1st_loan_pmt_due_dt IS NOT NULL AND fpd>=30 THEN 1 ELSE NULL END)AS 'FPD30_n',
                      COUNT(CASE WHEN 1st_loan_pmt_due_dt IS NOT NULL AND fpd<30 AND spd>=30 THEN 1 ELSE NULL END) AS 'SPD30_n',
                      COUNT(CASE WHEN 1st_loan_pmt_due_dt IS NOT NULL AND fpd<60 AND spd<30 AND tpd>=30 THEN 1 ELSE NULL END) AS 'TPD30_n'
                      FROM
                      -- Overdue form
                      Dsst.gdl_risk_triangle AS risk
                      LEFT JOIN
                      -- Primary table
                      Dsst.gdl_aprvadt_det AS main
                      ON
                      -- The contract number is the same
                      Risk.contra_no=main.contra_no
                      WHERE
                      -- xx products
                      CAST(risk.prod_cd AS STRING) IN ('0000', '0000')
                      AND
                      Risk.dt='20100000'
                      AND
                      Main.dt='20100000'
                      AND
                      -- The first repayment date is not empty
                      -- Q: You cannot specify a table with risk.1st_loan_pmt_due_dt for unknown reasons
                      1st_loan_pmt_due_dt IS NOT NULL
                      AND
                      -- The first repayment date is before today
                      1st_loan_pmt_due_dt<to_date(NOW())
                      AND
                      -- Activation date is 30 days ago
                      To_date(risk.active_dt)<DATE_SUB(to_date(NOW()),30)
                      AND
                      -- Application date is after 2010-00-00
                      To_date(risk.appl_tm)>='2000-00-00'
                      -- Groupby by business and cycle
                      GROUP BY 1,2,3,4
                      )
                      SELECT
                      -- t1 full table t1 four overdue indicators
                      T1.*, t2.FPD10_n, t2.FPD30_n, t2.SPD30_n, t2.TPD30_n
                      FROM
                      T1
                      -- Combine t2 to t1
                      LEFT JOIN
                      T2
                      ON
                      -- Merging fields that coincide on two tables
                      T1.biz_prov=t2.biz_prov
                      AND
                      T1.week_bucket=t2.week_bucket
                      AND
                      T1.category=t2.category
                      AND
                      T1.mer_name=t2.mer_name
                      -- Sort by merchant and cycle
                      ORDER BY 1,2,3,4
                      ;")
# SQL Query: Store Status Table (xx)
Mer_status<-sqlQuery(conn,
                     "SELECT
                     DISTINCT struc.mer_no,
                     Struc.mer_name,
                     CASE
                     WHEN struc.mer_status='09' THEN 'xx'
                     WHEN struc.mer_status='10' THEN 'xx'
                     ELSE struc.mer_status
                     END AS 'mer_status'
                     From
                     Dsst.gdl_mer_struc AS struc
                     JOIN
                     (
                     SELECT
                     *
                     FROM
                     Dsst.gdl_aprvadt_det
                     WHERE
                     Dt='20100000'
                     AND
                     -- xxxx
                     Prod_cd in ('0000', '0000')
                     AS main
                     ON struc.mer_no=main.mer_no
                     WHERE struc.dt='20100000'
                     ;",stringsAsFactor=FALSE)
Week_bucket<-sqlQuery(conn,
                     "Select
                     *
                     FROM  
                     risk_analysis.tmp_total_week_bucket_lisa
                     ORDER BY 
                     selectweek 
                     DESC;",stringsAsFactor=FALSE)
# Close database link
odbcClose(conn)
#================================================================= ==========#
#============================== Data Processing =========================== ====#
#================================================================= ==========#
#get week_bucket
Week_bucket_fpd10 <- week_bucket[2,'week_bucket']
Week_bucket_fpd30 <- week_bucket[3,'week_bucket']
Week_bucket_spd30 <- week_bucket[4,'week_bucket']
Week_bucket_tpd30 <- week_bucket[5,'week_bucket']

# Convert data format to characters
Mer_overdue$category<-as.character(mer_overdue$category)
Mer_overdue$biz_prov<-as.character(mer_overdue$biz_prov)
Mer_overdue$mer_name<-as.character(mer_overdue$mer_name)
Mer_overdue$week_bucket<-as.character(mer_overdue$week_bucket)

# mer_overdueAdd column: tt: Total contract volume of each store
# %>%:pipe function: The value of the left piece is sent to the expression of the right piece and is the first parameter of the right part expression function
Mer_app_tt<-mer_overdue %>% group_by(category,biz_prov,mer_name) %>% summarise(tt=sum(sign_no))
Mer_overdue<-left_join(mer_overdue,mer_app_tt,by=c('category','biz_prov','mer_name'))

# ta: Unique value category, biz_prov, mer_name, tt is the total contract amount of each store
Ta<-unique(mer_overdue[,c(1:3,ncol(mer_overdue))])

# Date parameter is modified here
Zj_wk_fpd10<-mer_overdue[which(mer_overdue$week_bucket==week_bucket_fpd10),c(1:3,11)]
Zj_wk_fpd30<-mer_overdue[which(mer_overdue$week_bucket==week_bucket_fpd30),c(1:3,12)]
Zj_wk_spd30<-mer_overdue[which(mer_overdue$week_bucket==week_bucket_spd30),c(1:3,13)]
Zj_wk_tpd30<-mer_overdue[which(mer_overdue$week_bucket==week_bucket_tpd30),c(1:3,14)]

# ta,zj_wk_fpd10,zj_wk_fpd30,zj_wk_spd30,zj_wk_tpd30 merged into zj_wk_fstpd
Zj_wk_fstpd<-ta ​​%>% left_join(.,zj_wk_fpd10,by=c('category','biz_prov','mer_name')) %>%
  Left_join(.,zj_wk_fpd30,by=c('category','biz_prov','mer_name')) %>%
  Left_join(.,zj_wk_spd30,by=c('category','biz_prov','mer_name')) %>%
  Left_join(.,zj_wk_tpd30,by=c('category','biz_prov','mer_name'))

# zj_wk_fstpdAdd column: keymer
# mutate(): Perform data operations on existing columns and add them as new columns
Zj_wk_fstpd<-mutate(zj_wk_fstpd,keymer=paste0(zj_wk_fstpd$category,zj_wk_fstpd$biz_prov,zj_wk_fstpd$mer_name))

# add_tips is a line in zj_wk_fstpd that meets certain conditions
Add_tips<-zj_wk_fstpd[which(zj_wk_fstpd$fpd10_n>=2|zj_wk_fstpd$fpd30_n>=2|zj_wk_fstpd$spd30_n>=1|zj_wk_fstpd$tpd30_n>=1),]
#================================================================= ==========#
#==================================================================================================== #
#================================================================= ==========#
# tips_add1.csv
# Delete the line of xx in add_tips that has appeared in old_dt, delete the keymer column
If (which(add_tips$keymer %in% old_dt$keymer)==0){
  Add1<-add_tips[,-ncol(add_tips)]
} else {
Add1<-add_tips[-which(add_tips$keymer %in% old_dt$keymer),-ncol(add_tips)]
}
# Add a new column tab and assign a value of 1 (closed for 1 week), move to the first column
Add1$tab<-1
Add1<-add1[,c(ncol(add1),1:ncol(add1)-1)]
# merge xx status
Add1 <- left_join(add1,mer_status[,c("mer_name","mer_status")],by='mer_name')
#Write csv
Write.csv(add1,'tips_add1.csv',fileEncoding = 'GBK')

# tips_add2.csv
#Update observation for 2 weeks
Old_tab1<-old_dt[which(old_dt$tab==1),c(1:9,ncol(old_dt))]
Add_tab2<-zj_wk_fstpd[which(zj_wk_fstpd$keymer %in% old_tab1$keymer),]
Old_tab1<-old_tab1[,-ncol(old_tab1)]
Add_tab2<-add_tab2[,-ncol(add_tab2)]
Add2<-left_join(old_tab1,add_tab2,by=c('category','biz_prov','mer_name'))
Add2$total_sign<-add2$tt
Add2$tt<-NULL
Add2$tab<-2
# merge xx status
Add2 <- left_join(add2,mer_status[,c("mer_name","mer_status")],by='mer_name')
#Write csv
Write.csv(add2,'tips_add2.csv',fileEncoding = 'GBK')

# tips_add3.csv
#Update observation for 3 weeks
Old_tab2<-old_dt[which(old_dt$tab==2),c(1:13,ncol(old_dt))]
Add_tab3<-zj_wk_fstpd[which(zj_wk_fstpd$keymer %in% old_tab2$keymer),]
Old_tab2<-old_tab2[,-ncol(old_tab2)]
Add_tab3<-add_tab3[,-ncol(add_tab3)]
Add3<-left_join(old_tab2,add_tab3,by=c('category','biz_prov','mer_name'))
Add3$total_sign<-add3$tt
Add3$tt<-NULL
Add3$tab<-3
# merge xx status
Add3 <- left_join(add3,mer_status[,c("mer_name","mer_status")],by='mer_name')
#Write csv
Write.csv(add3,'tips_add3.csv',fileEncoding = 'GBK')

# tips_add4.csv
#Update observation for 4 weeks
Old_tab3<-old_dt[which(old_dt$tab==3),c(1:17,ncol(old_dt))]
Add_tab4<-zj_wk_fstpd[which(zj_wk_fstpd$keymer %in% old_tab3$keymer),]
Old_tab3<-old_tab3[,-ncol(old_tab3)]
Add_tab4<-add_tab4[,-ncol(add_tab4)]
Add4<-left_join(old_tab3,add_tab4,by=c('category','biz_prov','mer_name'))
Add4$total_sign<-add4$tt
Add4$tt<-NULL
Add4$tab<-4
# merge xx status
Add4 <- left_join(add4,mer_status[,c("mer_name","mer_status")],by='mer_name')
#Write csv
Write.csv(add4,'tips_add4.csv',fileEncoding = 'GBK')
