/*此处皆为impala下的SQL语句,涉及商业机密对一些文字和数字进行了删除处理*/

/*1. XX申请明细二合一*/

SELECT

"XX"
AS '产品品类',
main.appl_tm,
main.appl_no,
main.mer_name AS '商户名称',
main.appl_status AS '最终状态',
main.final_checker AS '终审人员',
GROUP_CONCAT(rule.rule_content) AS '原因'

FROM

dsst.gdl_aprvadt_det AS main
LEFT JOIN
(
SELECT 

appl_no,
rule_content,
dt

FROM
dsst.gdl_aprvadt_trig_rule

WHERE

dt=from_unixtime(unix_timestamp()-86400,'yyyyMMdd')
  
AND
rule_content like ('HC%')

) AS  rule

ON
main.appl_no = rule.appl_no

WHERE

main.dt=from_unixtime(unix_timestamp()-86400,'yyyyMMdd')

AND
(
to_date(main.appl_tm) 
BETWEEN to_date(date_sub(now(),interval 1 day))
AND to_date(date_sub(now(),interval 1 day))
)

AND 
main.prod_cd IN ('0000')

GROUP BY 
'产品品类',
main.appl_tm,
main.appl_no,
main.mer_name,
main.appl_status,
main.final_checker

ORDER BY main.appl_no

;





/*2. 不同省份分时间段申请量*/

SELECT

a.biz_prov AS '省份',
((year(a.appl_tm) * 100 ) + weekofyear(a.appl_tm)) AS 'week',
COUNT(a.appl_no) AS '申请量',
COUNT(CASE WHEN a.appl_status in ('A','N','J') THEN a.appl_no END ) AS '通过量',
COUNT(CASE WHEN a.appl_status in ('A','N','J') THEN a.appl_no END ) / COUNT(a.appl_no) AS '通过率'

FROM

dsst.gdl_aprvadt_det AS a

WHERE

a.dt=FROM_UNIXTIME(UNIX_TIMESTAMP()-86400,'yyyyMMdd') 

AND
a.prod_cd IN
('0000','0000','0000')

AND 
a.mer_name NOT IN ('XXX')

GROUP BY

a.biz_prov,
((year(a.appl_tm) * 100 ) + weekofyear(a.appl_tm))

;





/*3. XXX模型监控_1*/

WITH
t1 AS (
SELECT
main.appl_no,
main.appl_tm,
main.appl_lim,
main.basic_checker,
main.final_checker,
main.dt,
count(rule.rule_content) AS HC
FROM
dsst.gdl_aprvadt_det AS main
LEFT JOIN
(
SELECT 
appl_no,
rule_content,
dt
FROM
dsst.gdl_aprvadt_trig_rule
WHERE
dt=from_unixtime(unix_timestamp()-86400,'yyyyMMdd')
AND
rule_content like ('HC%')
) AS rule
ON
main.appl_no = rule.appl_no
WHERE
main.dt=from_unixtime(unix_timestamp()-86400,'yyyyMMdd')
AND
(
to_date(main.appl_tm) 
BETWEEN to_date(date_sub(now(),interval 1 day))
AND to_date(date_sub(now(),interval 1 day))
)
AND 
main.prod_cd IN ('0000','0000','0000')
GROUP BY 
1,2,3,4,5,6
),
t2 AS (
SELECT
main.appl_tm,
main.appl_no,
blaze.round_par,
blaze.xxx,
blaze.yyy
FROM
dsst.gdl_aprvadt_det main
LEFT JOIN 
fdl.zzz blaze
ON
main.appl_no=blaze.appl_no 
AND 
blaze.round_par = 'blaze_ivk_Round2c'
WHERE
main.dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400,'yyyyMMdd')
AND
(
blaze.dt
BETWEEN FROM_TIMESTAMP(date_sub(now(),interval 1 day),'yyyyMMdd')
AND FROM_TIMESTAMP(date_sub(now(),interval 1 day),'yyyyMMdd')
)
AND
main.prod_cd IN ('0000','0000','0000')
AND
(
to_date(main.appl_tm) 
BETWEEN to_date(date_sub(now(),interval 1 day))
AND to_date(date_sub(now(),interval 1 day))
)
)
SELECT 
t1.appl_no,
t1.appl_tm,
t1.appl_lim,
t1.basic_checker,
t1.final_checker,
t1.HC,
t2.round_par,
t2.tempVector6_r,
t2.pos_jxl_model3_jxlposscorev3
FROM
t1
LEFT JOIN
t2
ON
t1.appl_no = t2.appl_no
;





/*4. 申请情况+逾期情*/
WITH
-- t1.申请情况
t1 AS (
SELECT
-- 1.类别
CASE WHEN prod_cd IN ('0000','0000','0000') THEN '医美'
WHEN prod_cd IN ('0000','0000','0000') THEN '生美' 
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
dt=FROM_UNIXTIME(UNIX_TIMESTAMP()-86400,'yyyyMMdd') 
AND
-- 雪球美业产品
prod_cd IN ('0000','0000','0000')
AND
-- 申请日期在2017-02-01以后
to_date(appl_tm)>='2017-02-01'
-- 按商户和周期groupby
GROUP BY 1,2,3,4
),
-- t2.逾期情况
t2 AS (
SELECT
-- 1.类别
CASE WHEN main.prod_cd IN ('0000','0000','0000') THEN '医美'
WHEN main.prod_cd IN ('0000','0000','0000') THEN '生美' 
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
-- 雪球美业产品
CAST(risk.prod_cd AS STRING) IN ('0000','0000','0000')
AND 
risk.dt=FROM_UNIXTIME(UNIX_TIMESTAMP()-86400,'yyyyMMdd') 
AND 
main.dt=FROM_UNIXTIME(UNIX_TIMESTAMP()-86400,'yyyyMMdd')
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
-- 申请日期在2017-02-01以后
to_date(risk.appl_tm)>='2017-02-01'
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
;