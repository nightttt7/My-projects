/ *Here are the SQL statements under impala, involving the trade secrets to delete some text and numbers */

/*1. XX application details two in one*/

SELECT

"XX"
AS 'Product Category',
Main.appl_tm,
Main.appl_no,
Main.mer_name AS 'business name',
Main.appl_status AS 'final state',
Main.final_checker AS 'final reviewer',
GROUP_CONCAT(rule.rule_content) AS 'reason'

FROM

Dsst.gdl_aprvadt_det AS main
LEFT JOIN
(
SELECT

Appl_no,
Rule_content,
Dt

FROM
Dsst.gdl_aprvadt_trig_rule

WHERE

Dt=from_unixtime(unix_timestamp()-86400,'yyyyMMdd')
  
AND
Rule_content like ('HC%')

) AS rule

ON
Main.appl_no = rule.appl_no

WHERE

Main.dt=from_unixtime(unix_timestamp()-86400,'yyyyMMdd')

AND
(
To_date(main.appl_tm)
BETWEEN to_date(date_sub(now(),interval 1 day))
AND to_date(date_sub(now(),interval 1 day))
)

AND
Main.prod_cd IN ('0000')

GROUP BY
'Product category',
Main.appl_tm,
Main.appl_no,
Main.mer_name,
Main.appl_status,
Main.final_checker

ORDER BY main.appl_no

;





/*2. Number of applications in different provinces by time period*/

SELECT

A.biz_prov AS 'province',
((year(a.appl_tm) * 100 ) + weekofyear(a.appl_tm)) AS 'week',
COUNT(a.appl_no) AS 'application volume',
COUNT(CASE WHEN a.appl_status in ('A','N','J') THEN a.appl_no END ) AS 'throughput',
COUNT(CASE WHEN a.appl_status in ('A','N','J') THEN a.appl_no END ) / COUNT(a.appl_no) AS 'pass rate'

FROM

Dsst.gdl_aprvadt_det AS a

WHERE

A.dt=FROM_UNIXTIME(UNIX_TIMESTAMP()-86400, 'yyyyMMdd')

AND
A.prod_cd IN
('0000', '0000', '0000')

AND
A.mer_name NOT IN ('XXX')

GROUP BY

A.biz_prov,
((year(a.appl_tm) * 100 ) + weekofyear(a.appl_tm))

;




/*3. XXX model monitoring_1*/

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





/*4. Application Status + Overdue*/
WITH
-- t1. Application status
T1 AS (
SELECT
-- 1. Category
CASE WHEN prod_cd IN ('0000','0000','0000') THEN 'mb'
WHEN prod_cd IN ('0000','0000','0000') THEN 'lb'
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
Dt=FROM_UNIXTIME(UNIX_TIMESTAMP()-86400, 'yyyyMMdd')
AND
-- Snowball Beauty Products
Prod_cd IN ('0000', '0000', '0000')
AND
-- Application date is after 2017-02-01
To_date(appl_tm)>='2017-02-01'
-- Groupby by business and cycle
GROUP BY 1,2,3,4
),
-- t2. Overdue situation
T2 AS (
SELECT
-- 1. Category
CASE WHEN main.prod_cd IN ('0000','0000','0000') THEN 'mb'
WHEN main.prod_cd IN ('0000','0000','0000') THEN 'lb'
END AS 'category',
-- 2. Provinces
Main.biz_prov,
-- 3. Merchant name
Main.mer_name,
-- 4. The date of week 1
CASE WHEN DAYOFWEEK(risk.appl_tm) = 2 THEN to_date(risk.appl_tm)
WHEN DAYOFWEEK(risk.appl_tm) = 3 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP), INTERVAL 1 DAY))
WHEN DAYOFWEEK(risk.appl_tm) = 4 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP), INTERVAL 2 DAY))
WHEN DAYOFWEEK(risk.appl_tm) = 5 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP), INTERVAL 3 DAY))
WHEN DAYOFWEEK(risk.appl_tm) = 6 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP), INTERVAL 4 DAY))
WHEN DAYOFWEEK(risk.appl_tm) = 7 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP), INTERVAL 5 DAY))
WHEN DAYOFWEEK(risk.appl_tm) = 1 THEN to_date(DATE_SUB(CAST(risk.appl_tm AS TIMESTAMP), INTERVAL 6 DAY))
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
-- Snowball Beauty Products
CAST(risk.prod_cd AS STRING) IN ('0000', '0000', '0000')
AND
Risk.dt=FROM_UNIXTIME(UNIX_TIMESTAMP()-86400, 'yyyyMMdd')
AND
Main.dt=FROM_UNIXTIME(UNIX_TIMESTAMP()-86400, 'yyyyMMdd')
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
-- Application date is after 2017-02-01
To_date(risk.appl_tm)>='2017-02-01'
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
;
