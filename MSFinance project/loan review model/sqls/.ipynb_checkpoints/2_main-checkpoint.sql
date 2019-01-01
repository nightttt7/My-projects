WITH
t1 AS (
SELECT
main.appl_no,
main.appl_tm,
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
dsst.gdl_aprvadt_yyyy_yyyy
WHERE
dt=from_unixtime(unix_timestamp()-86400,'yyyyMMdd')
AND
(
rule_content like ('HC%')
OR
rule_content like ('FD006%')
OR
rule_content like ('FD007%')
)
) AS rule
ON
main.appl_no = rule.appl_no
WHERE
main.dt=from_unixtime(unix_timestamp()-86400,'yyyyMMdd')
AND
(
to_date(main.appl_tm) 
BETWEEN to_date('2018-04-10')
AND to_date(date_sub(now(),interval 1 day))
)
AND 
main.prod_cd IN ('0000','0000','0000')
GROUP BY 
1,2,3,4,5
),
t2 AS (
SELECT * FROM risk_analysis.tmp_xxx_xxxmodel
)
SELECT
t1.appl_tm,
t1.appl_no,
t1.basic_checker,
t1.final_checker,
t1.HC,
t2.appl_status,
t2.appl_lim,
t2.bt3_score,
t2.jxlPosScoreV3,
t2.workflowCode
FROM
t1
LEFT JOIN
t2
ON
t1.appl_no = t2.appl_no
;