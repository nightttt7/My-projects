-- Test passed
-- Delete existing tmp_sqq_macu table
DROP TABLE IF EXISTS risk_analysis.tmp_sqq_macu;
-- Create a new tmp_sqq_macu table
CREATE TABLE risk_analysis.tmp_sqq_macu
COMMENT 'wholemain&customchain'
STORED AS PARQUET
AS
-- Create an internal table
WITH
Main AS (
SELECT
Appl_no
FROM
Risk_analysis.tmp_sqq_main
),
Cust AS (
SELECT
App_no,
Gender_cd,
Mo_earn,
Oth_earn,
Oth_loan,
Currt_city,
Edu_degree,
Soc_id,
Work_life,
Unit_name,
Unit_addr_city,
Marg_status,
House_cond
FROM
Dsst.fdl_cust_prim_appl_chain
WHERE
CHAIN_STATUS='active'
AND
End_date = '47121231'
)
-- Query block
SELECT
Main.appl_no,
Cust.gender_cd,
Cust.mo_earn,
Cust.oth_earn,
Cust.oth_loan,
Cust.currt_city,
Cust.edu_degree,
Cust.soc_id,
Cust.work_life,
Cust.unit_name,
Cust.unit_addr_city,
Cust.marg_status,
Cust.house_cond
FROM
Main
LEFT JOIN
Cust
ON
Main.appl_no = cust.app_no
;
-- SQL ends
-- Refresh
INVALIDATE METADATA risk_analysis.tmp_sqq_macu;
