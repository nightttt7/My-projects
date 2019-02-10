-- not tested
-- Delete existing tmp_sqq_maoth table
DROP TABLE IF EXISTS risk_analysis.tmp_sqq_maoth;
-- Create a new tmp_sqq_maoth table
CREATE TABLE risk_analysis.tmp_sqq_maoth
COMMENT 'main&others(gdl_risk_triangle,gdl_mer_struc,gdl_cust_info)'
STORED AS PARQUET
AS
-- Create an internal table
WITH
Main AS (
SELECT
Appl_no,
Mer_no,
Union_id,
Contra_no
FROM
Risk_analysis.tmp_sqq_main
),
Mer AS (
SELECT
Mer_no,
Mer_name,
Mer_status
FROM
Dsst.gdl_mer_struc
WHERE
Dt = '20180609'
-- dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400, 'yyyyMMdd')
),
Triangle AS (
SELECT
Contra_no,
Max_dpd,
Curt_dpd,
Contra_status,
Curt_term,
Fpd,
Spd,
Tpd,
Qpd,
Fvpd,
Sxpd,
Svpd,
Etpd,
Nnpd,
Tnpd,
Evpd,
Twpd
FROM
Dsst.gdl_risk_triangle
WHERE
Dt = '20180609'
-- dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400, 'yyyyMMdd')
),
Xsell AS (
SELECT
Union_id,
Prod_cd,
Effc_start_dt,
Effc_end_dt
FROM
Dsst.fdl_cam_uds_x_sell_chain
WHERE
CHAIN_STATUS='active'
AND
End_date = '47121231'
)
-- Query block
SELECT
Main.appl_no,
Main.mer_no,
Main.union_id,
Main.contra_no,
Mer.mer_name,
Mer.mer_status,
Triangle.max_dpd,
Triangle.curt_dpd,
Triangle.contra_status,
Triangle.curt_term,
Triangle.fpd,
Triangle.spd,
Triangle.tpd,
Triangle.qpd,
Triangle.fvpd,
Triangle.sxpd,
Triangle.svpd,
Triangle.etpd,
Triangle.nnpd,
Triangle.tnpd,
Triangle.evpd,
Triangle.twpd,
Xsell.prod_cd AS 'xprod_cd',
Xsell.effc_start_dt,
Xsell.effc_end_dt
FROM
Main
LEFT JOIN
Mer
ON
Main.mer_no = mer.mer_no
LEFT JOIN
Triangle
ON
Main.contra_no = triangle.contra_no
LEFT JOIN
Xsell
ON
Main.union_id = xsell.union_id
;
-- SQL ends
-- Refresh
INVALIDATE METADATA risk_analysis.tmp_sqq_maoth;
