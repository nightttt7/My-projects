-- Test passed
-- Delete existing bak_sqq_custom_qual table
DROP TABLE IF EXISTS risk_analysis.bak_sqq_custom_qual;
-- Refresh the bak_sqq_modelscore table
INVALIDATE METADATA risk_analysis.bak_sqq_modelscore;
-- Create a new bak_sqq_custom_qual table
CREATE TABLE risk_analysis.bak_sqq_custom_qual
COMMENT 'Service Credit Group Qualification Data'
STORED AS PARQUET
AS
-- Query block
SELECT
Mabl.*,
Maru.rule_id_cat,
Maru.ops2ce,
Macu.gender_cd,
Macu.mo_earn,
Macu.oth_earn,
Macu.oth_loan,
Macu.currt_city,
Macu.edu_degree,
Macu.soc_id,
Macu.work_life,
Macu.unit_name,
Macu.unit_addr_city,
Macu.marg_status,
Macu.house_cond,
Maoth.mer_name,
Maoth.mer_status,
Maoth.max_dpd,
Maoth.curt_dpd,
Maoth.contra_status,
Maoth.curt_term,
Maoth.fpd,
Maoth.spd,
Maoth.tpd,
Maoth.qpd,
Maoth.fvpd,
Maoth.sxpd,
Maoth.svpd,
Maoth.etpd,
Maoth.nnpd,
Maoth.tnpd,
Maoth.evpd,
Maoth.twpd,
Maoth.xprod_cd,
Maoth.effc_start_dt,
Maoth.effc_end_dt,
Ms.age,
ms.Zhima_m,
Ms.bt3_m,
Ms.pbjxl_m,
Ms.cashl_m,
Ms.posjxl20_m,
Ms.posjxl21_m,
Ms.ins_m,
Ms.jxl_m,
Ms.cup_m
FROM
Risk_analysis.tmp_sqq_mabl AS mabl
JOIN
Risk_analysis.tmp_sqq_maru AS maru
ON
Mabl.appl_no = maru.appl_no
JOIN
Risk_analysis.tmp_sqq_macu AS macu
ON
Mabl.appl_no = macu.appl_no
JOIN
Risk_analysis.tmp_sqq_maoth AS maoth
ON
Mabl.appl_no = maoth.appl_no
LEFT JOIN
Risk_analysis.bak_sqq_modelscore AS ms
ON
Mabl.appl_no = ms.appl_no
;
-- SQL ends
-- Refresh
INVALIDATE METADATA risk_analysis.bak_sqq_custom_qual;
