INVALIDATE METADATA risk_analysis.bak_sqq_modelscore;
DROP TABLE IF EXISTS risk_analysis.bak_sqq_model3;
CREATE TABLE risk_analysis.bak_sqq_model3 
COMMENT 'bak_sqq_model3'
STORED AS PARQUET
AS
SELECT
rule.appl_no,
rule.round_par,
rule.workflow_cd,
rule.rule_content,
rule.rule_id,
main.prod_cd,
main.contra_no,
main.mer_no,
main.appl_tm,
main.appl_lim,
main.appl_loan_term,
main.appl_status,
main.biz_city_cd,
risk.max_dpd,
risk.fpd,
modelscore.Zhima_m,
modelscore.Zhimamsxf_m,
modelscore.bt3_m,
modelscore.pbjxl_m,
modelscore.cashl_m,
modelscore.posjxl20_m,
modelscore.posjxl21_m,
modelscore.ins_m,
modelscore.jxl_m,
modelscore.cup_m
FROM
risk_analysis.tmp_sqq_model3_rule AS rule
LEFT JOIN
risk_analysis.tmp_sqq_model3_main AS main
ON
rule.appl_no = main.appl_no
LEFT JOIN
risk_analysis.tmp_sqq_model3_risk AS risk
ON
rule.appl_no = risk.appl_no
LEFT JOIN
risk_analysis.bak_sqq_modelscore AS modelscore
ON
rule.appl_no = modelscore.appl_no
;
INVALIDATE METADATA risk_analysis.bak_sqq_model3;
-- Test pass
