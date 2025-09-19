library(shiny)
# X-ray updates
updateRadioButtons(session, "radio_xray_head", selected = df$radio_xray_head)
updateRadioButtons(session, "radio_xray_neck", selected = df$radio_xray_neck)
updateRadioButtons(session, "radio_xray_chest", selected = df$radio_xray_chest)
updateRadioButtons(session, "radio_xray_abd", selected = df$radio_xray_abd)
updateRadioButtons(session, "radio_xray_spine", selected = df$radio_xray_spine)
updateRadioButtons(session, "radio_xray_pelvisobs", selected = df$radio_xray_pelvisobs)
updateRadioButtons(session, "radio_xray_pelvisuro", selected = df$radio_xray_pelvisuro)
updateRadioButtons(session, "radio_xray_pelvisother", selected = df$radio_xray_pelvisother)
updateRadioButtons(session, "radio_xray_lue", selected = df$radio_xray_lue)
updateRadioButtons(session, "radio_xray_rue", selected = df$radio_xray_rue)
updateRadioButtons(session, "radio_xray_lle", selected = df$radio_xray_lle)
updateRadioButtons(session, "radio_xray_rle", selected = df$radio_xray_rle)
updateRadioButtons(session, "radio_xray_vascular", selected = df$radio_xray_vascular)

# Ultrasound updates
updateRadioButtons(session, "radio_ultra_head", selected = df$radio_ultra_head)
updateRadioButtons(session, "radio_ultra_neck", selected = df$radio_ultra_neck)
updateRadioButtons(session, "radio_ultra_chest", selected = df$radio_ultra_chest)
updateRadioButtons(session, "radio_ultra_abd", selected = df$radio_ultra_abd)
updateRadioButtons(session, "radio_ultra_spine", selected = df$radio_ultra_spine)
updateRadioButtons(session, "radio_ultra_pelvisobs", selected = df$radio_ultra_pelvisobs)
updateRadioButtons(session, "radio_ultra_pelvisuro", selected = df$radio_ultra_pelvisuro)
updateRadioButtons(session, "radio_ultra_pelvisother", selected = df$radio_ultra_pelvisother)
updateRadioButtons(session, "radio_ultra_lue", selected = df$radio_ultra_lue)
updateRadioButtons(session, "radio_ultra_rue", selected = df$radio_ultra_rue)
updateRadioButtons(session, "radio_ultra_lle", selected = df$radio_ultra_lle)
updateRadioButtons(session, "radio_ultra_rle", selected = df$radio_ultra_rle)
updateRadioButtons(session, "radio_ultra_vascular", selected = df$radio_ultra_vascular)

# CT Scan updates
updateRadioButtons(session, "radio_ctscan_head", selected = df$radio_ctscan_head)
updateRadioButtons(session, "radio_ctscan_neck", selected = df$radio_ctscan_neck)
updateRadioButtons(session, "radio_ctscan_chest", selected = df$radio_ctscan_chest)
updateRadioButtons(session, "radio_ctscan_abd", selected = df$radio_ctscan_abd)
updateRadioButtons(session, "radio_ctscan_spine", selected = df$radio_ctscan_spine)
updateRadioButtons(session, "radio_ctscan_pelvisobs", selected = df$radio_ctscan_pelvisobs)
updateRadioButtons(session, "radio_ctscan_pelvisuro", selected = df$radio_ctscan_pelvisuro)
updateRadioButtons(session, "radio_ctscan_pelvisother", selected = df$radio_ctscan_pelvisother)
updateRadioButtons(session, "radio_ctscan_lue", selected = df$radio_ctscan_lue)
updateRadioButtons(session, "radio_ctscan_rue", selected = df$radio_ctscan_rue)
updateRadioButtons(session, "radio_ctscan_lle", selected = df$radio_ctscan_lle)
updateRadioButtons(session, "radio_ctscan_rle", selected = df$radio_ctscan_rle)
updateRadioButtons(session, "radio_ctscan_vascular", selected = df$radio_ctscan_vascular)

# MRI updates
updateRadioButtons(session, "radio_mri_head", selected = df$radio_mri_head)
updateRadioButtons(session, "radio_mri_neck", selected = df$radio_mri_neck)
updateRadioButtons(session, "radio_mri_chest", selected = df$radio_mri_chest)
updateRadioButtons(session, "radio_mri_abd", selected = df$radio_mri_abd)
updateRadioButtons(session, "radio_mri_spine", selected = df$radio_mri_spine)
updateRadioButtons(session, "radio_mri_pelvis_obs", selected = df$radio_mri_pelvis_obs)
updateRadioButtons(session, "radio_mri_pelvis_uro", selected = df$radio_mri_pelvis_uro)
updateRadioButtons(session, "radio_mri_pelvis_other", selected = df$radio_mri_pelvis_other)
updateRadioButtons(session, "radio_mri_lue", selected = df$radio_mri_lue)
updateRadioButtons(session, "radio_mri_rue", selected = df$radio_mri_rue)
updateRadioButtons(session, "radio_mri_lle", selected = df$radio_mri_lle)
updateRadioButtons(session, "radio_mri_rle", selected = df$radio_mri_rle)
updateRadioButtons(session, "radio_mri_vascular", selected = df$radio_mri_vascular)


updateRadioButtons(session, "consult_gensurg_recom", selected = df$consult_gensurg_recom)
updateRadioButtons(session, "consult_gensurg_called", selected = df$consult_gensurg_called)
updateRadioButtons(session, "consult_gensurg_arrived", selected = df$consult_gensurg_arrived)

updateRadioButtons(session, "consult_ortho_recom", selected = df$consult_ortho_recom)
updateRadioButtons(session, "consult_ortho_called", selected = df$consult_ortho_called)
updateRadioButtons(session, "consult_ortho_arrived", selected = df$consult_ortho_arrived)

updateRadioButtons(session, "consult_neuro_recom", selected = df$consult_neuro_recom)
updateRadioButtons(session, "consult_neuro_called", selected = df$consult_neuro_called)
updateRadioButtons(session, "consult_neuro_arrived", selected = df$consult_neuro_arrived)

updateRadioButtons(session, "consult_vasc_recom", selected = df$consult_vasc_recom)
updateRadioButtons(session, "consult_vasc_called", selected = df$consult_vasc_called)
updateRadioButtons(session, "consult_vasc_arrived", selected = df$consult_vasc_arrived)

updateRadioButtons(session, "consult_ent_recom", selected = df$consult_ent_recom)
updateRadioButtons(session, "consult_ent_called", selected = df$consult_ent_called)
updateRadioButtons(session, "consult_ent_arrived", selected = df$consult_ent_arrived)

updateRadioButtons(session, "consult_plastic_recom", selected = df$consult_plastic_recom)
updateRadioButtons(session, "consult_plastic_called", selected = df$consult_plastic_called)
updateRadioButtons(session, "consult_plastic_arrived", selected = df$consult_plastic_arrived)

updateRadioButtons(session, "consult_other_recom", selected = df$consult_other_recom)
updateRadioButtons(session, "consult_other_called", selected = df$consult_other_called)
updateRadioButtons(session, "consult_other_arrived", selected = df$consult_other_arrived)

#Code of treatment
# Analgesic
updateRadioButtons(session, "treat_analgesic_recom", selected = df$treat_analgesic_recom)
updateRadioButtons(session, "treat_analgesic_received", selected = df$treat_analgesic_received)

# Anticoagulant / Blood thinner
updateRadioButtons(session, "treat_anticoagulant_recom", selected = df$treat_anticoagulant_recom)
updateRadioButtons(session, "treat_anticoagulant_received", selected = df$treat_anticoagulant_received)

# Antitetanus
updateRadioButtons(session, "treat_antitetanus_recom", selected = df$treat_antitetanus_recom)
updateRadioButtons(session, "treat_antitetanus_received", selected = df$treat_antitetanus_received)

# Antibiotic
updateRadioButtons(session, "treat_antibiotic_recom", selected = df$treat_antibiotic_recom)
updateRadioButtons(session, "treat_antibiotic_received", selected = df$treat_antibiotic_received)

# Fluid - Crystalloid
updateRadioButtons(session, "treat_crystalloid_recom", selected = df$treat_crystalloid_recom)
updateRadioButtons(session, "treat_crystalloid_received", selected = df$treat_crystalloid_received)

# Fluid - Colloid
updateRadioButtons(session, "treat_colloid_recom", selected = df$treat_colloid_recom)
updateRadioButtons(session, "treat_colloid_received", selected = df$treat_colloid_received)

# Blood
updateRadioButtons(session, "treat_blood_recom", selected = df$treat_blood_recom)
updateRadioButtons(session, "treat_blood_received", selected = df$treat_blood_received)

# Tranexamic Acid
updateRadioButtons(session, "treat_tranexacid_recom", selected = df$treat_tranexacid_recom)
updateRadioButtons(session, "treat_tranexacid_received", selected = df$treat_tranexacid_received)

# PPI / H2 Blocker
updateRadioButtons(session, "treat_ppi_recom", selected = df$treat_ppi_recom)
updateRadioButtons(session, "treat_ppi_received", selected = df$treat_ppi_received)

# Other (specify)
updateRadioButtons(session, "treat_other_recom", selected = df$treat_other_recom)
updateRadioButtons(session, "treat_other_received", selected = df$treat_other_received)

# Splint / Cast / Sling
updateRadioButtons(session, "treat_split_recom", selected = df$treat_split_recom)
updateRadioButtons(session, "treat_split_received", selected = df$treat_split_received)

# External Reduction
updateRadioButtons(session, "treat_reduct_recom", selected = df$treat_reduct_recom)
updateRadioButtons(session, "treat_reduct_received", selected = df$treat_reduct_received)

# Debridement / Foreign Body Removal / Laceration Repair
updateRadioButtons(session, "treat_debride_recom", selected = df$treat_debride_recom)
updateRadioButtons(session, "treat_debride_received", selected = df$treat_debride_received)

# Nasogastric Tube Placement
updateRadioButtons(session, "treat_nasogastric_recom", selected = df$treat_nasogastric_recom)
updateRadioButtons(session, "treat_nasogastric_received", selected = df$treat_nasogastric_received)

# Urinary Catheter Placement
updateRadioButtons(session, "treat_catheter_recom", selected = df$treat_catheter_recom)
updateRadioButtons(session, "treat_catheter_received", selected = df$treat_catheter_received)



# Code for Labs and disposition
updateRadioButtons(session, "upt_recommend", selected = df$upt_recommend)
updateRadioButtons(session, "upt_performed", selected = df$upt_performed)
updateRadioButtons(session, "upt_result", selected = df$upt_result)

updateRadioButtons(session, "hgb_recommend", selected = df$hgb_recommend)
updateRadioButtons(session, "hgb_performed", selected = df$hgb_performed)

updateRadioButtons(session, "bg_recommend", selected = df$bg_recommend)
updateRadioButtons(session, "bg_performed", selected = df$bg_performed)
updateRadioButtons(session, "bg_result", selected = df$bg_result)

updateRadioButtons(session, "disposition", selected = df$disposition)
updateRadioButtons(session, "disposition_transfer", selected = df$disposition_transfer)
