#chinedu output
## READING IN DATA
    #f_lists
    hospitals_total <- read.csv("in_process_data/facility_lists/ossap updates/inprocess_data/f_agg_h.csv", stringsAsFactors=FALSE)
    schools_total <- read.csv("in_process_data/facility_lists/ossap updates/inprocess_data/f_agg_e.csv", stringsAsFactors=FALSE)
    #baseline
    h_total <- read.csv("in_process_data/facility_lists/ossap updates/inprocess_data/b_agg_h.csv", stringsAsFactors=FALSE)
    e_total <- read.csv("in_process_data/facility_lists/ossap updates/inprocess_data/b_agg_e.csv", stringsAsFactors=FALSE)
    #both
    hospitals_agg <- read.csv("in_process_data/facility_lists/summary_health.csv", stringsAsFactors=FALSE)
    schools_agg <- read.csv("in_process_data/facility_lists/summary_education.csv", stringsAsFactors=FALSE)

##########################
####exploring data further
##########################
##missing facility lists##
schools_total_missing <- schools_total
schools_total_missing <- subset(schools_total_missing, is.na(schools_total_missing$facility_counts))
schools_total_missing$X <- NULL
schools_total_missing$facility_counts <- NULL
schools_total_missing$label <- NULL
schools_total_missing$X_lga_id <- NULL
education_missing <- arrange(schools_total_missing, zone, state, lga)
write.csv(education_missing, "~/Desktop/missing_facilities.csv", row.names=F)
      
##status of facility lists##
#education#
schools_agg$sound_yn <- "yes"
schools_agg$sound_yn <- ifelse(schools_agg$facility_counts / schools_agg$baseline_facilitiy_count < 0.7, "NO - more than 10 facilities discrepancy",
                               schools_agg$sound_yn)
schools_agg$X <- NULL
schools_agg_submit <- subset(schools_agg, !is.na(schools_agg$facility_counts))     
education_submissions <- arrange(schools_agg_submit, zone, state, lga)     
write.csv(education_submissions, "in_process_data/facility_lists/ossap updates/education_submissions.csv", row.names=F)
#health
hospitals_agg$sound_yn <- "yes - more reported in facility lists"
hospitals_agg$sound_yn <- ifelse(hospitals_agg$facility_counts / hospitals_agg$base_line_faciliti_count < 0.7, "NO - more captured in baseline",
                                 hospitals_agg$sound_yn)
hospitals_agg$X <- NULL
hospitals_agg_submit <- subset(hospitals_agg, !is.na(hospitals_agg$facility_counts))
health_submissions <- arrange(hospitals_agg_submit, zone, state, lga)    
write.csv(health_submissions, "in_process_data/facility_lists/ossap updates/health_submissions.csv", row.names=F)

##outliers##
#education
less_f_than_b_e <- subset(schools_agg,  sound_yn == "NO - more captured in baseline")
less_f_than_b_e <- merge(less_f_than_b_e, subset(schools, select=c(lga_id, ta_name)), by.x="X_lga_id", by.y="lga_id", all.x=T)
less_f_than_b_e <- less_f_than_b_e[!(duplicated(less_f_than_b_e$lga)),]
education_problematic <- arrange(less_f_than_b_e, state, zone, lga)
#   less_f_than_b_e <- subset(less_f_than_b_e, !str_detect(less_f_than_b_e$ta_name, 
#                         'Akima|AKIMA|Kabiru|KABIRU|ABAYOMI|AHMED IBRAHIM MOHAMMED|TEGA DAVID|Tega David Ayuya'))
write.csv(less_f_than_b_e, "in_process_data/facility_lists/ossap updates/education_problematic.csv", row.names=F)
    
#health
less_f_than_b_h <- subset(hospitals_agg,  sound_yn == "NO - more captured in baseline")
less_f_than_b_h <- merge(less_f_than_b_h, subset(hospitals, select=c(lga_id, ta_name)), by.x="X_lga_id", by.y="lga_id", all.x=T)
less_f_than_b_h <- less_f_than_b_h[!(duplicated(less_f_than_b_h$lga)),]
health_problematic <- arrange(less_f_than_b_h, state, zone, lga)
#   less_f_than_b_h <- subset(less_f_than_b_h, !str_detect(less_f_than_b_h$ta_name, 
#                         'Akima|AKIMA|Kabiru|KABIRU|ABAYOMI|AHMED IBRAHIM MOHAMMED|TEGA DAVID|Tega David Ayuya'))
# 
write.csv(less_f_than_b_h, "in_process_data/facility_lists/ossap updates/health_problematic.csv", row.names=F)
    
###### TAs
#education
# tas_e <- merge(schools_agg, subset(schools, select=c(lga_id, ta_name)), by.x="X_lga_id", by.y="lga_id", all.x=T)
# tas_e <- tas_e[!(duplicated(tas_e$lga)),]
# #health
# tas_h <- merge(hospitals_agg, subset(hospitals, select=c(lga_id, ta_name)), by.x="X_lga_id", by.y="lga_id", all.x=T)
# tas_h <- tas_h[!(duplicated(tas_h$lga)),]

##ta's we need to call back for...##
##re-doing the facility list submission
#   ta_call_back <- subset(p_index1, select=c(ta_name, mylga), mylga=="")

# ####### random sample
# sFh_1 <- subset(hospitals, lga_id==80)
# sBh_1 <- subset(health, X_lga_id==80)
# sFh_2 <- subset(hospitals, lga_id==434)
# sBh_2 <- subset(health, X_lga_id==434)
# sFh_3 <- subset(hospitals, lga_id==55)
# sBh_3 <- subset(health, X_lga_id==55)
# sFh_4 <- subset(hospitals, lga_id==532)
# sBh_4 <- subset(health, X_lga_id==290)
# sFh_5 <- subset(hospitals, lga_id==607)
# sBh_5 <- subset(health, X_lga_id==607)
# write.csv(sFh_1, "in_process_data/facility_lists/randomized samples/health/sample1_FACILITY.csv", row.names=F)
# write.csv(sBh_1, "in_process_data/facility_lists/randomized samples/health/sample1_BASELINE.csv", row.names=F)
# write.csv(sFh_2, "in_process_data/facility_lists/randomized samples/health/sample2_FACILITY.csv", row.names=F)
# write.csv(sBh_2, "in_process_data/facility_lists/randomized samples/health/sample2_BASELINE.csv", row.names=F)
# write.csv(sFh_3, "in_process_data/facility_lists/randomized samples/health/sample3_FACILITY.csv", row.names=F)
# write.csv(sBh_3, "in_process_data/facility_lists/randomized samples/health/sample3_BASELINE.csv", row.names=F)
# write.csv(sFh_4, "in_process_data/facility_lists/randomized samples/health/sample4_FACILITY.csv", row.names=F)
# write.csv(sBh_4, "in_process_data/facility_lists/randomized samples/health/sample4_BASELINE.csv", row.names=F)
# write.csv(sFh_5, "in_process_data/facility_lists/randomized samples/health/sample5_FACILITY.csv", row.names=F)
# write.csv(sBh_5, "in_process_data/facility_lists/randomized samples/health/sample5_BASELINE.csv", row.names=F)

######## Aggregation by state
# Hospitals
hospital_agg_state <- ddply(hospitals_agg, .(state,zone), summarise, 
      base_line_faciliti_count = sum(base_line_faciliti_count, na.rm=T),
      facility_counts = sum(facility_counts,na.rm=T))
hospital_agg_state$sound_yn <- ifelse(hospital_agg_state$facility_counts <= hospital_agg_state$base_line_faciliti_count, "NO - more captured in baseline",
                                     "yes - more reported in facility lists")
hospital_agg_state <- arrange(hospital_agg_state, zone, state)
    
# Schools 
schools_agg_state <- ddply(schools_agg, .(state,zone), summarise, 
                           baseline_facilitiy_count = sum(baseline_facilitiy_count, na.rm=T),
                           facility_counts = sum(facility_counts,na.rm=T))
schools_agg_state$sound_yn <- ifelse(schools_agg_state$facility_counts <= schools_agg_state$baseline_facilitiy_count, "NO - more captured in baseline",
                                      "yes - more reported in facility lists")
schools_agg_state <- arrange(schools_agg_state, zone, state)

# outputs
write.csv(hospital_agg_state, "in_process_data/facility_lists/ossap updates/summary_health_by_STATE.csv", row.names=F)
write.csv(schools_agg_state, "in_process_data/facility_lists/ossap updates/summary_education_by_STATE.csv", row.names=F)