
######################
#######baseline####### 
######################
#education
e_661 <- read.csv("in_process_data/nmis/Education_661_ALL_FACILITY_INDICATORS.csv",
                  stringsAsFactors=F)
e_661 <- subset(e_661, select=c(X_lga_id, mylga_zone, mylga_state, mylga, ward, community, school_name, level_of_education, uuid))
e_661 <- rename(e_661, c("mylga" = "lga", "mylga_state" = "state", "mylga_zone" = "zone", "X_lga_id" = "lga_id"))
# OUTPUT SHOULD BE 0
anyDuplicated(e_661$uuid)

e_113 <- read.csv("in_process_data/facility_lists/raw data/113/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov16_without_emptyobs.csv",
                  stringsAsFactors=F)
e_113 <- subset(e_113, subset=(gps != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
e_113$uuid <- sapply(paste(e_113$gps, e_113$photo), FUN=digest)
e_113 <- add_lga_id(e_113, "lga", "state")
e_113 <- subset(e_113, select=c(lga_id, zone, state, lga, ward, community, school_name, level_of_education, uuid))
# OUTPUT SHOULD BE 0
anyDuplicated(e_113$uuid)

e_pilot <- read.csv("in_process_data/facility_lists/raw data/113/Pilot_Education_cleaned_2011Oct4.csv",
                    stringsAsFactors=F)
e_pilot <- subset(e_pilot, subset=(gps != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
e_pilot$uuid <- sapply(paste(e_pilot$gps, e_pilot$photo), FUN=digest)
e_pilot <- add_lga_id(e_pilot, "lga", "state")
e_pilot <- subset(e_pilot, select=c(lga_id, zone, state, lga, ward, community, school_name, level_of_education, uuid))
# OUTPUT SHOULD BE 0
anyDuplicated(e_pilot$uuid)

edu <- rbind.fill(e_113, e_661, e_pilot)  
edu <- subset(edu, select=c(lga_id, zone, state, lga, ward, community, school_name, level_of_education, uuid))
edu <- cleanweirdchars(cleanweirdchars(cleanweirdchars(edu, 'community'), 'school_name'), 'ward')
# OUTPUT SHOULD BE 0
anyDuplicated(edu$uuid)


#health
h_661 <- read.csv("in_process_data/nmis/Health_661_ALL_FACILITY_INDICATORS.csv",
                  stringsAsFactors=F)
h_661 <- subset(h_661, select=c(X_lga_id, mylga_zone, mylga_state, mylga, ward, community, facility_name, facility_type, uuid))
h_661 <- rename(h_661, c("mylga" = "lga", "mylga_state" = "state", "mylga_zone" = "zone", "X_lga_id" = "lga_id"))
# OUTPUT SHOULD BE 0
anyDuplicated(h_661$uuid)

# OUTPUT SHOULD BE 0
h_113 <- read.csv("in_process_data/facility_lists/raw data/113/Health_PhaseII_RoundI&II&III_Clean_2011.11.16.csv",
                  stringsAsFactors=F)
h_113 <- subset(h_113, subset=(geocodeoffacility != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
h_113$uuid <- sapply(paste(h_113$geocodeoffacility, h_113$photo), FUN=digest)
h_113 <- add_lga_id(h_113, "lga", "state")
h_113 <- subset(h_113, select=c(lga_id, zone, state, lga, ward, community, facility_name, facility_type, uuid))
# THE Following line is dangerous to replicate;  WAS WRITTEN AFTER CAREFUL INSPECTION OF DUPLICATES
h_113 <- subset(h_113, !duplicated(h_113$uuid))
# OUTPUT SHOULD BE 0
anyDuplicated(h_113$uuid)

h_pilot <- read.csv("in_process_data/facility_lists/raw data/113/Pilot_Data_Hlth_Clean_2011.09.02.csv", stringsAsFactors=F)
h_pilot <- subset(h_pilot, subset=(geocodeoffacility != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
h_pilot$uuid <- sapply(paste(h_pilot$geocodeoffacility, h_pilot$photo), FUN=digest)
h_pilot <- add_lga_id(h_pilot, "lga", "state")
h_pilot <- subset(h_pilot, select=c(lga_id, zone, state, lga, ward, community, facility_name, facility_type, uuid))
# OUTPUT SHOULD BE 0
anyDuplicated(h_pilot$uuid)

health <- rbind.fill(h_113, h_pilot, h_661)
health <- subset(health, select=c(lga_id, zone, state, lga, ward, community, facility_name, facility_type, uuid))
health <- cleanweirdchars(cleanweirdchars(cleanweirdchars(health, 'community'), 'facility_name'), 'ward')

# OUTPUT SHOULD BE 0
anyDuplicated(health$uuid)

#FINISH RENAME 
edu <- rename(edu, c("school_name" = "facility_name", "uuid" = "long_id","level_of_education" = "facility_type"))
health <- rename(health, c("uuid" = "long_id"))

##zaiming cleaning
#####
#####
#####

edu <- shortid_generate(edu, 'B')

health <- shortid_generate(health, 'B')

#names(edu)
edu <- facility_name_fix_FBe(df=edu, school_name_col="facility_name")
edu <- ward_comm_fix_FBe_Bh(df=edu, ward_col="ward", comunity_col="community")

#names(health)
health <- facility_name_fix_FBh(df=health, facility_name_col="facility_name")
health <- ward_comm_fix_FBe_Bh(df=health, ward_col="ward", comunity_col="community")

write.csv(edu, "in_process_data/facility_lists/BASELINE_schools.csv", row.names=F)
write.csv(health, "in_process_data/facility_lists/BASELINE_hospitals.csv", row.names=F)

## AGGREGATION BY LGA: baseline
health <- idata.frame(health)
edu <- idata.frame(edu)
h_total <- ddply(health, .(lga_id), nrow)
e_total <- ddply(edu, .(lga_id), nrow)
write.csv(h_total, "in_process_data/facility_lists/ossap updates/inprocess_data/b_agg_h.csv", row.names=F)
write.csv(e_total, "in_process_data/facility_lists/ossap updates/inprocess_data/b_agg_e.csv", row.names=F)

######################
#######merging####### 
######################
#education
baseline_total <- merge(lgas, e_total, by.x="X_lga_id", by.y="lga_id", all.x=T)
baseline_total <- rename(baseline_total, c("V1" = "baseline_facilitiy_count"))
schools_total <- merge(lgas, schools_total, by.x="X_lga_id", by.y="lga_id", all.x=T) 
schools_agg <- merge(baseline_total, schools_total, by="X_lga_id", all.x=F)
schools_agg <- merge(schools_agg, subset(rural_urban, select=c("lga_id", "urban_rural")), by.x="X_lga_id", by.y="lga_id")
names(schools_agg)[2] <- "lga"
names(schools_agg)[3] <- "state"
names(schools_agg)[4] <- "zone"
schools_agg <- subset(schools_agg, select=c(X_lga_id, lga, state, zone, baseline_facilitiy_count, facility_counts, urban_rural))
schools_agg <- arrange(schools_agg, zone, state, lga)
#hospital
baseline_total_h <- merge(lgas, h_total, by.x="X_lga_id", by.y="lga_id", all.x=T)
baseline_total_h <- rename(baseline_total_h, c("V1" = "base_line_faciliti_count"))
hospitals_total <- merge(lgas, hospitals_total, by.x="X_lga_id", by.y="lga_id", all.x=T)
hospitals_agg <- merge(baseline_total_h, hospitals_total, by="X_lga_id", all.y=T)
hospitals_agg <- subset(hospitals_agg, select=c(X_lga_id, lga.x, state.x, zone.x, base_line_faciliti_count, facility_counts))
names(hospitals_agg)[2] <- "lga"
names(hospitals_agg)[3] <- "state"
names(hospitals_agg)[4] <- "zone"
hospitals_agg <- merge(hospitals_agg, subset(rural_urban, select=c("lga_id", "urban_rural")), by.x="X_lga_id", by.y="lga_id", all.x=T)
hospitals_agg <- arrange(hospitals_agg, zone, state, lga)

## WRITING OUT: baseline&facility list data
write.csv(hospitals_agg, "in_process_data/facility_lists/summary_health.csv", row.names=F)
write.csv(schools_agg, "in_process_data/facility_lists/summary_education.csv", row.names=F)

#### graphs
ggplot(data=schools_agg, aes(x=baseline_facilitiy_count, y=facility_counts, label=lga)) + 
  geom_point(position="jitter") + geom_abline(intercept=0, slope=1) + labs(title = "Education", x = "NMIS", y="Facility lists") + stat_smooth(method="lm")
ggplot(data=hospitals_agg, aes(x=base_line_faciliti_count, y=facility_counts, label=lga)) +  
  geom_point(position="jitter") + geom_abline(intercept=0, slope=1) + labs(title = "Health", x = "NMIS", y="Facility Lists") + stat_smooth(method="lm")


