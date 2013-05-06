#Change the directory in source function into your own local directory
source('C:/Users/zmyao/Documents/GitHub/facility_list_data/Refactored/facility_lists_shared_functions.R')
setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning")
#setwd("c:/Users/zmyao/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning")

#####################
#######merging####### 
#####################


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
