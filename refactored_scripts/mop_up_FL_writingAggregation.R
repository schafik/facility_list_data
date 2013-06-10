
## WRITING OUT ## 
#zaiming cleaning
index <- which(is.na(schools$Schools.ward_num) & !is.na(schools$Schools.ward_name))
schools$Schools.ward_num[index] <- schools$Schools.ward_name[index]
rm(index)
schools$Schools.ward_name <- NULL
schools <- rename(schools, c("Schools.school_name" = "facility_name"))
schools <- rename(schools, c("Schools.level_of_education" = "facility_type"))
schools <- rename(schools, c("Schools.school_managed" = "managed_by"))
schools <- rename(schools, c("Schools.ward_num" = "ward"))
schools <- rename(schools, c("Schools.com_name" = "community"))
schools <- rename(schools, c("mylga_zone" = "zone"))
schools <- rename(schools, c("mylga_state" = "state"))
schools <- rename(schools, c("mylga" = "lga"))
hospitals <- rename(hospitals, c("HealthFacilities.health_facility_name" = "facility_name"))
hospitals <- rename(hospitals, c("HealthFacilities.health_facility_type" = "facility_type"))
hospitals <- rename(hospitals, c("HealthFacilities.ward_name" = "ward"))
hospitals <- rename(hospitals, c("HealthFacilities.com_name_h" = "community"))
hospitals <- rename(hospitals, c("mylga_zone" = "zone"))
hospitals <- rename(hospitals, c("mylga_state" = "state"))
hospitals <- rename(hospitals, c("mylga" = "lga"))

#Name spelling standardization
hospitals <- ward_comm_fix_Fh(hospitals, 'ward', 'community')
hospitals <- facility_name_fix_FBh(df=hospitals, facility_name_col= 'facility_name')

schools <- ward_comm_fix_FBe_Bh(df=schools, ward_col='ward', comunity_col='community')
schools <- facility_name_fix_FBe(df=schools, school_name_col= 'facility_name')

#writing
schools <- subset(schools, select=c(-ta_name))
hospitals <- subset(hospitals, select=c(-ta_name))
write.csv(schools, "in_process_data/facility_lists/FACILITY_LIST_schools.csv", row.names=F, quote=F)
write.csv(hospitals, "in_process_data/facility_lists/FACILITY_LIST_hospitals.csv", row.names=F, quote=F)

## AGGREGATION BY LGA: facility lists
schools <- idata.frame(schools)
hospitals <- idata.frame(schools)  
schools_total <- ddply(schools, .(lga_id), summarize, facility_counts=length(lga_id))
hospitals_total <- ddply(hospitals, .(lga_id), summarize, facility_counts=length(lga_id))
write.csv(hospitals_total, "in_process_data/facility_lists/ossap updates/inprocess_data/f_agg_h.csv", row.names=F)
write.csv(schools_total, "in_process_data/facility_lists/ossap updates/inprocess_data/f_agg_e.csv", row.names=F)


