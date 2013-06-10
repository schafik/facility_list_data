setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning")
#setwd("c:/Users/zmyao/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning")

##related data
# rural_urban <- read.csv("c:/Users/zmyao/Dropbox/Nigeria/Needs Assessment/data/population_Geo.csv")
rural_urban <- read.csv("~/Dropbox/Nigeria/Needs Assessment/data/population_Geo.csv")
one48 <- read.csv("148lga_final_list.csv")
lgas <- read.csv("lgas.csv")
#getting correct lga names/ids
lga_corrections <- read.csv('in_process_data/nmis/nmis_lga_corrections.csv', stringsAsFactors=FALSE)
nmis_lga_mapping <- read.csv('in_process_data/nmis/nmis_lga_mapping.csv', stringsAsFactors=FALSE)

###############################
##importing/slicin/dicin data##
###############################

# READ IN "p-index" data, and clean it up
p_index1 <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_05_14_14_28_44_p.csv",
                     stringsAsFactors=F)
p_index2 <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_29_06_14_15_p.csv",
                     stringsAsFactors=F)
p_index3 <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_05_23_10_43_55_p.csv",
                     stringsAsFactors=F)

p_index1 <- p_index1[!(p_index1$mylga==""),]
p_index3 <- p_index3[!(p_index3$mylga==""),]
p_index1 <- add_lga_id(p_index1)
p_index2 <- add_lga_id(p_index2)
p_index3 <- add_lga_id(p_index3)
p_index1 <- rename(p_index1, c("X_parent_index" = "X_pindex"))
p_index2 <- rename(p_index2, c("X_parent_index" = "X_pindex"))
p_index3 <- rename(p_index3, c("X_parent_index" = "X_pindex"))


schools1_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_05_14_14_28_44_e.csv",
                         stringsAsFactors=F)
schools2_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_29_06_14_15_e.csv",
                         stringsAsFactors=F)
schools3_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_05_23_10_43_55_e.csv",
                         stringsAsFactors=F)

hospitals1_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_05_14_14_28_44_h.csv",
                           stringsAsFactors=F)
hospitals2_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_29_06_14_15_h.csv",
                           stringsAsFactors=F)
hospitals3_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_05_23_10_43_55_h.csv",
                           stringsAsFactors=F)

# LEAVE the _raw dataframes alone; they are useful for debugging
# the following dataframes (schools1, .., hospitals1, ..) will be modified in the script
schools1 <- schools1_raw
schools2 <- schools2_raw
schools3 <- schools3_raw
hospitals1 <- hospitals1_raw
hospitals2 <- hospitals2_raw
hospitals3 <- hospitals3_raw

print_numbers("Just after reading")

## combine with the parent to pull in lga information, etc.
schools1 <- merge_with_parent(p_index1, schools1)
schools2 <- merge_with_parent(p_index2, schools2)
schools3 <- merge_with_parent(p_index3, schools3)

hospitals1 <- merge_with_parent(p_index1, hospitals1)
hospitals2 <- merge_with_parent(p_index2, hospitals2)
hospitals3 <- merge_with_parent(p_index3, hospitals3)
print_numbers("After merging schools and hospitals with parent info")

## cleaning from blank lga information
schools1 <- subset(schools1, !(mylga == ""))
schools2 <- subset(schools2, !(mylga == ""))
schools3 <- subset(schools3, !(mylga == ""))
hospitals1 <- subset(hospitals1, !(mylga == ""))
hospitals2 <- subset(hospitals2, !(mylga == ""))
hospitals3 <- subset(hospitals3, !(mylga == ""))
print_numbers("After cleaning")

## joining together
schools <- rbind.fill(schools1, schools2, schools3)
hospitals <- rbind.fill(hospitals1, hospitals2, hospitals3)
print_numbers2("After Joining")

# MANAGED BY FOR HOSPITALS
hospitals$managed_by <- 
  ifelse(hospitals$HealthFacilities.facility_owner_manager.federalgovernment, 'fed_gov',
         ifelse(hospitals$HealthFacilities.facility_owner_manager.stategovernment, 'state_gov',
                ifelse(hospitals$HealthFacilities.facility_owner_manager.lga, 'lga_gov',
                       'priv_or_religious')))

#subsetting data
schools <- subset(schools, select=c(lga_id, mylga_zone, mylga_state, mylga, ta_name,
                                    Schools.school_name, Schools.level_of_education,
                                    Schools.school_managed, Schools.ward_name, 
                                    Schools.ward_num, Schools.com_name))
# DON'T ARRANGE: SHORT ID DEPENDS ON ORDER OF INPUT
hospitals <- subset(hospitals, select=c(lga_id, mylga_zone, mylga_state, mylga, ta_name,
                                        HealthFacilities.health_facility_name, 
                                        HealthFacilities.health_facility_type,
                                        HealthFacilities.ward_name, HealthFacilities.com_name_h,
                                        managed_by))
# DON'T ARRANGE: SHORT ID DEPENDS ON ORDER OF INPUT



