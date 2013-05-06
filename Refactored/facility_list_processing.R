#Change the directory in source function into your own local directory
source('C:/Users/zmyao/Documents/GitHub/facility_list_data/Refactored/facility_lists_shared_functions.R')
setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning")
#setwd("c:/Users/zmyao/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning")

###########################
#######Facility List####### 
###########################



#related data
# rural_urban <- read.csv("c:/Users/zmyao/Dropbox/Nigeria/Needs Assessment/data/population_Geo.csv")
rural_urban <- read.csv("~/Dropbox/Nigeria/Needs Assessment/data/population_Geo.csv")
one48 <- read.csv("148lga_final_list.csv")
lgas <- read.csv("lgas.csv")

##getting correct lga names/ids##
lga_corrections <- read.csv('in_process_data/nmis/nmis_lga_corrections.csv', stringsAsFactors=FALSE)
nmis_lga_mapping <- read.csv('in_process_data/nmis/nmis_lga_mapping.csv', stringsAsFactors=FALSE)

###############################
##importing/slicin/dicin data##
###############################

# READ IN "p-index" data, and clean it up
p_index1 <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_04_30_10_35_43_p.csv",
                     stringsAsFactors=F)
p_index2 <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_29_06_14_15_p.csv",
                     stringsAsFactors=F)
p_index3 <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_05_04_19_00_18_p.csv",
                     stringsAsFactors=F)

p_index1 <- p_index1[!(p_index1$mylga==""),]
p_index3 <- p_index3[!(p_index3$mylga==""),]
p_index1 <- add_lga_id(p_index1)
p_index2 <- add_lga_id(p_index2)
p_index3 <- add_lga_id(p_index3)
p_index1 <- rename(p_index1, c("X_parent_index" = "X_pindex"))
p_index2 <- rename(p_index2, c("X_parent_index" = "X_pindex"))
p_index3 <- rename(p_index3, c("X_parent_index" = "X_pindex"))


schools1_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_04_30_10_35_43_e.csv",
                         stringsAsFactors=F)
schools2_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_29_06_14_15_e.csv",
                         stringsAsFactors=F)
schools3_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_05_04_19_00_18_e.csv",
                         stringsAsFactors=F)

hospitals1_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_04_30_10_35_43_h.csv",
                           stringsAsFactors=F)
hospitals2_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_29_06_14_15_h.csv",
                           stringsAsFactors=F)
hospitals3_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_05_04_19_00_18_h.csv",
                           stringsAsFactors=F)

# LEAVE the _raw dataframes alone; they are useful for debugging
# the following dataframes (schools1, .., hospitals1, ..) will be modified in the script
schools1 <- schools1_raw
schools2 <- schools2_raw
schools3 <- schools3_raw
hospitals1 <- hospitals1_raw
hospitals2 <- hospitals2_raw
hospitals3 <- hospitals3_raw

## PRINT NUMBERS IS A USEFUL DEBUGGING FUNCTION -- prints out stuff for all three datasets for schools and hospitals
print_numbers <- function(where="") {
    print(where)
    print("Number of schools:")
    print(paste(nrow(schools1), nrow(schools2), nrow(schools3)))
    print("Number of health facilities:")
    print(paste(nrow(hospitals1), nrow(hospitals2), nrow(hospitals3)))
    print("Number of LGAs for education:")
    print(length(levels(factor(c(as.character(schools1$mylga), as.character(schools2$mylga), as.character(schools3$mylga))))))
    print("Number of LGAs for health:")
    print(length(levels(factor(c(as.character(hospitals1$mylga), as.character(hospitals2$mylga), as.character(hospitals3$mylga))))))
}
## PRINT NUMBERS2 IS A USEFUL DEBUGGING FUNCTION -- prints out stuff for schools hospitals once datasets merged
print_numbers2 <- function(where="") {
    print(where)
    print("Number of schools:")
    print(paste(nrow(schools)))
    print("Number of health facilities:")
    print(paste(nrow(hospitals)))
    print("Number of LGAs for education:")
    print(length(levels(factor(c(as.character(schools$mylga))))))
    print("Number of LGAs for health:")
    print(length(levels(factor(c(as.character(hospitals$mylga))))))
}

print_numbers("Just after reading")

## combine with the parent to pull in lga information, etc.
merge_with_parent <- function(parentDF, childDF) {
    childDF <- rename(childDF, c("X_index" = "uuid"))
    merge(parentDF, childDF, by.x = "X_index", by.y="X_parent_index", all.y=T)
}
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

#CLEANING:test/non-character/blank facility names
print_numbers2("Before name cleaning")



#education
schools <- cleanweirdchars(schools, "Schools.school_name")
schools <- cleanweirdchars(schools, "Schools.ward_name")
schools <- cleanweirdchars(schools, "Schools.ward_num")
schools <- cleanweirdchars(schools, "Schools.com_name")
schools <- schools[!str_detect(schools$Schools.school_name, '[*]'),]
schools <- subset(schools, !Schools.school_name %in% c("", "1"))

# MAKE SURE NONE OF THESE GENERATES WARNINGS -- ideally, they are 0 row dataframes
c_e1 <- schools[which(!str_detect(schools$Schools.school_name, '[a-zA-Z]')),]
c_e1 <- schools[which(!str_detect(schools$Schools.ward_name, '[a-zA-Z0-9]*')),]
c_e1 <- schools[which(!str_detect(schools$Schools.com_name, '[a-zA-Z]*')),]
c_e1 <- schools[which(!str_detect(schools$Schools.ward_num, '[a-zA-Z0-9]*')),]

#health
hospitals <- cleanweirdchars(hospitals, "HealthFacilities.health_facility_name")
hospitals <- cleanweirdchars(hospitals, "HealthFacilities.ward_name")
hospitals <- cleanweirdchars(hospitals, "HealthFacilities.com_name_h")

# remove facility names that only contain *, ^, or '
hospitals <- subset(hospitals, !HealthFacilities.health_facility_name %in% c("", "00", "33"))
hospitals <- hospitals[!str_detect(hospitals$HealthFacilities.health_facility_name, '^[ *^]*$'),]
hospitals <- hospitals[!str_detect(hospitals$HealthFacilities.health_facility_name, "\'\'"),]
hospitals <- hospitals[!str_detect(hospitals$HealthFacilities.health_facility_name, "0000+"),]
hospitals <- hospitals[!str_detect(hospitals$HealthFacilities.health_facility_name, "ˆ +$"),]

# MAKE SURE NONE OF THESE GENERATES WARNINGS
c_h1 <- hospitals[which(!str_detect(hospitals$HealthFacilities.health_facility_name, '[a-zA-Z]')), ]
c_h1 <- hospitals[which(!str_detect(hospitals$HealthFacilities.ward_name, '[a-zA-Z0-9]*')), ]
c_h1 <- hospitals[which(!str_detect(hospitals$HealthFacilities.com_name_h, '[a-zA-Z]*')), ]

print_numbers2("After name cleaning")

#CLEANING:duplicates                  
#education
c_e2 <- subset(schools, duplicated(schools[,1:11]) | duplicated(schools[,1:11], fromLast=T))
c_e2 <- arrange(c_e2, mylga_zone, mylga_state, mylga, Schools.school_name)
schools <- subset(schools, !duplicated(schools[,1:11]))
#health
c_h2 <- subset(hospitals, duplicated(hospitals[,1:10]) | duplicated(hospitals[,1:10], fromLast=T))
c_h2 <- arrange(c_h2, mylga_zone, mylga_state, mylga, HealthFacilities.health_facility_name)  
hospitals <- subset(hospitals, !duplicated(hospitals[,1:10]))
print_numbers2("After duplicate cleaning")

#ID:long_id
#education
test_e <- subset(schools, select=c(mylga, Schools.ward_name, 
                                   Schools.ward_num, Schools.com_name, Schools.school_managed,
                                   Schools.level_of_education, Schools.school_name, ta_name))
# IF THE OUTPUT IS NOT 0, something is wrong
nrow(test_e) - nrow(unique(test_e))
schools$paste <- paste0(schools$mylga, schools$Schools.ward_name, 
                        schools$Schools.ward_num, schools$Schools.com_name, schools$Schools.school_managed,
                        schools$Schools.level_of_education, schools$Schools.school_name, schools$ta_name)
schools$long_id <- sapply(schools$paste, FUN=digest)
schools$paste <- NULL
#health
test_h <- subset(hospitals, select=c(mylga,  ta_name, HealthFacilities.health_facility_name,
                                     HealthFacilities.health_facility_type, HealthFacilities.ward_name, 
                                     HealthFacilities.com_name_h, managed_by))
# IF THE OUTPUT IS NOT 0, something is wrong
nrow(test_h) - nrow(unique(test_h))
attach(hospitals)
hospitals$paste <- paste0(mylga, HealthFacilities.ward_name, HealthFacilities.health_facility_type, 
                          HealthFacilities.com_name_h,HealthFacilities.health_facility_name,
                          ta_name, managed_by)
detach(hospitals) 
hospitals$long_id <- sapply(hospitals$paste, FUN=digest)      
hospitals$paste <- NULL

# IF THE OUTPUT IS NOT 0, something is wrong
nrow(hospitals) - length(unique(hospitals$long_id)) 
nrow(schools) - length(unique(schools$long_id)) 




#education
schools <- ddply(schools, .(lga_id), function(x) shortid_generate(x, 'F'))

#health
hospitals <- ddply(hospitals, .(lga_id), function(x) shortid_generate(x, 'F'))


## WRITING OUT ## 
#zaiming cleaning
#education
index <- which(is.na(schools$Schools.ward_num) & !is.na(schools$Schools.ward_name))
schools$Schools.ward_num[index] <- schools$Schools.ward_name[index]
rm(index)
schools$Schools.ward_name <- NULL

#rename
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
schools_total <- ddply(schools, .(lga_id), summarize, facility_counts=length(lga_id))
hospitals_total <- ddply(hospitals, .(lga_id), summarize, facility_counts=length(lga_id))
write.csv(hospitals_total, "in_process_data/facility_lists/ossap updates/inprocess_data/f_agg_h.csv", row.names=F)
write.csv(schools_total, "in_process_data/facility_lists/ossap updates/inprocess_data/f_agg_e.csv", row.names=F)