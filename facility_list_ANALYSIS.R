setwd("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning")
#setwd("c:/Users/zmyao/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning")
library(digest)
library(data.table)
library(formhub)
library(ggplot2)
library(doBy)
library(gdata)
library(plyr)
"clean" = function(dt, cols, rows, value) {
    if (any(rows)) {
        set(dt, rows, cols, value) }}

#related data
rural_urban <- read.csv("~/Dropbox/Nigeria/Needs Assessment/data/population_Geo.csv")
one48 <- read.csv("148lga_final_list.csv")
lgas <- read.csv("lgas.csv")

##getting correct lga names/ids##
lga_corrections <- read.csv('in_process_data/nmis/nmis_lga_corrections.csv', stringsAsFactors=FALSE)
nmis_lga_mapping <- read.csv('in_process_data/nmis/nmis_lga_mapping.csv', stringsAsFactors=FALSE)
add_lga_id = function(df) {
    df$unique_lga <- ifelse(df$mylga %in% c('ERROR', NA),
                            NA,
                            str_c(df$mylga_state, df$mylga, sep="_"))
    df$unique_lga <- recodeVar(df$unique_lga, src=lga_corrections$orginal, tgt=lga_corrections$corrected)
    df$lga_id <- as.numeric(recodeVar(as.character(df$unique_lga), src=nmis_lga_mapping$unique_slug, tgt=nmis_lga_mapping$id))
    df
}
###############################
##importing/slicin/dicin data##
###############################

# READ IN "p-index" data, and clean it up
p_index1 <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_04_30_10_35_43_p.csv",
                     stringsAsFactors=F)
p_index2 <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_29_06_14_15_p.csv",
                     stringsAsFactors=F)
p_index3 <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_05_03_10_59_04_p.csv",
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
schools3_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_05_03_10_59_04_e.csv",
                         stringsAsFactors=F)

hospitals1_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_04_30_10_35_43_h.csv",
                           stringsAsFactors=F)
hospitals2_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_29_06_14_15_h.csv",
                           stringsAsFactors=F)
hospitals3_raw <- read.csv("in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_05_03_10_59_04_h.csv",
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

## dropping non-public facilities
# drop_private_facilities_edu <- function(df) {
#   df$public <- df$Schools.school_managed %in% c("fed_gov", "loc_gov", "st_gov")
#   subsettedDf <- subset(df, public==T)
#   subsettedDf
# }
# schools1 <- drop_private_facilities_edu(schools1)
# schools2 <- drop_private_facilities_edu(schools2)
# schools3 <- drop_private_facilities_edu(schools3)
# drop_private_facilities_health <- function(df) {
#   df$public <- !(df$HealthFacilities.facility_owner_manager.charitable_ngo ==T |
#                    df$HealthFacilities.facility_owner_manager.private_forprofit == T |
#                    df$HealthFacilities.facility_owner_manager.religious_org == T)
#   df <- subset(df, public == T) 
#   df
# }
# hospitals1 <- drop_private_facilities_health(hospitals1)
# hospitals2 <- drop_private_facilities_health(hospitals2)
# hospitals3 <- drop_private_facilities_health(hospitals3)
# print_numbers("After dropping private schools")

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
schools <- arrange(schools, mylga_zone, mylga_state, mylga, Schools.level_of_education, Schools.school_managed)

hospitals <- subset(hospitals, select=c(lga_id, mylga_zone, mylga_state, mylga, ta_name,
                                        HealthFacilities.health_facility_name, 
                                        HealthFacilities.health_facility_type,
                                        HealthFacilities.ward_name, HealthFacilities.com_name_h,
                                        managed_by))
hospitals <- arrange(hospitals, mylga_zone, mylga_state, mylga, HealthFacilities.health_facility_type)

#CLEANING:test/non-character/blank facility names
print_numbers2("Before name cleaning")
#education
c_e1 <- schools[which(!str_detect(schools$Schools.school_name, '[a-zA-Z]')),]
schools <- schools[!str_detect(schools$Schools.school_name, '[*]'),]
schools <- subset(schools, !Schools.school_name %in% c("", "1"))
# TODO: SIMPLIFY
schools$Schools.school_name <- str_replace_all(schools$Schools.school_name, "\xd5", "")
schools$Schools.school_name <- str_replace_all(schools$Schools.school_name, "\xd0", "")
schools$Schools.school_name <- str_replace_all(schools$Schools.school_name, "\xd2", "")
schools$Schools.school_name <- str_replace_all(schools$Schools.school_name, "\xd3", "")
schools$Schools.school_name <- str_replace_all(schools$Schools.school_name, "\xd4", "")
schools$Schools.school_name <- str_replace_all(schools$Schools.school_name, "\xe7", "")
#health
c_h1 <- hospitals[which(!str_detect(hospitals$HealthFacilities.health_facility_name, '[a-zA-Z]')), ]
hospitals <- subset(hospitals, !HealthFacilities.health_facility_name %in% c("", "00", "33"))
hospitals <- hospitals[!str_detect(hospitals$HealthFacilities.health_facility_name, '[*^]'),]
hospitals <- hospitals[!str_detect(hospitals$HealthFacilities.health_facility_name, "\'\'"),]
hospitals <- hospitals[!str_detect(hospitals$HealthFacilities.health_facility_name, "0000+"),]
hospitals <- hospitals[!str_detect(hospitals$HealthFacilities.health_facility_name, "ˆ +$"),]
hospitals$HealthFacilities.health_facility_name <- str_replace_all(hospitals$HealthFacilities.health_facility_name, "\xd5", "")
hospitals$HealthFacilities.health_facility_name <- str_replace_all(hospitals$HealthFacilities.health_facility_name, "\xd4", "")
hospitals$HealthFacilities.health_facility_name <- str_replace_all(hospitals$HealthFacilities.health_facility_name, "\xd0", "")
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


#ID:random character id
#education
id_generate <- function(df) { 
#     l <- letters
    set.seed(1)
    x <- sample(1:99999, dim(df)[1], replace=F)
    as.hexmode(x)
    df$random_id <- as.character(as.hexmode(x))
#     x1 <- l[sample(1:26, dim(df)[1], replace=T)]
#     x2 <- l[sample(1:26, dim(df)[1], replace=T)]
#     x3 <- l[sample(1:26, dim(df)[1], replace=T)]
#     x4 <- l[sample(1:26, dim(df)[1], replace=T)]
#     x5 <- l[sample(1:26, dim(df)[1], replace=T)]
   
#     df$random_id <- paste0(x1, x2, x3, x4, x5)
    return(df) }    
schools <- id_generate(schools)
#This is for tesing if the random_id is unique within each lga
#If output is "integer(0)" then we're good
t <- ddply(schools, .(lga_id), summarise, 
           unique_short_id = length(unique(random_id)), 
           n_fac = length(random_id))
which(t$unique_short_id != t$n_fac)
#health
hospitals <- id_generate(hospitals)
#This is for tesing if the random_id is unique within each lga
#If output is "integer(0)" then we're good
t <- ddply(hospitals, .(lga_id), summarise, 
           unique_short_id = length(unique(random_id)), 
           n_fac = length(random_id))
which(t$unique_short_id != t$n_fac)

##ID:assigning new data from refresh character ID
#reading in facilities that already have ID 
old_schools <- read.csv("in_process_data/facility_lists/FACILITY_LIST_schools.csv")
old_hospitals <- read.csv("in_process_data/facility_lists/FACILITY_LIST_hospitals.csv")
#merge  
  #... <- merge(old_schools, schools, ....)
  #... <- merge(old_hospitals, hospitals, ....)

# ##ID:sequential IDs
# #education
# #order by lga_id  and submition time
# schools <- arrange(schools, lga_id, end)
# #Create serial from 1 to number of records in that lga
# schools <- ddply(schools, .(lga_id), transform, 
#                  seq_id = 1:length(lga_id))
# #adding leading "0"s 
# idx <- which(sapply(schools$seq_id, nchar) == 1)
# schools$seq_id[idx] <- paste0("00", schools$seq_id[idx])
# idx <- which(sapply(schools$seq_id, nchar) == 2)
# schools$seq_id[idx] <- paste0("0", schools$seq_id[idx])
# #adding the leading character
# schools$seq_id <- paste0("FE", schools$seq_id)
# #health
# #order by lga_id  and submition time
# hospitals <- arrange(hospitals, lga_id, end)
# #Create serial from 1 to number of records in that lga
# hospitals <- ddply(hospitals, .(lga_id), transform, 
#                    seq_id = 1:length(lga_id))
# #adding leading "0"s 
# idx <- which(sapply(hospitals$seq_id, nchar) == 1)
# hospitals$seq_id[idx] <- paste0("00", hospitals$seq_id[idx])
# idx <- which(sapply(hospitals$seq_id, nchar) == 2)
# hospitals$seq_id[idx] <- paste0("0", hospitals$seq_id[idx])
# #adding the leading character
# hospitals$seq_id <- paste0("FH", hospitals$seq_id)

## WRITING OUT ## 
  #zaiming cleaning
    #education
    index <- which(is.na(schools$Schools.ward_num) & !is.na(schools$Schools.ward_name))
    schools$Schools.ward_num[index] <- schools$Schools.ward_name[index]
    rm(index)
    schools$Schools.ward_name <- NULL
    
    ward_comm_fix_edu_f <- function(df, ward_col, comunity_col)
    {
        #    ward_col <- "Schools.ward_num"
        #    comunity_col <- "Schools.com_name"
        #    df <- schools
        # Take out "ward"
        df[,ward_col] <- str_replace(df[,ward_col], ignore.case("ward"), "")
        # trim off leading & tailing blanks
        df[, ward_col] <- str_trim(df[, ward_col])
        df[, comunity_col] <- str_trim(df[, comunity_col])
        # trim off "0" in front of 01,02 & etc
        df[which(str_detect(df[, ward_col], '^[0-9]+$')), ward_col] <- str_replace(df[which(str_detect(df[,ward_col], '^[0-9]+$')), ward_col], "^0+", "")
        # replace consecutive blanks with only one blank
        df[,ward_col] <- gsub('  +', " ", df[,ward_col], ignore.case=T)
        df[,comunity_col] <- gsub('  +', " ", df[,comunity_col], ignore.case=T)
        return(df)
    } 
    facility_name_fix_edu_f <- function(df, school_name_col)
    {
        #     df <- schools
        #     school_name_col <- "Schools.school_name" 
        df[, school_name_col] <- gsub('comm(\\.| )|comm$',  "Community ", df[, school_name_col], ignore.case=T)
        df[, school_name_col] <- gsub('(sch(\\.| )|sch$)',  "School ", df[, school_name_col], ignore.case=T)
        df[, school_name_col] <- gsub('(sec(\\.| )|sec$)',  "Secondary ", df[, school_name_col], ignore.case=T)
        df[, school_name_col] <- gsub('snr(\\.| )|snr$|snr)',  "Senior ", df[, school_name_col], ignore.case=T)
        df[, school_name_col] <- gsub('(nur/(pri|pry)(.|$))|N/P(\\.| |$)',  "Nursery/Primary ", df[, school_name_col], ignore.case=T)
        df[, school_name_col] <- gsub('(pri|pry|prim)(\\.| )',  "Primary ", df[, school_name_col], ignore.case=T)
        df[, school_name_col] <- gsub('jnr',  "Junior ", df[, school_name_col], ignore.case=T)
        return(df)
    }
    #health 
    ward_comm_fix_health_f <- function(df, ward_col, comunity_col)
    {
        #ward_col <- "HealthFacilities.ward_name"
        #comunity_col <- "HealthFacilities.com_name_h"
        #df <- hospitals
        # Take out "ward"
        df[,ward_col] <- str_replace(df[,ward_col], ignore.case("ward"), "")
        # find row.name for those com == NA & ward contain (",", "/")
        idx <- which(is.na(df[, comunity_col]) & str_detect(df[, ward_col], '[/,]') )
        # replace comm with string after "/"
        df[idx, comunity_col] <- str_trim(str_replace(df[idx,ward_col], '[a-zA-Z0-9 \')]+[/,]', ""))
        # replace war with string before "/"
        df[idx, ward_col] <- str_trim(str_replace(str_extract(df[idx, ward_col], '[a-zA-Z0-9 \')]+[/,]'), "[/,]$", ""))
        # trim off leading & tailing blanks
        df[, ward_col] <- str_trim(df[, ward_col])
        # trim off "0" in front of 01,02 & etc
        df[which(str_detect(df[, ward_col], '^[0-9]+$')), ward_col] <- str_replace(df[which(str_detect(df[,ward_col], '^[0-9]+$')), ward_col], "^0+", "")
        # replace consecutive blanks with only one blank
        df[,ward_col] <- gsub('  +', " ", df[,ward_col], ignore.case=T)
        df[,comunity_col] <- gsub('  +', " ", df[,comunity_col], ignore.case=T)
        return(df)
    } 
    facility_name_fix_health_f <- function(df, facility_name_col)
    {
        #    df <- hospitals
        #    facility_name_col <- "HealthFacilities.health_facility_name"  
        df[,facility_name_col] <- sub('pry.+health.|PRI.+HEALTH',  "Primary Health ", df[, facility_name_col], ignore.case=T)
        df[, facility_name_col] <- sub('center', "Centre", df[, facility_name_col], ignore.case=T)
        df[, facility_name_col] <- sub('B(\\.| )H(\\.| )C\\.|BHC', "Basic Health Centre", df[, facility_name_col], ignore.case=T)
        df[, facility_name_col] <- sub('P.H.C.+(clinic|centre)|PHC.+(clinic|centre)', "PHCC", df[, facility_name_col], ignore.case=T)
        df[, facility_name_col] <- sub('p h c c', "PHCC ", df[, facility_name_col], ignore.case=T)
        df[, facility_name_col] <- sub('P(\\.| )H(\\.| )C\\.|pri.+Health.centre', "PHC", df[, facility_name_col], ignore.case=T)
        df[, facility_name_col] <- sub('(H/(P|post)|health post|HP)', "Health Post", df[, facility_name_col], ignore.case=T)
        df[, facility_name_col] <- sub('hosp\\.', "Hospital", df[, facility_name_col], ignore.case=T)
        df[, facility_name_col] <- sub('/mat(\\.| |)', "/Maternity ", df[, facility_name_col], ignore.case=T)
        df[, facility_name_col] <- sub('hosp/', "Hospital/ ", df[, facility_name_col], ignore.case=T)
        df[, facility_name_col] <- sub('gen(\\.| )', "General ", df[, facility_name_col], ignore.case=T)
        df[, facility_name_col] <- sub('comp(\\.| )', "Comprehensive ", df[, facility_name_col], ignore.case=T)
        df[, facility_name_col] <- sub('h/c |h/c$', "HC ", df[, facility_name_col], ignore.case=T)
        return(df)
    }

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


+#Name spelling standardization
names(hospitals)

hospitals <- ward_comm_fix_health_f(hospitals, 'ward', 'community')
hospitals <- facility_name_fix_health_f(df=hospitals, facility_name_col= 'facility_name')

names(schools)

schools <- ward_comm_fix_edu_f(df=schools, ward_col='ward', comunity_col='community')
schools <- facility_name_fix_edu_f(df=schools, school_name_col= 'facility_name')



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

######################
#######baseline####### 
######################
#education
edu <- read.csv("in_process_data/nmis/Education_661_ALL_FACILITY_INDICATORS.csv",
                stringsAsFactors=F)
# OUTPUT SHOULD BE 0
anyDuplicated(edu$uuid)

e_113 <- read.csv("in_process_data/facility_lists/raw data/113/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov16_without_emptyobs.csv",
                  stringsAsFactors=F)
e_113 <- subset(e_113, subset=(gps != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
e_113$uuid <- sapply(paste(e_113$gps, e_113$photo), FUN=digest)
# OUTPUT SHOULD BE 0
anyDuplicated(e_113$uuid)


e_pilot <- read.csv("in_process_data/facility_lists/raw data/113/Pilot_Education_cleaned_2011Oct4.csv",
                    stringsAsFactors=F)
e_pilot <- subset(e_pilot, subset=(gps != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
e_pilot$uuid <- sapply(paste(e_pilot$gps, e_pilot$photo), FUN=digest)
# OUTPUT SHOULD BE 0
anyDuplicated(e_pilot$uuid)

e_113 <- rbind.fill(e_113, e_pilot)
edu <- rbind.fill(e_113, edu)  
edu <- subset(edu, select=c(X_lga_id, zone, state, lga, ward, community, school_name, level_of_education, uuid))
# OUTPUT SHOULD BE 0
anyDuplicated(edu$uuid)


#health
health <- read.csv("in_process_data/nmis/Health_661_ALL_FACILITY_INDICATORS.csv",
                   stringsAsFactors=F)
# OUTPUT SHOULD BE 0
anyDuplicated(health$uuid)

h_113 <- read.csv("in_process_data/facility_lists/raw data/113/Health_PhaseII_RoundI&II&III_Clean_2011.11.16.csv",
                  stringsAsFactors=F)
h_113 <- subset(h_113, subset=(geocodeoffacility != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
h_113$uuid <- sapply(paste(h_113$geocodeoffacility, h_113$photo), FUN=digest)
# THE Following line is dangerous to replicate;  WAS WRITTEN AFTER CAREFUL INSPECTION OF DUPLICATES
h_113 <- subset(h_113, !duplicated(h_113$uuid))
# OUTPUT SHOULD BE 0
anyDuplicated(h_113$uuid)

h_pilot <- read.csv("in_process_data/facility_lists/raw data/113/Pilot_Data_Hlth_Clean_2011.09.02.csv", stringsAsFactors=F)
h_pilot <- subset(h_pilot, subset=(geocodeoffacility != "n/a")) # REMOVING ALL FACILITIES WITHOUT GEO CODE
h_pilot$uuid <- sapply(paste(h_pilot$geocodeoffacility, h_pilot$photo), FUN=digest)
# OUTPUT SHOULD BE 0
anyDuplicated(h_pilot$uuid)

h_113 <- rbind.fill(h_113, h_pilot)
health <- rbind.fill(h_113, health)  
health <- subset(health, select=c(X_lga_id, zone, state, lga, ward, community, facility_name, facility_type, uuid))

# OUTPUT SHOULD BE 0
anyDuplicated(h_pilot$uuid)

##zaiming cleaning
#education
ward_comm_fix_edu_b <- function(df, ward_col, comunity_col)
{
    #ward_col <- "ward"
    #comunity_col <- "community"
    #df <- edu   
    # Take out "ward"
    df[,ward_col] <- str_replace(df[,ward_col], ignore.case("ward"), "")
    # trim off leading & tailing blanks
    df[, ward_col] <- str_trim(df[, ward_col])
    df[, comunity_col] <- str_trim(df[, comunity_col])
    # trim off "0" in front of 01,02 & etc
    df[which(str_detect(df[, ward_col], '^[0-9]+$')), ward_col] <- str_replace(df[which(str_detect(df[,ward_col], '^[0-9]+$')), ward_col], "^0+", "")
    # replace consecutive blanks with only one blank
    df[,ward_col] <- gsub('  +', " ", df[,ward_col], ignore.case=T)
    df[,comunity_col] <- gsub('  +', " ", df[,comunity_col], ignore.case=T)
    return(df)
} 
facility_name_fix_edu_b <- function(df, school_name_col)
{
#     df <- edu
#     school_name_col <- "school_name"     
    df[, school_name_col] <- gsub('comm(\\.| )|comm$',  "Community ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('(sch(\\.| )|sch$)',  "School ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('(sec(\\.| )|sec$)',  "Secondary ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('snr(\\.| )|snr$|snr)',  "Senior ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('(nur/(pri|pry)(.|$))|N/P(\\.| |$)',  "Nursery/Primary ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('(pri|pry|prim)(\\.| )',  "Primary ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('jnr',  "Junior ", df[, school_name_col], ignore.case=T)
    return(df)
}
#health
ward_comm_fix_health_b <- function(df, ward_col, comunity_col)
{
#     ward_col <- "ward"
#     comunity_col <- "community"
#     df <- health
    # Take out "ward"
    df[,ward_col] <- str_replace(df[,ward_col], ignore.case("ward"), "")
    # trim off leading & tailing blanks
    df[, ward_col] <- str_trim(df[, ward_col])
    df[, comunity_col] <- str_trim(df[, comunity_col])
    # trim off "0" in front of 01,02 & etc
    df[which(str_detect(df[, ward_col], '^[0-9]+$')), ward_col] <- str_replace(df[which(str_detect(df[,ward_col], '^[0-9]+$')), ward_col], "^0+", "")
    # replace consecutive blanks with only one blank
    df[,ward_col] <- gsub('  +', " ", df[,ward_col], ignore.case=T)
    df[,comunity_col] <- gsub('  +', " ", df[,comunity_col], ignore.case=T)
    return(df)
} 
facility_name_fix_health_b <- function(df, facility_name_col)
{
#     df <- health
#     facility_name_col <- "facility_name"     
    df[,facility_name_col] <- sub('pry.+health.|PRI.+HEALTH',  "Primary Health ", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('center', "Centre", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('B(\\.| )H(\\.| )C\\.|BHC', "Basic Health Centre", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('P.H.C.+(clinic|centre)|PHC.+(clinic|centre)', "PHCC", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('p h c c', "PHCC ", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('P(\\.| )H(\\.| )C\\.|pri.+Health.centre', "PHC", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('(H/(P|post)|health post|HP)', "Health Post", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('hosp\\.', "Hospital", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('/mat(\\.| |)', "/Maternity ", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('hosp/', "Hospital/ ", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('gen(\\.| )', "General ", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('comp(\\.| )', "Comprehensive ", df[, facility_name_col], ignore.case=T)
    df[, facility_name_col] <- sub('h/c |h/c$', "HC ", df[, facility_name_col], ignore.case=T)
    return(df)
}

names(edu)
edu <- facility_name_fix_edu_b(df=edu, school_name_col="school_name")
edu <- ward_comm_fix_edu_b(df=edu, ward_col="ward", comunity_col="community")

names(health)
health <- facility_name_fix_health_b(df=health, facility_name_col="facility_name")
health <- ward_comm_fix_health_b(df=health, ward_col="ward", comunity_col="community")

edu <- id_generate(edu)
health <- id_generate(health)

#writing
edu <- rename(edu, c("X_lga_id" = "lga_id"))
edu <- rename(edu, c("school_name" = "facility_name"))
edu <- rename(edu, c("uuid" = "long_id"))
edu <- rename(edu, c("level_of_education" = "facility_type"))
health <- rename(health, c("X_lga_id" = "lga_id"))
health <- rename(health, c("uuid" = "long_id"))
write.csv(edu, "in_process_data/facility_lists/BASELINE_schools.csv", row.names=F, quote=F)
write.csv(health, "in_process_data/facility_lists/BASELINE_hospitals.csv", row.names=F, quote=F)

## AGGREGATION BY LGA: baseline
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

################
####more outputs
################
# #TA names
# lists <- formhubDownload("NMIS_FacilityLists_for_CoverageAnalysis", "technical_assistant", "sc4l3up!")@data
# lists2 <- formhubDownload("NMIS_FacilityLists_for_CoverageAnalysis_2", "technical_assistant", "sc4l3up!")@data
# l <- rbind.fill(lists, lists2)
# l <- subset(l, select=c(mylga_zone, mylga_state, mylga, ta_name))
# 
# ta_names <- l[!duplicated(l$ta_name),]
# ta_names <- ta_names[!is.na(ta_names$mylga),]
# clean(ta_names, 'ta_name', 
#       which(ta_names$ta_name %in% c('Akima Edet', 'Edet Akima', 
#                                     'Nanle Dalu', 'TERESA O. PATRICK', 'TEGA DAVID  AYUYA', 'jonah michael',
#                                     'KABIRU SAMAILA', 'okeatyo oluwafemi', 'oketayo oluwfemi',
#                                     'Engr. Mai modu', 'Engr  Idris Abdullahi', 'DANLAMI BALA GWAMAJA',
#                                     'DANLAMI BALA GWAMMAJA', 'Engr.Abba A. Yusuf', 'FOLORUNSO,Mark E.O.', 
#                                     'Ahmed Sarkin Zamfara', 'AHMED IBRAHIM MUHD', 'Ahmed S. Zamfara Zurmi',
#                                     'YAHAYA  JIBRIL', 'FOLORUNSO MARK E.O.', 'Engr Abba A. Yusuf', 
#                                     'OLOLADE DANMOLA ADELEKAN', 'YAHAYAJIBRIL', 'TEGA DAVID AYUYA', 
#                                     'FOLORUNSO, MARK E.O.', 'ADESINA ADEJARE RASHEED', 'Adeleke  Kamoru Adedoja',
#                                     'AHMAD S/ZAMFARA', 'OLOLADE DANMOLA-ADELEKAN', 'Adeleke kamoru Adedoja', 
#                                     'FOLORUNSO Mark.E.O.', 'FOLRUNSO, Mark E.O.', 'YUSUF H. OLUWATOSIN',
#                                     'Onyeiwu Leonard C.', 'OGIDIGA LAWSON OKPOEGERI', 'OGIDIGA LAWSON OKPOEGEBRI',
#                                     'KABIRU SAMAIAL', 'JONAH MICHAEL', 'Engr Idris  Abdullahi', 'DANALAMI BALA GWAMMAJA'
#       )), NA)
# ta_names <- ta_names[!is.na(ta_names$ta_name),]
# ta_names <- ta_names[!duplicated(ta_names$mylga),]
# ta_names <- ta_names[,c(1,4)]
# ta_names <- arrange(ta_names, mylga_zone)
# ta_names$ta_name <- toupper(as.character(ta_names$ta_name))
# write.csv(ta_names, "presentations/mop_up/TA_names.csv", row.names=F)
# 
# paste("Number of TAs that have submitted facility lists:", length(ta_names$mylga) )
# paste("Number of LGAs that have submitted facility lists:", nrow(schools_agg))
# paste("Number of Education facility lists with at least the number of facilities as baseline:", 
#       nrow(subset(schools_agg, facility_counts > baseLine_total)))
# paste("Number of Health facility lists with at least the number of facilities as baseline:", 
#       nrow(subset(hospitals_agg, facility_counts > base_line_faciliti_count)))
