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
# p_index1 <- read.xls("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_04_03_09_37_51.xls", sheet=1)
# p_index2 <- read.xls("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_03_13_48_46.xls", sheet=1)
# p_index3 <- read.xls("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_04_10_10_22_06.xls", sheet=1)
p_index1 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_04_30_10_35_43_p.csv",
                     stringsAsFactors=F)
p_index2 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_29_06_14_15_p.csv",
                     stringsAsFactors=F)
p_index3 <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_05_02_05_54_53_p.csv",
                     stringsAsFactors=F)
p_index1 <- p_index1[!(p_index1$mylga==""),]
p_index3 <- p_index3[!(p_index3$mylga==""),]
p_index1 <- add_lga_id(p_index1)
p_index2 <- add_lga_id(p_index2)
p_index3 <- add_lga_id(p_index3)
p_index1 <- rename(p_index1, c("X_parent_index" = "X_pindex"))
p_index2 <- rename(p_index2, c("X_parent_index" = "X_pindex"))
p_index3 <- rename(p_index3, c("X_parent_index" = "X_pindex"))

# schools1_raw <- read.xls("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_04_03_09_37_51.xls", sheet=2)
# schools2_raw <- read.xls("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_03_13_48_46.xls", sheet=2)
# schools3_raw <- read.xls("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_04_10_10_22_06.xls", sheet=2)
schools1_raw <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_04_30_10_35_43_e.csv",
                         stringsAsFactors=F)
schools2_raw <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_29_06_14_15_e.csv",
                         stringsAsFactors=F)
schools3_raw <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_05_02_05_54_53_e.csv",
                         stringsAsFactors=F)

# hospitals1_raw <- read.xls("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_04_03_09_37_51.xls", sheet=3)   
# hospitals2_raw <- read.xls("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_03_13_48_46.xls", sheet=3)
# hospitals3_raw <- read.xls("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_04_10_10_22_06.xls", sheet=3)
hospitals1_raw <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2013_04_30_10_35_43_h.csv",
                           stringsAsFactors=F)
hospitals2_raw <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_2_2013_04_29_06_14_15_h.csv",
                           stringsAsFactors=F)
hospitals3_raw <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/facility_lists/raw data/NMIS_FacilityLists_for_CoverageAnalysis_3_2013_05_02_05_54_53_h.csv",
                           stringsAsFactors=F)
schools1 <- schools1_raw
schools2 <- schools2_raw
schools3 <- schools3_raw
hospitals1 <- hospitals1_raw
hospitals2 <- hospitals2_raw
hospitals3 <- hospitals3_raw

## reading in
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

# rename child's X_index to uuid
renameColumn <- function(df, oldname, newname) {
    index = which(names(df) == oldname)
    stopifnot(length(index) <= 1) # assumption: oldname only occurs once
    names(df)[index] <- newname
    df
}

## combine with the parent to pull in lga information, etc.
merge_with_parent <- function(parentDF, childDF) {
    childDF <- renameColumn(childDF, "X_index", "uuid")
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
print_numbers("After Joining")

#subsetting data
schools <- subset(schools, select=c(lga_id, mylga_zone, mylga_state, mylga, ta_name,
                                    Schools.school_name, Schools.level_of_education,
                                    Schools.school_managed, Schools.school_managed_other, Schools.ward_name, 
                                    Schools.ward_num, Schools.com_name, 
                                    today, X_submission_time.x, X_submission_time.y, start, end))
schools <- arrange(schools, mylga_zone, mylga_state, mylga, Schools.level_of_education, Schools.school_managed)

hospitals <- subset(hospitals, select=c(lga_id, mylga_zone, mylga_state, mylga, ta_name,
                                        HealthFacilities.health_facility_name, 
                                        HealthFacilities.health_facility_type,
                                        HealthFacilities.ward_name, HealthFacilities.com_name_h,
                                        HealthFacilities.facility_owner_manager.federalgovernment, 
                                        HealthFacilities.facility_owner_manager.stategovernment,
                                        HealthFacilities.facility_owner_manager.lga, HealthFacilities.facility_owner_manager.other,
                                        today, X_submission_time.x, 
                                        X_submission_time.y, start, end))
hospitals <- arrange(hospitals, mylga_zone, mylga_state, mylga, HealthFacilities.health_facility_type)

#CLEANING:test/non-character/blank facility names
print_numbers2("Before name cleaning")
#education
c_e1 <- schools[which(!str_detect(schools$Schools.school_name, '[a-zA-Z]')),]
schools <- schools[!str_detect(schools$Schools.school_name, '[*]'),]
schools <- subset(schools, !Schools.school_name %in% c("", "1"))
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
first_value <- function(df)
{
    return(head(df,1))
}     
#education
c_e2 <- subset(schools, duplicated(schools[,1:12]) | duplicated(schools[,1:12], fromLast=T))
c_e2 <- arrange(c_e2, mylga_zone, mylga_state, mylga, Schools.school_name)  
schools <- ddply(schools, .(lga_id, mylga_zone, mylga_state, mylga, ta_name, 
                            Schools.school_name, Schools.level_of_education, Schools.school_managed, 
                            Schools.school_managed_other, Schools.ward_name, Schools.ward_num, 
                            Schools.com_name), first_value)                                       
#health
c_h2 <- subset(hospitals, duplicated(hospitals[,1:13]) | duplicated(hospitals[,1:13], fromLast=T))
c_h2 <- arrange(c_h2, mylga_zone, mylga_state, mylga, HealthFacilities.health_facility_name)  
hospitals <- ddply(hospitals, .(lga_id, mylga_zone, mylga_state, mylga, ta_name, 
                                HealthFacilities.health_facility_name, HealthFacilities.health_facility_type, 
                                HealthFacilities.ward_name, HealthFacilities.com_name_h, 
                                HealthFacilities.facility_owner_manager.federalgovernment, 
                                HealthFacilities.facility_owner_manager.stategovernment, HealthFacilities.facility_owner_manager.lga, 
                                HealthFacilities.facility_owner_manager.other), first_value)
print_numbers2("After duplicate cleaning")

#ID:random character id
#education
id_generate <- function(df) { 
    l <- letters
    set.seed(1)
    x1 <- l[sample(1:26, dim(df)[1], replace=T)]
    x2 <- l[sample(1:26, dim(df)[1], replace=T)]
    x3 <- l[sample(1:26, dim(df)[1], replace=T)]
    x4 <- l[sample(1:26, dim(df)[1], replace=T)]
    x5 <- l[sample(1:26, dim(df)[1], replace=T)]
    df$random_id <- paste0(x1, x2, x3, x4, x5)
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

#ID:sequential IDs
#education
#order by lga_id  and submition time
schools <- arrange(schools, lga_id, end)
#Create serial from 1 to number of records in that lga
schools <- ddply(schools, .(lga_id), transform, 
                 seq_id = 1:length(lga_id))
#adding leading "0"s 
idx <- which(sapply(schools$seq_id, nchar) == 1)
schools$seq_id[idx] <- paste0("00", schools$seq_id[idx])
idx <- which(sapply(schools$seq_id, nchar) == 2)
schools$seq_id[idx] <- paste0("0", schools$seq_id[idx])
#adding the leading character
schools$seq_id <- paste0("FE", schools$seq_id)
#health
#order by lga_id  and submition time
hospitals <- arrange(hospitals, lga_id, end)
#Create serial from 1 to number of records in that lga
hospitals <- ddply(hospitals, .(lga_id), transform, 
                   seq_id = 1:length(lga_id))
#adding leading "0"s 
idx <- which(sapply(hospitals$seq_id, nchar) == 1)
hospitals$seq_id[idx] <- paste0("00", hospitals$seq_id[idx])
idx <- which(sapply(hospitals$seq_id, nchar) == 2)
hospitals$seq_id[idx] <- paste0("0", hospitals$seq_id[idx])
#adding the leading character
hospitals$seq_id <- paste0("FH", hospitals$seq_id)

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
schools <- rename(schools, c("Schools.school_managed" = "school_managed"))
schools <- rename(schools, c("Schools.school_managed_other" = "school_managed_other"))
schools <- rename(schools, c("Schools.ward_num" = "ward"))
schools <- rename(schools, c("Schools.com_name" = "community"))
schools <- rename(schools, c("X_submission_time.x" = "X_submission_time_x"))
schools <- rename(schools, c("X_submission_time.y" = "X_submission_time_y"))      
hospitals <- rename(hospitals, c("HealthFacilities.health_facility_name" = "facility_name"))
hospitals <- rename(hospitals, c("HealthFacilities.health_facility_type" = "facility_type"))
hospitals <- rename(hospitals, c("HealthFacilities.ward_name" = "ward"))
hospitals <- rename(hospitals, c("HealthFacilities.com_name_h" = "community"))
hospitals <- rename(hospitals, c("HealthFacilities.facility_owner_manager.federalgovernment" = "facility_owner_federalgov"))
hospitals <- rename(hospitals, c("HealthFacilities.facility_owner_manager.stategovernment" = "facility_owner_stategov"))
hospitals <- rename(hospitals, c("HealthFacilities.facility_owner_manager.lga" = "facility_owner_lgagov"))
hospitals <- rename(hospitals, c("HealthFacilities.facility_owner_manager.other" = "facility_owner_other"))
hospitals <- rename(hospitals, c("X_submission_time.x" = "X_submission_time_x"))
hospitals <- rename(hospitals, c("X_submission_time.y" = "X_submission_time_y"))

#writing
write.csv(schools, "in_process_data/facility_lists/FACILITY_LIST_schools.csv", row.names=F)
write.csv(hospitals, "in_process_data/facility_lists/FACILITY_LIST_hospitals.csv", row.names=F)

## AGGREGATION BY LGA: facility lists
schools_total <- ddply(schools, .(lga_id), summarize, facility_counts=length(lga_id))
hospitals_total <- ddply(hospitals, .(lga_id), summarize, facility_counts=length(lga_id))
write.csv(hospitals_total, "in_process_data/facility_lists/ossap updates/inprocess_data/f_agg_h.csv", row.names=F)
write.csv(schools_total, "in_process_data/facility_lists/ossap updates/inprocess_data/f_agg_e.csv", row.names=F)

######################
#######baseline####### 
######################
#education
edu <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Education_661_ALL_FACILITY_INDICATORS.csv",
                stringsAsFactors=F)
# edu$public <- !(edu$school_managed %in% c('priv_noprofit', 'priv_noprofit', 'faith_org'))
# edu <- subset(edu, public == T)
e_113 <- read.csv("in_process_data/facility_lists/raw data/113/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov16_without_emptyobs.csv",
                  stringsAsFactors=F)
# e_113$public <- !(e_113$school_managed_priv_profit == T | e_113$school_managed_priv_noprofit == T)
e_113$uuid <- e_113$X_id
e_pilot <- read.csv("in_process_data/facility_lists/raw data/113/Pilot_Education_cleaned_2011Oct4.csv",
                    stringsAsFactors=F)
# e_pilot$public <- T 
e_pilot$uuid <- sapply(e_pilot$gps, FUN=digest)
e_113 <- rbind.fill(e_113, e_pilot)
# e_113 <- subset(e_113, public==T)
edu <- rbind.fill(e_113, edu)  
edu <- subset(edu, select=c(X_lga_id, zone, state, lga, ward, community, school_name, level_of_education, uuid))

#health
health <- read.csv("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/nmis/Health_661_ALL_FACILITY_INDICATORS.csv",
                   stringsAsFactors=F)
# health$public <- health$facility_owner_manager.federalgovernment | health$facility_owner_manager.stategovernment |
#   health$facility_owner_manager.lga
# health <- subset(health, public == T)
h_113 <- read.csv("in_process_data/facility_lists/raw data/113/Health_PhaseII_RoundI&II&III_Clean_2011.11.16.csv",
                  stringsAsFactors=F)
# h_113$public <- h_113$facility_owner_manager %in% c('federalgovernment', 'federalgovrenment', 'stategovernment', 'lga')
# h_113 <- subset(h_113, public==T)
h_113$uuid <- h_113$X_id 
h_pilot <- read.csv("in_process_data/facility_lists/raw data/113/Pilot_Data_Hlth_Clean_2011.09.02.csv", stringsAsFactors=F)
# h_pilot$public <- h_pilot$facility_owner_manager %in% c('federalgovrenment', 'stategovernment', 'lga') 
# h_pilot <- subset(h_pilot, public==T)
h_pilot$uuid <- sapply(h_pilot$geoid, FUN=digest)
h_113 <- rbind.fill(h_113, h_pilot)
# h_113 <- subset(h_113, public==T)
health <- rbind.fill(h_113, health)  
health <- subset(health, select=c(X_lga_id, zone, state, lga, ward, community, facility_name, facility_type, uuid))
#zaiming cleaning
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

write.csv(edu, "in_process_data/facility_lists/BASELINE_schools.csv", row.names=F)
write.csv(health, "in_process_data/facility_lists/BASELINE_hospitals.csv", row.names=F)

## AGGREGATION BY LGA: baseline
h_total <- ddply(health, .(X_lga_id), nrow)
e_total <- ddply(edu, .(X_lga_id), nrow)
write.csv(h_total, "in_process_data/facility_lists/ossap updates/inprocess_data/b_agg_h.csv", row.names=F)
write.csv(e_total, "in_process_data/facility_lists/ossap updates/inprocess_data/b_agg_e.csv", row.names=F)

######################
#######merging####### 
######################
#education
baseline_total <- merge(lgas, e_total, by="X_lga_id", all.x=T)
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
baseline_total_h <- merge(lgas, h_total, by.x="X_lga_id", by.y="X_lga_id", all.x=T)
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
