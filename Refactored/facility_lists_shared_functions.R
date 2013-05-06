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

# Add lga_id from NMIS data
add_lga_id = function(df, lga_colname="mylga", state_colname="mylga_state") {
    df$unique_lga <- tolower(ifelse(df[,lga_colname] %in% c('ERROR', NA),
                                    NA,
                                    str_c(df[,state_colname], df[,lga_colname], sep="_")))
    df$unique_lga <- str_replace_all(df$unique_lga, " ", "_")
    df$unique_lga <- recodeVar(df$unique_lga, src=lga_corrections$orginal, tgt=lga_corrections$corrected)
    df$lga_id <- as.numeric(recodeVar(as.character(df$unique_lga), src=nmis_lga_mapping$unique_slug, tgt=nmis_lga_mapping$id))
    df
}


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

#Clean up messing unicode errors
cleanweirdchars <- function(df, col) {
    df[,col] <- str_replace_all(df[,col], "\xd0|\xd1|\xd2|\xd3|\xd4|\xd5|\xe7", "")
    df[,col] <- str_replace_all(df[,col], "\\\\", "/")
    df[,col] <- str_replace_all(df[,col], '"', "'")
    df[,col] <- str_replace_all(df[,col], ",", ";")
    df
}


#ID:short id, consisting of 3 characters
# Tested: ddply won't messed up with the chronological sequence of records, and the rng will spits identical numbers if we keep fix seed 
shortid_generate <- function(df, prefix) 
{ 
    l <- letters
    set.seed(1)
    x <- sample(0:26^3-1, dim(df)[1], replace=F)
    
    digits <- vector(mode="list", length=3)
    tmp <- x
    for (i in 3:1)
    {
        digits[[i]] <- (tmp %% 26) + 1
        tmp <- tmp %/% 26
    }
    df$short_id <- paste0(prefix,':', l[digits[[3]]],l[digits[[2]]],l[digits[[1]]])
    
    # test that these are unique by lga before returning
    numberofshortids <- length(unique(df$short_id))
    numberoffacilities <- length(df$short_id)
    stopifnot(numberofshortids == numberoffacilities)
    
    return(df) 
}

####
#### String cleaning function for Ward,community & facility_name

#Fix Ward&community for Education Facility_list, Education Baseline & health Baseline
ward_comm_fix_FBe_Bh <- function(df, ward_col, comunity_col)
{
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

#Fix Facility Name for Education Facility_list & Education Baseline
facility_name_fix_FBe <- function(df, school_name_col)
{
    df[, school_name_col] <- gsub('comm(\\.| )|comm$',  "Community ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('(sch(\\.| )|sch$)',  "School ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('(sec(\\.| )|sec$)',  "Secondary ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('snr(\\.| )|snr$|snr)',  "Senior ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('(nur/(pri|pry)(.|$))|N/P(\\.| |$)',  "Nursery/Primary ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('(pri|pry|prim)(\\.| )',  "Primary ", df[, school_name_col], ignore.case=T)
    df[, school_name_col] <- gsub('jnr',  "Junior ", df[, school_name_col], ignore.case=T)
    return(df)
}

#Fix Ward&community for Health Facility_list 
ward_comm_fix_Fh <- function(df, ward_col, comunity_col)
{
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

#Fix Facility Name for Health Facility_list & Health Baseline
facility_name_fix_FBh <- function(df, facility_name_col)
{
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
