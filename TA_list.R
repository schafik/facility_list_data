setwd('c:/Users/zmyao/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/')
require(stringr)

nc <- read.csv('./in_process_data/facility_lists/Data Matcher/back check/education_nc.csv')
ne <- read.csv('./in_process_data/facility_lists/Data Matcher/back check/education_ne.csv')
nw <- read.csv('./in_process_data/facility_lists/Data Matcher/back check/education_nw.csv')
se <- read.csv('./in_process_data/facility_lists/Data Matcher/back check/education_se.csv')
ss <- read.csv('./in_process_data/facility_lists/Data Matcher/back check/education_ss.csv')
sw <- read.csv('./in_process_data/facility_lists/Data Matcher/back check/education_sw.csv')

checked_list <- rbind(nc,ne,nw,se,ss,sw)
# names(checked_list)
checked_list <- subset(checked_list, select=c('lga_id.x', 'ta_name'))
checked_list <- rename(checked_list, c('lga_id.x' = 'lga_id'))
checked_list <- checked_list[!duplicated(checked_list$ta_name),]
checked_list <- checked_list$ta_name

checked_list <- gsub('[.]', '',checked_list)
#checked_list <- gsub(' +', ' ',checked_list)
checked_list <- tolower(checked_list)
checked_list <- trim(checked_list)


ta_list <- read.csv('./in_process_data/facility_lists/ta_list_checked.csv', stringsAsFactors=F)
ta_list$ta_name <- gsub('[.]', '',ta_list$ta_name)
#ta_list$ta_name <- gsub(' +', ' ',ta_list$ta_name)
ta_list$ta_name <- tolower(ta_list$ta_name)
ta_list$ta_name <- trim(ta_list$ta_name)

#add column that checks if the TA's is being checked
ta_list$checked <- ta_list$ta_name %in% checked_list

# If output == 0 then there's no spelling mistake in names
checked_list[which(!(checked_list %in% ta_list$ta_name))]
#checked_list[order(as.character(checked_list))]



write.csv(ta_list, './in_process_data/facility_lists/ta_list_checked.csv', row.names=F)


unique(ta_list$ta_name)
unique(ta_list[(ta_list$checked == F),]$ta_name)
