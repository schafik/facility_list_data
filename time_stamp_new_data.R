#run folowing line BEFORE SUBSETTING
schools$end <- ymd_hms(str_extract(schools$end, '^[^+Z]*(T| )[^+Z-]*'))
hospitals$end <- ymd_hms(str_extract(hospitals$end, '^[^+Z]*(T| )[^+Z-]*'))


schools <- subset(schools, end > ymd_hms('2013-05-28-03:39:00 p.m.'))
hospitals <- subset(hospitals, end > ymd_hms('2013-05-28-03:39:00 p.m.'))

#run folowing line BEFORE OUTPUT
schools_old <- read.csv('./in_process_data/facility_lists/Facility list snapshot/FACILITY_LIST_schools_full_may_28.csv')
hospitals_old <- read.csv('./in_process_data/facility_lists/Facility list snapshot/FACILITY_LIST_hospitals_full_may_28.csv')

schools <- rbind(schools_old,schools)
hospitals <- rbind(hospitals_old,hospital)s

