
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


#ID:short id, consisting of 3 characters
# Tested: ddply won't messed up with the chronological sequence of records, and the rng will spits identical numbers if we keep fix seed 
#education
schools <- shortid_generate(schools, 'F')

#health
hospitals <- shortid_generate(hospitals, 'F')






                         
                         
                         
                         