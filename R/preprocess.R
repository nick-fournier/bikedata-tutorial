

#This file preprocesses the SFMTA bike counts, Streetlight Inc counts, and SF Landuse data
#I wrote this in a hurry, appologies for lack of clarity.

#Load the necessary packages
library(data.table)
library(lubridate)


#### importing data to R environment ####
# Unzip the street light data
if(length(list.files("./raw data/streetlight data/monthly")) == 0)
  unzip("./streetlight data/taz_za_bike_monthly.zip", exdir="./streetlight data")

# Get file names for the unzipped files
stl_paths <- list.files("./raw data/streetlight data/monthly", full.names = T)
stl_paths <- stl_paths[grepl("salesreq", stl_paths)]

# Read the files into a list of data frames. Then 'bind' the data frames into one big one
counts_strtlght <- rbindlist(lapply(stl_paths, fread))


# Import sensor count data
counts_sfmta <- fread("./raw data/sfbike data/HOURLYTABLE.csv")

#Import landuse data
landuse <- fread("./processed/landuse_taz.csv")

#Bike infra
infra <- fread("./processed/bikelanes_taz.csv", colClasses = list(character='taz_id'))

      
# Import the relationship file and hourly counts, specify column dtypes
relates <- fread("./processed/joined_taz_sensors.csv",
                 colClasses = list(character=c("location","taz_id","identifier"),
                                   integer=c("counterid","counter"),
                                   numeric=c("lat","lon")))


#### Cleanup the data ####
# Remove empty zones from relationship
relates <- relates[relates$taz_id!="", .(counterid, taz_id, location, lat, lon)]


# Find which of our TAZ IDs are in the streetlight data
bool_filter <- counts_strtlght$`Zone ID` %in% relates[!is.na(taz_id), taz_id]
counts_strtlght <- counts_strtlght[bool_filter]


# Fix awkward column names that might throw errors later
colnames(counts_sfmta) <- c('counterid', 'location', 'hour', 'month', 'day_type', 'year', 'count')
counts_sfmta$count <- as.numeric(counts_sfmta$count)
counts_sfmta$day_type <- tolower(counts_sfmta$day_type)

#too many to do manually, will do some quick filters
colnames(counts_strtlght) <- gsub(" ", "_", colnames(counts_strtlght))
colnames(counts_strtlght) <- gsub("\\(|\\)|\\$", "", colnames(counts_strtlght))
colnames(counts_strtlght) <- gsub("between_", "", colnames(counts_strtlght))
colnames(counts_strtlght) <- tolower(colnames(counts_strtlght))
setnames(counts_strtlght, c('zone_id','zone_traffic_trip_counts'), c('taz_id','strtlght_count'))

#Remove columns
counts_strtlght <- counts_strtlght[, !grepl('duration|adjusted|length', colnames(counts_strtlght)), with=F]

# Extract the month
counts_strtlght$month <- as.Date(paste0(counts_strtlght$data_period, "01"), "%Y%m%d")
#days per month
counts_strtlght$days_per_month <- lubridate::days_in_month(counts_strtlght$month)
#month name
counts_strtlght$month <- strftime(counts_strtlght$month, format = "%B")


# Fix some time increment labels
counts_strtlght[grepl("0: ", day_type)]$day_type <- 'all'
counts_strtlght[grepl("1: ", day_type)]$day_type <- 'weekday'
counts_strtlght[grepl("2: ", day_type)]$day_type <- 'weekend'

counts_strtlght[grepl("0: ", day_part)]$day_part <- 'all'
counts_strtlght[grepl("1: ", day_part)]$day_part <- 'early_am'
counts_strtlght[grepl("2: ", day_part)]$day_part <- 'peak_am'
counts_strtlght[grepl("3: ", day_part)]$day_part <- 'mid'
counts_strtlght[grepl("4: ", day_part)]$day_part <- 'peak_pm'
counts_strtlght[grepl("5: ", day_part)]$day_part <- 'late_pm'


##### Aggregate the streetlight count data ####
counts_strtlght[ , strtlght_count := strtlght_count / days_per_month]
#Remove the all category & days per month
counts_strtlght <- counts_strtlght[day_part != 'all' & day_type != 'all', !c("days_per_month")]


# setup out aggregation columns
del_cols = c("type_of_travel", "zone_type", "data_period", "intersection_type", "zone_district_id" , "zone_name")
base_cols = c("taz_id", "day_type","day_part","month")
sum_cols = "strtlght_count"
avg_cols = colnames(counts_strtlght)[!(colnames(counts_strtlght) %in% c(base_cols, sum_cols, del_cols))]


# Aggregate trip start and ends
#The data.table way is a bit harder to understand, but more flexible and faster.
#data.table[{filter rows here}, {lapply(list, func) is a compact loop}, by={group by cols}, .SDcols = {col names for lapply}]
agg_sum <- counts_strtlght[ , lapply(.SD, sum), by = base_cols,  .SDcols = sum_cols]
agg_avg <- counts_strtlght[ , lapply(.SD, weighted.mean, w=strtlght_count), by = base_cols, .SDcols = avg_cols]

# joins by the base cols
counts_strtlght_agg <- merge(agg_sum, agg_avg, by = base_cols)



#### Aggregate sfmta count data ####

# Aggregate counts into the peak hours
# 0: all day, 1: 12am-6am, 2: 6am-10am, 3: 10am-3pm, 4: 3pm-7pm, 5: 7pm-12am

#Turn 0 into 24 so it's included in 7pm to 12am, instead of 12am to 6am.
counts_sfmta_agg <- counts_sfmta
counts_sfmta_agg[hour == 0]$hour <- 24
counts_sfmta_agg$day_part <- .bincode(counts_sfmta_agg$hour, breaks = c(0, 6, 10, 15, 19, 24), include.lowest = T)

#Convert to to character. used factor as a shortcut
counts_sfmta_agg$day_part <- as.character(
  factor(counts_sfmta_agg$day_part, 
         labels = c('early_am', 'peak_am', 'mid', 'peak_pm', 'late_pm'))
)

# Aggregate counts into average hourly
counts_sfmta_agg <- counts_sfmta_agg[ , .('sfmta_count'=mean(count, na.rm=T)), by=.(counterid, day_type, month, day_part)]


#Check if any missing matches
any(is.na(merge(unique(counts_sfmta_agg[,.(day_part, day_type, month)]),
                unique(counts_strtlght_agg[,.(day_part, day_type, month)]), all=T)))


#### Aggregate land use areas ####
landuse_agg <- landuse[LANDUSE != "MISSING DATA" | STREET != "", ]
landuse_agg$SHAPE_Area <- landuse_agg[ , SHAPE_Area / 43560]
landuse_agg <- landuse_agg[ , lapply(.SD, sum), by = .(taz_id),
                            .SDcols=c('RESUNITS','CIE','MED','MIPS','RETAIL','PDR','VISITOR','TOTAL_USES','SHAPE_Area')]
landuse_agg <- landuse_agg[ , lapply(.SD, function(x) (x/(SHAPE_Area))), by=.(taz_id, SHAPE_Area, TOTAL_USES)]
landuse_agg <- landuse_agg[ , !"TOTAL_USES"]


#### Bike lane aggregates
infra_agg <- infra[ , sum(TAZ_FAC_LE), by = c('taz_id','facility_t')]
infra_agg <- dcast(infra_agg, taz_id~facility_t, value.var = 'V1')
infra_agg <- infra_agg[ , lapply(.SD, function(x) ifelse(is.na(x),0,x))]
colnames(infra_agg) <- gsub(" ", "_", colnames(infra_agg))


#### No month ####
counts_sfmta_agg_tot <- counts_sfmta_agg[ , .('sfmta_count' = sum(sfmta_count)), by = .(counterid, day_type, day_part)]


# setup out aggregation columns
del_cols = c("month")
base_cols = c("taz_id", "day_type","day_part")
sum_cols = "strtlght_count"
avg_cols = colnames(counts_strtlght_agg)[!(colnames(counts_strtlght_agg) %in% c(base_cols, sum_cols, del_cols))]

agg_sum <- counts_strtlght_agg[ , .('strtlght_count' = sum(strtlght_count)), by = base_cols]
agg_avg <- counts_strtlght_agg[ , lapply(.SD, weighted.mean, w=strtlght_count), by = base_cols, .SDcols = avg_cols]

# joins by the base cols
counts_strtlght_agg_tot <- merge(agg_sum, agg_avg, by = base_cols)




#Save
fwrite(counts_strtlght_agg_tot, './notebook/data/streetlight_counts.csv')
fwrite(counts_sfmta_agg_tot, './notebook/data/sfmta_counts.csv')
fwrite(counts_strtlght_agg, './notebook/data/streetlight_counts_monthly.csv')
fwrite(counts_sfmta_agg, './notebook/data/sfmta_counts_monthly.csv')
fwrite(relates, './notebook/data/relation.csv')
fwrite(landuse_agg, './notebook/data/landuse.csv')
fwrite(infra_agg, './notebook/data/bikeinfra.csv')

rm(list=ls())






