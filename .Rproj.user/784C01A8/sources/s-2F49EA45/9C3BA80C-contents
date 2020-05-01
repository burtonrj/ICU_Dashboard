##### Load Data
admissions <- read_excel('data.xlsx', sheet = 'admissions')
occ <- read_excel('data.xlsx', sheet = 'occupancy')

##### Variable check
# Admissions
expected <- c("Unit","NHS_Number","Hospital_Number","covid19","ICUAdmit","ICUDischarge",
              "Unit_Outcome","Hosp_Disch_Date","Hosp_Outcome","age","sex","height",             
              "weight","AP2","HIV/AIDS","Cancer","Chemotherapy","ChronicHeart",        
              "ChronicRenal","ChronicResp","LiverCirrhosis","Home_Ventilate",
              "Immunosup","Leukaemia","Leukaemia_Chron","Lymphoma","Portal_Hyper",
              "Radiotherapy","MechanicalVentilation","DaysVentilated","RenalRT")
assert("Column names for admissions does not match expected", 
       expected == colnames(admissions))

# Occupancy
expected <-  c("Date","covid","LOC_0100", "LOC_0200", "LOC_0300",
               "LOC_0400", "LOC_0500","LOC_0600","LOC_0700", "LOC_0800",
               "LOC_0900", "LOC_1000", "LOC_1100", "LOC_1200","LOC_1300",
               "LOC_1400", "LOC_1500", "LOC_1600", "LOC_1700", "LOC_1800",
               "LOC_1900","LOC_2000", "LOC_2100", "LOC_2200", "LOC_2300",
               "LOC_2400")
assert("Column names for occupancy does not match expected", 
       expected == colnames(occ))

##### Formatting
# Admissions - datetime
dt_cols <- c("ICUAdmit", "ICUDischarge")
d_cols <- c("Hosp_Disch_Date")
admissions <- mutate_at(admissions, dmy_hms, .vars=dt_cols)
admissions <- mutate_at(admissions, ymd, .vars=d_cols)
# Admissions - chr columns
c_cols <- c("Unit", "Hospital_Number")
admissions <- mutate_at(admissions, as.character, .vars=c_cols)
# Admissions - int columns
i_cols <- c("NHS_Number", "age", "height", 
            "weight", "AP2", "DaysVentilated",
            "RenalRT")
admissions <- mutate_at(admissions, as.numeric, .vars=i_cols)
# Admissions - factors
not_f_cols <- unlist(list(d_cols, dt_cols, c_cols, i_cols))
f_cols <- setdiff(colnames(admissions), not_f_cols)
admissions <- mutate_at(admissions, as.factor, .vars=f_cols)
# Occupancy
occ$Date <- ymd(occ$Date)
c <- colnames(occ)
c <- c[c != "Date"]
occ <- mutate_at(occ, as.integer, .vars=c)

##### Wrangle
## Occupancy
process_datetime <- function(date, time){
  hour <- as.character(as.numeric(substr(time, 5, 6)) - 1)
  if(nchar(hour) == 1){
    hour <- paste("0", hour, sep="")
  }
  time <- paste(hour, "59", sep=":")
  return(paste(date, time, sep=" "))
}

# Summarise occupancy into hourly aggregates
occ <- gather(occ, 'Time', 'CareLevel', 3:26) %>% 
  select(c('Date', 'Time', 'CareLevel', 'covid'))
occ$DateTime <- unlist(map2(occ$Date, occ$Time, process_datetime))
occ$DateTime <- ymd_hm(occ$DateTime)
occ <- occ %>% select(c('DateTime', 'CareLevel', 'covid'))
occ$covid <- replace_na(occ$covid, 0)
occ <- drop_na(occ)

care_n <- function(x, ...){
  total <- nrow(x)
  total.covid <- sum(x$covid)
  non.covid <- total -  total.covid
  l3 <- x %>% filter(CareLevel == 3) %>% nrow()
  l3.covid <- x %>% filter((CareLevel == 3) & (covid == 1)) %>% nrow()
  l3.non.covid = l3 - l3.covid
  return(data.frame(list(Total=total,
                         TotalCovid=total.covid,
                         TotalNonCovid=non.covid,
                         L3=l3,
                         L3Covid=l3.covid,
                         L3NonCovid=l3.non.covid)))
}
occ.aggregates <- occ %>% group_by(DateTime) %>% group_modify(care_n)
occ.aggregates.tidy <- occ.aggregates %>% gather("Variable", "Count", 2:7)
