library(tidycensus)
library(tidyverse)

# initialize searching and get the variables
# census_api_key("761716f058077be0e2feba549a3c1c8d5e009187", install = TRUE, overwrite = TRUE) # kelsey
# options(tigris_use_cache = TRUE)

# get variable list of 5-year acs data in 2017
metadata <- load_variables(2017, "acs5", cache = TRUE)
colnames(metadata)[1] <- "variable"
metadata <- metadata %>% 
  mutate(table = str_extract(variable, "^([A-Z]|[0-9])+"))

# get table b19001 for block groups -- table universe is households
incomes <- get_acs(geography = "block group", state = "48", county = "201", variables = metadata$variable[metadata$table == "B19001"], geometry = FALSE, year = 2017, survey = "acs5") %>% 
  as.data.frame(stringsAsFactors=F)

# read in hud ami data for harris county
ami <- "E:/Dropbox (Traffic Engineers)/kelsey/Section8-FY19.csv" %>% 
  read.csv() %>% 
  filter(State == 48 & County == 201) %>% 
  transmute(State, County, AMI4_ELI = ELI_4, AMI4_50 = l50_4, AMI4_80 = l80_4, AMI4_100 = median2019, AMI_120 = median2019 * 1.2)
# note these are thresholds for 4-person households -- may want to see if data is available broken down by household

# 
incomes <- incomes %>% 
  left_join(metadata %>% 
              select(variable, label), 
            by = "variable") %>%
  mutate(income_min = str_extract(label, "\\$[0-9]{2,3} [0-9]{3}"), 
         income_max = str_extract(label, "\\$[0-9]{2,3} [0-9]{3}$"))
incomes[c("income_min", "income_max")] <- incomes[c("income_min", "income_max")] %>% 
  lapply(str_replace, "\\$", "") %>% 
  lapply(str_replace, " ", "") %>%
  lapply(as.numeric)
incomes <- incomes %>%
  mutate(income_min = ifelse(str_detect(label, "Less than"), 0, income_min), 
         var_name = ifelse(str_detect(label, "Total$"), "hhs_total", 
                           ifelse(str_detect(label, "200"), "hhs_above_200k",
                                  paste0("hhs_", income_min/1000, "k_", round(income_max/1000), "k"))))
incomes[c("pct_below_eli", "pct_below_ami50", "pct_below_ami80", "pct_below_ami100", "pct_below_ami120")] <- ami[c("AMI4_ELI", "AMI4_50", "AMI4_80", "AMI4_100", "AMI_120")] %>% 
  lapply(function(ami_thresh){
    pct_below <- (ami_thresh - incomes$income_min)/(incomes$income_max+1 - incomes$income_min)
    pct_below[incomes$income_min >= ami_thresh] <- 0
    pct_below[incomes$income_max <= ami_thresh] <- 1
    return(pct_below)
  })
  
# estimate households below % ami threshholds for block groups
incomes_ami <- incomes %>% 
  group_by(GEOID) %>% 
  summarise(hhs_below_eli = sum(pct_below_eli * estimate, na.rm = T), 
            hhs_below_ami50 = sum(pct_below_ami50 * estimate, na.rm = T), 
            hhs_below_ami80 = sum(pct_below_ami80 * estimate, na.rm = T), 
            hhs_below_ami100 = sum(pct_below_ami100 * estimate, na.rm = T), 
            hhs_below_ami120 = sum(pct_below_ami120 * estimate, na.rm = T)) %>%
  ungroup()

# create wide version of b19001 table and join calculated % ami variables
incomes_wide <- incomes %>% 
  select(GEOID, var_name, estimate) %>% 
  spread(var_name, estimate) %>% 
  left_join(incomes_ami, by = "GEOID")

# calculate percentage of households below % ami thresholds
incomes_wide[c("pct_below_eli", "pct_below_ami50", "pct_below_ami80", "pct_below_ami100", "pct_below_ami120")] <- incomes_wide[c("hhs_below_eli", "hhs_below_ami50", "hhs_below_ami80", "hhs_below_ami100", "hhs_below_ami120")] %>%
  lapply(function(v){
    v/incomes_wide$hhs_total
  })

# write csv
write.csv(incomes_wide, "E:/Dropbox (Traffic Engineers)/kelsey/blockgroups_income_ami.csv", row.names = F)
