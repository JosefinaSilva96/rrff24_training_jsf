# Reproducible Research Fundamentals 
# 02. Data construction
# RRF - 2024 - Construction

# Load the dataset
data_path <- "C:/Users/wb631166/OneDrive - WBG/Desktop/Reproducible Research Fundamentals 2024/RRF - public/Course Materials/DataWork/Data"
data      <- read_dta(file.path(data_path, "Raw/TZA_CCT_baseline.dta"))

#Exercise 1: Plan construction outputs
#Are all the variables required for analysis present in the clean datasets?
#Which datasets and variables do you need to use to create the outcomes of
#interest?
#Do you need to transform the existing variables to create the new ones?
#Area un acres: area variable:standardized to acre
#Household consumption: food and nonfood consumption variables: standaedized to USD
#Any member sick in the last days:any member sick variable: collapsed to HH level
#Any member can read or write:any member can read/write variables collapsed HH Level
#average sick days: 

# Preliminary - Load Data ----
# Load household-level data (HH)
hh_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))

# Load HH-member data
mem_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))

# Load secondary data
secondary_data <- read_dta(file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))

# Exercise 1: Plan construction outputs ----
# Plan the following outputs:
#1. Area un acres: area variable:standardized to acre
#2. Household consumption: food and nonfood consumption variables: standaedized to USD
#3. Any member sick in the last days:any member sick variable: collapsed to HH level
#4. Any member can read or write:any member can read/write variables collapsed HH Level
#5. average sick days: average sick day variable
# 6. Total treatment cost in USD.treatment cost variable collapsed to HH level
# 7. Total medical facilities. sum of clinics and hospitals

# Exercise 2: Standardize conversion values ----
# Define standardized conversion values:
# 1. Conversion factor for acres.
# 2. USD conversion factor.

#Exercise 2: Standardize  conversion values

acre_conv <- 2.47
usd <- 0.00037


#Data Cobstruction: HH

#Area in acres (converts units for farming units)

hh_data <- hh_data %>% 
    mutate(area_acre = case_when(
        ar_unit == 1 ~ ar_farm, 
        ar_unit == 2 ~ ar_farm * acre_conv 
    )) %>%
    mutate (area_acre = replace_na (area_acre, 0)) %>%
    set_value_labels(area_acre = "Area farmed in acres")

#Consumption in USD 

 hh_data<- hh_data %>%
     mutate(across(c(food_cons, nonfood_cons),
                   ~ .x * usd, 
                   .names= "{.col}_usd"))
 

# Data construction: Household (HH) ----
# Instructions:
# 1. Convert farming area to acres where necessary.
# 2. Convert household consumption for food and nonfood into USD.

# Exercise 3: Handle outliers ----
# decide to create our own function 
# you can use custom Winsorization function to handle outliers.
 
winsor_function <- function(dataset, var, min = 0.00, max = 0.95) {
    var_sym <- sym(var)
    
    percentiles <- quantile(
        dataset %>% pull(!!var_sym), probs = c(min, max), na.rm = TRUE
    )
    
    min_percentile <- percentiles[1]
    max_percentile <- percentiles[2]
    
    dataset %>%
        mutate(
            !!paste0(var, "_w") := case_when(
                is.na(!!var_sym) ~ NA_real_,
                !!var_sym <= min_percentile ~ percentiles[1],
                !!var_sym >= max_percentile ~ percentiles[2],
                TRUE ~ !!var_sym
            )
        )
}

# Tips: Apply the Winsorization function to the relevant variables.
# Create a list of variables that require Winsorization and apply the function to each.

#Winsorize selected variables in the dataset
 
win_vars<- c("area_acre", "food_cons_usd", "nonfood_cons") 

# Apply the custom winsor function to each variable in win_vars 

for (var in win_vars){
    hh_data<- winsor_function(hh_data, var)
}

#Update the labels to reflect that winsorization was applied

hh_data <- hh_data  %>%
    mutate(across(ends_with("_w"),
                  ~ labelled(.x, label=paste0(attr(.x, label),
                                              "(Winsorized 0.05)"))))

 
 
# Exercise 4.1: Create indicators at household level ----
# Instructions:
# Collapse HH-member level data to HH level.
# Plan to create the following indicators:
# 1. Any member was sick.
# 2. Any member can read/write.
# 3. Average sick days.
# 4. Total treatment cost in USD.

hh_men_collapsed<- mem_data %>%
    group_by(hhid) %>%
    sumarise(
        sick = max (sick, na.rm = TRUE),
        read = max (read, na.rm = TRUE),
        days_sick = if_else(all(is.na(days_sick)), NA_real_, mean(days_sick, na.rm = TRUE)), 
        treat_cost_usd = if_else(all(is.na(treat_cost)), NA_real_, sum(treat_cost_usd, na.rm = TRUE) * usd)
    ) %>%
    ungroup() %<%
    mutate(treat_cost_usd = if_else(is.na(treat_cost_usd), mean(treat_cost_usd, na.rm = TRUE), treat_cost_usd))
set_variable_labels(
    read = "Any member can read/write", 
    sick= "Any member was sick in the las 4 weeks", 
    days_sick = "Average sick days", 
    treat_cost_usd = "Total cost of treatment (USD)"
           
    )



# Exercise 4.2: Data construction: Secondary data ----
# Instructions:
# Calculate the total number of medical facilities by summing relevant columns.

secondary_data<- seondary_data %>% 
    mutate(n_medical = rowSums(select(., n_clinic, n_hospital), na.rm = TRUE)) 


# Apply appropriate labels to the new variables created.

 var_label (secondary_data$n_medical) <- "No. of medical facilities"

# Exercise 5: Merge HH and HH-member data ----
# Instructions:
# Merge the household-level data with the HH-member level indicators.
# After merging, ensure the treatment status is included in the final dataset.
 
#Merge HH and HH-member datasets 
 
 final_hh_data<- hh_data %>% left_join(hh_men_collapsed, by = "hhid")
 
#Load treatment status and merge
 
 treat_status <- read_dta(file.path(data_path, "Raw/treat_status.dta"))
 
 final_hh_data <- final_hh_data %>% 
     left_join(treat_status, by = "vid")
     
 

# Exercise 6: Save final dataset ----
# Instructions:
# Only keep the variables you will use for analysis.
# Save the final dataset for further analysis.
# Save both the HH dataset and the secondary data.
 

 #Save the final merged data data for analysis 
 
 write_dta(final_hh_data, file.path(data_path, "Final/TZA_CCT_analysis"))
 
 #Save the final secondary data for analysis
 
 write_dta(secondary_data, file.path(data_path, "Final/TZA_amenity_analysis.dta"))

# Tip: Ensure all variables are correctly labeled 

