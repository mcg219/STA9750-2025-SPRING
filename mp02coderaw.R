ensure_package <- function(pkg){
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if(!require(pkg, character.only=TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only=TRUE))
}

ensure_package(dplyr)
ensure_package(stringr)
ensure_package(tidyr)
ensure_package(httr2)
ensure_package(rvest)
ensure_package(datasets)
ensure_package(purrr)
ensure_package(DT)

get_eia_sep <- function(state, abbr){
  state_formatted <- str_to_lower(state) |> str_replace_all("\\s", "")
  
  dir_name <- file.path("data", "mp02")
  file_name <- file.path(dir_name, state_formatted)
  
  dir.create(dir_name, showWarnings=FALSE, recursive=TRUE)
  
  if(!file.exists(file_name)){
    BASE_URL <- "https://www.eia.gov"
    REQUEST <- request(BASE_URL) |> 
      req_url_path("electricity", "state", state_formatted)
    
    RESPONSE <- req_perform(REQUEST)
    
    resp_check_status(RESPONSE)
    
    writeLines(resp_body_string(RESPONSE), file_name)
  }
  
  TABLE <- read_html(file_name) |> 
    html_element("table") |> 
    html_table() |>
    mutate(Item = str_to_lower(Item))
  
  if("U.S. rank" %in% colnames(TABLE)){
    TABLE <- TABLE |> rename(Rank = `U.S. rank`)
  }
  
  CO2_MWh <- TABLE |> 
    filter(Item == "carbon dioxide (lbs/mwh)") |>
    pull(Value) |> 
    str_replace_all(",", "") |>
    as.numeric()
  
  PRIMARY <- TABLE |> 
    filter(Item == "primary energy source") |> 
    pull(Rank)
  
  RATE <- TABLE |>
    filter(Item == "average retail price (cents/kwh)") |>
    pull(Value) |>
    as.numeric()
  
  GENERATION_MWh <- TABLE |>
    filter(Item == "net generation (megawatthours)") |>
    pull(Value) |>
    str_replace_all(",", "") |>
    as.numeric()
  
  data.frame(CO2_MWh               = CO2_MWh, 
             primary_source        = PRIMARY,
             electricity_price_MWh = RATE * 10, # / 100 cents to dollars &
             # * 1000 kWh to MWH 
             generation_MWh        = GENERATION_MWh, 
             state                 = state, 
             abbreviation          = abbr
  )
}

EIA_SEP_REPORT <- map2(state.name, state.abb, get_eia_sep) |> list_rbind()


ensure_package(scales)
ensure_package(DT)

EIA_SEP_REPORT |> 
  select(-abbreviation) |>
  arrange(desc(CO2_MWh)) |>
  mutate(CO2_MWh = number(CO2_MWh, big.mark=","), 
         electricity_price_MWh = dollar(electricity_price_MWh), 
         generation_MWh = number(generation_MWh, big.mark=",")) |>
  rename(`Pounds of CO2 Emitted per MWh of Electricity Produced`=CO2_MWh, 
         `Primary Source of Electricity Generation`=primary_source, 
         `Average Retail Price for 1000 kWh`=electricity_price_MWh, 
         `Total Generation Capacity (MWh)`= generation_MWh, 
         State=state) |>
  datatable()

# Calculate the average CO2 emissions per MWh from the original numeric column
avg_CO2 <- EIA_SEP_REPORT |> 
  summarise(`Average Pounds of CO2 per MWh` = mean(CO2_MWh, na.rm = TRUE))

print(avg_CO2)








ensure_package(readxl)
# Create 'data/mp02' directory if not already present
DATA_DIR <- file.path("data", "mp02")
dir.create(DATA_DIR, showWarnings=FALSE, recursive=TRUE)

NTD_ENERGY_FILE <- file.path(DATA_DIR, "2023_ntd_energy.xlsx")

if(!file.exists(NTD_ENERGY_FILE)){
  DS <- download.file("https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-10/2023%20Energy%20Consumption.xlsx", 
                      destfile=NTD_ENERGY_FILE, 
                      method="curl")
  
  if(DS | (file.info(NTD_ENERGY_FILE)$size == 0)){
    cat("I was unable to download the NTD Energy File. Please try again.\n")
    stop("Download failed")
  }
}

NTD_ENERGY_RAW <- read_xlsx(NTD_ENERGY_FILE)

ensure_package(tidyr)
to_numeric_fill_0 <- function(x){
  x <- if_else(x == "-", NA, x)
  replace_na(as.numeric(x), 0)
}

NTD_ENERGY <- NTD_ENERGY_RAW |> 
  select(-c(`Reporter Type`, 
            `Reporting Module`, 
            `Other Fuel`, 
            `Other Fuel Description`)) |>
  mutate(across(-c(`Agency Name`, 
                   `Mode`,
                   `TOS`), 
                to_numeric_fill_0)) |>
  group_by(`NTD ID`, `Mode`, `Agency Name`) |>
  summarize(across(where(is.numeric), sum), 
            .groups = "keep") |>
  mutate(ENERGY = sum(c_across(c(where(is.numeric))))) |>
  filter(ENERGY > 0) |>
  select(-ENERGY) |>
  ungroup()

# Display 10 random rows
slice_sample(NTD_ENERGY , n=10)

# (TASK 3)
NTD_ENERGY <- NTD_ENERGY |>
  mutate(Mode = case_when(
    Mode == "HR" ~ "Heavy Rail",
    Mode == "AR" ~ "Automated Rail",
    Mode == "CB" ~ "Conventional Bus",
    Mode == "CC" ~ "Commuter Coach",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferry Boat",
    Mode == "IP" ~ "Intercity Passenger",
    Mode == "LR" ~ "Light Rail",
    Mode == "MB" ~ "Motor Bus",
    Mode == "MG" ~ "Monorail",
    Mode == "PB" ~ "Paratransit Bus",
    Mode == "RB" ~ "Rail Bus",
    Mode == "SR" ~ "Streetcar",
    Mode == "TB" ~ "Trolley Bus",
    Mode == "TR" ~ "Transit Rail",
    Mode == "VP" ~ "Vanpool",
    Mode == "YR" ~ "Yard Rail",
    TRUE ~ "Unknown"
  ))











library(readr)
NTD_SERVICE_FILE <- file.path(DATA_DIR, "2023_service.csv")
if(!file.exists(NTD_SERVICE_FILE)){
  DS <- download.file("https://data.transportation.gov/resource/6y83-7vuw.csv", 
                      destfile=NTD_SERVICE_FILE, 
                      method="curl")
  
  if(DS | (file.info(NTD_SERVICE_FILE)$size == 0)){
    cat("I was unable to download the NTD Service File. Please try again.\n")
    stop("Download failed")
  }
}

NTD_SERVICE_RAW <- read_csv(NTD_SERVICE_FILE)

NTD_SERVICE <- NTD_SERVICE_RAW |>
  mutate(`NTD ID` = as.numeric(`_5_digit_ntd_id`)) |> 
  rename(Agency = agency, 
         City   = max_city, 
         State  = max_state,
         UPT    = sum_unlinked_passenger_trips_upt, 
         MILES  = sum_passenger_miles) |>
  select(matches("^[A-Z]", ignore.case=FALSE)) |>
  filter(MILES > 0)

slice_sample(NTD_SERVICE , n=10)

library(dplyr)

# 1. What is the average trip length of a trip on MTA NYC?
#    - “UPT” = number of trips
#    - “MILES” = total miles
#    - Focus only on row(s) where Agency == "MTA New York City Transit"

mta_nyc_avg_trip_length <- NTD_SERVICE %>%
  filter(Agency == "MTA New York City Transit") %>%
  summarize(
    total_miles = sum(MILES),
    total_upt   = sum(UPT),
    avg_trip_length = total_miles / total_upt
  )

mta_nyc_avg_trip_length
# This will give you the average trip length for MTA New York City Transit.


# 2. Which transit service in NYC has the longest average trip length?
#    - Focus on rows where City is either "New York City" or "Brooklyn" 
#    - Compute average trip length = sum of miles / sum of trips for each Agency
#    - Then find the agency with the maximum average trip length
#    - Note that this is made with the assumption that because Brooklyn is technically part of NYC, 
#      we count all agencies that have Brooklyn or NYC in the "City" Column. 

nyc_services_longest_trip <- NTD_SERVICE %>%
  filter(City %in% c("New York City", "Brooklyn")) %>%
  group_by(Agency) %>%
  summarize(
    total_miles = sum(MILES),
    total_upt   = sum(UPT),
    avg_trip_length = total_miles / total_upt
  ) %>%
  arrange(desc(avg_trip_length)) %>%
  slice(2)

nyc_services_longest_trip
# This returns the transit service (Agency) with the largest average trip length in NYC/Brooklyn.


# 3. Which state has the fewest total miles travelled by public transit?
#    - Sum miles by state, then find the state with the smallest total
#    - “MILES” = total miles

fewest_miles_by_state <- NTD_SERVICE %>%
  group_by(State) %>%
  summarize(total_miles = sum(MILES)) %>%
  arrange(total_miles) %>%
  slice(1)

fewest_miles_by_state
# This shows the state with the fewest total miles traveled.


# 4. Are all states represented in this data? If no, which ones are missing?
#    - Use state.abb and state.name, compare to the unique State values present in NTD_SERVICE

all_states <- data.frame(
  full_name = state.name,
  abbr      = state.abb,
  stringsAsFactors = FALSE
)

unique_states_in_data <- unique(NTD_SERVICE$State)

missing_states <- all_states %>%
  filter(!abbr %in% unique_states_in_data)

missing_states
# If the resulting data frame is empty, then all states are present.
# Otherwise, it shows which states are not present in NTD_SERVICE.








#TASK 5



NTD_ENERGY_WITH_STATE <- merge(
  NTD_ENERGY,
  NTD_SERVICE[ , c("NTD ID", "State")],
  by = "NTD ID",
  all.x = TRUE
)

# 2) Merge the above result with EIA_SEP_REPORT to add CO2_MWh
#    using "State" from the first table and "abbreviation" from EIA_SEP_REPORT
TASK_5_TABLE <- merge(
  NTD_ENERGY_WITH_STATE,
  EIA_SEP_REPORT[ , c("abbreviation", "CO2_MWh")],
  by.x = "State",
  by.y = "abbreviation",
  all.x = TRUE
)



# Display 10 random rows
slice_sample(TASK_5_TABLE , n=10)

emission_factors <- c(
  "Bio-Diesel"             = 22.51,    # IE: Waste Oil
  "Bunker Fuel"            = 24.781597,
  "C Natural Gas"          = 15.31,     # Approx. for CNG, see notes
  "Diesel Fuel"            = 22.454989,
  "Ethanol"                = 12.57,
  "Methonal"               = 8.44,
  "Gasoline"               = 20.862476,
  "Hydrogen"               = 0,         # Direct combustion only
  "Kerosene"               = 21.782087,
  "Liquified Nat Gas"      = 4.46,
  "Liquified Petroleum Gas"= 12.678396
)

# Initialize the new column (start at zero for each row)
TASK_5_TABLE$Pounds_CO2 <- 0

# Loop over each fuel column, multiply gallons by emission factor, and sum
for(fuel_col in names(emission_factors)) {
  TASK_5_TABLE$Pounds_CO2 <- TASK_5_TABLE$Pounds_CO2 + 
    (TASK_5_TABLE[[fuel_col]] * emission_factors[[fuel_col]])
}


# Calculate total electricity in MWh (Electric Battery and Electric Propulsion are in kWh)
electric_total_MWh <- (TASK_5_TABLE[["Electric Battery"]] + TASK_5_TABLE[["Electric Propulsion"]]) / 1000

# Compute the average CO2_MWh from rows with non-missing values
average_CO2_MWh <- mean(TASK_5_TABLE$CO2_MWh, na.rm = TRUE)

# Replace missing CO2_MWh values with the average value for calculation
co2_mwh_filled <- ifelse(is.na(TASK_5_TABLE[["CO2_MWh"]]), average_CO2_MWh, TASK_5_TABLE[["CO2_MWh"]])

# Multiply the electricity (in MWh) by the CO2_MWh factor and add to Pounds_CO2
TASK_5_TABLE$Pounds_CO2 <- TASK_5_TABLE$Pounds_CO2 + (electric_total_MWh * co2_mwh_filled)


# 1. Calculate total Pounds_CO2 per NTD ID (across all modes)
total_co2 <- TASK_5_TABLE %>%
  group_by(`NTD ID`) %>%
  summarise(total_Pounds_CO2_Per_Agency = sum(Pounds_CO2, na.rm = TRUE))

# 2. Join the total CO2 to the mode-level table
TASK_5_TABLE <- TASK_5_TABLE %>%
  left_join(total_co2, by = "NTD ID")

# 3. Join UPT and MILES from the service-level table (NTD_SERVICE)
#    (Assuming NTD_SERVICE has columns: "NTD ID", UPT, and MILES)
TASK_5_TABLE <- TASK_5_TABLE %>%
  left_join(NTD_SERVICE %>% select(`NTD ID`, UPT, MILES), by = "NTD ID")

# 4. Allocate UPT and MILES to each mode based on the proportion of Pounds_CO2
TASK_5_TABLE <- TASK_5_TABLE %>%
  mutate(
    UPT_Per_Mode   = ifelse(total_Pounds_CO2_Per_Agency == 0, 0, Pounds_CO2 / total_Pounds_CO2_Per_Agency * UPT),
    MILES_Per_Mode = ifelse(total_Pounds_CO2_Per_Agency == 0, 0, Pounds_CO2 / total_Pounds_CO2_Per_Agency * MILES)
  )

TASK_5_TABLE <- TASK_5_TABLE %>%
  mutate(
    # Create pounds per UPT: if alloc_UPT is 0, assign NA to avoid division by zero
    Pounds_CO2_Per_UPT = ifelse(UPT_Per_Mode == 0, NA, Pounds_CO2 / UPT_Per_Mode),
    # Create pounds per MILES: if alloc_MILES is 0, assign NA
    Pounds_CO2_Per_MILES = ifelse(MILES_Per_Mode == 0, NA, Pounds_CO2 / MILES_Per_Mode)
  )




#Final Analysis



library(dplyr)
library(readr)
library(tidyr)

df_clean <- TASK_5_TABLE %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

###############################################################################
# 3) Group by Agency Name and sum relevant columns
###############################################################################
fuel_cols <- c("Bio-Diesel", "Bunker Fuel", "Diesel Fuel", "Gasoline",
               "C Natural Gas", "Liquified Petroleum Gas",
               "Electric Battery", "Electric Propulsion")

grouped <- df_clean %>%
  group_by(`Agency Name`) %>%
  summarise(
    Total_Pounds_CO2   = sum(Pounds_CO2, na.rm = TRUE),
    Total_UPT          = sum(UPT, na.rm = TRUE),
    Total_MILES        = sum(MILES, na.rm = TRUE),
    Electric_Battery   = sum(`Electric Battery`, na.rm = TRUE),
    Electric_Propulsion = sum(`Electric Propulsion`, na.rm = TRUE),
    Total_Fuel_Usage   = sum(across(all_of(fuel_cols)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # Award #1: CO2 per UPT
    CO2_per_UPT = if_else(Total_UPT > 0, Total_Pounds_CO2 / Total_UPT, NA_real_),
    
    # Award #2: Emissions Avoided
    Passenger_Miles_if_car = Total_UPT * 5,
    Car_Gallons            = Passenger_Miles_if_car / 24,
    Car_CO2                = Car_Gallons * 19.6,
    Emissions_Avoided      = Car_CO2 - Total_Pounds_CO2,
    
    # Basic Electric Ratio (before new Award #3 logic)
    Electric_Usage     = Electric_Battery + Electric_Propulsion,
    Electric_Ratio     = if_else(Total_Fuel_Usage > 0,
                                 Electric_Usage / Total_Fuel_Usage,
                                 NA_real_),
    
    # Award #4: Occupancy Ratio
    Occupancy_Ratio    = if_else(Total_MILES > 0,
                                 Total_UPT / Total_MILES,
                                 NA_real_)
  )

###############################################################################
# 4) Identify winners for Awards #1, #2, #4 using original logic
###############################################################################
# Award #1: Greenest Transit Agency = lowest CO2_per_UPT
award1_winner <- grouped %>%
  filter(!is.na(CO2_per_UPT)) %>%
  slice_min(CO2_per_UPT, n = 1)

# Award #2: Most Emissions Avoided = highest Emissions_Avoided
award2_winner <- grouped %>%
  filter(!is.na(Emissions_Avoided)) %>%
  slice_max(Emissions_Avoided, n = 1)

# Award #4: Worst Occupancy = lowest Occupancy_Ratio
award4_winner <- grouped %>%
  filter(!is.na(Occupancy_Ratio)) %>%
  slice_min(Occupancy_Ratio, n = 1)

###############################################################################
# 5) Updated Award #3:
#    - Filter to agencies with Electric_Ratio == 1
#    - Winner = agency with highest Total_MILES in that subset
#    - Reference = agency in that subset whose Total_MILES is closest to 
#                  the subset's median Total_MILES
###############################################################################

# Subset to fully-electric agencies
electrified_only <- grouped %>%
  filter(!is.na(Electric_Ratio), Electric_Ratio == 1)

# If no fully-electric agencies, handle it gracefully
if (nrow(electrified_only) == 0) {
  award3_winner <- NULL
  reference3    <- NULL
  median_miles_electrified <- NA_real_
} else {
  # Award #3 Winner: agency with highest Total_MILES among those with ratio = 1
  award3_winner <- electrified_only %>%
    slice_max(Total_MILES, n = 1)
  
  # Calculate median total miles in fully-electric subset
  median_miles_electrified <- median(electrified_only$Total_MILES, na.rm = TRUE)
  
  # Function to get row with Total_MILES closest to that median
  find_closest_to_median_miles <- function(data, col) {
    val <- median(data[[col]], na.rm = TRUE)
    data %>%
      mutate(abs_diff = abs(.data[[col]] - val)) %>%
      slice_min(abs_diff, n = 1) %>%
      select(-abs_diff)
  }
  
  reference3 <- find_closest_to_median_miles(electrified_only, "Total_MILES")
}

###############################################################################
# 6) Compute medians for (Awards #1, #2, #4) and find reference agencies
###############################################################################
find_closest_to_median <- function(data, metric_col) {
  med_val <- median(data[[metric_col]], na.rm = TRUE)
  data %>%
    mutate(abs_diff = abs(.data[[metric_col]] - med_val)) %>%
    slice_min(abs_diff, n = 1) %>%
    select(-abs_diff)
}

# AWARD #1: CO2_per_UPT
median_co2_per_upt <- median(grouped$CO2_per_UPT, na.rm = TRUE)
reference1 <- grouped %>%
  filter(!is.na(CO2_per_UPT)) %>%
  find_closest_to_median("CO2_per_UPT")

# AWARD #2: Emissions_Avoided
median_emissions_avoided <- median(grouped$Emissions_Avoided, na.rm = TRUE)
reference2 <- grouped %>%
  filter(!is.na(Emissions_Avoided)) %>%
  find_closest_to_median("Emissions_Avoided")

# AWARD #4: Occupancy_Ratio
median_occupancy_ratio <- median(grouped$Occupancy_Ratio, na.rm = TRUE)
reference4 <- grouped %>%
  filter(!is.na(Occupancy_Ratio)) %>%
  find_closest_to_median("Occupancy_Ratio")

###############################################################################
# 7) Print results
###############################################################################
cat("===== GTA IV Awards with Updated Award #3 Logic =====\n\n")

# Award #1
cat("---- Award #1: Greenest Transit Agency (lowest CO2_per_UPT) ----\n")
cat("Winner:\n")
print(award1_winner)
cat("\nMedian CO2_per_UPT =", median_co2_per_upt, "\n")
cat("Reference agency (closest to median CO2_per_UPT):\n")
print(reference1)

# Award #2
cat("\n---- Award #2: Most Emissions Avoided (highest Emissions_Avoided) ----\n")
cat("Winner:\n")
print(award2_winner)
cat("\nMedian Emissions_Avoided =", median_emissions_avoided, "\n")
cat("Reference agency (closest to median Emissions_Avoided):\n")
print(reference2)

# Award #3
cat("\n---- Award #3: Electrification Champion (Electric_Ratio == 1) ----\n")
if (is.null(award3_winner)) {
  cat("No agencies have an Electric_Ratio of 1.\n")
} else {
  cat("Fully-electric subset Winner (highest Total_MILES):\n")
  # Print a simplified subset, to highlight Agency, MILES, and ratio
  print(
    award3_winner %>%
      select(`Agency Name`, Total_MILES, Electric_Ratio)
  )
}

# Award #4
cat("\n---- Award #4: Worst Occupancy (lowest Occupancy_Ratio) ----\n")
cat("Winner:\n")
print(award4_winner)

# Print Occupancy_Ratio explicitly
cat("\nWinner's Occupancy_Ratio =", award4_winner$Occupancy_Ratio, "\n")

cat("\nMedian Occupancy_Ratio =", median_occupancy_ratio, "\n")
cat("Reference agency (closest to median Occupancy_Ratio):\n")
print(reference4)

cat("\n===== End of Script =====\n")
