##SECONDARY ANALYSIS:##

# Installing packages needed:
#install.packages("tidyverse")
library(tidyverse)

#install.packages("dplyr")
library(dplyr)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("scales")
library(scales)



#install.packages("here")
library(here)

#install.packages("readr")
library(readr)

#install.packages("data.table")
library(data.table)

#install.packages("patchwork") 
library(patchwork)

#setwd("") # To where you have saved the SCMD dataset, called: "lithium-full.xlsx"

# Output directories
output_dir <- here("output")
plots_dir <- here("output", "plots")
data_dir <- here("output", "data")
dir.create(output_dir, showWarnings = FALSE)
dir.create(plots_dir, showWarnings = FALSE)
dir.create(data_dir, showWarnings = FALSE)

# importing dataset
#install.packages("readxl")
library(readxl)

#install.packages("sf")
library(sf)

Lithium_SCMD <- read_excel(here("data", "secondary_care", "lithium-full.xlsx")) # Secondary care Lithium dataset for 2019-2024

#### Cleaning/ Organising dataset for plotting ########
# Mapping regions
org_mapping <- read_excel(here("data", "secondary_care", "org-mapping.xlsx")) # importing dataset that can map ods_code to regions in secondary care dataset

Lithium_SCMD <- Lithium_SCMD %>%
  left_join(org_mapping %>% select(ods_code, region), by = "ods_code")

unique(Lithium_SCMD$region) # ALL organisations are now attributed to a region

#View(Lithium_SCMD) # checking

# converting to date from date-time, will make plotting easier
class(Lithium_SCMD$year_month) #[1] "POSIXct" "POSIXt" 

Lithium_SCMD <- Lithium_SCMD %>%
  mutate(year_month = as.Date(year_month))

unique(Lithium_SCMD$year_month)  # date-time to just date

#View(Lithium_SCMD) # checking

# calculating DDDs for carbonate and citrate
Lithium_SCMD <- Lithium_SCMD %>%
  mutate(
    # total_mg only needed for Lithium carbonate tablets
    total_mg = case_when(
      ingredient_name == "Lithium carbonate" & quantity_basis == "tablet" ~ converted_quantity * strength_numerator_value,
      TRUE ~ NA_real_
    ),
    
    # mmol calculation
    mmol = case_when(
      ingredient_name == "Lithium carbonate" ~ total_mg / 37.04,
      
      # Lithium citrate — match by product name
      ingredient_name == "Lithium citrate" & grepl("509", vmp_name) ~ (converted_quantity / 5) * 5.4,
      ingredient_name == "Lithium citrate" & grepl("520", vmp_name) ~ (converted_quantity / 5) * 5.4,
      ingredient_name == "Lithium citrate" & grepl("1.018", vmp_name) ~ (converted_quantity / 5) * 10.8,
      
      TRUE ~ NA_real_
    ),
    
    # DDD calculation
    DDD = mmol / 24
  )


####### Prescribing trends over the years 2019-2024 in Secondary care ########

# creating a year column
Lithium_SCMD <- Lithium_SCMD %>%
  mutate(year = format(as.Date(year_month), "%Y"))

Secondary_DDD_by_year <- Lithium_SCMD %>%
  filter(year != "2025") %>% # as 2025 does not include data for the whole year
  group_by(year) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup()

#View(Secondary_DDD_by_year)

# Line graph for prescribing trends over time
secondary_line <- ggplot(Secondary_DDD_by_year, aes(x = as.integer(year), y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = "#00BFC4") +
  geom_point(size = 3, color = "#F8766D") +
  labs(
    title = "Secondary Care: Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year (2019–2024)",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  scale_y_continuous(
    limits = c(0, 1.2),
    expand = c(0, 0),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  scale_x_continuous(breaks = 2019:2024) +
  coord_cartesian(ylim = c(0, 1.2)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

plot(secondary_line)
ggsave(here(plots_dir, "secondary_line_trends.png"), secondary_line, width = 8, height = 5, dpi = 300)

# Accompanying bar plot for prescribing trends over time
secondary_bar <- ggplot(Secondary_DDD_by_year, aes(x = as.factor(year), y = total_DDD / 1e6)) +
  geom_bar(stat = "identity", fill = "#00BFC4", width = 0.6) +
  geom_text(
    aes(label = format(round(total_DDD / 1e6, 1), nsmall = 1)),
    vjust = -0.5,
    size = 4.2,
    fontface = "bold"
  ) +
  labs(
    title = "Secondary Care: Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year (2019–2024)",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  scale_y_continuous(
    labels = function(x) format(x, scientific = FALSE, big.mark = ","),
    expand = expansion(mult = c(0, 0.1))  # Adds space above bars for text
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )
plot(secondary_bar)
ggsave(here(plots_dir, "secondary_bar_trends.png"), secondary_bar, width = 8, height = 5, dpi = 300)

############################Secondary care Mapping plot, 2024 DDD / population ############################

# NHS England Regions (ONS Jan 2024)
nhs_regions_file <- here("analysis", "NHS_England_Regions_January_2024_EN_BGC.geojson")

# Load NHS England Regions (ONS Jan 2024) via sf
nhs_regions_sf <- st_read(nhs_regions_file, quiet = TRUE)
# Transform to WGS84 for proper display (ONS BGC uses British National Grid EPSG:27700)
if (st_crs(nhs_regions_sf)$input != "EPSG:4326") {
  nhs_regions_sf <- st_transform(nhs_regions_sf, 4326)
}
# Map NHSER code or ONS name to our region names
nhser_to_region <- c(
  "Y56" = "London", "Y58" = "South West", "Y59" = "South East",
  "Y60" = "Midlands", "Y61" = "East Of England", "Y62" = "North West",
  "Y63" = "North East And Yorkshire",
  "London" = "London", "South West" = "South West", "South East" = "South East",
  "Midlands" = "Midlands", "East of England" = "East Of England",
  "North West" = "North West", "North East and Yorkshire" = "North East And Yorkshire"
)
nhser_to_Region <- c(
  "Y56" = "London", "Y58" = "South West", "Y59" = "South East",
  "Y60" = "Midlands", "Y61" = "East of England", "Y62" = "North West",
  "Y63" = "North East And Yorkshire",
  "London" = "London", "South West" = "South West", "South East" = "South East",
  "Midlands" = "Midlands", "East of England" = "East of England",
  "North West" = "North West", "North East and Yorkshire" = "North East And Yorkshire"
)
# Find code or name column (ONS uses NHSER20CD, NHSER24CD, NHSER20NM, etc.)
code_col <- names(nhs_regions_sf)[grepl("NHSER.*CD|NHSER.*Code|^code$", names(nhs_regions_sf), ignore.case = TRUE)][1]
name_col <- names(nhs_regions_sf)[grepl("NHSER.*NM|^name$|^NAME$", names(nhs_regions_sf), ignore.case = TRUE)][1]
if (is.na(code_col)) code_col <- name_col
if (is.na(code_col)) code_col <- names(nhs_regions_sf)[!names(nhs_regions_sf) %in% c("geometry")][1]
region_from_code <- nhser_to_region[as.character(nhs_regions_sf[[code_col]])]
region_from_name <- if (!is.na(name_col)) nhser_to_region[as.character(nhs_regions_sf[[name_col]])] else rep(NA_character_, nrow(nhs_regions_sf))
Region_from_code <- nhser_to_Region[as.character(nhs_regions_sf[[code_col]])]
Region_from_name <- if (!is.na(name_col)) nhser_to_Region[as.character(nhs_regions_sf[[name_col]])] else rep(NA_character_, nrow(nhs_regions_sf))

nhs_regions_sf <- nhs_regions_sf %>%
  mutate(
    region = coalesce(region_from_code, region_from_name),
    Region = coalesce(Region_from_code, Region_from_name)
  ) %>%
  filter(!is.na(region))

if (nrow(nhs_regions_sf) == 0) {
  stop("No NHS regions matched. ONS file columns: ", paste(names(st_read(nhs_regions_file, quiet = TRUE)), collapse = ", "))
}

# Secondary care Mapping plot, focusing on all of 2024 DDD / population
# getting the sum of DDD per region (2024 total DDD for each region/ regional population estimates)
lithium_df <- Lithium_SCMD %>%
  group_by(region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  mutate(region = as.factor(region)) %>%
  filter(!is.na(region))

total_DDD_by_region_2024 <- Lithium_SCMD %>%
  filter(year(year_month) == 2024) %>%
  group_by(region) %>%
  summarise(total_DDD_2024 = sum(DDD, na.rm = TRUE))

secondary_lithium_df <- lithium_df %>%
  left_join(total_DDD_by_region_2024, by = "region")

#View(secondary_lithium_df)

# Using most recent ONS population estimates- mid 2023 ONS population estimates xlsx file
# Now adding population data to this dataset, that I got from population_data, estimates from ONS
NorthEast_Yorkt_pop =  8220282
NorthWest_pop = 7515718
midlands_pop =  10951858
east_pop = 6401418
london_pop = 8869043
SouthEast_pop = 9387286
SouthWest_pop = 5766937


secondary_lithium_df <- secondary_lithium_df %>%
  mutate(population = case_when(
    region == "North East And Yorkshire" ~ NorthEast_Yorkt_pop,
    region == "North West" ~ NorthWest_pop,
    region == "Midlands" ~ midlands_pop,
    region == "East Of England" ~ east_pop,
    region == "London" ~ london_pop,
    region == "South East" ~ SouthEast_pop,
    region == "South West" ~ SouthWest_pop
  ))

#View(secondary_lithium_df)

secondary_lithium_df <- secondary_lithium_df %>%
  mutate(`DDD/population` = total_DDD_2024 / population)

# Join NHS boundaries with secondary care data
coverage_data_secondary <- nhs_regions_sf %>%
  left_join(secondary_lithium_df, by = "region")

unique_values <- sort(unique(coverage_data_secondary$`DDD/population`))
breaks <- c(0, unique_values, max(unique_values, na.rm = TRUE) + 10)
labels <- scales::label_number(breaks)

secondary_coverage_plot <- coverage_data_secondary %>%
  ggplot() +
  geom_sf(aes(fill = `DDD/population`), colour = "black", linewidth = 0.8) +
  geom_sf_text(aes(label = region), colour = "white", size = 3) +
  scale_fill_gradientn(
    colors = c("#ffd13a", "#ff7c00", "#f20c51"),
    breaks = breaks,
    labels = labels,
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.2, 0.5),
    legend.text = element_text(hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(face = "bold")   # <-- Here is the bolding
  ) +
  guides(fill = guide_legend(title = "Lithium (DDD)/ population")) +
  coord_sf(datum = NA) +
  xlab("") +
  ylab("")


# Display the plot
plot(secondary_coverage_plot)
ggsave(here(plots_dir, "secondary_coverage_map.png"), secondary_coverage_plot, width = 8, height = 6, dpi = 300)

# Accompanying barplot: 2024 DDD/pop
secondaryhist = ggplot(secondary_lithium_df, aes(x = region, y = `DDD/population`)) +
  geom_col(fill = "#FF0000", color = "#FF6f6f") +
  geom_text(aes(label = round(`DDD/population`, 3)), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  xlab("Region") +
  ylab("Lithium usage (Total DDD for 2024) / population") +
  labs(
    title = "Regional Lithium Use in Secondary Care",
    subtitle = "Average DDDs per Person (2024) (stock movement)"  # <- Subtitle added here
  ) +
  scale_y_continuous(
    limits = c(0, 0.03),
    breaks = seq(0, 0.03, by = 0.01),
    labels = scales::number_format(accuracy = 0.001)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic"),  # Optional styling
    plot.margin = margin(10, 10, 10, 10)
  )


plot(secondaryhist)
ggsave(here(plots_dir, "secondary_hist_ddd_pop.png"), secondaryhist, width = 8, height = 5, dpi = 300)

#### Secondary regional Prescribing trends over time - using region, year and population #### - NEEDED later for plotting
Secondary_DDD_by_year_region <- Lithium_SCMD %>%
  filter(year != "2025") %>%
  group_by(year, region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup()

#View(Secondary_DDD_by_year_region)
print(Secondary_DDD_by_year_region$total_DDD)


population_df <- tibble(
  region = c("North East And Yorkshire", "North West", "Midlands", "East Of England", 
             "London", "South East", "South West"),
  population = c(8220282, 7515718, 10951858, 6401418, 8869043, 9387286, 5766937)
)

Secondary_DDD_by_year_region <- Secondary_DDD_by_year_region %>%
  left_join(population_df, by = "region")

Secondary_DDD_by_year_region <- Secondary_DDD_by_year_region %>%
  mutate(DDD_population = total_DDD / population) 

#View(Secondary_DDD_by_year_region)

seven_region_secondary <- ggplot(Secondary_DDD_by_year_region, aes(x = as.integer(year), y = DDD_population, color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Secondary Care: Lithium Prescribing Trends Per Population",
    subtitle = "DDD per population issued per year by region (2019–2024)",
    x = "Year",
    y = NULL,
    color = "Region"
  ) +
  scale_y_continuous(
    limits = c(0, 0.03),           # Adjusted here
    expand = c(0, 0),
    labels = scales::number_format(accuracy = 0.001)  # Show decimals, 3 digits
  ) +
  scale_x_continuous(breaks = 2019:2024) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )
plot(seven_region_secondary)
ggsave(here(plots_dir, "secondary_seven_region_trends.png"), seven_region_secondary, width = 10, height = 6, dpi = 300)

## PRIMARY ANALYSIS:##

# Set working directory
#setwd("") # to where primary care dataset is

# Load required libraries
library(dplyr)

#### Cleaning/ Organising dataset for plotting ########
# (Addressing: DATE-REGION-MG-CHEMICAL)

# Import primary care dataset (manually loaded as 'primary_lithium')
PrimaryCare_Lithium <- read.csv(gzfile(here("data", "primary_care", "primary_lithium.csv.gz"))) # Primary care Lithium dataset for 2015-2024

# Convert 'month' column to proper Date format for time series plotting
PrimaryCare_Lithium <- PrimaryCare_Lithium %>%
  mutate(month = as.Date(month))

## PRACTICE CODES TO PRIMARY CARE DATA - Load practice codes
Practice_codes <- read_excel(here("data", "primary_care", "practice_codes.xlsx"))  

# Merge with PrimaryCare_Lithium to attach regional team info
merged_data <- PrimaryCare_Lithium %>%
  left_join(Practice_codes, by = c("practice" = "code"))

# Filter for setting == 4 only
merged_data <- merged_data %>%
  filter(setting == 4)

## ADD STRENGTH (mg) INFORMATION - Load strength (numerator value) info

df_primarycare2 <- read.csv(here("data", "primary_care", "primary_care.csv"))

# Merge to bring in 'strnt_nmrtr_val' from bnf_code
PRIMARYCARE_dataset <- merge(
  merged_data,
  df_primarycare2[, c("bnf_code", "strnt_nmrtr_val")],
  by = "bnf_code",
  all.x = TRUE
)

## CLASSIFY CHEMICAL TYPE - (makes plotting easier) (Carbonate, Citrate, Other)
PRIMARYCARE_dataset <- PRIMARYCARE_dataset %>%
  mutate(
    chemical = case_when(
      bnf_name %in% c(
        "Camcolit 250 tablets",
        "Camcolit 400 modified-release tablets",
        "Liskonum 450mg modified-release tablets",
        "Lithium carbonate 200mg modified-release tablets",
        "Lithium carbonate 200mg/5ml oral suspension",
        "Lithium carbonate 250mg tablets",
        "Lithium carbonate 400mg modified-release tablets",
        "Lithium carbonate 450mg modified-release tablets",
        "Lithonate 400mg modified-release tablets",
        "Priadel 200mg modified-release tablets",
        "Priadel 400mg modified-release tablets"
      ) ~ "Lithium Carbonate",
      bnf_name %in% c(
        "Li-Liquid 1.018g/5ml oral solution",
        "Li-Liquid 509mg/5ml oral solution",
        "Lithium citrate 1.018g/5ml oral solution",
        "Lithium citrate 509mg/5ml oral solution",
        "Lithium citrate 520mg/5ml oral solution sugar free",
        "Priadel 520mg/5ml liquid",
        "Lithium citrate 10.8mmol/5ml oral solution sugar free"
      ) ~ "Lithium Citrate",
      TRUE ~ "Other"
    ),
    
    # Calculate total mg dispensed
    quantity_mg = quantity * strnt_nmrtr_val,
    
    # Convert total mg to mmol based on chemical type
    mmol = case_when(
      chemical == "Lithium Carbonate" ~ quantity_mg / 37.04,
      chemical == "Lithium Citrate" ~ quantity_mg / 69.98,
      TRUE ~ NA_real_
    ),
    
    # Calculate Daily Defined Dose (DDD)
    DDD = mmol / 24
  )


# Mapping regions
# Define region mapping
region_mapping <- c(
  "Y60" = "Midlands",
  "Y63" = "North East And Yorkshire",
  "Y59" = "South East",
  "Y61" = "East of England",
  "Y62" = "North West",
  "Y56" = "London",
  "Y58" = "South West"
)

# Add a 'Region' column using mapping
PRIMARYCARE_dataset$Region <- region_mapping[PRIMARYCARE_dataset$regional_team]

PRIMARYCARE_dataset <- PRIMARYCARE_dataset %>%
  filter(month >= as.Date("2015-01-01") & month <= as.Date("2024-12-31")) # so dataset only includes complete annual data, removing 2025


####### Prescribing trends over the years 2015-2024 in PRIMARY care ########
# creating a year column
PRIMARYCARE_dataset <- PRIMARYCARE_dataset %>%
  mutate(year = format(as.Date(month), "%Y"))

#View(PRIMARYCARE_dataset)

Primaryy_DDD_by_year <- PRIMARYCARE_dataset %>%
  filter(year != "2025") %>% # as 2025 does not include data for the whole year
  group_by(year) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup()

print(Primaryy_DDD_by_year)

# Line graph for prescribing trends over time 2015-2024
#Y-axis starting from  zero
primary_line <- ggplot(Primaryy_DDD_by_year, aes(x = as.integer(year), y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = "orange") +
  geom_point(size = 3, color = "blue") +
  labs(
    title = "Primary Care: Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year (2015–2024)",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  scale_y_continuous(
    limits = c(0, 14),    # force lower limit 0, upper limit automatic
    expand = c(0, 0),    
    labels = function(x) format(x, scientific = FALSE, big.mark = ",")
  ) +
  scale_x_continuous(breaks = 2015:2024) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

print(primary_line)
ggsave(here(plots_dir, "primary_line_trends.png"), primary_line, width = 8, height = 5, dpi = 300)

combined_plot <- primary_line / secondary_line #combining prescribing trends for both primary and secondary
print(combined_plot)
ggsave(here(plots_dir, "combined_primary_secondary_trends.png"), combined_plot, width = 8, height = 10, dpi = 300)

# Accompanying bar plot for prescribing trends over time
primary_bar <- ggplot(Primaryy_DDD_by_year, aes(x = as.factor(year), y = total_DDD / 1e6)) +
  geom_bar(stat = "identity", fill = "orange", width = 0.6) +
  geom_text(
    aes(label = format(round(total_DDD / 1e6, 1), nsmall = 1)),
    vjust = -0.5,
    size = 4.2,
    fontface = "bold"
  ) +
  labs(
    title = "Primary Care: Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year (2015–2024)",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  scale_y_continuous(
    labels = function(x) format(x, scientific = FALSE, big.mark = ","),
    expand = expansion(mult = c(0, 0.1))  # Adds space above bars for text
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )
plot(primary_bar)
ggsave(here(plots_dir, "primary_bar_trends.png"), primary_bar, width = 8, height = 5, dpi = 300)

############################Primary care Mapping plot, 2024 DDD / population ############################

# Primary care Mapping plot, focusing on all of 2024 DDD / population
lithium_df_primary <- PRIMARYCARE_dataset %>%
  group_by(Region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  mutate(Region = as.factor(Region)) %>%
  filter(!is.na(Region))

total_primary_DDD_by_region_2024 <- PRIMARYCARE_dataset %>%
  filter(year(month) == 2024) %>%
  group_by(Region) %>%
  summarise(total_DDD_2024 = sum(DDD, na.rm = TRUE))

primary_lithium_df <- lithium_df_primary %>%
  left_join(total_primary_DDD_by_region_2024, by = "Region")

#View(primary_lithium_df)

# Using most recent ONS population estimates- mid 2023 ONS population estimates xlsx file
# Now adding population data to this dataset, that I got from population_data, estimates from ONS
NorthEast_Yorkt_pop =  8220282
NorthWest_pop = 7515718
midlands_pop =  10951858
east_pop = 6401418
london_pop = 8869043
SouthEast_pop = 9387286
SouthWest_pop = 5766937


primary_lithium_df <- primary_lithium_df %>%
  mutate(population = case_when(
    Region == "North East And Yorkshire" ~ NorthEast_Yorkt_pop,
    Region == "North West" ~ NorthWest_pop,
    Region == "Midlands" ~ midlands_pop,
    Region == "East of England" ~ east_pop,
    Region == "London" ~ london_pop,
    Region == "South East" ~ SouthEast_pop,
    Region == "South West" ~ SouthWest_pop
  ))

#View(primary_lithium_df)

primary_lithium_df <- primary_lithium_df %>%
  mutate(`DDD/population` = total_DDD_2024 / population)

print(primary_lithium_df)

# Join NHS boundaries with primary care data (uses Region)
coverage_data_primary <- nhs_regions_sf %>%
  left_join(primary_lithium_df, by = "Region")

unique_values <- sort(unique(coverage_data_primary$`DDD/population`))
breaks <- c(0, unique_values, max(unique_values, na.rm = TRUE) + 10)
labels <- scales::label_number(breaks)

primary_coverage_plot <- coverage_data_primary %>%
  ggplot() +
  geom_sf(aes(fill = `DDD/population`), colour = "black", linewidth = 0.8) +
  geom_sf_text(aes(label = Region), colour = "white", size = 3) +
  scale_fill_gradientn(
    colors = c("#ffd13a", "#ff7c00", "#f20c51"),
    breaks = breaks,
    labels = labels,
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.2, 0.5),
    legend.text = element_text(hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(face = "bold")   # <-- Here is the bolding
  ) +
  guides(fill = guide_legend(title = "Lithium (DDD)/ population")) +
  coord_sf(datum = NA) +
  xlab("") +
  ylab("")


# Display the plot
plot(primary_coverage_plot)
ggsave(here(plots_dir, "primary_coverage_map.png"), primary_coverage_plot, width = 8, height = 6, dpi = 300)

# Accompanying barplot: 2024 DDD/pop
library(ggplot2)

primaryhist = ggplot(primary_lithium_df, aes(x = Region, y = `DDD/population`)) +
  geom_col(fill = "#1f77b4", color = "#6baed6") +
  geom_text(aes(label = sprintf("%.3f", `DDD/population`)), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  xlab("Region") +
  ylab("Lithium usage (Total DDD for 2024) / population") +
  labs(
    title = "Regional Lithium Use in Primary Care",
    subtitle = "Average DDDs per Person (2024) (prescription)"  # <- Correct subtitle syntax
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic"),  # Optional styling
    plot.margin = margin(10, 10, 10, 10)
  )

plot(primaryhist)
ggsave(here(plots_dir, "primary_hist_ddd_pop.png"), primaryhist, width = 8, height = 5, dpi = 300)

#### Primary regional Prescribing trends over time - using region, year and population #### 
library(dplyr)
library(lubridate)

population_df <- tibble(
  Region = c("North East And Yorkshire", "North West", "Midlands", "East of England", 
             "London", "South East", "South West"),
  population = c(8220282, 7515718, 10951858, 6401418, 8869043, 9387286, 5766937)
)

Primary_DDD_by_year_region <- PRIMARYCARE_dataset %>%
  mutate(month = as.Date(month)) %>%
  mutate(year = year(month)) %>%
  filter(year != 2025) %>%
  group_by(year, Region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(population_df, by = "Region") %>%
  mutate(DDD_population = total_DDD / population)

print(Primary_DDD_by_year_region, n = nrow(Primary_DDD_by_year_region))


#### FP10 analysis ####
# 2017-2024
Hospital_FP10_data <- read_excel(here("data", "secondary_care_fp10", "FP10_data.xlsx"))

colnames(Hospital_FP10_data)

# Convert the PERIOD column to Date format
library(lubridate)

Hospital_FP10_data <- Hospital_FP10_data %>%
  mutate(PERIOD = as.Date(paste0(PERIOD, "01"), format = "%Y%m%d"))

unique(Hospital_FP10_data$PERIOD) 

# Check rows where both columns are NA before merging
Hospital_FP10_data %>%
  filter(is.na(BNF_CODE)) %>%
  distinct(PERIOD) #all of 2024 and a column called NA

#MERGING same column names
Hospital_FP10_data <- Hospital_FP10_data %>%
  mutate(
    BNF_CODE            = coalesce(BNF_CODE, `BNF CODE`),
    BNF_NAME            = coalesce(BNF_NAME, `BNF NAME`),
    HOSPITAL_TRUST_CODE = coalesce(HOSPITAL_TRUST_CODE, `HOSPITAL TRUST CODE`),
    HOSPITAL_TRUST      = coalesce(HOSPITAL_TRUST, `HOSPITAL TRUST`),
    TOTAL_QUANTITY      = coalesce(TOTAL_QUANTITY, `TOTAL QUANTITY`),
    TOTAL_ITEMS         = coalesce(TOTAL_ITEMS, `TOTAL ITEMS`),
    TOTAL_ACTUAL_COST   = coalesce(TOTAL_ACTUAL_COST, `TOTAL ACTUAL COST`),
    TOTAL_NIC           = coalesce(TOTAL_NIC, `TOTAL NIC`)
  ) %>%
  select(-`BNF CODE`,
         -`BNF NAME`,
         -`HOSPITAL TRUST CODE`,
         -`HOSPITAL TRUST`,
         -`TOTAL QUANTITY`,
         -`TOTAL ITEMS`,
         -`TOTAL ACTUAL COST`,
         -`TOTAL NIC`)


# figuring out what PERIODs to remove (already in Date format)
periods_to_remove <- as.Date(paste0(c(
  "202401", "202402", "202403", "202404",
  "202405", "202406", "202407", "202408",
  "202409", "202410", "202411", "202412"
), "01"), format = "%Y%m%d")

# Remove rows with those periods or NA PERIOD
Hospital_FP10_data <- Hospital_FP10_data %>%
  filter(!(PERIOD %in% periods_to_remove), !is.na(PERIOD))

unique(Hospital_FP10_data$BNF_CODE) # ALL NAs gone, so now dataset includes only 2017-2023

# importing dataset that includes Hospital FP10s data for just 2024
Hospital_FP10_data2024 <- read_excel(here("data", "secondary_care_fp10", "FP10_2024.xlsx"))
colnames(Hospital_FP10_data2024) 
colnames(Hospital_FP10_data)

# Standardising column names in Hospital_FP10_data2024
Hospital_FP10_data2024 <- Hospital_FP10_data2024 %>%
  rename(
    BNF_CODE = `BNF CODE`,
    BNF_NAME = `BNF NAME`,
    HOSPITAL_TRUST_CODE = `HOSPITAL TRUST CODE`,
    HOSPITAL_TRUST = `HOSPITAL TRUST`,
    TOTAL_QUANTITY = `TOTAL QUANTITY`,
    TOTAL_ITEMS = `TOTAL ITEMS`,
    TOTAL_ACTUAL_COST = `TOTAL ACTUAL COST`,
    TOTAL_NIC = `TOTAL NIC`
  )

Hospital_FP10_data2024 <- Hospital_FP10_data2024 %>%
  mutate(PERIOD = as.Date(paste0(as.character(PERIOD), "01"), format = "%Y%m%d"))

Hospital_FP10_data2024 <- Hospital_FP10_data2024 %>%
  mutate(TOTAL_ACTUAL_COST = as.numeric(TOTAL_ACTUAL_COST))

Hospital_FP10_all <- bind_rows(Hospital_FP10_data, Hospital_FP10_data2024)

#View(Hospital_FP10_all)
unique(Hospital_FP10_all$PERIOD) # all months are accounted for from now from 2017-2024

unique(Hospital_FP10_all$BNF_CODE) # ALL BNF CODES ARE INCLUDED.

# NOW SHAPING UP DATASET TO INCLUDE DATA I NEED:
# chemical 
unique(Hospital_FP10_all$BNF_NAME)

New_Hospital_FP10_data <- Hospital_FP10_all %>%
  mutate(
    chemical = case_when(
      BNF_NAME %in% c(
        "Lithium carbonate 400mg modified-release tablets",
        "Lithium carbonate 200mg modified-release tablets",
        "Priadel 200mg modified-release tablets",
        "Priadel 400mg modified-release tablets",
        "Lithium carbonate 450mg modified-release tablets",
        "Priadel 520mg/5ml liquid",
        "Lithium carbonate 250mg tablets",
        "Camcolit 400 modified-release tablets",
        "Priadel_Tab 400mg",
        "Lithium Carb_Tab 400mg M/R",
        "Lithium Carb_Tab 200mg M/R",
        "Priadel_Tab 200mg",
        "Camcolit 400_Tab 400mg",
        "Lithium Carb_Tab 250mg",
        "Camcolit 250_Tab 250mg",
        "Lithium Carb_Tab 450mg M/R",
        "Lithium Carb_Liq Spec 200mg/5ml",
        "Lithium carbonate 200mg/5ml oral suspension",
        "Camcolit 250 tablets",
        "Liskonum 450mg modified-release tablets",
        "Liskonum_Tab 450mg M/R"
      ) ~ "Lithium Carbonate",
      
      BNF_NAME %in% c(
        "Li-Liquid 509mg/5ml oral solution",
        "Lithium citrate 509mg/5ml oral solution",
        "Lithium citrate 520mg/5ml oral solution sugar free",
        "Lithium Cit_Oral Soln 1.018g/5ml",
        "Lithium Cit_Oral Soln 520mg/5ml S/F",
        "Lithium Cit_Oral Soln 509mg/5ml",
        "Li-Liquid_Syr 10.8mmol/5ml",
        "Priadel_Liq 520mg/5ml S/F",
        "Li-Liquid_Syr 5.4mmol/5ml",
        "Lithium citrate 1.018g/5ml oral solution",
        "Li-Liquid 1.018g/5ml oral solution"
      ) ~ "Lithium Citrate",
      
      TRUE ~ "Other"
    )
  )

unique(New_Hospital_FP10_data$chemical)


# DDD
New_Hospital_FP10_data <- New_Hospital_FP10_data %>%
  mutate(
    # Step 1: Classify lithium salt by chemical column
    chemical = case_when(
      str_detect(chemical, regex("Lithium Carbonate", ignore_case = TRUE)) ~ "Lithium carbonate",
      str_detect(chemical, regex("Lithium Citrate", ignore_case = TRUE)) ~ "Lithium citrate",
      TRUE ~ "Other"
    ),
    
    # Step 2: Identify quantity_basis (liquid or tablet)
    quantity_basis = case_when(
      str_detect(BNF_NAME, regex("liq|oral soln|solution|syrup|ml", ignore_case = TRUE)) ~ "ml",
      str_detect(BNF_NAME, regex("tab|tablet", ignore_case = TRUE)) ~ "tablet",
      TRUE ~ NA_character_
    ),
    
    # Step 3: Extract strength numerator value (in mg) from BNF_NAME
    strength_numerator_value = case_when(
      str_detect(BNF_NAME, "mg/5ml") ~ as.numeric(str_extract(BNF_NAME, "(?i)(\\d+\\.?\\d*)(?=mg/5ml)")),
      str_detect(BNF_NAME, "mg") & !str_detect(BNF_NAME, "mg/5ml") ~ as.numeric(str_extract(BNF_NAME, "(?i)(\\d+\\.?\\d*)(?=mg)")),
      str_detect(BNF_NAME, "g/5ml") ~ as.numeric(str_extract(BNF_NAME, "(?i)(\\d+\\.?\\d*)(?=g/5ml)")) * 1000,
      TRUE ~ NA_real_
    ),
    
    # Step 4: Calculate total_mg only for Lithium carbonate tablets
    total_mg = case_when(
      chemical == "Lithium carbonate" & quantity_basis == "tablet" ~ TOTAL_QUANTITY * strength_numerator_value,
      TRUE ~ NA_real_
    ),
    
    # Step 5: Calculate mmol
    mmol = case_when(
      chemical == "Lithium carbonate" ~ total_mg / 37.04,
      
      # For Lithium citrate, match product codes from BNF_NAME 
      chemical == "Lithium citrate" & str_detect(BNF_NAME, "509") ~ (TOTAL_QUANTITY / 5) * 5.4,
      chemical == "Lithium citrate" & str_detect(BNF_NAME, "520") ~ (TOTAL_QUANTITY / 5) * 5.4,
      chemical == "Lithium citrate" & str_detect(BNF_NAME, "1.018") ~ (TOTAL_QUANTITY / 5) * 10.8,
      
      TRUE ~ NA_real_
    ),
    
    # Step 6: Calculate DDD
    DDD = mmol / 24
  )


# REGION MAPPING
org_mapping <- read_excel(here("data", "secondary_care", "org-mapping.xlsx"))

region_mapping <- c(
  "Y60" = "Midlands",
  "Y63" = "North East And Yorkshire",
  "Y59" = "South East",
  "Y61" = "East of England",
  "Y62" = "North West",
  "Y56" = "London",
  "Y58" = "South West",
  "RCE" = "North East And Yorkshire", 
  "RK7" = "North East And Yorkshire",  
  "RQ4" = "Midlands",                    
  "RRQ" = "London",                      
  "RNJ" = "London",                  
  "RMK" = "North West",                  
  "RNH" = "London",                     
  "G6V" = "London"                       
)


New_Hospital_FP10_data <- New_Hospital_FP10_data %>%
  mutate(trust_code_prefix = substr(HOSPITAL_TRUST_CODE, 1, 3)) %>%   # Extract prefix
  left_join(
    org_mapping %>% 
      select(ods_code, region) %>% 
      mutate(ods_prefix = substr(ods_code, 1, 3)),
    by = c("trust_code_prefix" = "ods_prefix")
  ) %>%
  filter(trust_code_prefix != "Y99") %>%                              # Remove "Y99"
  mutate(region = if_else(
    is.na(region) & trust_code_prefix %in% names(region_mapping),
    region_mapping[trust_code_prefix],
    region
  ))


unique(New_Hospital_FP10_data$region) # all region codes are now assoictaed to a region


####### Prescribing trends over the years 2017-2024 in Secondary care ########
# creating a year column
Hospital_FP10_data <- New_Hospital_FP10_data %>%
  mutate(PERIOD = format(as.Date(PERIOD), "%Y"))

HospitalFP10_DDD_by_year <- Hospital_FP10_data %>%
  group_by(PERIOD) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE)) %>%
  ungroup()

print(HospitalFP10_DDD_by_year)

max_value <- max(HospitalFP10_DDD_by_year$total_DDD) / 1e6  # in millions
hospitalFP10_line <- ggplot(HospitalFP10_DDD_by_year, 
                            aes(x = as.integer(PERIOD), y = total_DDD / 1e6)) +
  geom_line(linewidth = 1.2, color = "#2E8B57") +
  geom_point(size = 3, color = "#FFD700") +
  labs(
    title = "Secondary care (Hospital FP10): Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year (2017–2024)",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  scale_y_continuous(
    limits = c(0, max_value * 1.1),  # add 10% headroom
    expand = c(0, 0),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_x_continuous(breaks = 2017:2024) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Display the plot
plot(hospitalFP10_line)
ggsave(here(plots_dir, "hospital_fp10_line_trends.png"), hospitalFP10_line, width = 8, height = 5, dpi = 300)

# prescribing trends over time 
#COMBINED PLOT 
# Combine all years and ensure numeric
all_years <- c(
  as.numeric(Primaryy_DDD_by_year$year),
  as.numeric(Secondary_DDD_by_year$year),
  as.numeric(HospitalFP10_DDD_by_year$PERIOD)
)

combined_line_plot <- ggplot() +
  # Primary
  geom_line(data = Primaryy_DDD_by_year, 
            aes(x = as.integer(year), y = total_DDD / 1e6), 
            color = "orange", linewidth = 1.2) +
  geom_point(data = Primaryy_DDD_by_year, 
             aes(x = as.integer(year), y = total_DDD / 1e6), 
             color = "blue", size = 3) +
  
  # Secondary
  geom_line(data = Secondary_DDD_by_year, 
            aes(x = as.integer(year), y = total_DDD / 1e6), 
            color = "#00BFC4", linewidth = 1.2) +
  geom_point(data = Secondary_DDD_by_year, 
             aes(x = as.integer(year), y = total_DDD / 1e6), 
             color = "#F8766D", size = 3) +
  
  # Hospital FP10
  geom_line(data = HospitalFP10_DDD_by_year, 
            aes(x = as.integer(PERIOD), y = total_DDD / 1e6), 
            color = "#2E8B57", linewidth = 1.2) +
  geom_point(data = HospitalFP10_DDD_by_year, 
             aes(x = as.integer(PERIOD), y = total_DDD / 1e6), 
             color = "#FFD700", size = 3) +
  
  labs(
    title = "Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  
  # Y-axis: in 1 million increments
  scale_y_continuous(
    limits = c(0, ceiling(max(
      max(Primaryy_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(Secondary_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(HospitalFP10_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      1.2
    ) * 1.1)),
    breaks = seq(0, ceiling(max(
      max(Primaryy_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(Secondary_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(HospitalFP10_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      1.2
    ) * 1.1), by = 1),
    expand = c(0, 0),
    labels = scales::label_number(accuracy = 1)
  ) +
  
  # X-axis
  scale_x_continuous(
    breaks = seq(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE)),
    limits = c(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE))
  ) +
  
  # Theme: grid ON, axis lines ON
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey80"),  # Keep major grid
    panel.grid.minor = element_blank(),                 # Hide minor grid
    axis.line = element_line(color = "black"),          # Show X and Y axes
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )

# Show plot
print(combined_line_plot)
ggsave(here(plots_dir, "combined_line_all_sources.png"), combined_line_plot, width = 10, height = 6, dpi = 300)

# WITH FIGURE LEGEND
combined_line_plot <- ggplot() +
  # Primary
  geom_line(data = Primaryy_DDD_by_year, 
            aes(x = as.integer(year), y = total_DDD / 1e6, color = "Primary Care"), 
            linewidth = 1.2) +
  geom_point(data = Primaryy_DDD_by_year, 
             aes(x = as.integer(year), y = total_DDD / 1e6), 
             color = "blue", size = 3) +  # point color NOT in legend
  
  # Secondary
  geom_line(data = Secondary_DDD_by_year, 
            aes(x = as.integer(year), y = total_DDD / 1e6, color = "Secondary Care"), 
            linewidth = 1.2) +
  geom_point(data = Secondary_DDD_by_year, 
             aes(x = as.integer(year), y = total_DDD / 1e6), 
             color = "#F8766D", size = 3) +  # point color NOT in legend
  
  # Hospital FP10
  geom_line(data = HospitalFP10_DDD_by_year, 
            aes(x = as.integer(PERIOD), y = total_DDD / 1e6, color = "Hospital FP10 Care"), 
            linewidth = 1.2) +
  geom_point(data = HospitalFP10_DDD_by_year, 
             aes(x = as.integer(PERIOD), y = total_DDD / 1e6), 
             color = "#FFD700", size = 3) +  # point color NOT in legend
  
  scale_color_manual(
    name = "Care Type",
    values = c(
      "Primary Care" = "orange",
      "Secondary Care" = "#00BFC4",
      "Hospital FP10 Care" = "#2E8B57"
    )
  ) +
  
  labs(
    title = "Lithium Prescribing Trends Over Time",
    subtitle = "Total Daily Defined Doses (DDD) issued per year",
    x = "Year",
    y = "Total DDD (millions)"
  ) +
  
  scale_y_continuous(
    limits = c(0, ceiling(max(
      max(Primaryy_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(Secondary_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(HospitalFP10_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      1.2
    ) * 1.1)),
    breaks = seq(0, ceiling(max(
      max(Primaryy_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(Secondary_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      max(HospitalFP10_DDD_by_year$total_DDD / 1e6, na.rm = TRUE),
      1.2
    ) * 1.1), by = 1),
    expand = c(0, 0),
    labels = scales::label_number(accuracy = 1)
  ) +
  
  scale_x_continuous(
    breaks = seq(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE)),
    limits = c(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.x = element_text(face = "plain"),
    axis.title.y = element_text(face = "plain"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    legend.background = element_rect(color = "black", linewidth = 0.5),
    legend.key = element_rect(fill = "white", color = NA)
  )

print(combined_line_plot)
ggsave(here(plots_dir, "combined_line_all_sources_legend.png"), combined_line_plot, width = 10, height = 6, dpi = 300)

#### Secondary regional Prescribing trends over time - using region, year and population ####  - for later plotting
HospitalFP10_DDD_by_year_region <- Hospital_FP10_data %>%
  mutate(
    year = as.integer(PERIOD)  # Convert character year string to integer
  ) %>%
  group_by(year, region) %>%
  summarise(total_DDD = sum(DDD, na.rm = TRUE), .groups = 'drop')

population_df <- tibble(
  region = c("North East And Yorkshire", "North West", "Midlands", "East Of England", 
             "London", "South East", "South West"),
  population = c(8220282, 7515718, 10951858, 6401418, 8869043, 9387286, 5766937)
)

HospitalFP10_DDD_by_year_region <- HospitalFP10_DDD_by_year_region %>%
  left_join(population_df, by = "region")

HospitalFP10_DDD_by_year_region <- HospitalFP10_DDD_by_year_region %>%
  mutate(DDD_population = total_DDD / population) 

#View(HospitalFP10_DDD_by_year_region)

########################################################

# Hospital FP10 data care Mapping plot, focusing on all of 2024 DDD / population
# getting the sum of DDD per region  - needs to be edited this code for 2024 DDD / population (find new population estimates)
# New_Hospital_FP10_data - RUN TO GET THIS DATASET CLEAN FIRST
library(lubridate)

Hospital_FP10_total_DDD_by_region_2024 <- New_Hospital_FP10_data %>%
  mutate(PERIOD_date = as.Date(PERIOD)) %>%           # convert character to Date
  filter(year(PERIOD_date) == 2024) %>%               # filter for year 2024
  group_by(region) %>%
  summarise(total_DDD_2024 = sum(DDD, na.rm = TRUE)) %>%
  mutate(
    name = case_when(
      region == "East Of England" ~ "UKH",
      region == "North West" ~ "UKD",
      region == "North East And Yorkshire" ~ "UKE",
      region == "London" ~ "UKF",
      region == "Midlands" ~ "UKG",
      region == "South East" ~ "UKJ",
      region == "South West" ~ "UKK",
      TRUE ~ NA_character_
    ),
    region = as.factor(region)
  ) %>%
  filter(!is.na(region))


#View(Hospital_FP10_total_DDD_by_region_2024)

# Using most recent ONS population estimates- mid 2023 ONS population estimates xlsx file
# Now adding population data to this dataset, that I got from population_data, estimates from ONS

NorthEast_Yorkt_pop =  8220282
NorthWest_pop = 7515718
midlands_pop =  10951858
east_pop = 6401418
london_pop = 8869043
SouthEast_pop = 9387286
SouthWest_pop = 5766937


Hospital_FP10_total_DDD_by_region_2024 <- Hospital_FP10_total_DDD_by_region_2024 %>%
  mutate(population = case_when(
    region == "North East And Yorkshire" ~ NorthEast_Yorkt_pop,
    region == "North West" ~ NorthWest_pop,
    region == "Midlands" ~ midlands_pop,
    region == "East Of England" ~ east_pop,
    region == "London" ~ london_pop,
    region == "South East" ~ SouthEast_pop,
    region == "South West" ~ SouthWest_pop
  ))

#View(Hospital_FP10_total_DDD_by_region_2024)

Hospital_FP10_total_DDD_by_region_2024 <- Hospital_FP10_total_DDD_by_region_2024 %>%
  mutate(`DDD/population` = total_DDD_2024 / population)

print(Hospital_FP10_total_DDD_by_region_2024)

# Join NHS boundaries with Hospital FP10 data (uses region)
coverage_data_fp10 <- nhs_regions_sf %>%
  left_join(Hospital_FP10_total_DDD_by_region_2024, by = "region")

unique_values <- sort(unique(coverage_data_fp10$`DDD/population`))
breaks <- c(0, unique_values, max(unique_values, na.rm = TRUE) + 10)
labels <- scales::label_number(breaks)

FP10_coverage_plot <- coverage_data_fp10 %>%
  ggplot() +
  geom_sf(aes(fill = `DDD/population`), colour = "black", linewidth = 0.8) +
  geom_sf_text(aes(label = region), colour = "white", size = 3) +
  scale_fill_gradientn(
    colors = c("#ffd13a", "#ff7c00", "#f20c51"),
    breaks = breaks,
    labels = labels,
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.2, 0.5),
    legend.text = element_text(hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(face = "bold", size = 16)   # Bold and bigger title
  ) +
  guides(fill = guide_legend(title = "Lithium (DDD)/ population")) +
  coord_sf(datum = NA) +
  labs(
    title = "Hospital FP10 data",
    subtitle = "Total Daily Defined Dose of Lithium in 2024 regionally, per population estimates"
  ) +
  xlab("") +
  ylab("")


# Display the plot
plot(FP10_coverage_plot)
ggsave(here(plots_dir, "fp10_coverage_map.png"), FP10_coverage_plot, width = 8, height = 6, dpi = 300)

# Accompanying barplot: 2024 DDD/pop
# Calculate max value of DDD/population, ignoring NAs
max_y <- max(Hospital_FP10_total_DDD_by_region_2024$`DDD/population`, na.rm = TRUE)

# Adding a 10% buffer to the top of the y-axis for neatness
buffer <- max_y * 0.1

FP10hist = ggplot(Hospital_FP10_total_DDD_by_region_2024, aes(x = region, y = `DDD/population`)) +
  geom_col(fill = "#FFD700", color = "#FFD700") +
  geom_text(aes(label = round(`DDD/population`, 3)), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  xlab("Region") +
  ylab("Lithium usage (Total DDD for 2024) / population") +
  labs(
    title = "Regional Lithium Use in Secondary Care, FP10 hopsital data",
    subtitle = "Average DDDs per Person (2024)"
  ) +
  scale_y_continuous(
    limits = c(0, max_y + buffer),
    breaks = scales::pretty_breaks(n = 5),  # auto-select nice breaks
    labels = scales::number_format(accuracy = 0.001)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic"),
    plot.margin = margin(10, 10, 10, 10)
  )

plot(FP10hist)
ggsave(here(plots_dir, "fp10_hist_ddd_pop.png"), FP10hist, width = 8, height = 5, dpi = 300)

#### STACKED BAR PLOT FOR 2024 ####


#stacked bar plot for primary and all of secondary:
# Prepare primary data
primary_lithium_df <- primary_lithium_df %>%
  mutate(
    Region = ifelse(tolower(Region) == "east of england", "East of England", Region),
    Region = as.character(Region),
    Source = "Primary"
  )

# Prepare secondary data
secondary_lithium_df <- secondary_lithium_df %>%
  mutate(
    Region = ifelse(tolower(region) == "east of england", "East of England", region),
    Region = as.character(Region),
    Source = "Secondary"
  )

# Prepare hospital data
Hospital_FP10_total_DDD_by_region_2024 <- Hospital_FP10_total_DDD_by_region_2024 %>%
  mutate(
    Region = ifelse(tolower(region) == "east of england", "East of England", as.character(region)),
    Source = "Hospital FP10"
  ) %>%
  select(-region)


# Combine all datasets
combined_df_all <- bind_rows(primary_lithium_df, secondary_lithium_df, Hospital_FP10_total_DDD_by_region_2024)

# Make sure Source is a factor with the right order
combined_df_all <- combined_df_all %>%
  mutate(Source = factor(Source, levels = c("Primary", "Secondary", "Hospital FP10")))

# Plot: stacked bar plot
stacked_bar_plot <- ggplot(combined_df_all, aes(x = Region, y = `DDD/population`, fill = Source)) +
  geom_col(color = "black") +  # stacked by default
  scale_fill_manual(
    values = c("Primary" = "orange", "Secondary" = "#00BFC4", "Hospital FP10" = "#2E8B57"
    )
  ) +
  
  theme_minimal() +
  labs(
    title = "Regional Lithium Use by Care Level",
    subtitle = "Primary, Secondary, Hospital (FP10) in 2024",
    x = "Region",
    y = "Lithium usage (DDD/population)",
    fill = "Care Level"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 12, face = "bold")
  )
plot(stacked_bar_plot)
ggsave(here(plots_dir, "stacked_bar_regional_by_care.png"), stacked_bar_plot, width = 10, height = 6, dpi = 300)

# national line plots: 
colnames(Primary_DDD_by_year_region)
colnames(Secondary_DDD_by_year_region)
colnames(HospitalFP10_DDD_by_year_region)

library(dplyr)
library(ggplot2)

# Clean and standardise each dataset
Primary_clean <- Primary_DDD_by_year_region %>%
  mutate(
    year = as.integer(year),
    region = Region
  ) %>%
  select(year, region, total_DDD, population, DDD_population)

Secondary_clean <- Secondary_DDD_by_year_region %>%
  mutate(year = as.integer(year)) %>%
  select(year, region, total_DDD, population, DDD_population)

Hospital_clean <- HospitalFP10_DDD_by_year_region %>%
  mutate(year = as.integer(year)) %>%
  select(year, region, total_DDD, population, DDD_population)

# Combine all datasets
combined_data <- bind_rows(Primary_clean, Secondary_clean, Hospital_clean)

# Filter for year >= 2019
filtered_data <- combined_data %>%
  filter(year >= 2019)

# Group by year and sum total_DDD
summed_data <- filtered_data %>%
  group_by(year) %>%
  summarise(total_DDD_sum = sum(total_DDD, na.rm = TRUE))

library(ggplot2)

# Compute max in millions for y-axis scaling
max_total_millions <- max(summed_data$total_DDD_sum) / 1e6

#View(summed_data)

# Create the styled plot
national_ddd_plot <- ggplot(summed_data, aes(x = as.integer(year), y = total_DDD_sum / 1e6)) +
  geom_line(color = "#1f78b4", linewidth = 1.2) +
  geom_point(color = "#e31a1c", size = 3) +
  labs(
    title = "National DDD Trends from All Sources",
    subtitle = "Summed total DDD (in millions) across Primary, Secondary, and Hospital FP10 (2019–2024)",
    x = "Year",
    y = "Total DDD (Millions)"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),     # 10% padding above
    limits = c(0, max_total_millions * 1.1),  # y starts at 0, ends 10% above max
    labels = scales::label_number(accuracy = 0.1)  # or adjust accuracy as needed
  ) +
  scale_x_continuous(breaks = 2019:2025) +    # adjust range as needed
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )

plot(national_ddd_plot) #- figure 1
ggsave(here(plots_dir, "national_ddd_trends.png"), national_ddd_plot, width = 8, height = 5, dpi = 300)

print(summed_data)

##### NATIONAL PLOT BY REGION - FIGURE 3
library(dplyr)
library(ggplot2)


# Combine and clean the data as before
standardise_region <- function(region) {
  region <- tolower(region)  # Make everything lowercase
  region <- trimws(region)   # Remove leading/trailing spaces
  region <- gsub(" of ", " Of ", region)  # Capitalise 'Of' where appropriate
  region <- tools::toTitleCase(region)    # Convert to title case
  return(region)
}

Primary_clean <- Primary_DDD_by_year_region %>%
  mutate(
    year = as.integer(year),
    region = standardise_region(Region)  
  ) %>%
  select(year, region, total_DDD, population, DDD_population) 

Secondary_clean <- Secondary_DDD_by_year_region %>%
  mutate(
    year = as.integer(year),
    region = standardise_region(region)
  ) %>%
  select(year, region, total_DDD, population, DDD_population)

Hospital_clean <- HospitalFP10_DDD_by_year_region %>%
  mutate(
    year = as.integer(year),
    region = standardise_region(region)
  ) %>%
  select(year, region, total_DDD, population, DDD_population)


# Combine datasets
combined_data <- bind_rows(Primary_clean, Secondary_clean, Hospital_clean)
colnames(combined_data)
# Filter for years >= 2019
filtered_data <- combined_data %>%
  filter(year >= 2019)

# Group by both year and region, and sum total_DDD
summed_by_region <- filtered_data %>%
  group_by(year, region) %>%
  summarise(total_DDD_pop_sum = sum(DDD_population, na.rm = TRUE), .groups = "drop")


# Combine and clean the data as before
regional_trends_plot <- ggplot(summed_by_region, aes(x = year, y = total_DDD_pop_sum , color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "DDD Trends by Region (All Data Sources Combined)",
    subtitle = "Summed total DDD per year by region across Primary, Secondary, and Hospital FP10 (2019–Present)",
    x = "Year",
    y = "Total DDD (Millions)/Population",
    color = "Region"
  ) +
  scale_y_continuous(
    limits = c(0, 0.3),
    breaks = c(0.0, 0.05, 0.1, 0.15, 0.2),
    labels = function(x) format(x, nsmall = 1)
  ) +
  scale_x_continuous(breaks = 2019:2025) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )
plot(regional_trends_plot)
ggsave(here(plots_dir, "regional_ddd_trends.png"), regional_trends_plot, width = 10, height = 6, dpi = 300)

#print(summed_by_region$total_DDD_pop_sum)
#print(summed_by_region, n = 42)
#View(coverage_data)
#View(summed_by_region)
#View(Hospital_FP10_data)
#View(HospitalFP10_DDD_by_year_region)

#### SAVE ALL OUTPUTS ####
write.csv(Secondary_DDD_by_year, here(data_dir, "secondary_DDD_by_year.csv"), row.names = FALSE)
write.csv(secondary_lithium_df, here(data_dir, "secondary_lithium_by_region.csv"), row.names = FALSE)
write.csv(Secondary_DDD_by_year_region, here(data_dir, "secondary_DDD_by_year_region.csv"), row.names = FALSE)
write.csv(Primaryy_DDD_by_year, here(data_dir, "primary_DDD_by_year.csv"), row.names = FALSE)
write.csv(primary_lithium_df, here(data_dir, "primary_lithium_by_region.csv"), row.names = FALSE)
write.csv(Primary_DDD_by_year_region, here(data_dir, "primary_DDD_by_year_region.csv"), row.names = FALSE)
write.csv(HospitalFP10_DDD_by_year, here(data_dir, "hospital_fp10_DDD_by_year.csv"), row.names = FALSE)
write.csv(Hospital_FP10_total_DDD_by_region_2024, here(data_dir, "hospital_fp10_DDD_by_region_2024.csv"), row.names = FALSE)
write.csv(summed_data, here(data_dir, "national_DDD_summed.csv"), row.names = FALSE)
write.csv(summed_by_region, here(data_dir, "regional_DDD_trends.csv"), row.names = FALSE)
write.csv(combined_df_all, here(data_dir, "combined_regional_by_care_2024.csv"), row.names = FALSE)
message("All outputs saved to ", output_dir)
