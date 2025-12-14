# libraries and packages

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(stringr)
library(purrr)
library(tidyr)
library(reshape2)
library(scales)

#install.packages("treemapify")
library(treemapify)

#install.packages("wordcloud")
library(wordcloud)

#install.packages("RColorBrewer")
library(RColorBrewer)

#install.packages("packcircles")

library(packcircles)

#install.packages("ggforce")
library(ggforce)

library(zoo)


# Set this directory to your project/Rscript location.
setwd("C:/Users/Ed/OneDrive/Documents/FSU/2025Fall/CSC-8008-DataExplorationAnalytics_Visualization/project")

#set firstYear value to be the beginning year of data to pull in
firstYear <- 2010

#set lastYear value to be the last year desired 
lastYear <- 2025

#initial dataframe/csv file(s) is/are created with 

#CombineAllTheDataFiles.R will create the appropriate csv files.

vaers_data <- read.csv(paste0(firstYear, "-", lastYear, "_vaers_data.csv"), 
                      stringsAsFactors = FALSE)

vaers_symptoms <- read.csv(paste0(firstYear, "-", 
                          lastYear, "_vaers_symptoms.csv"),  
                          stringsAsFactors = FALSE)

vaers_vax <- read.csv(paste0(firstYear, "-", lastYear, "_vaers_vax.csv"),  
                      stringsAsFactors = FALSE)

##read in the cleaned data from CombineAllTheDataFiles.R

cleaned_data <- read.csv(paste0(firstYear, "-", lastYear, 
                        "_Cleaned_Vaers_data.csv"),  stringsAsFactors = FALSE)


#default chart display settings (if possible)
#consistent colors
event_colors <- c(
  "Death"             = "#8E1B1B",
  "Hospitalization"   = "#FF7F7F",
  "LifeThreatening"   = "#CC5500",
  "ER_Visit"          = "#D0B220",
  "Disabled"          = "#2C6E8E"
)

# Create a Dataframe that has the calculated fields
calculated_data <- data.frame(cleaned_data)

calculated_data$AGE_GROUP <- cut(
  calculated_data$`AGE_YRS`,
  breaks = c(-Inf, 5, 12, 19, 24, 34, 44, 54, 64, Inf), 
  labels = c("0–5", "6–12", "13–19", "20–24", "25–34", "35–44", "45–54",
             "55–64", "65+"),
  right = TRUE 
)

# create Adverse_Event to know if any fo the adverse events happened
calculated_data$ADVERSE_EVENT <- ifelse(
  calculated_data$DIED == "Y" |
    calculated_data$HOSPITAL == "Y" |
    calculated_data$L_THREAT == "Y" |
    calculated_data$ER_VISIT == "Y" |
    calculated_data$DISABLE == "Y",
  1,
  0
)

#create a YEAR_MONTH_DATE field for graphing
calculated_data <- calculated_data %>%
  mutate(
    YEAR_MONTH_DATE = as.Date(
      format(as.Date(RECVDATE, format = "%m/%d/%Y"), "%Y-%m-01")
    )
  )

#change + to , to show multiple vaccines consistently.
calculated_data$VAX_NAMES <- gsub("\\+", ", ", calculated_data$VAX_NAMES)

#add month value from RECVDATE
calculated_data$MONTH <- as.integer(format(
  as.Date(calculated_data$RECVDATE, format = "%m/%d/%Y"),
  "%m")
)

#add Year value from RECVDATE
calculated_data$YEAR <- as.integer(format(
  as.Date(calculated_data$RECVDATE, format = "%m/%d/%Y"),
  "%Y")
)

#create new dataframe with a single VAX_NAME per row
#necessary for charts/graphs
# **** Counts won't be perfect because for mutliple vax records
# the number will skew if a combination has one bad vax and one
# more safer vax.

single_vax <- calculated_data %>%
  separate_rows(VAX_NAMES, sep = ",") %>%
  mutate(
    VAX_NAMES        = trimws(VAX_NAMES),                        
    GENERIC_VAX_NAME = trimws(gsub("\\(.*$", "", VAX_NAMES))      
  )

##################################################################
### Vaccines with the most Adverse impact
###
##################################################################

#gets the total count of vaccines administered, the # that lead to
#adverse events, and the percentage.
vax_most_adverse_events <- single_vax %>%
  group_by(GENERIC_VAX_NAME) %>%
  summarise(
    TOTAL_ADMINISTERED = n(),
    ADVERSE_EVENT_COUNT = sum(ADVERSE_EVENT ==1, na.rm = TRUE),
    PERCENT_ADVERSE = round((ADVERSE_EVENT_COUNT /TOTAL_ADMINISTERED * 100), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(ADVERSE_EVENT_COUNT))

vax_most_adverse_percentage <- vax_most_adverse_events %>%
  arrange(desc(PERCENT_ADVERSE))

vax_most_administered_count <-vax_most_adverse_events %>%
  arrange(desc(TOTAL_ADMINISTERED))


##################################################################
###Creating Tends by VAX_NAME for top 5 vaccines for adverse events
###by percentage
##################################################################

#Get the top 5 vaccines with adverse events reported

top_vax_adverse_events <- vax_most_adverse_events %>%
  slice_head(n =6) %>%
  pull(GENERIC_VAX_NAME)

# remove COVID19 to see how the other vaccines compare
top_vax_adverse_events_No_Covid <- 
  top_vax_adverse_events[top_vax_adverse_events != "COVID19"]

single_vax_nocovid <- single_vax %>%
  filter(GENERIC_VAX_NAME != "COVID19")

#get the total Percentage of Adverse_events for Non-covid top5 vaccines
kpi_pct_adverse_NOCOVID <- single_vax_nocovid %>%
  summarise(
    total_reports = n(),
    adverse_event_count = sum(ADVERSE_EVENT == 1, na.rm = TRUE),
    pct_adverse = (adverse_event_count / total_reports) * 100
  )

# create the dataframe
generic_single_vax_trends_NOCOVID <- single_vax_nocovid %>%
  filter(YEAR > firstYear) %>%                                   
  group_by(GENERIC_VAX_NAME, YEAR_MONTH_DATE) %>%
  summarise(
    total_reports = n(),                                   
    adverse_event_count = sum(ADVERSE_EVENT == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_adverse = if_else(                                
      total_reports > 0,
      (adverse_event_count / total_reports) * 100,
      NA_real_
    )
  ) %>%
  filter(GENERIC_VAX_NAME %in% top_vax_adverse_events_No_Covid)


#show the % and the total numbers with No Covid.
round(kpi_pct_adverse_NOCOVID$pct_adverse, 2)

kpi_adverse_event_total_NOCOVID = 
  sum(single_vax_nocovid$ADVERSE_EVENT ==1, na.rm = TRUE)

kpi_total_reports_NOCOVID = nrow(single_vax_nocovid)

# adverse_even count trend line, NO COVID
ggplot(generic_single_vax_trends_NOCOVID, 
       aes(x = YEAR_MONTH_DATE,
           y = adverse_event_count,
           color = GENERIC_VAX_NAME)) +
  geom_line(linewidth = 2) +
  labs(
    title = "Adverse Events by Year for Top Vaccines (Excluding Covid19)",
    x = "Year",
    y = "Adverse Events",
    color = "Vaccine"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

##################################################
## End top 5 Vaccines (No Covid) KPI/sparkline
##################################################

#################################################
###Creating Tends for COVID19 vaccines for adverse events
###by percentage
#################################################


single_vax_covid_only <- single_vax %>%
  filter(GENERIC_VAX_NAME == "COVID19")

###############STOP HERE BROKEN?#########################
# Create the trend line without COVID19
generic_covid_only_trends <- single_vax_covid_only %>%
  group_by(YEAR_MONTH_DATE) %>%
  summarise(
    total_reports = n(),
    adverse_event_count = sum(ADVERSE_EVENT == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_adverse = if_else(
      total_reports > 0,
      adverse_event_count / total_reports * 100,
      NA_real_
    )
  )


ggplot(generic_covid_only_trends, 
       aes(x = YEAR_MONTH_DATE,
           y = adverse_event_count)) +
  geom_line(linewidth = 2) +
  labs(
    title = "Adverse Events by Year for Covid Vaccines",
    x = "Year",
    y = "Adverse Events"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


#show the % and the total numbers forCovid.

kpi_pct_adverse_COVID <- single_vax_covid_only %>%
  summarise(
    total_reports = n(),
    adverse_event_count = sum(ADVERSE_EVENT == 1, na.rm = TRUE),
    pct_adverse = (adverse_event_count / total_reports) * 100
  )

round(kpi_pct_adverse_COVID$pct_adverse, 2)
kpi_adverse_event_total_COVID = sum(single_vax_covid_only$ADVERSE_EVENT ==1, 
                                    na.rm = TRUE)

kpi_total_reports_COVID = nrow(single_vax_covid_only)


################################################
## END Covid only KPI and Trend for Management
################################################

### SHOW THE MOST DANGEROUS MANUFACTURERS INCLUDING COVID.


#get adverse events by manufacturer, non-covid
adverse_by_manu <- single_vax_nocovid %>%
  filter(!grepl("UNKNOWN", VAX_MANU, ignore.case = TRUE)) %>%
  group_by(VAX_MANU) %>%
  summarise(
    Death           = sum(DIED == "Y", na.rm = TRUE),
    Hospitalization = sum(HOSPITAL == "Y", na.rm = TRUE),
    LifeThreatening = sum(L_THREAT == "Y", na.rm = TRUE),
    ER_VISIT        = sum(ER_VISIT == "Y", na.rm = TRUE),
    Disabled        = sum(DISABLE == "Y", na.rm = TRUE),
    .groups = "drop"
  )


# compute top 5
adverse_by_manu_top5 <- adverse_by_manu %>%
  mutate(
    total_events = Death + Hospitalization + LifeThreatening +
      ER_VISIT + Disabled
  ) %>%
  arrange(desc(total_events)) %>%
  slice_head(n = 5)

adverse_by_manu_long <- adverse_by_manu_top5 %>%
  pivot_longer(
    cols = c(Death, Hospitalization, LifeThreatening, ER_VISIT, Disabled),
    names_to = "Event_Type",
    values_to = "Count"
  )

#create the treemap
ggplot(
  adverse_by_manu_long,
  aes(
    area = Count,
    fill = Event_Type,
    subgroup = VAX_MANU
  )
) +
  geom_treemap() +
  
  # Subgroup border (around each manufacturer)
  geom_treemap_subgroup_border(color = "white", size = 2) +
  
  # Manufacturer name inside each big block
  geom_treemap_subgroup_text(
    aes(label = VAX_MANU),
    place = "bottom",
    grow = TRUE,
    alpha = 0.5,
    colour = "black",
    fontface = "bold",
    min.size = 6
  ) +
  scale_fill_manual(
    values = c(
      "Death"             = "#8E1B1B",
      "Hospitalization"   = "#FF7F7F",
      "LifeThreatening"   = "#CC5500",
      "ER_VISIT"          = "#D0B220",
      "Disabled"          = "#2C6E8E"
    )
  ) +

  labs(
    title = "Top 5 Non-Covid Manufacturer Adverse Events",
    fill  = "Event Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(2, "lines")
  )


####################################
## Covid manufacturers treemap.
##

adverse_by_manu_covid <- single_vax_covid_only %>%
  filter(!grepl("UNKNOWN", VAX_MANU, ignore.case = TRUE)) %>%
  group_by(VAX_MANU) %>%
  summarise(
    Death           = sum(DIED == "Y", na.rm = TRUE),
    Hospitalization = sum(HOSPITAL == "Y", na.rm = TRUE),
    LifeThreatening = sum(L_THREAT == "Y", na.rm = TRUE),
    ER_VISIT        = sum(ER_VISIT == "Y", na.rm = TRUE),
    Disabled        = sum(DISABLE == "Y", na.rm = TRUE),
    .groups = "drop"
  )


# compute top 5
adverse_by_manu_top5_covid <- adverse_by_manu_covid %>%
  mutate(
    total_events = Death + Hospitalization + LifeThreatening +
      ER_VISIT + Disabled
  ) %>%
  arrange(desc(total_events)) %>%
  slice_head(n = 5)

adverse_by_manu_long_covid <- adverse_by_manu_top5_covid %>%
  pivot_longer(
    cols = c(Death, Hospitalization, LifeThreatening, ER_VISIT, Disabled),
    names_to = "Event_Type",
    values_to = "Count"
  )

#create the treemap
###NOTE, the 4th and 5th manufactures are very tiny.  TOOLTIP THIS
## OR Hover in Tableau
ggplot(
  adverse_by_manu_long_covid,
  aes(
    area = Count,
    fill = Event_Type,
    subgroup = VAX_MANU,
    label = VAX_MANU
  )
) +
  geom_treemap() +
  
  # Subgroup border (around each manufacturer)
  geom_treemap_subgroup_border(color = "white", size = 2) +
  
  # Manufacturer name inside each big block
  geom_treemap_subgroup_text(
    place = "bottom",
    grow = TRUE,
    alpha = 0.5,
    colour = "black",
    fontface = "bold",
    min.size = 6
  ) +
  scale_fill_manual(
    values = c(
      Death           = "#8E1B1B",   
      Hospitalization = "#FF7F7F",   
      LifeThreatening = "#CC5500",   
      ER_VISIT        = "#D0B220",   
      Disabled        = "#2C6E8E"    
    )
  ) +
  
  labs(
    title = "Top 5 Covid Manufacturer Adverse Events",
    fill  = "Event Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(2, "lines")
  )

####COULD add a 3rd treemap with all vaccine manufacturers together
#### maybe. #########


#####################################################################
######## Risk Matrices
#############################################

######################
#####Covid risk table
#####################

covid_risk_table <- single_vax_covid_only %>%
  summarise(
    Death           = sum(DIED == "Y", na.rm = TRUE),
    LifeThreatening = sum(L_THREAT == "Y", na.rm = TRUE),
    Hospitalization = sum(HOSPITAL == "Y", na.rm = TRUE),
    Disabled        = sum(DISABLE == "Y", na.rm = TRUE),
    ER              = sum(ER_VISIT == "Y", na.rm = TRUE)
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Event",
    values_to = "Count"
  ) %>%
  mutate(
    Severity = case_when(
      Event %in% c("Death", "LifeThreatening") ~ "High",
      Event %in% c("Hospitalization", "Disabled") ~ "Medium",
      Event == "ER" ~ "Low"
    ),
    Frequency = case_when(
      Count > 50000 ~ "High",
      Count > 10000 ~ "Medium",
      TRUE ~ "Low"
    )
  )

#Add frequency category to each event.
covid_risk_table <- covid_risk_table %>%
  mutate(
    Severity = factor(Severity, levels = c("Low", "Medium", "High")),
    Frequency = factor(Frequency, levels = c("Low", "Medium", "High"))
  )

#produce 1 tile per cell
covid_risk_matrix <- covid_risk_table %>%
  group_by(Severity, Frequency) %>%
  summarise(
    Total_Count = sum(Count),
    Label = paste0(Event, " (", Count, ")", collapse = "\n"),
    .groups = "drop"
  )

ggplot(covid_risk_matrix,
       aes(x = Frequency, y = Severity, fill = Total_Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label),
            color = "white",
            fontface = "bold",
            lineheight = 0.9,
            size = 8) +
  scale_fill_gradient2(
    low = "#F1C40F",    
    mid = "#CC5500",    
    high = "#E74C3C",  
    midpoint = median(covid_risk_matrix$Total_Count)
  ) +
  labs(
    title = "COVID Vaccine Adverse Event Risk Matrix",
    x = "Frequency of Event",
    y = "Severity Level",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 14)
  )


######################
#####Non-Covid risk table
#####################


non_covid_risk_table <- single_vax_nocovid %>%
  summarise(
    Death           = sum(DIED == "Y", na.rm = TRUE),
    LifeThreatening = sum(L_THREAT == "Y", na.rm = TRUE),
    Hospitalization = sum(HOSPITAL == "Y", na.rm = TRUE),
    Disabled        = sum(DISABLE == "Y", na.rm = TRUE),
    ER              = sum(ER_VISIT == "Y", na.rm = TRUE)
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Event",
    values_to = "Count"
  ) %>%
  mutate(
    Severity = case_when(
      Event %in% c("Death", "LifeThreatening") ~ "High",
      Event %in% c("Hospitalization", "Disabled") ~ "Medium",
      Event == "ER" ~ "Low"
    ),
    Frequency = case_when(
      Count > 50000 ~ "High",
      Count > 10000 ~ "Medium",
      TRUE ~ "Low"
    )
  )


#Add frequency category to each event.
non_covid_risk_table <- non_covid_risk_table %>%
  mutate(
    Severity = factor(Severity, levels = c("Low", "Medium", "High")),
    Frequency = factor(Frequency, levels = c("Low", "Medium", "High"))
  )

#produce 1 tile per cell
non_covid_risk_matrix <- non_covid_risk_table %>%
  group_by(Severity, Frequency) %>%
  summarise(
    Total_Count = sum(Count),
    Label = paste0(Event, " (", Count, ")", collapse = "\n"),
    .groups = "drop"
  )

ggplot(non_covid_risk_matrix,
       aes(x = Frequency, y = Severity, fill = Total_Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label),
            color = "white",
            fontface = "bold",
            lineheight = 0.9,
            size = 8) +
  scale_fill_gradient2(
    low = "#F1C40F",    
    mid = "#CC5500",    
    high = "#E74C3C",  
    midpoint = median(covid_risk_matrix$Total_Count)
  ) +
  labs(
    title = "Non-COVID Vaccine Adverse Event Risk Matrix",
    x = "Frequency of Event",
    y = "Severity Level",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text  = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#################################################
########### End Risk Matrices
################################################

###I kind of like the next 2 charts,
###****THIS IS PROBABLY BETTER FOR HOSPITAL STAFF.


#serious events by manufacturer & age group
serious_by_manufacturer_age <- single_vax_covid_only %>%
  filter(VAX_NAMES != "UNKNOWN", VAX_NAMES != "UNKONWN") %>%   
  group_by(VAX_NAMES, AGE_GROUP) %>%
  summarise(
    Death        = sum(DIED == "Y", na.rm = TRUE),
    Hospital     = sum(HOSPITAL == "Y", na.rm = TRUE),
    LifeThreat   = sum(L_THREAT == "Y", na.rm = TRUE),
    Disabled     = sum(DISABLE == "Y", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = c(Death, Hospital, LifeThreat, Disabled),
    names_to = "Event_Type",
    values_to = "Count"
  )

#Stacked bar chart  *Would need tweaking, interesting view though
ggplot(serious_by_manufacturer_age,
       aes(x = AGE_GROUP, y = Count, fill = Event_Type)) +
  geom_col(position = "stack") +
  facet_wrap(~ VAX_NAMES, ncol = 2) +
  labs(
    title = "Serious COVID-19 Vaccine Reactions by Age Group and Manufacturer",
    x = "Age Group",
    y = "Number of Serious Events",
    fill = "Event Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

### THIS CHART MIGHT BE WORTH SALVAGING### NEED TABLE FOR IT THOUGH.
ggplot(serious_by_manufacturer_age,
       aes(x = AGE_GROUP, y = Count, fill = VAX_NAMES)) +
  geom_col(position = position_dodge(width = 0.8)) +
  facet_wrap(~ Event_Type, scales = "free_y") +
  labs(
    title = "Serious COVID-19 Events by Age Group and Manufacturer",
    x = "Age Group",
    y = "Count",
    fill = "Manufacturer"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

###END Stacked Bar Chart FOR ABOVE###############


####################################################################
### End EXEC Level charts
###
####################################################################



####################################################################
### Hospital staff charts
###
####################################################################


##############################################
###System by age group treemap
##############################################

##create Symptoms dataframe

#for all vaccines
vaccine_symptoms <- single_vax %>%
  # 1. Clean extra commas and spaces
  mutate(
    SYMPTOMS = SYMPTOMS %>%
      str_replace_all(",+", ",") %>%        
      str_replace_all("^,|,$", "") %>%      
      str_trim()                           
  ) %>%
  
  separate_rows(SYMPTOMS, sep = ",") %>%
  mutate(SYMPTOMS = str_trim(SYMPTOMS)) %>%
  filter(SYMPTOMS != "", !is.na(SYMPTOMS))

top_symptoms <- vaccine_symptoms %>%
  count(SYMPTOMS, sort = TRUE)


#### Top Symptoms by Age group

top_symptoms_by_age <- vaccine_symptoms %>%
  group_by(AGE_GROUP, SYMPTOMS) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  arrange(AGE_GROUP, desc(Count))


top10_symptoms_by_age <- top_symptoms_by_age %>%
  group_by(AGE_GROUP) %>%
  slice_head(n = 10) %>%
  ungroup()

ggplot(top10_symptoms_by_age, 
       # Define the aesthetics for treemapify
       # area = tile size, fill = color, subgroup = first grouping level
       aes(area = Count, 
           fill = AGE_GROUP,
           subgroup = AGE_GROUP, 
           label = SYMPTOMS)) + 
  
  geom_treemap() +
  geom_treemap_text(
    color = "black", 
    place = "centre", 
    size = 10,
    grow = FALSE, 
    fontface = "bold"
  ) +
  
  geom_treemap_subgroup_border(color = "white", size = 3) +
  labs(
    title = "Top 10 Vaccine Symptoms by Age Group",
    fill = "Age Group" # Title for the color legend
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

##############################################
###END System by age group treemap
##############################################

#####################################################
### Adverse Events by Vaccine type Horizontal Bar by AGE_GROUP
####################################################


event_counts_wide_faceted <- calculated_data %>%
  filter(VAX_TYPES != "", !is.na(VAX_TYPES), 
         AGE_GROUP != "Unknown/Invalid Age") %>%

  group_by(VAX_TYPES, AGE_GROUP) %>%
  summarise(
    Death             = sum(DIED == "Y", na.rm = TRUE),
    LifeThreatening   = sum(L_THREAT == "Y", na.rm = TRUE),
    Hospitalization   = sum(HOSPITAL == "Y", na.rm = TRUE),
    Disabled          = sum(DISABLE == "Y", na.rm = TRUE),
    ER_Visit          = sum(ER_VISIT == "Y", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Total_Serious_Events = Death + LifeThreatening + Hospitalization 
         + Disabled + ER_Visit)

global_totals <- event_counts_wide_faceted %>%
  group_by(VAX_TYPES) %>%
  summarise(Global_Total = sum(Total_Serious_Events)) %>%
  arrange(desc(Global_Total))

top_10_vax_order <- global_totals %>% slice_head(n = 10) %>% pull(VAX_TYPES)

top_10_adverse_long_faceted <- event_counts_wide_faceted %>%
  filter(VAX_TYPES %in% top_10_vax_order) %>%
  mutate(VAX_TYPES = factor(VAX_TYPES, levels = rev(top_10_vax_order))) %>%
  pivot_longer(
    cols = c(Death, LifeThreatening, Hospitalization, Disabled, ER_Visit),
    names_to = "Outcome_Type",
    values_to = "Count"
  ) %>%
  filter(Count > 0)

ggplot(top_10_adverse_long_faceted,
       aes(x = VAX_TYPES, 
           y = Count,
           fill = Outcome_Type)) +
  
  geom_col() +
  coord_flip() +
  facet_wrap(~ AGE_GROUP, scales = "free_y", ncol = 3) +
  scale_fill_manual(
    values = event_colors,
    name = "Event Type"
  ) +
  labs(
    title = "Adverse Events by Vaccines and Age Group",
    x = "Vaccine Type",
    y = "Count of Adverse Events"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    # Reduce space between bars/facets
    panel.spacing = unit(1, "lines") 
  )

###########################################################
###END DIFF STACKED BAR CHART by Age Group
###################################

#####################################################
####adverse_events for multiple vaccines.
#######################################################

multi_vax_clean <- calculated_data %>%
  mutate(
    combo_list = strsplit(VAX_NAMES, ","),
    combo_list = map(combo_list, ~ trimws(.x)),
    combo_list = map(combo_list, sort),  # alphabetize
    combo_key = map_chr(combo_list, ~ paste(.x, collapse = ", "))
  )

#summary the combinations
combo_summary <- multi_vax_clean %>%
  group_by(combo_key) %>%
  summarise(
    total_reports = n(),
    adverse_events = sum(ADVERSE_EVENT == 1, na.rm = TRUE),
    percent_adverse = round((adverse_events / total_reports) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(adverse_events))


ggplot(combo_summary %>% slice_max(adverse_events, n = 10),
       aes(x = reorder(combo_key, adverse_events),
           y = adverse_events)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top Multi-Vaccine Combinations Associated With Adverse Events",
    x = "Vaccine Combination",
    y = "Adverse Events"
  ) +
  theme_minimal()


#####################################################
####End adverse_events for multiple vaccines.
#######################################################

####################################################################
### END Hospital staff charts
###
####################################################################


####################################################################
### General Public Charts
###
####################################################################

# Canva for charts, 
# pulled up a good image from:
# https://www.statista.com/chart/21641/historical-morbidity-and-vaccinations/

# Calculate overall safety #s.

serious_events_percent = round((sum(calculated_data$ADVERSE_EVENT, 
                        na.rm = TRUE)/nrow(calculated_data) * 100), 2)


# deaths percentage
deaths_caused_percent = round((sum(calculated_data$DIED == "Y", na.rm = TRUE)/
                         nrow(calculated_data) * 100), 2)

surviving_percent = 100 - deaths_caused_percent

total_deaths_from_vaccines = sum(calculated_data$DIED == "Y", na.rm = TRUE)


####################################################
########## Circle Chart of Symptoms
####################################################

top_symptoms_for_vax <- vaccine_symptoms %>%
  group_by(SYMPTOMS) %>%
  summarise(
    Report_Count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Report_Count)) %>%
  slice_head(n = 10)


packing <- circleProgressiveLayout(
  top_symptoms_for_vax$Report_Count,
  sizetype = "area"
)

# Bind layout (x, y, radius)
symptom_layout <- bind_cols(top_symptoms_for_vax, packing)

circle_polys <- circleLayoutVertices(packing, npoints = 100)

# Add labels + counts to the polygon data for coloring (optional)
circle_polys <- circle_polys %>%
  mutate(
    SYMPTOMS = symptom_layout$SYMPTOMS[id],
    Report_Count = symptom_layout$Report_Count[id]
  )

#Plot the circles
ggplot() +
  geom_polygon(
    data = circle_polys,
    aes(x = x, y = y, group = id, fill = Report_Count),
    colour = "grey30",
    alpha = 0.9
  ) +
  geom_text(
    data = symptom_layout,
    aes(x = x, y = y, label = SYMPTOMS),
    size = 4.5,           
    fontface = "bold",    
    color = "black"       
  ) +
  scale_fill_gradient(
    low = "#D6EAF8",   
    high = "#2E86C1",  
    name = "Report Count"
  ) +
  coord_equal() +
  theme_void() +
  labs(
    #title = "Top 10 Symptoms "
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  )

#####################################################
############# End Circle Chart
###################################################

#####################################################
############# Bar Chart
###################################################

#show how many adverse events vs total doses

doses_bar_chart <- calculated_data %>%
  # Convert YEAR_MONTH_DATE to a Date object
  mutate(Date = as.Date(YEAR_MONTH_DATE)) %>%
  
  # Calculate the Quarter and Year
  mutate(
    Year = year(Date),
    Quarter = quarter(Date),
    # Create the label for the X-axis (e.g., "2020 Q1")
    YearQuarter = factor(paste0(Year, " Q", Quarter))
  ) %>%
  
  # Group by the new Quarter/Year field
  group_by(YearQuarter) %>%
  summarise(
    Total_Reports = n(),
    Total_Adverse = sum(ADVERSE_EVENT, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Filter out any dates with zero reports
  filter(Total_Reports > 0) %>%
  # Arrange the data chronologically 
  arrange(YearQuarter)


ggplot(doses_bar_chart, aes(x = YearQuarter)) +
  
  geom_col(
    aes(y = Total_Reports, fill = "Total Doses Administered"),
    width = 0.8, 
    position = position_identity(),
    color = "darkgray"
  ) +
  
  geom_col(
    aes(y = Total_Adverse, fill = "Event Recorded"),
    width = 0.4, 
    position = position_identity(),
    color = "black"
  ) +
  
  scale_fill_manual(
    name = NULL,
    values = c("Total Doses Administered" = "#0072B2", "Event Recorded" = "#D55E00")
  ) +
  
  # Customize Axes and Theme
  scale_y_continuous(
    name = "Count of Doses Administered (All Vaccine Types)",
    labels = scales::comma
  ) +
  
  labs(
    title = "Adverse Events vs. Total Doses by Quarter",
    x = "Year and Quarter"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


#####################################################
############# End Bar Chart
###################################################

####################################################################
### END General Public Charts
###
####################################################################


####################################################################
### Analysts  Charts
###
####################################################################


##########################################
############# Box and whisker plot ###################
##########################################

#Age distribution of Adverse Events by Vaccine Type

# Filter ONLY adverse-event reports and valid ages
age_dist <- single_vax %>%
  filter(
    ADVERSE_EVENT == 1,
    !is.na(AGE_YRS),
    AGE_YRS > 0,      
    AGE_YRS < 120    
  ) %>%
  group_by(GENERIC_VAX_NAME)

# Create the boxplot
ggplot(age_dist,
       aes(x = GENERIC_VAX_NAME,
           y = AGE_YRS,
           fill = GENERIC_VAX_NAME)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.4) +
  labs(
    title = "Age Distribution of Adverse Events by Vaccine Type",
    x = "Vaccine Type",
    y = "Age (Years)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "none"
  )

##############################################
#####  End Box and whisker plot ########################
###############################################


###############################################
#####Scatterplot with linear regression######
###############################################

# Bin age into meaningful groups
age_risk_group <- single_vax %>%
  filter(!is.na(AGE_GROUP)) %>%
  mutate(
    age_mid = str_extract(AGE_GROUP, "^\\d+") %>% as.numeric() %>% (`+`)(5)
  ) %>% 
  group_by(age_mid) %>%
  summarise(
    adverse_rate = mean(ADVERSE_EVENT == 1, na.rm = TRUE),
    reports      = n(),
    .groups      = "drop"
  )

ggplot(age_risk_group, aes(x = age_mid, y = adverse_rate)) +
  geom_point(color = "#2E86C1", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1.2) +
  labs(
    title = "Scatterplot: Age Group (Numeric Midpoint) vs Adverse Event Rate",
    x = "Age (Group Midpoint)",
    y = "Adverse Event Rate",   
    caption = "Adverse-event rate declines with age, which is 
    counter-intuitive; differences in reporting patterns or vaccine 
    distribution may explain this trend."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

#######################################
### End Scatterplot ##################
######################################


#######################################
#########rolling trend ######################
#######################################

##Shows how much is being reported, include overall trend.

vaers_time_series_data <- calculated_data %>%
  filter(!is.na(YEAR_MONTH_DATE)) %>%
  
  group_by(report_month = YEAR_MONTH_DATE) %>%
  summarise(
    # Count the number of unique reports received that month
    report_volume = n_distinct(VAERS_ID),
    .groups = "drop"
  ) %>%
  # Calculate the Rolling 3-Month Average for the trend line
  mutate(
    rolling_avg_volume = rollmean(report_volume, k = 3, fill = NA, 
                                  align = "right")
  )

max_volume <- max(vaers_time_series_data$report_volume, na.rm = TRUE)
max_rolling <- max(vaers_time_series_data$rolling_avg_volume, na.rm = TRUE)
scaling_factor <- max_volume / max_rolling * 0.85


# set title using variables for the year
plot_title = paste0("Adverse Event Volume and Smoothed Trend (", firstYear,
    "–", lastYear, ")")
    
# Create the plot
dual_axis_plot <- ggplot(vaers_time_series_data, aes(x = report_month)) +
  
  # Bar, Raw Volume
  geom_bar(
    aes(y = report_volume),
    stat = "identity",
    fill = "#0072B2", 
    alpha = 0.7 
  ) +
  
  # Line,  Rolling Average Trend
  geom_line(

    aes(y = rolling_avg_volume * scaling_factor, 
        color = "3-Month Rolling Average"),
    linewidth = 1.5,
    na.rm = TRUE 
  ) +
  geom_point(
    aes(y = rolling_avg_volume * scaling_factor),
    size = 2.5,
    color = "#D55E00", 
    na.rm = TRUE
  ) +
  scale_y_continuous(
    name = "Monthly Report Counts",
    labels = comma,
    sec.axis = sec_axis(
      ~ . / scaling_factor,
      name = "3-Month Rolling Average",
      labels = comma
    )
  ) +
  scale_x_date(
    name = "Report Month",
    date_breaks = "6 months", 
    date_labels = "%b %Y" 
  ) +
  
  labs(
    title = plot_title,
  ) +
  scale_color_manual(values = c("3-Month Rolling Average" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.y.left = element_text(face = "bold", color = "#0072B2"),
    axis.text.y.right = element_text(face = "bold", color = "#D55E00"),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

print(dual_axis_plot)

##############################
#### End Adverse Event Volume & Smoothed Trend
#################################################


##########################################
### Adverse Event Composition over time
##################################################

event_type_composition_data <- calculated_data %>%
  filter(!is.na(YEAR_MONTH_DATE)) %>%
  
  # Calculate the count for each adverse event type per month
  group_by(report_month = YEAR_MONTH_DATE) %>%
  summarise(
    # Count the reports where the indicator field is "Y"
    Died = sum(DIED == "Y", na.rm = TRUE),
    Hospital = sum(HOSPITAL == "Y", na.rm = TRUE),
    L_Threat = sum(L_THREAT == "Y", na.rm = TRUE),
    Disable = sum(DISABLE == "Y", na.rm = TRUE),
    ER_Visit = sum(ER_VISIT == "Y", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Pivot the data from wide to long format
  # We want a single column for the metric (Count) and one for the category (Event_Type)
  pivot_longer(
    cols = c(Died, Hospital, L_Threat, Disable, ER_Visit),
    names_to = "Event_Type",
    values_to = "Count"
  ) %>%
  # Filter out any month/type combinations where the count is zero to keep the dataset clean
  filter(Count > 0)

# Define the order of severity for better visual stacking (Death on top, ER Visit at the bottom)
severity_order <- c("ER_Visit", "Disable", "Hospital", "L_Threat", "Died")

# Define a color palette that emphasizes severity, using the ColorBrewer Reds/Oranges/Greys
event_colors <- c(
  "ER_Visit" = "#D9D9D9", # Light Grey (Least Severe)
  "Disable"  = "#BDBDBD", # Medium Grey
  "Hospital" = "#FDBB84", # Ligh Orange
  "L_Threat" = "#E34A33", # Red-Orange
  "Died"     = "#800026"  # Dark Red (Most Severe)
)


#i know this is terrible, but it's my code to do with as I want!
plot_title = paste0("Composition of Adverse Events Over Time (", firstYear,
    "–", lastYear, ")")

composition_plot <- ggplot(event_type_composition_data, 
                           aes(x = report_month, y = Count, fill = 
                                 factor(Event_Type, levels = severity_order))) +
  

  geom_area(position = "stack", alpha = 0.8) +
  
  # Apply the custom color palette
  scale_fill_manual(values = event_colors, name = "Report Severity Type") +

  scale_y_continuous(
    name = "Monthly Report Volume (Stacked Count)",
    labels = comma
  ) +
  scale_x_date(
    name = "Report Month",
    date_breaks = "6 months", 
    date_labels = "%b %Y" 
  ) +
  
  # --- THEME AND LABELS ---
  labs(
    title = plot_title,
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

# Display the final plot
print(composition_plot)


##########################################
### END Adverse Event composition over time
##################################################


print(summary(calculated_data))

cat("====Data Cleaned ====")
calculated_data %>% 
  count(VAX_NAMES, sort = TRUE) %>%
  head(10)

calculated_data %>%
  count(SEX, sort=TRUE)

calculated_data %>%
  summarise(
    Died_Count = sum(DIED == "Y", na.rm = TRUE),
    Hospital_Count = sum(HOSPITAL == "Y", na.rm = TRUE),
    Life_Threatening_Count = sum(L_THREAT == "Y", na.rm = TRUE),
    Disable_Count = sum(DISABLE == "Y", na.rm = TRUE),
    ER_Visit_Count = sum(ER_VISIT == "Y", na.rm = TRUE)
  )

calculated_data %>%
  summarise(
    Missing_SEX = sum(is.na(SEX)),
    Missing_AGE_YRS = sum(is.na(AGE_YRS)),
    Missing_VAX_NAME = sum(is.na(VAX_NAMES)),
    Missing_Died = sum(is.na(DIED))
  )

cat('=============END cleaned data===============')
 
####################################################################
### END Analysts  Charts
###
####################################################################


