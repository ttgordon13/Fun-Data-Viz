
# --------------------------------------------------------------------------
# Libraries
# --------------------------------------------------------------------------
library(civis)
library(shiny)
library(plotly)
library(maps)
library(cowplot)
library(extrafont)
library(officer)
library(flextable)
library(tidyverse)


# --------------------------------------------------------------------------
# Build data
# --------------------------------------------------------------------------
# County-level
df <- str_c("
            select distinct
            state,
            state_full,
            county_name,
            registered_voters,
            dem_twoway,
            dem_twoway16,
            total_votes,
            turnout_2016,            
            dem_twoway::real - dem_twoway16::real as support_swing,
            total_votes::real - turnout_2016::real as turnout_swing,
            total_votes::real/turnout_2016::real as pct_of_2016_turnout
            from data.election_data
            where county_name is not null 
              and state is not null;") %>%
  sql() %>%
  read_civis() 

# Change to lower case
df$county_name <- tolower(df$county_name)
df$state_full <-  tolower(df$state_full)


# Map data
county_map <- map_data('county')


# Merge base data
county_map_data <- merge(df, county_map, by.x = c("state_full","county_name"), by.y = c("region","subregion"))
county_map_data <- county_map_data[order(county_map_data$order),]



# -----------------------------------------------------------------------------
# Support Swing map
# -----------------------------------------------------------------------------
states <- c("FL","PA","MI","WI","AZ","NV","NC","GA")

for(j in 1:length(states)) {
  
  s <- states[j]

county_plot_data <- county_map_data %>%
  filter(state == s)

min_swing <- min(county_plot_data$support_swing, na.rm = T)
max_swing <- max(county_plot_data$support_swing, na.rm = T)
swing_range <- ceiling(max(c(abs(min_swing), abs(max_swing))) * 100 / 5) * 5 / 100
limits_min <- 0 - swing_range
limits_max <- swing_range

support_swing_map <- 
  ggplot(data = county_plot_data, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) + 
  geom_polygon(mapping = aes(fill = support_swing), color = "white") +
  geom_polygon(color = "#DDDDDD", fill = NA) + 
  scale_fill_gradient2(	low = "#FF4137", mid = "white", high = "#4C95D7",
                        midpoint = 0.0,
                        guide = guide_colorbar(	barwidth = 12, barheight = 0.5, nbin = 10, 
                                                title = "2016-2020 Support Swing", title.position = "top"),
                        labels = scales::percent_format(accuracy = 1),
                        limits = c(limits_min, limits_max),
                        breaks = seq(limits_min, limits_max, by = 0.05)) +
  theme_minimal() +
  theme(	axis.line = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "bottom",
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         text = element_text(family = "Roboto"))

assign(paste0("support_swing_map_", s), support_swing_map)
}









# -----------------------------------------------------------------------------
# Support Swing Flextable
# -----------------------------------------------------------------------------
state_support_2016_base <- str_c(
  "select
  state_code as state,
  sum(dem_votes) as total_dem_votes,
  sum(dem_votes) + sum(rep_votes) as vote_total,
  total_dem_votes::float/vote_total as dem_twoway16
  from post_mortem.historic_county_level_pres_results
  where cycle = 2016
  group by 1") %>% 
  sql() %>%
  read_civis() 

state_support_2020_base <- str_c(
  "select
  state,
  sum(biden_votes) as total_biden_votes,
  sum(biden_votes) + sum(trump_votes) as vote_total,
  total_biden_votes::float/vote_total as dem_twoway20
  from data.election_data
  group by 1") %>%
  sql() %>%
  read_civis() 


states <- c("FL","PA","MI","WI","AZ","NV","NC","GA")

for(j in 1:length(states)) {
  
  s <- states[j]
  s_full <- (state.name[which(state.abb == s)])

state_support_2016 <- state_support_2016_base %>%
  filter(state == s)

state_support_2020 <- state_support_2020_base %>%
  filter(state == s)

state_support <- 
  inner_join(state_support_2016, state_support_2020, by = "state") %>%
  mutate(dem_twoway_swing = round(dem_twoway20 - dem_twoway16, 3)) %>%
  mutate(county_name = s_full, pct_of_electorate = 1, rank = 1) %>%
  select(state, county_name, dem_twoway16, dem_twoway20, dem_twoway_swing, pct_of_electorate, rank)

county_support <- df %>%
  filter(state == s) %>%
  mutate(pct_of_electorate = total_votes/sum(total_votes, na.rm = T)) %>%
  arrange(desc(pct_of_electorate)) %>%
  mutate(rank = row_number() + 1,
         dem_twoway20 = dem_twoway,
         dem_twoway_swing = support_swing) %>%
  select(state, county_name, dem_twoway16, dem_twoway20, dem_twoway_swing, pct_of_electorate, rank)

county_flex_df <-
  bind_rows(state_support, county_support) %>%
  arrange(desc(pct_of_electorate)) %>%
  filter(rank <= 11) %>%
  select(county_name, dem_twoway16, dem_twoway20, dem_twoway_swing, pct_of_electorate) %>%
  mutate(county_name = str_to_title(county_name)) 

support_swing_flex <- 
  county_flex_df %>%
  flextable() %>%
  font(fontname = "Roboto", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  # Header
  bold(part = "header") %>%
  align(align = "center", part = "header", j = c("dem_twoway16", "dem_twoway20", "dem_twoway_swing", "pct_of_electorate")) %>%
  set_header_labels(county_name = "County", dem_twoway16 = "Clinton '16", dem_twoway20 = "Biden '20", dem_twoway_swing = "Swing", pct_of_electorate = "Pct. of Electorate") %>%
  hline(i = 1, border = fp_border(color = "black", style = "solid"), part = "body") %>%
  # County Column
  bold(j = "county_name") %>%
  align(align = "left", part = "all", j = "county_name") %>%
  # Numeric Columns
  align(align = "center", j = c("dem_twoway16", "dem_twoway20", "dem_twoway_swing", "pct_of_electorate")) %>%
  set_formatter(	dem_twoway16 = function(x) paste0(format(round(x * 100, 1), nsmall = 1), "%"),
                 dem_twoway20 = function(x) paste0(format(round(x * 100, 1), nsmall = 1), "%"),
                 dem_twoway_swing = function(x) paste0(format(round(x * 100, 1), nsmall = 1), "%"),
                 pct_of_electorate = function(x) paste0(format(round(x * 100, 1), nsmall = 1), "%")) %>%
  width(width = c(1.25, 1.15, 1.15, 1.15, 1.15))

assign(paste0("support_swing_flex_", s), support_swing_flex)
}

# -----------------------------------------------------------------------------
# Turnout Swing Map
# -----------------------------------------------------------------------------
states <- c("FL","PA","MI","WI","AZ","NV","NC","GA")

for(j in 1:length(states)) {
  
  s <- states[j]
  
  county_plot_data <- county_map_data %>%
    filter(state == s)

min_swing <- min(county_plot_data$pct_of_2016_turnout)
med_swing <- median(county_plot_data$pct_of_2016_turnout)
max_swing <- max(county_plot_data$pct_of_2016_turnout)
limits_min <- floor(min_swing * 100 / 5) / 100 * 5
median <- floor(med_swing * 100 / 5) / 100 * 5
limits_max <- ceiling(max_swing * 100 / 5) / 100 * 5

turnout_swing_map <- 
  ggplot(data = county_plot_data, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) + 
  geom_polygon(mapping = aes(fill = pct_of_2016_turnout), color = "#FFFFFF") +
  geom_polygon(color = "#DDDDDD", fill = NA) + 
  scale_fill_gradient2(	low = "#A6611A", mid = "#FFFFFF", high = "#018571",
                        midpoint = 1,
                        guide = guide_colorbar(	barwidth = 12, barheight = 0.5, nbin = 10, 
                                                title = "2016-2020 Turnout Ratio", title.position = "top"),
                        labels = scales::percent_format(accuracy = 1),
                        limits = c(limits_min, limits_max)) +
  theme_minimal() +
  theme(	axis.line = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "bottom",
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         text = element_text(family = "Roboto"))


assign(paste0("turnout_swing_map_", s), turnout_swing_map)
}







# -----------------------------------------------------------------------------
# Turnout Swing Flextable
# -----------------------------------------------------------------------------
turnout_base <- str_c(
  "select
  a.state,
  a.vf_reg_county_name as county_name,
  b.total_votes as turnout16,
  a.total_votes as turnout20,
  turnout20::float/turnout16::float as turnout_ratio
  from election_night_modeling.training_master a
  join post_mortem.historic_county_level_pres_results b on a.county_fips::float=b.county_fips::float
  where b.cycle = 2016") %>%
  sql() %>%
  read_civis() 


states <- c("FL","PA","MI","WI","AZ","NV","NC","GA")

for(j in 1:length(states)) {
  
  s <- states[j]
  s_full <- (state.name[which(state.abb == s)])
  
  
turnout <- turnout_base %>%
  filter(state == s) %>%
  arrange(desc(turnout20)) %>%
  mutate(county_name = str_to_title(county_name),
         rank = row_number() + 1) 

turnout_flex_df <- turnout %>%
  add_row(state = "FL", county_name = s_full, turnout16 = sum(turnout$turnout16, na.rm = T), turnout20 = sum(turnout$turnout20, na.rm = T), 
          turnout_ratio = sum(turnout$turnout20, na.rm = T)/sum(turnout$turnout16, na.rm = T), rank = 1) %>%
  filter(rank < 11) %>%
  arrange((rank)) %>%
  select(county_name, turnout16, turnout20, turnout_ratio)

turnout_swing_flex <- 
  turnout_flex_df %>%
  flextable() %>%
  font(fontname = "Roboto", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  # Header
  bold(part = "header") %>%
  align(align = "center", part = "header", j = c("turnout16", "turnout20", "turnout_ratio")) %>%
  set_header_labels(county_name = "County", turnout16 = "2016 Turnout", turnout20 = "2020 Turnout", turnout_ratio = "'16 -'20 Ratio") %>%
  hline(i = 1, border = fp_border(color = "black", style = "solid"), part = "body") %>%
  # Numeric Columns
  align(align = "center", j = c("turnout16", "turnout20", "turnout_ratio")) %>%
  set_formatter(	turnout16 = function(x) case_when(x >= 1000000 ~ paste0(format(round(x/1000000, 1), nsmall = 1), "mm"), x >= 1000 ~ paste0(format(round(x/1000, 1), nsmall = 1), "k"), TRUE ~ as.character(x)),
                 turnout20 = function(x) case_when(x >= 1000000 ~ paste0(format(round(x/1000000, 1), nsmall = 1), "mm"), x >= 1000 ~ paste0(format(round(x/1000, 1), nsmall = 1), "k"), TRUE ~ as.character(x)),
                 turnout_ratio = function(x) paste0(format(round(x * 100, 1), nsmall = 1), "%")) %>%
  # Column Width
  width(width = c(1.25, 1.15, 1.15, 1.15))

assign(paste0("turnout_swing_flex_", s), turnout_swing_flex)
}






#------------------------------------------------------------------------------
# BUILD AND EXPORT SLIDES
#------------------------------------------------------------------------------

p <- read_pptx("C:/Users/Taylor Gordon/Documents/GitHub/forecast_template.pptx")

states <- c("FL","PA","MI","WI","AZ","NV","NC","GA")

for(i in 1:length(states)) {
  
  s <- states[i]
  
  # Support Swing by County Map & Flextable
  p <- 
    p %>%
    add_slide(layout = "Title, Subtitle, Picture, Flextable", master = "Priorities Template") %>%
    ph_with(value = "FORECASTED SUPPORT SWINGS FROM 2016",
            location = ph_location_label(ph_label = "Text Placeholder 12")) %>%
    ph_with(value = "SUPPORT SWINGS BY COUNTY",
            location = ph_location_label(ph_label = "Text Placeholder 14")) %>%
    ph_with(value = assign(paste0("support_swing_map_", s), get(paste0("support_swing_map_", s))),
            location = ph_location_label(ph_label = "Picture Placeholder 16")) %>%
    ph_with(value = assign(paste0("support_swing_flex_", s), get(paste0("support_swing_flex_", s))),
            location = ph_location_label(ph_label = "Table Placeholder 2"))
  
}

print(p, target = "support_swing_slides.pptx")







p <- read_pptx("C:/Users/Taylor Gordon/Documents/GitHub/forecast_template.pptx")

states <- c("FL","PA","MI","WI","AZ","NV","NC","GA")

for(i in 1:length(states)) {
  
  s <- states[i]
  
  # Turnout Swing by County Map & Flextable
  p <- 
    p %>%
    add_slide(layout = "Title, Subtitle, Picture, Flextable", master = "Priorities Template") %>%
    ph_with(value = "FORECASTED TURNOUT SWINGS FROM 2016",
            location = ph_location_label(ph_label = "Text Placeholder 12")) %>%
    ph_with(value = "TURNOUT SWINGS BY COUNTY",
            location = ph_location_label(ph_label = "Text Placeholder 14")) %>%
    ph_with(value = assign(paste0("turnout_swing_map_", s), get(paste0("turnout_swing_map_", s))),
            location = ph_location_label(ph_label = "Picture Placeholder 16")) %>%
    ph_with(value = assign(paste0("turnout_swing_flex_", s), get(paste0("turnout_swing_flex_", s))),
            location = ph_location_label(ph_label = "Table Placeholder 2"))
  
  
}

print(p, target = "turnout_swing_slides.pptx")
