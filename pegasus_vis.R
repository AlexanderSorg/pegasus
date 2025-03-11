library(dplyr)
library(ggplot2)
library(countrycode)

# UN Data
ideal_points <- read.csv("/IdealpointestimatesAll_Jun2024.csv")

## Add year column
ideal_points$year <- ideal_points$session + 1945

## Add distance to Israel ideal points for every country
israel_ideal_data <- ideal_points %>%
  filter(ccode == 666) %>%
  select(year, isr_ideal = IdealPointAll)

ideal_points <- ideal_points %>%
  left_join(israel_ideal_data, by = "year")

ideal_points$similarity = abs(ideal_points$IdealPointAll - ideal_points$isr_ideal)

max_value <- max(ideal_points$similarity, na.rm = TRUE)

## Create a new column with inverted values
ideal_points <- ideal_points %>%
  mutate(similarity_inverted = max_value - similarity)

## Subset for relevant countries: Mexico, Bahrain, Djibouti, Estonia, Germany, Hungary, India, Panama, Poland, Saudi Arabia, Uganda

pegasus_vis <- ideal_points %>% filter(ccode %in% c(70, 692, 522, 366, 255, 310, 750, 95, 290, 670, 500))

# Event years = purchase of Pegasus
event_years <- data.frame(
  ccode = c(70, 692, 522, 366, 255, 750, 95, 290, 670, 500),
  event_year = c(2011, 2017, 2018, 2019, 2019, 2017, 2012, 2017, 2017, 2019)
)

# Merge the event years into the original dataframe
pegasus_vis <- merge(pegasus_vis, event_years, by = "ccode")

# Create the event variable
pegasus_vis$Pegasus <- ifelse(pegasus_vis$year < pegasus_vis$event_year, "Before purchase", "After purchase")


pegasus_vis <- pegasus_vis %>%
  mutate(year_diff = year - event_year) %>%
  filter(year_diff >= -10)



pegasus_vis_before <- pegasus_vis %>% filter(year <= event_year)
pegasus_vis_after <- pegasus_vis %>% filter(year >= event_year)

ggplot() +
  geom_line(data = pegasus_vis, aes(x = year, y = similarity_inverted, group = Countryname, color = Pegasus)) +
  geom_vline(data = pegasus_vis %>% distinct(Countryname, event_year), 
             aes(xintercept = event_year), linetype = "dashed", color = "red") +
  facet_wrap(~ Countryname, scales = "free") +
  theme_minimal() +
  labs(title = "UN Votes: Alignment with Israel",
       x = "",
       y = "Ideal Point Alignment") +
  theme(panel.spacing = unit(2, "lines"),
        strip.text = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  scale_color_manual(values = c("Before purchase" = "blue", "After purchase" = "red"))




##### Agreement scores


ag_s <- read.csv("AgreementScoresAll_Jun2024.csv")

agreement_scores <- ag_s

agreement_scores$year <- agreement_scores$session.x + 1945

agreement_scores <- agreement_scores %>% filter(ccode1==666 & year > 2000)



pegasus_vis <- agreement_scores %>% filter(ccode2 %in% c(70, 692, 522, 366, 255, 310, 750, 95, 290, 670, 500))

# Event years = purchase of Pegasus
event_years <- data.frame(
  ccode2 = c(70, 692, 522, 366, 255, 750, 95, 290, 670, 500),
  event_year = c(2011, 2017, 2018, 2019, 2019, 2017, 2012, 2017, 2017, 2019)
)


# Merge the event years into the original dataframe
pegasus_vis <- merge(pegasus_vis, event_years, by = "ccode2")

pegasus_vis$Countryname <- countrycode(pegasus_vis$ccode2, "cown", "country.name")

# Create the event variable
pegasus_vis$Pegasus <- ifelse(pegasus_vis$year < pegasus_vis$event_year, "Before purchase", "After purchase")


pegasus_vis <- pegasus_vis %>%
  mutate(year_diff = year - event_year) %>%
  filter(year_diff >= -10)


ggplot() +
  geom_line(data = pegasus_vis, aes(x = year, y = agree, group = Countryname, color = Pegasus)) +
  geom_vline(data = pegasus_vis %>% distinct(Countryname, event_year), 
             aes(xintercept = event_year), linetype = "dashed", color = "red") +
  facet_wrap(~ Countryname, scales = "free") +
  theme_minimal() +
  labs(title = "UN Votes: Agreement with Israel",
       x = "",
       y = "UN Vote Agreement") +
  theme(panel.spacing = unit(2, "lines"),
        strip.text = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  scale_color_manual(values = c("Before purchase" = "blue", "After purchase" = "red"))



