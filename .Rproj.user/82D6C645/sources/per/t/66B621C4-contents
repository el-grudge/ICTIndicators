# Goal: Comparison, Ranking and Part-to-whole visualization.

library(tidyverse)
library(readxl)
library(lubridate)

df <- as.data.frame(read_excel('ITU_Key_2005-2019.xlsx'))
df <- df %>% mutate(Year = year(strptime(Year, "%Y")))
df <- filter(df, Year > 2014)

for (i in 3:length(colnames(df))){
  df[,i] <- as.numeric(df[,i])
}

indicators <- df$Indicator %>% unique()

# Fixed- and mobile-telephone subscriptions

df_1a <- gather(
  filter(
    select(
      df,
      -'Developed',
      -'Developing', 
      -'World', 
      -'LDCs'
    ),
    Indicator == indicators[1] | Indicator == indicators[3]
  ),
  key = Country.Region,
  value = Count.millions,
  Africa, `Arab States`, `Asia & Pacific`, CIS, Europe, `The Americas`
)

# Comparison
# line plots comparison
  ggplot(df_1a, aes(Year, Count.millions, fill = Country.Region, color = Country.Region)) +
    geom_line() + facet_wrap(vars(Indicator))

# bar chart comparison - ordered
df_1a %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = as.numeric(factor(Count.millions, levels = .$Count.millions))) %>%
  ggplot(aes(Year, Count.millions, fill = Country.Region, color = Country.Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(vars(Indicator))

# Ranking
# lollipop chart ranked
df_1a %>% 
  arrange(`Country.Region`) %>% 
  mutate(Count.millions = factor(Count.millions, levels = unique(.$Count.millions))) %>%
  ggplot(aes(Country.Region, as.numeric(Count.millions), fill = Country.Region)) +
  geom_point(size = 3, color = "red") +
  geom_segment(aes(x = Country.Region, xend = Country.Region, y = 0, yend = as.numeric(Count.millions))) +
  facet_grid(rows = vars(Indicator), cols = vars(Year)) + 
  coord_flip()

# Part-of-whole
# stacked bar chart
df_1a %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(Year, as.numeric(Count.millions), fill = factor(Country.Region))) +
    geom_bar(stat = "identity", position = "stack") + 
  facet_grid(vars(Indicator))

# stacked prop
df_1a %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(Year, as.numeric(Count.millions), fill = Country.Region)) + 
  geom_area(stat = "identity", position = "fill") +
  facet_grid(vars(Indicator))

# pie-chart
df_1a %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(x = "", y = as.numeric(Count.millions), fill = Country.Region)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme(axis.line = element_blank()) +
  coord_polar(theta = "y", start = 0) + 
  facet_grid(vars(Indicator))  


# Fixed- and mobile-broadband subscriptions

df_1b <- gather(
  filter(
    select(
      df,
      -'Developed',
      -'Developing', 
      -'World', 
      -'LDCs'
    ),
    Indicator == indicators[5] | Indicator == indicators[7]
  ),
  key = Country.Region,
  value = Count.millions,
  Africa, `Arab States`, `Asia & Pacific`, CIS, Europe, `The Americas`
)

# Comparison
# line plots comparison
ggplot(df_1b, aes(Year, Count.millions, fill = Country.Region, color = Country.Region)) +
  geom_line() + facet_wrap(vars(Indicator))

# bar chart comparison - ordered
df_1b %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = as.numeric(factor(Count.millions, levels = .$Count.millions))) %>%
  ggplot(aes(Year, Count.millions, fill = Country.Region, color = Country.Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(vars(Indicator))

# Ranking
# lollipop chart ranked
df_1b %>% 
  arrange(`Country.Region`) %>% 
  mutate(Count.millions = factor(Count.millions, levels = unique(.$Count.millions))) %>%
  ggplot(aes(Country.Region, as.numeric(Count.millions), fill = Country.Region)) +
  geom_point(size = 3, color = "red") +
  geom_segment(aes(x = Country.Region, xend = Country.Region, y = 0, yend = as.numeric(Count.millions))) +
  facet_grid(rows = vars(Indicator), cols = vars(Year)) + 
  coord_flip()

# Part-of-whole
# stacked bar chart
df_1b %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(Year, as.numeric(Count.millions), fill = factor(Country.Region))) +
  geom_bar(stat = "identity", position = "stack") + 
  facet_grid(vars(Indicator))

# stacked prop
df_1b %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(Year, as.numeric(Count.millions), fill = Country.Region)) + 
  geom_area(stat = "identity", position = "fill") +
  facet_grid(vars(Indicator))

# pie-chart
df_1b %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(x = "", y = as.numeric(Count.millions), fill = Country.Region)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme(axis.line = element_blank()) +
  coord_polar(theta = "y", start = 0) + 
  facet_grid(vars(Indicator))  


# Population covered by mobile-cellular and mobile-broadband network

df_1c <- gather(
  filter(
    select(
      df,
      -'Developed',
      -'Developing', 
      -'World', 
      -'LDCs'
    ),
    Indicator == indicators[9] | Indicator == indicators[11] | Indicator == indicators[13]
  ),
  key = Country.Region,
  value = Count.millions,
  Africa, `Arab States`, `Asia & Pacific`, CIS, Europe, `The Americas`
)

# Comparison
# line plots comparison
ggplot(df_1c, aes(Year, Count.millions, fill = Country.Region, color = Country.Region)) +
  geom_line() + facet_wrap(vars(Indicator))

# bar chart comparison - ordered
df_1c %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = as.numeric(factor(Count.millions, levels = .$Count.millions))) %>%
  ggplot(aes(Year, Count.millions, fill = Country.Region, color = Country.Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(vars(Indicator))

#Ranking
# lollipop chart ranked
df_1c %>% 
  arrange(`Country.Region`) %>% 
  mutate(Count.millions = factor(Count.millions, levels = unique(.$Count.millions))) %>%
  ggplot(aes(Country.Region, as.numeric(Count.millions), fill = Country.Region)) +
  geom_point(size = 3, color = "red") +
  geom_segment(aes(x = Country.Region, xend = Country.Region, y = 0, yend = as.numeric(Count.millions))) +
  facet_grid(rows = vars(Indicator), cols = vars(Year)) + 
  coord_flip()

# Part-of-whole
# stacked bar chart
df_1c %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(Year, as.numeric(Count.millions), fill = factor(Country.Region))) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(vars(Indicator))

# stacked prop
df_1c %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(Year, as.numeric(Count.millions), fill = Country.Region)) + 
  geom_area(stat = "identity", position = "fill") +
  facet_grid(vars(Indicator))

# pie-chart
df_1c %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(x = "", y = as.numeric(Count.millions), fill = Country.Region)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme(axis.line = element_blank()) +
  coord_polar(theta = "y", start = 0) + 
  facet_grid(vars(Indicator))  


# International bandwidth usage

df_1d <- gather(
  filter(
    select(
      df,
      -'Developed',
      -'Developing', 
      -'World', 
      -'LDCs'
    ),
    Indicator == indicators[15]
  ),
  key = Country.Region,
  value = Count.millions,
  Africa, `Arab States`, `Asia & Pacific`, CIS, Europe, `The Americas`
)

# Comparison
# line plots comparison
ggplot(df_1d, aes(Year, Count.millions, fill = Country.Region, color = Country.Region)) +
  geom_line() + facet_wrap(vars(Indicator))

# bar chart comparison - ordered
df_1d %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = as.numeric(factor(Count.millions, levels = .$Count.millions))) %>%
  ggplot(aes(Year, Count.millions, fill = Country.Region, color = Country.Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(vars(Indicator))

# Ranking
# lollipop chart ranked
df_1d %>% 
  arrange(`Country.Region`) %>% 
  mutate(Count.millions = factor(Count.millions, levels = unique(.$Count.millions))) %>%
  ggplot(aes(Country.Region, as.numeric(Count.millions), fill = Country.Region)) +
  geom_point(size = 3, color = "red") +
  geom_segment(aes(x = Country.Region, xend = Country.Region, y = 0, yend = as.numeric(Count.millions))) +
  facet_grid(rows = vars(Indicator), cols = vars(Year)) + 
  coord_flip()

# Part-of-whole
# stacked bar chart
df_1d %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(Year, as.numeric(Count.millions), fill = factor(Country.Region))) +
  geom_bar(stat = "identity", position = "stack") + 
  facet_grid(vars(Indicator))

# stacked prop
df_1d %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(Year, as.numeric(Count.millions), fill = Country.Region)) + 
  geom_area(stat = "identity", position = "fill") +
  facet_grid(vars(Indicator))

# pie-chart
df_1d %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(x = "", y = as.numeric(Count.millions), fill = Country.Region)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme(axis.line = element_blank()) +
  coord_polar(theta = "y", start = 0) + 
  facet_grid(vars(Indicator))  


# Households with a computer and with Internet access

df_1e <- gather(
  filter(
    select(
      df,
      -'Developed',
      -'Developing', 
      -'World', 
      -'LDCs'
    ),
    Indicator == indicators[18] | Indicator == indicators[20]
  ),
  key = Country.Region,
  value = Count.millions,
  Africa, `Arab States`, `Asia & Pacific`, CIS, Europe, `The Americas`
)

# Comparison
# line plots comparison
ggplot(df_1e, aes(Year, Count.millions, fill = Country.Region, color = Country.Region)) +
  geom_line() + facet_wrap(vars(Indicator))

# bar chart comparison - ordered
df_1e %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = as.numeric(factor(Count.millions, levels = .$Count.millions))) %>%
  ggplot(aes(Year, Count.millions, fill = Country.Region, color = Country.Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(vars(Indicator))

# Ranking
# lollipop chart ranked
df_1e %>% 
  arrange(`Country.Region`) %>% 
  mutate(Count.millions = factor(Count.millions, levels = unique(.$Count.millions))) %>%
  ggplot(aes(Country.Region, as.numeric(Count.millions), fill = Country.Region)) +
  geom_point(size = 3, color = "red") +
  geom_segment(aes(x = Country.Region, xend = Country.Region, y = 0, yend = as.numeric(Count.millions))) +
  facet_grid(rows = vars(Indicator), cols = vars(Year)) + 
  coord_flip()

# Part-of-whole			   
# stacked bar chart
df_1e %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(Year, as.numeric(Count.millions), fill = factor(Country.Region))) +
  geom_bar(stat = "identity", position = "stack") + 
  facet_grid(vars(Indicator))

# stacked prop
df_1e %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(Year, as.numeric(Count.millions), fill = Country.Region)) + 
  geom_area(stat = "identity", position = "fill") +
  facet_grid(vars(Indicator))

# pie-chart
df_1e %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(x = "", y = as.numeric(Count.millions), fill = Country.Region)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme(axis.line = element_blank()) +
  coord_polar(theta = "y", start = 0) + 
  facet_grid(vars(Indicator))  


# Individuals using the Internet

df_1f <- gather(
  filter(
    select(
      df,
      -'Developed',
      -'Developing', 
      -'World', 
      -'LDCs'
    ),
    Indicator == indicators[22]
  ),
  key = Country.Region,
  value = Count.millions,
  Africa, `Arab States`, `Asia & Pacific`, CIS, Europe, `The Americas`
)

# Comparison
# line plots comparison
ggplot(df_1f, aes(Year, Count.millions, fill = Country.Region, color = Country.Region)) + 
  geom_line() + facet_wrap(vars(Indicator))

# bar chart comparison - ordered
df_1f %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = as.numeric(factor(Count.millions, levels = .$Count.millions))) %>%
  ggplot(aes(Year, Count.millions, fill = Country.Region, color = Country.Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(vars(Indicator))

# Ranking
# lollipop chart ranked
df_1f %>% 
  arrange(`Country.Region`) %>% 
  mutate(Count.millions = factor(Count.millions, levels = unique(.$Count.millions))) %>%
  ggplot(aes(Country.Region, as.numeric(Count.millions), fill = Country.Region)) +
  geom_point(size = 3, color = "red") +
  geom_segment(aes(x = Country.Region, xend = Country.Region, y = 0, yend = as.numeric(Count.millions))) +
  facet_grid(rows = vars(Indicator), cols = vars(Year)) + 
  coord_flip()

# Part-of-whole
# stacked bar chart
df_1f %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(Year, as.numeric(Count.millions), fill = factor(Country.Region))) +
  geom_bar(stat = "identity", position = "stack") + 
  facet_grid(vars(Indicator))

# stacked prop
df_1f %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(Year, as.numeric(Count.millions), fill = Country.Region)) + 
  geom_area(stat = "identity", position = "fill") +
  facet_grid(vars(Indicator))

# pie-chart
df_1f %>% 
  arrange(Country.Region) %>% 
  mutate(Count.millions = factor(Count.millions, levels = .$Count.millions)) %>%
  ggplot(aes(x = "", y = as.numeric(Count.millions), fill = Country.Region)) + 
  geom_bar(stat = "identity", position = "fill") +
  theme(axis.line = element_blank()) +
  coord_polar(theta = "y", start = 0) + 
  facet_grid(vars(Indicator))