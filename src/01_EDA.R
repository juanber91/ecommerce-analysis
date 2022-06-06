###############################################################
#
# Author: Juan Martínez Parente
# Date: June 2022
#
###############################################################

library(tidyr)
library(dplyr)
library(magrittr)
library(readr)
library(janitor)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggrepel)


# Loading datasets --------------------------------------------------------

files <- list.files('data', full.names = TRUE, recursive = TRUE)
names <- list.files('data', full.names = TRUE, recursive = FALSE)
names <- stringr::str_remove(names, 'data/')

for(i in 1:length(files)) {
  name <- names[i]
  assign(name, read_csv(files[i]))
}


# Some recoding coercing --------------------------------------------------

users %<>%
  mutate(age_group = case_when(age < 18 ~ '17 or younger',
                               age < 30 ~ '18 to 29',
                               age < 50 ~ '30 to 49',
                               age < 65 ~ '50 to 64',
                               TRUE ~ '65 or older'))

products %<>%
  mutate(gap_n = retail_price - cost,
         gap_p = retail_price/cost - 1)

orders %<>%
  mutate(across(contains('_at'), ~as.Date(str_sub(., 1, 10))))

order_items %<>%
  mutate(across(contains('_at'), ~as.Date(str_sub(., 1, 10))))


# EDA ---------------------------------------------------------------------

### User demographics

users %>% tabyl(age_group)
users %>% tabyl(gender)
users %>% tabyl(gender, age_group) %>% janitor::adorn_percentages()

length(unique(users$city))

users %>%
  filter(city != 'null') %>%
  count(country, city) %>%
  arrange(-n) %>%
  mutate(percent = n/sum(n)) %>%
  mutate(cumsum = cumsum(percent)) %>%
  top_n(100, n) %>%
  count(country)

top_countries <- users %>%
  tabyl(country) %>%
  arrange(-n) %>%
  top_n(5, percent) %>%
  mutate(cumsum = cumsum(percent))

top_countries


### Orders

order_detail <- order_items %>%
  left_join(users %>%
              select(id, age_group, gender, country, city),
            by = c('user_id' = 'id')) %>%
  left_join(products %>%
              select(id, category, brand, name, retail_price, department),
            by = c('product_id' = 'id'))


# Countries of origin
order_detail %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  mutate(p = n/sum(n),
         cumsum = cumsum(p))


# Cities of origin
order_detail %>%
  group_by(city) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  mutate(p = n/sum(n),
         cumsum = cumsum(p))

# Mean retail price by gender
order_detail %>%
  group_by(gender, order_id) %>%
  summarise(retail_price = sum(retail_price)) %>%
  group_by(gender) %>%
  summarise(retail_price = mean(retail_price))


# 2022 data incomplete, so we use YTD data

orders_by_year <-
  orders %>%
  filter(yday(created_at) <= yday(max(orders$created_at))) %>%
  mutate(year = as.factor(year(created_at))) %>%
  group_by(year) %>%
  summarise(orders = n())

orders_by_year %>%
  ggplot() +
  geom_col(aes(year, orders, fill = year)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = rev(c('#0070c0', '#00b050', '#c00000', '#ffcc00'))) +
  labs(x = '', y = '', title = '') +
  theme_test() +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(size = 0.1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        # axis.title = element_blank(),
        legend.position = 'none')

ggsave('YTD.png', width = 14, height = 14, units = 'cm')


# Unit sales time series
order_detail %>%
  filter(created_at <= '2022-06-05',
         country %in% top_countries$country) %>%
  group_by(country, week = floor_date(created_at, '1 week', 1)) %>%
  summarise(n = n(), .groups = 'drop') %>%
  ggplot() +
  geom_line(aes(week, n, color = country), size = 0.5) +
  scale_y_continuous(labels = scales::comma,
                     breaks = 800*1:10) +
  scale_color_manual(values = c('#0070c0', '#00b050', '#c00000', '#7030a0', '#ed7d31')) +
  labs(x = '', y = 'Units', title = '', color = '') +
  theme_test() +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(size = 0.1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'bottom')

ggsave('unitsales2.png', width = 15, height = 7.5, units = 'cm')


# Unit sales YTD growth

order_detail %>%
  filter(yday(created_at) <= yday(max(orders$created_at))) %>%
  filter(country %in% top_countries$country) %>%
  group_by(year(created_at)) %>%
  summarise(retail_price = sum(retail_price)) %>%
  mutate(lag = lag(retail_price),
         growth = retail_price/lag-1)

order_detail %>%
  filter(yday(created_at) <= yday(max(orders$created_at))) %>%
  filter(country %in% top_countries$country) %>%
  group_by(country, year = year(created_at)) %>%
  summarise(quantity = n()) %>%
  mutate(lag = lag(quantity),
         growth = quantity/lag-1) %>%
  select(-quantity, - lag) %>%
  pivot_wider(names_from = year, values_from = growth)


# Time elapsed between order creation and delivery
orders %>%
  left_join(users %>% select(id, country), by = c('user_id' = 'id')) %>%
  mutate(lag1 = as.numeric(shipped_at - created_at),
         lag2 = as.numeric(delivered_at - shipped_at)) %>%
  # group_by(country) %>%
  summarise(lag1 = mean(lag1, na.rm = T),
            lag2 = mean(lag2, na.rm = T)) %>%
  mutate(lag3 = lag1 + lag2) %>%
  arrange(lag3)

# Number of orders per user
orders %>%
  count(user_id) %>%
  arrange(-n) %>%
  tabyl(n)

# Order status distribution
orders %>%
  filter(status %in% c('Cancelled', 'Complete', 'Returned')) %>%
  tabyl(status)

orders %>%
  filter(status %in% c('Cancelled', 'Complete', 'Returned')) %>%
  mutate(month_created = floor_date(created_at, '1 month')) %>%
  count(month_created, status) %>%
  group_by(month_created) %>%
  mutate(p = n/sum(n)) %>%
  ggplot() +
  geom_line(aes(month_created, p, color = status)) +
  scale_y_continuous(labels = scales::percent_format(1)) +
  scale_color_manual(values = c('#0070c0', '#00b050', '#c00000', '#7030a0', '#ed7d31')) +
  labs(x = '', y = '% of orders', title = '', color = '') +
  theme_test() +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(size = 0.1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'bottom')

ggsave('cancelled.png', width = 13, height = 10, units = 'cm')


### Products

# Product category analysis

products %>%
  group_by(category) %>%
  summarise(dif = mean(gap_n),
            pct = mean(gap_p)) %>%
  arrange(-pct) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(pct, reorder(category, pct)), fill = '#0070c0') +
  geom_text(aes(pct, reorder(category, pct),
                label = scales::percent(pct, 0.1)),
            hjust = -0.1) +
  scale_x_continuous(labels = scales::percent_format(1), expand = c(0,0,0,0.3)) +
  labs(x = '', y = '', title = '', fill = '') +
  theme_test() +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'bottom')

ggsave('categories.png', width = 15, height = 18, units = 'cm')

products %>%
  group_by(category) %>%
  summarise(dif = mean(gap_n),
            pct = mean(gap_p)) %>%
  arrange(-pct) %>%
  ungroup() %>%
  left_join(order_detail %>%
              group_by(category) %>%
              summarise(n = n()) %>%
              ungroup() %>%
              mutate(p = n/sum(n),
                     cumsum = cumsum(p))) %>%
  ggplot() +
  geom_vline(aes(xintercept = mean(pct)), color = 'grey') +
  geom_hline(aes(yintercept = mean(p)), color = 'grey') +
  geom_point(aes(pct, p), color = '#0070c0') +
  geom_text_repel(aes(pct, p, label = category), size = 3) +
  scale_x_continuous(labels = scales::percent_format(1)) +
  scale_y_continuous(labels = scales::percent_format(1)) +
  labs(x = 'Mean profit',
       y = 'Sales mix',
       title = '') +
  theme_test() +
  theme(axis.ticks = element_line(color = 'grey'))

ggsave('map.png', width = 17, height = 16, units = 'cm')






