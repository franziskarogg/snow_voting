library(ggplot2)
library(fixest)
library(terra)
library(sf)
library(tidyverse)
library(modelsummary)

#Load Datasets
load("/Users/Franzi/Desktop/snow_voting/snow_voting/create_data/datasets.RData")
munic24 = st_read("/Users/Franzi/Desktop/snow_voting/snow_voting/input_data/STATISTIK_AUSTRIA_GEM_20240101/STATISTIK_AUSTRIA_GEM_20240101.shp")

df_mean$pop_foreign = as.numeric(gsub(",", ".", df_mean$pop_foreign))
df_max$pop_foreign = as.numeric(gsub(",", ".", df_max$pop_foreign))
#-------------------------------------------------------------------------------
#Descriptives 
#-------------------------------------------------------------------------------

hist(df_mean$population) 
#=> Log transformation of Population p. Municipality
df_mean = df_mean %>% 
  mutate(log_pop = log(population))
         
df_max = df_max %>% 
  mutate(log_pop = log(population))


#Comparing Top Quantile in Mean_2324 with Bottom Quantile in Mean_2324
df_mean = df_mean %>%
  mutate(snow_group = case_when(
    mean_2324 <= quantile(mean_2324, 0.25, na.rm = TRUE) ~ "Bottom 25%",
    mean_2324 >= quantile(mean_2324, 0.75, na.rm = TRUE) ~ "Top 25%",
    TRUE ~ "Middle 50%"
  ))











#-------------------------------------------------------------------------------
#Fixed effects 
#-------------------------------------------------------------------------------

m1 = feols(fpö_share ~ anomaly + altitude  + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu | district_id, 
           data = df_mean, 
           vcov = ~district_id)
summary(m1)

m2 = feols(fpö_share ~ mean_2324 + altitude  + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu | district_id, 
           data = df_mean, 
           vcov = ~district_id)
summary(m2)
m2b = feols (fpö_share ~ mean_2324*altitude  + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu | district_id, 
             data = df_mean, 
             vcov = ~district_id)


m3 = feols(greens_share ~ anomaly + altitude  + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu | district_id, 
           data = df_mean, 
           vcov = ~district_id)
summary(m3)

m4 = feols(greens_share ~ mean_2324 + altitude  + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu | district_id, 
           data = df_mean, 
           vcov = ~district_id)
summary(m4)


#-------------------------------------------------------------------------------
#Regression Models 
#-------------------------------------------------------------------------------

mr1 = lm (fpö_share ~ anomaly + altitude  + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu, data  = df_mean)
summary(mr1)

mr2 = lm (fpö_share ~ mean_2324 + altitude  + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu, data  = df_mean)
summary(mr2)
mr2b = lm(fpö_share ~ mean_2324*altitude  + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu, data  = df_mean)

mr3 = lm (greens_share ~ anomaly + altitude  + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu, data  = df_mean)
summary(mr3)

mr4 = lm (greens_share ~ mean_2324 + altitude  + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu, data  = df_mean)
summary(mr4)


#-------------------------------------------------------------------------------
#Robustness 
#-------------------------------------------------------------------------------

r1 = feols(fpö_share ~ max_2324 + altitude  + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu | district_id, 
             data = df_max, 
             vcov = ~district_id)
summary(r1)

r2 = feols(greens_share ~ max_2324 + altitude + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu | district_id, 
           data = df_max, 
           vcov = ~district_id)
summary(r2)

r3 = lm (fpö_share ~ max_2324 + altitude  + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu, 
         data  = df_max)
summary(r3)

r4 = lm(greens_share ~ max_2324 + altitude + turnout + log_pop + pop_over65 + pop_foreign + unemp_rate + share_tertiary_edu, 
        data = df_max)
summary(r4)






#-------------------------------------------------------------------------------
#Modelsummaries 
#-------------------------------------------------------------------------------

library(modelsummary)
library(sandwich)

# Table 1 - Anomaly
modelsummary(list("OLS FPÖ" = mr1, 
                  "FE FPÖ" = m1, 
                  "OLS Greens" = mr3, 
                  "FE Greens" = m3),
             stars = TRUE,
             vcov = list("HC1", ~district_id, "HC1", ~district_id),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "./tables/table_anomaly.tex")

# Table 2 - Mean Snow
modelsummary(list("OLS FPÖ" = mr2, 
                  "FE FPÖ" = m2, 
                  "OLS Greens" = mr4, 
                  "FE Greens" = m4),
             stars = TRUE,
             vcov = list("HC1", ~district_id, "HC1", ~district_id),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "./tables/table_mean.tex")

# Table 3 - Interaction - Heterogeneity across Altitude 
modelsummary(list("OLS FPÖ" = mr2b, 
                  "FE FPÖ" = m2b),
             stars = TRUE,
             vcov = list("HC1", ~district_id),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "./tables/table_interaction.tex")

# Table 4 - Robustness
modelsummary(list("OLS FPÖ" = r3, 
                  "FE FPÖ" = r1, 
                  "OLS Greens" = r4, 
                  "FE Greens" = r2),
             stars = TRUE,
             vcov = list("HC1", ~district_id, "HC1", ~district_id),
             gof_map = c("nobs", "r.squared", "adj.r.squared"),
             output = "./tables/table_robustness.tex")


#-------------------------------------------------------------------------------
#Figures 
#-------------------------------------------------------------------------------
plotformat = theme(
  plot.background = element_rect(fill = "white", color = NA),
  legend.key.size = unit(0.4, "cm"),
  legend.text = element_text(size = 7),
  legend.title = element_text(size = 8)
)


#Plotting mean snow-depth across municipalities, ANOMALY
p1 =  ggplot(snow_sf_mean) +
  geom_sf(aes(fill = anomaly), color = NA) +
  scale_fill_gradient2(
    low = "#a50f15",    # dark red = negative anomaly (less snow than usual)
    mid = "#f0f0f0",    # white = zero (normal)
    high = "#08306b",   # dark blue = positive anomaly (more snow than usual)
    midpoint = 0, 
    name = "Anomaly (m)"
  ) +
  theme_void() + plotformat

ggsave("./plots/snow_anomaly_map.png", plot = p1, width = 5, height = 3, dpi = 300)

#MEAN 23/24
p2 = ggplot(snow_sf_mean) +
  geom_sf(aes(fill = mean_2324), color = NA) +
  scale_fill_gradient2(
    low = "#08306b", mid = "#6baed6", high = "#deebf7",
    midpoint = 0, name = "Mean Snow Depth 23/24 (m)"
  ) +
  theme_void() + plotformat

ggsave("./plots/snow_mean_map.png", plot = p2, width = 5, height = 3, dpi = 300)

#Vote Shares of the National Election
p7 = df_mean %>%
  summarise(
    FPÖ = mean(fpö_share, na.rm = TRUE),
    Greens = mean(greens_share, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "party", values_to = "share") %>%
  ggplot(aes(x = party, y = share, fill = party)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("FPÖ" = "#003f7f", "Greens" = "#006d2c")) +
  theme_minimal() +
  labs(x = "", y = "Average Vote Share per Municipality (%)") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA))
ggsave("./plots/vote_shares_bar.png", plot = p7, width = 4, height = 3, dpi = 300)

##Plotting vote-shares across municipalities
munic24_votes = munic24 %>% 
  left_join(df_mean %>% select(muni_id , fpö_share, greens_share), 
            by = c("g_id" = "muni_id"))
summary(munic24_votes$fpö_share)
summary(munic24_votes$greens_share)

#FPÖ Map
p5 = ggplot(munic24_votes) +
  geom_sf(aes(fill = fpö_share), color = NA) +
  scale_fill_gradient(low = "#fee5d9", high = "#a50f15", name = "FPÖ Share (%)") +
  theme_void() + plotformat
          
p6 = ggplot(munic24_votes) +
  geom_sf(aes(fill = greens_share), color = NA) +
  scale_fill_gradient(low = "#e5f5e0", high = "#006d2c", name = "Greens Share (%)") +
  theme_void() + plotformat
          
ggsave("./plots/fpö_share_map.png", plot = p5, width = 5, height = 3, dpi = 300)
ggsave("./plots/greens_share_map.png", plot = p6, width = 5, height = 3, dpi = 300)

#Plots for vote shares in regions with high/low mean snow-depth 
p3 = ggplot(df_mean  %>% filter(snow_group != "Middle 50%"),
            aes(x = snow_group, y = fpö_share, fill = snow_group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Bottom 25%" = "#08306b", "Top 25%" = "#deebf7")) +
  theme_minimal() +
  labs(x = "Grouped Means of Snow Depth 23/24", y = "FPÖ Share (%)") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA))


#p4 = ggplot(df_mean, aes(x = mean_2324, y = fpö_share, color = region)) +
 # geom_point(alpha = 0.3) +
 # geom_smooth(method = "loess") +  # flexible trend, no linearity assumption
  #theme_minimal() +
  #labs(x = "Mean Snow Depth 2023/24", y = "FPÖ Share (%)")

ggsave("./plots/boxplot_fpö_snow.png", plot = p3, width = 5, height = 3, dpi = 300)
#ggsave("./plots/scatter_fpö_snow.png", plot = p4, width = 5, height = 3, dpi = 300)

p7 = df_mean %>%
  summarise(FPÖ = mean(fpö_share, na.rm = TRUE),
            Greens = mean(greens_share, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "party", values_to = "share") %>%
  ggplot(aes(x = party, y = share, fill = party)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("FPÖ" = "#a50f15", "Greens" = "#006d2c")) +
  theme_minimal() +
  labs(x = "", y = "Mean Vote Share (%)") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA))

ggsave("./plots/vote_shares_bar.png", plot = p7, width = 4, height = 3, dpi = 300)