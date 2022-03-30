#Run this code after "Prediction_resistance" and "bootstrapping_cleaned_code"
library(ggplot2)
library(gapminder)
library(patchwork)


combined_figure  = (without_bootstrap + proportion_resistance) / (human_graph + trend_total)

combined_figure 
ggsave("../Figures/bootstrap_4datasample_combined.png",width = 30, height = 15, units = "cm",dpi = 300)


Combined_trend6 = (trend_1 + trend_2 + trend_3) / (trend_4 + trend_5 + trend_6)
Combined_trend6 
ggsave("../Figures/trend_prediction6_graphs.png",width = 30, height = 15, units = "cm", dpi = 300)

Combined_trend6_jitter = (trend_1 + trend_2 ) / (trend_3 + trend_4) / (trend_5 + trend_6)
Combined_trend6_jitter
ggsave("../Figures/trend_prediction6_jitter.png",width = 30, height = 25, units = "cm", dpi = 300)
