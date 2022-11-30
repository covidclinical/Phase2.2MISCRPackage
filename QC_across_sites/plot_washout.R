head(misc_cases)

ggplot2::ggplot(data=misc_cases, aes(x=as.Date(period), y=distinct_patients)) +
  geom_point() +
  #geom_bar( stat = "identity") +
  geom_vline(xintercept = as.Date("2021-07-01"), colour="orange", ,linetype = "longdash") +
  geom_vline(xintercept = as.Date("2021-07-15"), colour="orange") +
  geom_vline(xintercept = as.Date("2021-07-29"), colour="orange", linetype = "longdash") +
  geom_vline(xintercept = as.Date("2022-01-01"), colour="darkgreen", linetype = "longdash") +
  geom_vline(xintercept = as.Date("2022-01-15"), colour="darkgreen") +
  geom_vline(xintercept = as.Date("2022-01-29"), colour="darkgreen", linetype = "longdash") +
  ggtitle("MISC patients per day") +
  ylab("Number of distinct MISC patients") + xlab("By day") +
  scale_x_continuous(breaks = seq(from = min(misc_cases$period), to = max(misc_cases$period), by = 14)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
