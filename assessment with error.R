nss %>%
  filter(
    QuestGroup %in% c("Assessment and Feedback", "Overall Satisfaction"),
    Likert == "Agreement",
    Grp_RG == "Yes"
  ) %>%
  group_by(Provider, Year) %>%
  mutate(mean = mean(PercRespondents)) %>%
  ggplot(aes(
    x = str_remove_all(Provider, c("The|the|of|University|university")) %>% fct_reorder(mean),
    y = PercRespondents,
    colour = as.factor(Year),
    size = str_detect(Provider, "Edinburgh")
  )) +
  geom_point() +
  scale_size_manual(values = c(1.5, 3), guide = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_colour_manual(values = c(
    "2017" = "#b0bec5",
    "2018" = 	"#637f6a",
    "2019" = "#ac4f00"
  )) +
  labs(
    x = "Provider",
    y = "% Respondents Agreed with Question",
    title = "Assessment and Feedback for Russell Group",
    caption = "providers ordered by mean score, 95% CI shown",
    colour = "year"
  ) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.2)) +
  facet_wrap(~str_wrap(QuestText, 25)) +
  coord_flip() +
  geom_errorbar(aes(ymin=ConfMin,ymax=ConfMax))



library(ghibli)
nss %>%
  filter(
    QuestGroup %in% c("Overall Satisfaction"),
    Likert == "Agreement",
    Grp_RG == "Yes"
  ) %>%
  group_by(Provider, Year) %>%
  mutate(mean = mean(PercRespondents)) %>%
  ggplot(aes(
    x = str_remove_all(Provider, c("The|the|of|University|university")) %>% fct_reorder(mean),
    y = PercRespondents,
    fill = as.factor(Year),
    colour = as.factor(Year)
  )) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_ghibli_d(name = "LaputaLight", direction = -1) +
  labs(
    x = "Provider",
    y = "% Respondents Agreed with Question",
    title = "NSS Overall Satisfaction for Russell Group",
    caption = "providers ordered by mean score, 95% CI shown",
    fill = "Year"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_errorbar(aes(ymin=ConfMin,ymax=ConfMax, color = as.factor(Year)), position = position_dodge2(preserve = "single")) +
  scale_color_ghibli_d(name= "LaputaMedium", direction = -1, guide = FALSE) +
  coord_flip()





nss %>%
  filter(
    QuestGroup %in% c("Overall Satisfaction"),
    Likert == "Agreement",
    Grp_RG == "Yes"
  ) %>%
  group_by(Provider, Year) %>%
  mutate(mean = mean(PercRespondents)) %>%
  ggplot(aes(
    x = Year,
    y = PercRespondents,
    fill = as.factor(Year),
    colour = as.factor(Year)
  )) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_ghibli_d(name = "LaputaLight", direction = -1) +
  labs(
    x = "Provider",
    y = "% Respondents Agreed with Question",
    title = "NSS Overall Satisfaction for Russell Group",
    caption = "providers ordered by mean score, 95% CI shown",
    fill = "Year"
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_errorbar(aes(ymin=ConfMin,ymax=ConfMax, color = as.factor(Year)), position = position_dodge2(preserve = "single")) +
  scale_color_ghibli_d(name= "LaputaMedium", direction = -1, guide = FALSE) +
  facet_wrap(vars(str_remove_all(Provider, c("The|the|of|University|university")) %>% fct_reorder(mean)), ncol =2) +
  coord_flip()














nss %>%
  filter(
    QuestGroup %in% c("Overall Satisfaction"),
    Likert == "Agreement",
    Grp_RG == "Yes"
  ) %>%
  group_by(Provider, Year) %>%
  mutate(mean = mean(PercRespondents)) %>%
  ggplot(aes(
    x = str_remove_all(Provider, c("The|the|of|University|university")) %>% fct_reorder(mean),
    y = PercRespondents,
    fill = as.factor(Year)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Provider",
    y = "% Respondents Agreed with Question",
    title = "Overall Satisfaction",
    caption = "providers ordered by mean score, 95% CI shown") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
 # facet_wrap(~Year) +
  theme(legend.position = c(0.85, 0.2)) +
  geom_errorbar(aes(ymin=ConfMin,ymax=ConfMax), position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
