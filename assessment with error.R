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
    "2017" = "#C3CDC5",
    "2018" = "#0C507D"
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
