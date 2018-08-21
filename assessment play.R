library(tidyverse)
library(nss)

library(NineteenEightyR)





dat <- nss %>%
  filter (QuestGroup %in% c ("Assessment and Feedback", "Overall Satisfaction"),
          Likert %in% c("StronglyDisagree", "Disagree", "Neither", "Agree", "StronglyAgree")) %>% 
  mutate (Grp_RG = case_when(Grp_RG %in% "Yes" ~ "Russell Group", TRUE ~ "Non Russell Group")) %>% 
  select(-Grp_CG, -Grp_GW4, -Grp_MillPlus, -Grp_ABSA, -Grp_N8, -Grp_NCUK, -Grp_Oxbrg, -Grp_SES, -Grp_UniAlli,-Grp_WhiteRose, -Grp_The1994) %>%
  mutate(Likert = as.character(Likert)) %>% 
  mutate_at(.vars = vars(Likert),
            parse_factor, levels = c("StronglyDisagree",
                                     "Disagree",
                                     "Neither",
                                     "Agree",
                                     "StronglyAgree"))




dat %>% 
  mutate(Likert = as.character(Likert)) %>% 
  mutate_at(.vars = vars(Likert),
            parse_factor, levels = c("StronglyAgree",
                                     "Agree",
                                     "Neither",
                                     "Disagree",
                                     "StronglyDisagree")) %>% 
  ggplot(aes(x = QuestText, y = PercRespondents, fill = Likert)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(limits = c("Overall, I am satisfied with the quality of the course",
                              "The criteria used in marking have been clear in advance",
                              "Marking and assessment has been fair",
                              "Feedback on my work has been timely",
                              "I have received helpful comments on my work")) +
  labs(x = "NSS Assessment and Feedback Questions",
       y = "Percentage Respondents") +
  scale_fill_manual(values = miami2() ) +
  theme_classic() +
  theme(legend.position = "top") +
  facet_wrap(vars(Year, Grp_RG), ncol = 2) +
  coord_flip()




nss %>%
  filter (QuestGroup %in% c ("Assessment and Feedback", "Overall Satisfaction"),
          Likert == "Agreement") %>%
  mutate (Grp_RG = case_when(Grp_RG %in% "Yes" ~ "Russell Group", TRUE ~ "Non Russell Group")) %>%
  ggplot (aes(x = fct_reorder(Provider, PercRespondents, .desc = TRUE), y = PercRespondents, 
              colour = QuestText, shape = QuestText)) +
  scale_shape_manual(values = c(16, 16, 16, 17, 16)) +
  scale_color_manual(values = c("thistle1","thistle2","thistle3","skyblue1", "thistle4")) +
  geom_point() + 
  facet_wrap(Year~Grp_RG, ncol =2) +
  labs (x = "Provider", y = "% Respondents Agreed with Question", title = "Assessment and Feedback for Russell Group\n and non Russell Group Unis") +
  theme(axis.text.x=element_blank(), legend.position = "left") +
  geom_ribbon(aes(ymin=ConfMin,ymax=ConfMax),alpha=0.3)



nss %>%
  filter (QuestGroup %in% c ("Assessment and Feedback", "Overall Satisfaction"),
          Likert == "Agreement", Grp_RG == "Yes") %>%
  ggplot (aes(x = Provider, y = PercRespondents, 
              colour = QuestText, shape = as.factor(Year))) +
  scale_shape_manual(values = c( 17, 16, 15)) +
  scale_color_manual(values = c("thistle1","thistle2","thistle3","skyblue1", "thistle4")) +
  geom_point() + 
  labs (x = "Provider", y = "% Respondents Agreed with Question", title = "Assessment and Feedback for Russell Group") +
  theme(legend.position = "right") +
  coord_flip() +
  geom_errorbar(aes(ymin=ConfMin,ymax=ConfMax))



nss %>%
  filter (QuestGroup %in% c ("Overall Satisfaction"),
          Likert == "Agreement", Grp_RG == "Yes") %>%
  ggplot (aes(x = Provider, y = PercRespondents, 
              colour = QuestText, shape = as.factor(Year))) +
  scale_shape_manual(values = c( 17, 16, 15)) +
  scale_color_manual(values = c("skyblue1")) +
  geom_point() + 
  labs (x = "Provider", y = "% Respondents Agreed with Question", title = "Assessment and Feedback for Russell Group") +
  theme(legend.position = "right") +
  coord_flip() +
  geom_errorbar(aes(ymin=ConfMin,ymax=ConfMax))
