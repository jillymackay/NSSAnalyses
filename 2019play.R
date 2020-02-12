RG <- nss %>%
  filter (Provider %in% c("The University of Birmingham",
                          "University of Bristol",
                          "Cardiff University",
                          "University of Durham",
                          "University of Edinburgh",
                          "University of Exeter",
                          "University of Glasgow",
                          "King's College London",
                          "The University of Leeds",
                          "The University of Liverpool",
                          "The University of Manchester",
                          "University of Newcastle upon Tyne",
                          "University of Nottingham, The",
                          "The University of Sheffield",
                          "University College London",
                          "The University of Warwick",
                          "The University of York", 
                          "University of Southampton",
                          "Queen Mary University of London",
                          "Queen's University of Belfast",
                          "The London School of Economics and Political Science")) %>%
  mutate (Strike2018 = case_when (Provider %in% c( "University of Bristol",
                                                   "Cardiff University",
                                                   "University of Durham",
                                                   "University of Edinburgh",
                                                   "University of Exeter",
                                                   "University of Glasgow",
                                                   "King's College London",
                                                   "The University of Leeds",
                                                   "The University of Liverpool",
                                                   "The University of Manchester",
                                                   "University of Newcastle upon Tyne",
                                                   "University of Nottingham, The",
                                                   "The University of Sheffield",
                                                   "University College London",
                                                   "The University of Warwick",
                                                   "The University of York", 
                                                   "University of Southampton",
                                                   "Queen Mary University of London",
                                                   "Queen's University of Belfast") ~ "IA Taken")) %>% 
  filter (Likert %in% c("StronglyDisagree", "Disagree", "Neither", "Agree", "StronglyAgree")) %>% 
    select(-Grp_CG, -Grp_GW4, -Grp_MillPlus, -Grp_ABSA, -Grp_N8, -Grp_NCUK, -Grp_Oxbrg, -Grp_SES, -Grp_UniAlli,-Grp_WhiteRose, -Grp_The1994, -Grp_RG) %>%
  mutate(Likert = as.character(Likert)) %>% 
  mutate_at(.vars = vars(Likert),
            parse_factor, levels = c("StronglyDisagree",
                                     "Disagree",
                                     "Neither",
                                     "Agree",
                                     "StronglyAgree"))
 

RG %>% 
  filter(Question == "Q27") %>% 
  ggplot (aes (x = Provider, y = PercRespondents, fill = as.factor(Year))) +
  geom_bar (stat = "identity", position = position_dodge2(preserve = "total")) +
  labs (title = "Overall Satisfaction per subject in the Russell Group (2018) \n Imperial College London, Cambridge and Oxford not included",
        y = "Overall Satisfaction (% Agree)",
        x = "Provider") +
  coord_flip() +
  theme_classic()


RG %>% 
  filter (Question == "Q27",
          Likert %in% c("StronglyDisagree", "Disagree", "Neither", "Agree", "StronglyAgree"),
          Provider == "University of Edinburgh") %>% 
  mutate(Likert = as.character(Likert)) %>% 
  mutate_at(.vars = vars(Likert),
            parse_factor, levels = c("StronglyAgree",
                                     "Agree",
                                     "Neither",
                                     "Disagree",
                                     "StronglyDisagree")) %>% 
  ggplot(aes(x = Year, y = PercRespondents, fill = Likert)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "NSS Overall Satisfaction",
       y = "Percentage Respondents") +
  scale_fill_manual(values = miami2() ) +
  theme_classic() +
  theme(legend.position = "top") 
