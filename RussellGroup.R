# Russell Group Universities

# Excludes University of Cambridge, University of Oxford, Imperial College London
#  because I can't find them in the data

RG18 <- read_excel("Data/NSS_taught_all18.xlsx", 
                    sheet = "NSS", skip = 3, col_types = "text") %>%
  rename (StronglyDisagree = "Answered 1",
          Disagree = "Answered 2",
          Neither = "Answered 3",
          Agree = "Answered 4",
          StronglyAgree = "Answered 5",
          Question = "Question Number") %>%
  mutate (StronglyDisagree = parse_double(StronglyDisagree),
          Disagree = parse_double(Disagree),
          Neither = parse_double(Neither),
          Agree = parse_double (Agree),
          StronglyAgree = parse_double (StronglyAgree),
          Agreement = Agree + StronglyAgree,
          Question = as_factor(Question)) %>%
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
  mutate (Strike = case_when (Provider %in% c( "University of Bristol",
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
                                               "Queen's University of Belfast") ~ "IA Taken"))


RG18Subjects <- read_excel("Data/NSS_taught_all18.xlsx", 
                            sheet = "NSS1", skip = 3, col_types = "text") %>%
  rename (StronglyDisagree = "Answered 1",
          Disagree = "Answered 2",
          Neither = "Answered 3",
          Agree = "Answered 4",
          StronglyAgree = "Answered 5",
          Question = "Question Number") %>%
  mutate (StronglyDisagree = parse_double(StronglyDisagree),
          Disagree = parse_double(Disagree),
          Neither = parse_double(Neither),
          Agree = parse_double (Agree),
          StronglyAgree = parse_double (StronglyAgree),
          Agreement = Agree + StronglyAgree,
          Question = as_factor(Question)) %>%
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
                          "The London School of Economics and Political Science"))






RGSubjects <- RG18Subjects  %>%
  select (Question, Agreement, Subject, Provider) %>%
  gather (variable, Agreement, -(Provider:Question)) %>%
  unite (temp, Question) %>%
  group_by (temp) %>%
  mutate (id = 1:n()) %>%
  spread (temp, Agreement) 


RGSubjects %>%
  ggplot (aes (x = Provider, y = Q27), fill = Subject) +
  geom_boxplot () +
  geom_jitter() +
  labs (title = "Overall Satisfaction per subject in the Russell Group (2018) \n Imperial College London, Cambridge and Oxford not included",
        y = "Overall Satisfaction (% Agree)",
        x = "Provider") +
  coord_flip() +
  theme_classic()



RGThemes %>%
  ggplot (aes (x = fct_reorder(Provider, Q27, .desc = TRUE), y = Q27), color = Strike) + 
  geom_point() +
  theme (axis.text.x = element_text (angle = 90, hjust = 1)) +
  labs (x = "Teaching Provider",
        y = "Total Agreement Overall Satisfaction",
        title = "Overall Satisfaction for the Russell Group in 2018",
        subtitle = "Excluding Imperial College London, Cambridge and Oxford")


RGSubjects %>%
  ggplot (aes (x = Provider, y = Q27, fill = Subject)) +
  geom_boxplot() +
  geom_violin(trim = FALSE) +
  geom_dotplot(binaxis = 'y', stackdir = 'center',
               stackratio = 1.5, dotsize = 0.5) +
  theme (legend.position = "none",
         axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs (y = "Overall Satisfaction") 
