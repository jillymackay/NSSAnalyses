# Russell Group Universities

# Excludes University of Cambridge, University of Oxford, Imperial College London
#  because I can't find them in the data

RG18 <- read_excel("Data/NSS_taught_all18.xlsx", 
                    sheet = "NSS", skip = 3) %>%
  rename (StronglyDisagree = "Answered 1",
          Disagree = "Answered 2",
          Neither = "Answered 3",
          Agree = "Answered 4",
          StronglyAgree = "Answered 5",
          Question = "Question Number") %>%
  mutate (Agreement = Agree + StronglyAgree,
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
                            sheet = "NSS1", skip = 3) %>%
  rename (StronglyDisagree = "Answered 1",
          Disagree = "Answered 2",
          Neither = "Answered 3",
          Agree = "Answered 4",
          StronglyAgree = "Answered 5",
          Question = "Question Number") %>%
  mutate (Agreement = Agree + StronglyAgree,
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
                          "The London School of Economics and Political Science"))%>%
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
        x = "Subject (JACS Level 1)") +
  coord_flip() +
  theme_classic()


RGThemes <- RG18 %>%
  select (Question, Agreement, Provider, Strike) %>%
  spread (key = Question, value = Agreement) %>%
  rowwise() %>%
  mutate ("Teaching On My Course" = mean(c(Q01, Q02, Q03, Q04)),
          "Learning Opportunities" = mean(c(Q05, Q06, Q07)),
          "Assessment and Feedback" = mean(c(Q08, Q09, Q10, Q11)),
          "Academic Support" = mean(c(Q12, Q13, Q14)),
          "Organisation and Management" = mean(c(Q15, Q16, Q17)),
          "Learning Resources" = mean(c(Q18, Q19, Q20)),
          "Learning Community" = mean(c(Q21, Q22)),
          "Student Voice" = mean(c(Q22, Q24, Q25)),
          "Student Union" = Q26,
          "Overall Satisfaction" = Q27)


rgteach <- ggplot  (aes(x =as.numeric(as.character(`Teaching On My Course`)), y = Q27), data = RGThemes) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  scale_x_continuous() +
  expand_limits(x=0, y=0) +
  xlab("Teaching on My Course") +
  ylab("% Overall Satisfaction")

rglearnopportunity <- rgteach +
  aes(x =as.numeric(as.character(`Learning Opportunities`)), y = Q27) +
  xlab("Learning Opportunities") +
  ylab("% Overall Satisfaction")

rgaandf <- rgteach +
  aes(x =as.numeric(as.character(`Assessment and Feedback`)), y = Q27) +
  xlab("Assessment and Feedback") +
  ylab("% Overall Satisfaction")

rgAcademicSupport <- rgteach +
  aes(x =as.numeric(as.character(`Academic Support`)), y = Q27) +
  xlab("Academic Support") +
  ylab("% Overall Satisfaction")

rgoandm <- rgteach +
  aes(x =as.numeric(as.character(`Organisation and Management`)), y = Q27) +
  xlab("Organisation and Management") +
  ylab("% Overall Satisfaction")

rglearnresource <- rgteach +
  aes(x =as.numeric(as.character(`Learning Resources`)), y = Q27) +
  xlab("Learning Resources") +
  ylab("% Overall Satisfaction")

rglearncommunity <- rgteach +
  aes(x =as.numeric(as.character(`Learning Community`)), y = Q27) +
  xlab("Learning Community") +
  ylab("% Overall Satisfaction")

rgstudentvoice <- rgteach +
  aes(x =as.numeric(as.character(`Student Voice`)), y = Q27) +
  xlab("Student voice") +
  ylab("% Overall Satisfaction")

rgstudentunion <- rgteach +
  aes(x =as.numeric(as.character(`Student Union`)), y = Q27) +
  xlab("Student Union") +
  ylab("% Overall Satisfaction")


RGProvSatis <- grid.arrange(rgteach, rglearnopportunity, rgaandf, rgAcademicSupport, rgoandm, rglearnresource, rglearncommunity, rgstudentvoice, rgstudentunion, top = "NSS 2018 - Relationship between theme and overall satisfaction across Russell Group \n Imperial College London, Cambridge and Oxford not included ")
RGProvSatis

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
