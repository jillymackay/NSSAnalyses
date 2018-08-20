library (tidyverse)
library (readxl)
library (forcats)
library(gridExtra)
library (reshape2)




# Data Processing
NSS18 <- read_excel("Data/NSS_taught_all18.xlsx", 
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
          Question = as_factor(Question))
# Use parse_double because parse_number gets confused with some values and reads them in as >1 (who know why?!)



NSS18Subjects <- read_excel("Data/NSS_taught_all18.xlsx", 
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
          Question = as_factor(Question))  %>%
  gather (key = "Likert", value = "PercRespondents",
          -"UKPRN",                     
          -"Provider",                  
          -"Subject Code",              
          -"Subject",                  
          -"Level",                     
          -"Question",
          -"Confidence interval - min", 
          -"Actual value",
          -"Confidence interval - max", 
          -"Response",                 
          -"Sample Size",              
          -"Two years aggregate data?")










# Sector Level Visualisations

SectorThemes <- NSS18 %>%
  select (Question, Agreement, Provider) %>%
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


agteach <- ggplot  (aes(x =as.numeric(as.character(`Teaching On My Course`)), y = Q27), data = SectorThemes) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  scale_x_continuous() +
  expand_limits(x=0, y=0) +
  xlab("Teaching on My Course") +
  ylab("% Overall Satisfaction")

aglearnopportunity <- agteach +
  aes(x =as.numeric(as.character(`Learning Opportunities`)), y = Q27) +
  xlab("Learning Opportunities") +
  ylab("% Overall Satisfaction")

agaandf <- agteach +
  aes(x =as.numeric(as.character(`Assessment and Feedback`)), y = Q27) +
  xlab("Assessment and Feedback") +
  ylab("% Overall Satisfaction")

agAcademicSupport <- agteach +
  aes(x =as.numeric(as.character(`Academic Support`)), y = Q27) +
  xlab("Academic Support") +
  ylab("% Overall Satisfaction")

agoandm <- agteach +
  aes(x =as.numeric(as.character(`Organisation and Management`)), y = Q27) +
  xlab("Organisation and Management") +
  ylab("% Overall Satisfaction")

aglearnresource <- agteach +
  aes(x =as.numeric(as.character(`Learning Resources`)), y = Q27) +
  xlab("Learning Resources") +
  ylab("% Overall Satisfaction")

aglearncommunity <- agteach +
  aes(x =as.numeric(as.character(`Learning Community`)), y = Q27) +
  xlab("Learning Community") +
  ylab("% Overall Satisfaction")

agstudentvoice <- agteach +
  aes(x =as.numeric(as.character(`Student Voice`)), y = Q27) +
  xlab("Student voice") +
  ylab("% Overall Satisfaction")

agstudentunion <- agteach +
  aes(x =as.numeric(as.character(`Student Union`)), y = Q27) +
  xlab("Student Union") +
  ylab("% Overall Satisfaction")


ProviderSatisfaction <- grid.arrange(agteach, aglearnopportunity, agaandf, agAcademicSupport, agoandm, aglearnresource, aglearncommunity, agstudentvoice, agstudentunion, top = "NSS 2018 - Relationship between theme and overall satisfaction across All UK ")
ProviderSatisfaction 




q1 <- ggplot  (aes(x =Q01, y = Q27), data = SectorThemes) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  scale_x_continuous() +
  expand_limits(x=0, y=0) +
  xlab("Staff Are Good At Explaining Things") +
  ylab("% Overall Satisfaction")

q2 <- q1 +
  aes(x =Q02, y = Q27) +
  xlab("Staff Have Made The Subject Interesting") +
  ylab("% Overall Satisfaction")

q3 <- q1 +
  aes(x =Q03, y = Q27) +
  xlab("The Course Is Intellectually Stimulating") +
  ylab("% Overall Satisfaction")

q4 <- q1 +
  aes(x =Q04, y = Q27) +
  xlab("Staff Have Made The Subject Interesting") +
  ylab("% Overall Satisfaction")

teaching <- grid.arrange(q1, q2, q3, q4, top = "NSS 2018 - Relationship between question and overall satisfaction across Providers")
teaching

q5 <- q1 +
  aes(x =Q05, y = Q27) +
  xlab("My course has provided me with opportunities to explore ideas or concepts in depth") +
  ylab("% Overall Satisfaction")

q6 <- q1 +
  aes(x =Q06, y = Q27) +
  xlab("My course has provided me with opportunities to bring information and ideas together from different topics") +
  ylab("% Overall Satisfaction")

q7 <- q1 +
  aes(x =Q04, y = Q27) +
  xlab("My course has provided me with opportunities to apply what I have learnt") +
  ylab("% Overall Satisfaction")

learnopps<- grid.arrange(q5, q6, q7, top = "NSS 2018 - Relationship between question and overall satisfaction across Provider")
learnopps


q8 <- q1 +
  aes(x =Q08, y = Q27) +
  xlab("The criteria used in marking have been clear in advance") +
  ylab("% Overall Satisfaction")


q9 <- q1 +
  aes(x =Q09, y = Q27) +
  xlab("Marking and assessment has been fair") +
  ylab("% Overall Satisfaction")


q10 <- q1 +
  aes(x =Q10, y = Q27) +
  xlab("Feedback on my work has been timely") +
  ylab("% Overall Satisfaction")


q11 <- q1 +
  aes(x =Q11, y = Q27) +
  xlab("I have received helpful comments on my work") +
  ylab("% Overall Satisfaction")

aandf<- grid.arrange(q8, q9, q10, q11, top = "NSS 2018 - Relationship between question and overall satisfaction across Providers")
aandf



# Subject Level Comparisons
SubjectThemes <- NSS18Subjects %>%
  select (Question, Agreement, Subject, Provider) %>%
  gather (variable, Agreement, -(Provider:Question)) %>%
  unite (temp, Question) %>%
  group_by (temp) %>%
  mutate (id = 1:n()) %>%
  spread (temp, Agreement) 


sq1 <- ggplot  (aes(x =Q01, y = Q27), data = SubjectThemes) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  scale_x_continuous() +
  expand_limits(x=0, y=0) +
  xlab("Staff Are Good At Explaining Things") +
  ylab("% Overall Satisfaction")

sq2 <- sq1 +
  aes(x =Q02, y = Q27) +
  xlab("Staff Have Made The Subject Interesting") +
  ylab("% Overall Satisfaction")

sq3 <- sq1 +
  aes(x =Q03, y = Q27) +
  xlab("The Course Is Intellectually Stimulating") +
  ylab("% Overall Satisfaction")

sq4 <- sq1 +
  aes(x =Q04, y = Q27) +
  xlab("Staff Have Made The Subject Interesting") +
  ylab("% Overall Satisfaction")

steaching <- grid.arrange(sq1, sq2, sq3, sq4, top = "NSS 2018 - Relationship between question and overall satisfaction across HECoS Subject x Provider")
steaching




sq5 <- sq1 +
  aes(x =Q05, y = Q27) +
  xlab("My course has provided me with opportunities to explore ideas or concepts in depth") +
  ylab("% Overall Satisfaction")

sq6 <- sq1 +
  aes(x =Q06, y = Q27) +
  xlab("My course has provided me with opportunities to bring information and ideas together from different topics") +
  ylab("% Overall Satisfaction")

sq7 <- sq1 +
  aes(x =Q04, y = Q27) +
  xlab("My course has provided me with opportunities to apply what I have learnt") +
  ylab("% Overall Satisfaction")

slearnopps<- grid.arrange(sq5, sq6, sq7, top = "NSS 2018 - Relationship between question and overall satisfaction across JACS1 Subject x Provider")
slearnopps


sq8 <- sq1 +
  aes(x =Q08, y = Q27) +
  xlab("The criteria used in marking have been clear in advance") +
  ylab("% Overall Satisfaction")


sq9 <- sq1 +
  aes(x =Q09, y = Q27) +
  xlab("Marking and assessment has been fair") +
  ylab("% Overall Satisfaction")


sq10 <- sq1 +
  aes(x =Q10, y = Q27) +
  xlab("Feedback on my work has been timely") +
  ylab("% Overall Satisfaction")


sq11 <- sq1 +
  aes(x =Q11, y = Q27) +
  xlab("I have received helpful comments on my work") +
  ylab("% Overall Satisfaction")

saandf<- grid.arrange(sq8, sq9, sq10, sq11, top = "NSS 2018 - Relationship between question and overall satisfaction across JACS1 Subject x Provider")
saandf



# Overall Satisfaction by Subject and Uni

SubjectThemes %>%
  ggplot (aes (x = Subject, y = Q27)) +
  geom_boxplot () +
  geom_jitter() +
  labs (title = "Boxplot of Overall Satisfaction by Subject",
        y = "Overall Satisfaction (% Agree)",
        x = "Subject (JACS Level 1)") +
  coord_flip() +
  theme_classic()



SubjectThemes %>%
  ggplot (aes (x = Provider, y = Q27)) +
  geom_boxplot () +
  geom_jitter() +
  labs (title = "Boxplot of Overall Satisfaction by Subject",
        y = "Overall Satisfaction (% Agree)",
        x = "Subject (JACS Level 1)") +
  coord_flip() +
  theme_classic()

