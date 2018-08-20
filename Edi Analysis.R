# Edinburgh


EDI <- RGSubjects %>%
  filter (Provider %in% "University of Edinburgh")



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

test <- RGThemes %>%
  gather(key = "Theme", value = Provider, "Teaching On My Course", 
         "Learning Opportunities",
         "Assessment and Feedback",
         "Academic Support",
         "Organisation and Management",
         "Learning Resources",
         "Learning Community",
         "Student Voice",
         "Student Union",
         "Overall Satisfaction")

colours <- c ("The University of Birmingham" = "#99a1af",
              "University of Bristol" = "#99a1af",
              "Cardiff University" = "#99a1af",
              "University of Durham" = "#99a1af",
              "University of Edinburgh" = "#e00b40",
              "University of Exeter" = "#99a1af",
              "University of Glasgow" = "#99a1af",
              "King's College London" = "#99a1af",
              "The University of Leeds" =  "#99a1af",
              "The University of Liverpool" = "#99a1af",
              "The University of Manchester" = "#99a1af",
              "University of Newcastle upon Tyne" =  "#99a1af",
              "University of Nottingham, The" = "#99a1af",
              "The University of Sheffield" = "#99a1af",
              "University College London" = "#99a1af",
              "The University of Warwick" = "#99a1af",
              "The University of York" = "#99a1af", 
              "University of Southampton" = "#99a1af",
              "Queen Mary University of London" = "#99a1af",
              "Queen's University of Belfast" = "#99a1af",
              "The London School of Economics and Political Science" = "#99a1af")


rgteach <- ggplot  (aes(x =as.numeric(as.character(`Teaching On My Course`)), y = Q27, colour = Provider), data = RGThemes) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  scale_colour_manual(values = colours) +
  scale_x_continuous() +
  theme (legend.position = "none") +
  expand_limits(x=0, y=0) +
  xlab("Teaching on My Course") +
  ylab("% Overall Satisfaction")

rglearnopportunity <- rgteach +
  aes(x =as.numeric(as.character(`Learning Opportunities`)), y = Q27) +
  xlab("Learning Opportunities")  +
  ylab("% Overall Satisfaction")

rgaandf <- rgteach +
  aes(x =as.numeric(as.character(`Assessment and Feedback`)), y = Q27) +
  xlab("Assessment and Feedback") +
  ylab("% Overall Satisfaction")

rgAcademicSupport <- rgteach +
  aes(x =as.numeric(as.character(`Academic Support`)), y = Q27) +
  xlab("Academic Support")  +
  ylab("% Overall Satisfaction")

rgoandm <- rgteach +
  aes(x =as.numeric(as.character(`Organisation and Management`)), y = Q27) +
  xlab("Organisation and Management")  +
  ylab("% Overall Satisfaction")

rglearnresource <- rgteach +
  aes(x =as.numeric(as.character(`Learning Resources`)), y = Q27) +
  xlab("Learning Resources")  +
  ylab("% Overall Satisfaction")

rglearncommunity <- rgteach +
  aes(x =as.numeric(as.character(`Learning Community`)), y = Q27) +
  xlab("Learning Community") +
  ylab("% Overall Satisfaction")

rgstudentvoice <- rgteach +
  aes(x =as.numeric(as.character(`Student Voice`)), y = Q27) +
  xlab("Student Voice")  +
  ylab("% Overall Satisfaction")

rgstudentunion <- rgteach +
  aes(x =as.numeric(as.character(`Student Union`)), y = Q27) +
  xlab("Student Union")  +
  ylab("% Overall Satisfaction")


RGProvSatis <- grid.arrange(rgteach, rglearnopportunity, rgaandf, rgAcademicSupport, rgoandm, rglearnresource, rglearncommunity, rgstudentvoice, rgstudentunion, top = "NSS 2018 - Relationship between theme and overall satisfaction across Russell Group \n Imperial College London, Cambridge and Oxford not included ")
RGProvSatis





EDI %>%
  ggplot (aes (x = fct_reorder(Subject, Q27, .desc = TRUE), y = Q27, color = Subject) ) + 
  geom_point(size = 5) +
  theme_classic() +
  theme (legend.position = "none") +
    labs (x = "Subject (From OfS Data)",
        y = "Total Agree",
        title = "Overall Satisfaction for the Edinburgh in 2018") +
  coord_flip()




NSS18Subjects %>%
  filter (Subject %in% "Veterinary sciences",
         Provider %in% c("University of Edinburgh", "University of Glasgow",
                         "The Royal Veterinary College", "University of Bristol",
                          "University of Nottingham, The"),
         Level %in% "First degree") %>%
  ggplot (aes (x = Question, y = Agreement, color = Provider) ) + 
  geom_point(size = 4) +
  theme_classic() +
  labs (x = "Provider (From OfS Data)",
        y = "Total Agree",
        title = "Overall Satisfaction for Vets in 2018") +
  coord_flip()



