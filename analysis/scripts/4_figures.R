
fig.path = here::here("analysis/results/figures")


# Figure 1: Public confidence in large corporations  ----
dfBigBusiness_m %>%
  filter(AntitrustDirectionCombo != "ambig") %>%
  mutate(AntitrustDirectionCombo = case_when(AntitrustDirectionCombo == "anti" ~ "High to moderate",
                                             AntitrustDirectionCombo == "pro" ~ "Little to none",
                                             T ~ "Ambiguous")) %>%
  ggplot(aes(x = year, y = RespPct, color = AntitrustDirectionCombo)) +
  geom_point() + theme_classic() + geom_smooth(method = "lm") +
  scale_color_grey(start = 0, end = 0.5) + 
  labs(x = "Year", y = "% of survey respondents",
       color = "Level of confidence",
       title = "Figure 1: Public confidence in big business", 
       caption = "Source: Gallup- Confidence in Institutions")+
  theme(legend.position = "bottom")
ggsave(path = fig.path, "1_public_confidence_business_lm.png")


# Figure 2: Majority of Americans Remain Unsatisfied with Size and Influence of Major corporations (Pew Data) ----
# Source: https://news.gallup.com/file/poll/188753/Satisfaction_with_Corporations_and_Regulations%20_160120.pdf; https://news.gallup.com/poll/188747/majority-americans-dissatisfied-corporate-influence.aspx

corpInfluence = data.frame(year = c(2001:2008, 2011:2016),
                           total_satisfied = c(48, 50, 43, 38, 38, 35, 39, 35, 29, 30, 35, 35, 36, 35),
                           total_dissatisfied = c(48, 47, 54, 61, 59, 62, 58, 61, 67, 64, 61, 63, 59, 63)
)

corpInfluence %>%
  rename(Satisfied = total_satisfied, Dissastisfied = total_dissatisfied) %>%
  pivot_longer(!year, names_to = "Total %") %>%
  ggplot(aes(x = year, y = value, color = `Total %` )) +
    geom_line() + geom_point() + theme_classic() + scale_color_grey() + expand_limits(y = c(0, 100)) +
    labs(x = "Year", y = "% of survey respondents",
         title = "Figure 2: Majority of Americans remain unsatisfied\nwith size and influence of major corporations")

ggsave(path = fig.path, "2_public_unsatisfied_with_business.png")

# Figure 3: Roper antitrust polls ----
dfRoper_m %>%
  filter(questionType == "overall") %>%
  pivot_wider(names_from = AntitrustDirectionCombo, values_from = RespPct) %>%
  mutate(proAntiRatio = pro/(pro+ anti)) %>%
  ggplot() +
  geom_point(aes(x = year, y = proAntiRatio)) +
  geom_smooth(aes(x = year, y = proAntiRatio), 
              method = "lm") +
  theme_classic() +
  labs(x = "Year", y = "Proportion of pro-expansion opinions",
       title = "Figure 3: Proportion of public in favor\nof expanding antitrust enforcement",
       caption = "Source: Roper Center for Public Opinion Research")

ggsave(path = fig.path, "3_proportion_pro_AE_polling.png")

# Figure 4: Mentions of antitrust and monopoly in presidential speeches ----
presidential_speeches %>%
  select(President:rowNumber) %>%
  pivot_longer(!c(President, rowNumber)) %>%
  mutate(name = fct_relevel(name, "Monopoly", "Antitrust")) %>%
  mutate(President = fct_reorder(President, rowNumber)) %>%
  ggplot(aes(x = value, y = President)) + 
    geom_bar(stat = 'identity') + facet_wrap(~name) + theme_classic() + scale_fill_grey() +
    labs(title = "Figure 4: Antitrust not mentioned in major\npresidential speeches since 1980",
         subtitle = "References to monopoly and antitrust in inagural and\nState of the Union speeches - per president (1932-2021)",
         x = "")

ggsave(path = fig.path, "4_presidentialSpeeches.png")

# Figure 5: Antitrust policy references in party platforms ----
party_platforms %>%
  select(election: rowNumber) %>%
  pivot_longer(!c(election, Label, rowNumber)) %>%
  mutate(Label = fct_reorder(Label, rowNumber)) %>%
  ggplot(aes(x = value, y = Label, fill = name)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    theme_classic() + scale_fill_grey() +
    labs(x = "", y = "",
         title = "Figure 5: Antitrust policy references in party platforms",
         fill = "Party") +
    theme(legend.position = "top")

ggsave(path = fig.path, "5_partyPlatforms.png")

# Figure 6: Antitrust policy references in SCT hearings ----
sct_hearings %>%
  mutate(rowNumber = as.double(`...1`)) %>%
  add_row(rowNumber = 32.5, Total = 70, lastName = "Bork") %>%
  mutate(lastName = fct_reorder(lastName, desc(rowNumber))) %>%
  ggplot(aes(x = Total, y = lastName)) + geom_bar(stat = "identity") +
    theme_classic() + scale_fill_grey() +
    geom_hline(yintercept = "Bork") +
    geom_label(aes(x = 78, y = "Kennedy", 
                  label = "Bork's nomination\nfails (1987)"), size = 3) +
    labs(x = "# of referencs to antitrust or monopoly in hearing", y = "Justice",
         title = "Figure 6: Supreme Court nominees begin discussing antitrust after Bork's failed nomination") 
ggsave(path = fig.path, "6_sctHearings.png")

# Figure 7: Agency budgets -----

agency_budgets_adjusted %>%    
  filter(budget_id == "competitionFunds" | budget_id == "doj_total_amt") %>%
    filter(year > 1952 & year < 2021) %>%
  ggplot(., aes(x = year, y = budget_allocated_amt_adj, 
                group = agency_id, color = agency_id)) + geom_point() +  geom_line() + 
  labs(colour = "Agency", title = "Figure 7: Agency budgets have not recovered since 1980",
       subtitle  = "Budgets over time: DoJ Antitrust division and FTC",
       y = "Dollars (real GDP per capita\nadjusted, 2011)",
       x = "Year",
       caption = "Source: Annual Reports of the FTC, the FTC's annual Congressional budget request, and DOJ website") + 
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) + expand_limits(y = 0) +
  theme_classic() + scale_color_grey() +
  theme(legend.position = "top")

ggsave(path = fig.path, "7_agencyBudgets.png")

# Figure 8: Agency budgets composition -----

agency_budgets_adjusted %>%
  filter(year > 1989) %>%
  filter(budget_id %in% c("doj_total_amt", "doj_filing_fee_amt",
                          "competitionFunds", "ftc_filing_fee_amt")) %>%
  mutate(budget_type_id = ifelse(budget_type_id == "Filing fee", "Filing fee", "Congressional appropriation")) %>%
  ggplot(aes(x = year, y=budget_allocated_amt_adj, fill = budget_type_id)) +
    geom_area(position = "identity",
              outline.type = "both",
              alpha = 0.5) + 
    facet_wrap(~agency_id) +
    geom_point() +
    geom_line() +
    scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) + expand_limits(y = 0) +
    theme_classic() + scale_fill_grey() +
    labs(x = "Year", y = "$ Millions (real GDP per capita adjusted)",
         fill = "",
         title = "Figure 8: Merger filing fees account for significant portion of\nDOJ's Antitrust division's budget starting in the mid-1990s") +
    theme(legend.position = "top")

ggsave(path = fig.path, "8_agencyBudgetsComposition.png")

# Figure 9: FTC FTEs -----

ftc_breakdown_df_read %>%
  select(year, competitionFTE, consumerProtectionFTE) %>%
  pivot_longer(!year, names_to = "type_FTE") %>%
  filter(!is.na(value)) %>%
  mutate(type_FTE = recode(type_FTE, 'consumerProtectionFTE' = 'Consumer Protection',
                           'competitionFTE' = 'Maintaining Competition')) %>%
  rename(Mission = type_FTE) %>%
  ggplot(aes(x = year, y = value, fill = Mission)) +
    geom_area(position = "identity",
              alpha =0.5) +
    geom_point() + geom_line() +
  theme_classic() + scale_fill_grey() +
  labs(x = "Year", y = "Full-time employees (FTEs)",
       fill = "FTC Mission",
       title = "Figure 9: FTC's employees by Mission") +
    theme(legend.position = "top")
ggsave(path = fig.path, "9_ftcFTEs.png")

# Figure 10: Supreme Court cases by enforcement level -----
antitrust_westlaw_sc_cases %>%
  select(year, proAntitrustEnforcement) %>%
  mutate(proAntitrustEnforcement_f = forcats::fct_rev(proAntitrustEnforcement)) %>%
  ggplot() + 
  geom_bar(aes(x = year, fill = proAntitrustEnforcement_f), position = "stack") + 
  scale_fill_manual(values = c("Yes" = "grey", "No" = "black")) + 
  theme_classic() + theme(legend.position = "top") +
  labs(title = "Figure 10: The evolution of the Supreme Court's\n antitrust jurisprudence, 1952-2021", 
       x = "Court term",
       y = "# of cases",
       fill = "Expands enforcement?") +
  scale_y_continuous(breaks= breaks_pretty()) 

ggsave(path = fig.path, "10_sctCaseEnforcement.png")

# Figure 11: SCT business-friendliness score over time ----

sctBFscore = court_justice_scores %>%
  filter(type_case_score == "bf_score") %>%
  ungroup() %>%
  select(court_term, score, chief_term) 

chiefTerms = sctBFscore %>%
  select(court_term, chief_term) %>%
  group_by(chief_term) %>%
  summarise(start = first(court_term), end = last(court_term), chief_term = first(chief_term)) %>%
  mutate(end = end + 0.99) %>%
  mutate(color = ifelse(row_number() %% 2 == 0, "blank", "grey"))

sctBFscore %>%
  ggplot() + 
    geom_line(aes(x = court_term, y = score)) +
    geom_point(aes(x = court_term, y = score)) +
    geom_rect(data = chiefTerms,
              aes(xmin = start, xmax = end, fill = color), 
              ymin = -Inf, ymax = Inf, alpha = 0.5) +
    geom_text(data = chiefTerms, 
              aes(x = end-1, y = 0.63, label = chief_term, angle = 90),
              size = 3, check_overlap = T) +
    expand_limits(y = c(0.2, 0.7)) +
    theme_classic() + scale_fill_grey() + 
    theme(legend.position = "none") + 
    labs(x = "Court term", y = "Business-friendliness score (by justice)",
         title = "Figure 11: The Court's friendliness to business\nhas increased over time",
         caption = "Source: Epstein, Landes and Poser (2013) and updated in 2020")

ggsave(path = fig.path, "11_sctBFCourtScore.png")

# Figure 12: Court's AE and PLB score over time -----

court_ae_score_top5_ends2015 %>%
  left_join(court_plb_score_top5_ends2015, by = "term", suffix = c("_ae", "_plc")) %>%
  pivot_longer(!term, names_to = "mean_type", values_to = "mean") %>%
  mutate(mean_type = case_when(mean_type == "mean_ae" ~ "Anti-enforcement",
                               T ~ "Pro-big company")) %>%
  ggplot(aes(x= term, y = mean, color = mean_type)) +
    geom_line() + geom_point() +
  expand_limits(y = c(0, 0.9)) +
  theme_classic() + scale_color_grey() + 
  theme(legend.position = "top") + 
  labs(x = "Court term", y = "Mean score (by justice)",
       color = "Score",
       title = "Figure 12: Anti-enforcement and pro-big\ncompany scores move in parallel",
       caption = "Based on Justices votes on antitrust cases mentioning the Sherman and Clayton Acts and data from\nEpstein, Landes, and Posner. (2013), as updated by Lee Epstein until 2020.")
ggsave(path = fig.path, "12_sctAEandPLBCourtScore.png")


# Figure 12.1: Court's AE and BF score over time -----

court_ae_score_top5_ends2015 %>%
  left_join(court_bf_score_top5_ends2015, by = "term", suffix = c("_ae", "_bf")) %>%
  pivot_longer(!term, names_to = "mean_type", values_to = "mean") %>%
  mutate(mean_type = case_when(mean_type == "mean_ae" ~ "Anti-enforcement",
                               T ~ "Business friendliness")) %>%
  ggplot(aes(x= term, y = mean, color = mean_type)) +
  geom_line() + geom_point() +
  expand_limits(y = c(0, 0.9)) +
  theme_classic() + scale_color_grey() + 
  theme(legend.position = "top") + 
  labs(x = "Court term", y = "Mean score (by justice)",
       color = "Score",
       title = "Figure 12: Anti-enforcement and business\nfriendliness scores move in parallel",
       caption = "Based on Justices votes on antitrust cases mentioning the Sherman and Clayton Acts and data from\nEpstein, Landes, and Posner. (2013), as updated by Lee Epstein until 2020.")
ggsave(path = fig.path, "12.1_sctAEandBFCourtScore.png")



# Figure 13: Comparing individual justices AE and BF scores -----
justice_scores %>%
  filter(score_type == "pe_score" | score_type == "bf") %>%
  filter(!(justiceName %in% c("BMKavanaugh", "NMGorsuch"))) %>%
  select(justiceName, justiceTitle, score_type, score, Party, era, year_oath) %>%
  mutate(score_type = ifelse(score_type == "pe_score", "ae_score", "bf_score"),
         score = ifelse(score_type == "bf_score", score, 1 - score)) %>%
  pivot_wider(names_from = score_type, values_from = score) %>%
  mutate(year_oath = as.integer(year_oath)) %>%
  arrange(year_oath, justiceName) %>%
  mutate(order = row_number()) %>%
  pivot_longer(ae_score: bf_score) %>%
  mutate(name = recode(name, "ae_score" = "Anti-enforcement", 
                       "bf_score" = "Business-friendliness")) %>%
  mutate(justiceTitle = fct_reorder(justiceTitle, order, .desc = T)) %>%
  ggplot() + 
  geom_point(aes(x = value, y = justiceTitle, color = name)) +
  labs(x = "Score", y = "Justice", color = "Score",
       title = "Business-friendliness and anti-enforcement\n scores by Justice",
       caption = "Business-friendliness data from Epstein, Landes, and Posner (2013) and updated in 2020;\nVoting data from the Supreme Court Database (2021 Version 1)") +
  theme_minimal() +
  scale_color_grey() +
  theme(legend.position="top", legend.box="vertical", legend.margin=margin(),  
        axis.title.x = element_blank(), axis.title.y = element_blank())
ggsave(path = fig.path, "13_sctJusticeAE_BFscores.png")

# Figure 14: Comparing individual justices AE and PLB scores --------
justice_scores %>%
  left_join(select(justice_per_court, c(justiceName, Party, era, year_oath))) %>%
  filter(score_type == "pe_score" | score_type == "psb_score") %>%
  filter(!(justiceName %in% c("BMKavanaugh", "NMGorsuch"))) %>%
  select(justiceName, score_type, score, era, Party, justiceLastName, year_oath, justiceTitle) %>%
  mutate(score_type = ifelse(score_type == "psb_score", "plb_score", "ae_score"),
         score = 1 - score) %>%
  pivot_wider(names_from = score_type, values_from = score) %>%
  mutate(year_oath = as.integer(year_oath)) %>%
  arrange(year_oath, justiceTitle) %>%
  mutate(order = row_number()) %>%
  pivot_longer(ae_score:plb_score) %>% 
  mutate(name = recode(name, "ae_score" = "Anti-enforcement",
                       "plb_score" = "Pro-big business")) %>%
  mutate(justiceTitle = fct_reorder(justiceTitle, order, .desc = T)) %>%
  ggplot() + 
  geom_point(aes(x = value, y = justiceTitle, color = name)) +
  labs(x = "Score", y = "Justice", 
       color = "Score",
       title = "Pro-big business and anti-enforcement\n friendliness by Justice",
       caption = "Voting data from the Supreme Court Database (2021 Version 1)") + 
  theme_minimal() +
  scale_color_grey() +
  theme(legend.position="top", legend.box="vertical", legend.margin=margin(),
        axis.title.x = element_blank(), axis.title.y = element_blank()) 
ggsave(path = fig.path, "14_sctJusticeAE_PLBscores.png")


# Figure 16: Composition of personnel/officials career trajectories -------

personnel_nominations_summary %>%
  filter(`Type of positions` != "Other") %>%
  ggplot(aes(x = `Type of positions`)) + 
  geom_col(aes(y = freq, fill = Era),
           position = position_dodge2()) +
  facet_wrap(vars(direction_f), scales = "free", dir = "v") +
  labs(title = "Composition of officials' career trajectories", y = "% of officials employed") +
  scale_fill_grey() +
  coord_flip() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white"))  +
  expand_limits(y = 100)
ggsave(path = fig.path, "16_personnelChanges.png")


# Figure 17: Government and private sector salaries

gov_law_salary_df_adjusted_fig = gov_law_salary_df_adjusted %>%
  filter(!(personnel_type %in% c("President", "Chief Justice", "Federal Reserve Chair"))) %>%
  select(year, personnel_type, salary_adj) %>%
  pivot_wider(names_from = personnel_type, 
              values_from = salary_adj,
              values_fn = mean)
  
colors = c("Appeals court judge"  = "grey",
           "Law firm associate- first year" = "grey",
           "GS-15 staff average" = "grey",
           "Avg comp per equity partner" = "orange",
           "FTC Chair/AAG at DOJ" = "purple",
           "DC median house price" = "green")
  
ggplot(data = gov_law_salary_df_adjusted_fig,
       aes(x = year)) +
  geom_point(aes(y = `Appeals court judge`, color = "Appeals court judge"), size = 0.5) +
  geom_line(aes(y = `Appeals court judge`, color = "Appeals court judge")) +
  
  geom_point(aes(y = `Law firm associate- first year`, color = "Law firm associate- first year"), size = 0.5) +
  geom_line(aes(y = `Law firm associate- first year`, color = "Law firm associate- first year")) +
  
  geom_point(aes(y = `GS-15 staff average`, color = "GS-15 staff average"), size = 0.5) +
  geom_line(aes(y = `GS-15 staff average`, color = "GS-15 staff average")) +
  
  geom_point(aes(y = `Avg comp per equity partner`, color = "Avg comp per equity partner"), size = 0.5) +
  geom_line(data = gov_law_salary_df_adjusted_fig,
            aes(y = `Avg comp per equity partner`, color = "Avg comp per equity partner")) +
  
  geom_point(aes(y = `FTC Chair/AAG at DOJ`, color = "FTC Chair/AAG at DOJ"), size = 0.5) +
  geom_line(aes(y = `FTC Chair/AAG at DOJ`, color = "FTC Chair/AAG at DOJ")) +
  
  geom_point(aes(y = `DC median house price`, color = "DC median house price"), size = 0.5) +
  geom_line(aes(y = `DC median house price`, color = "DC median house price"), linetype = 2) +
  
    scale_color_manual(values = colors) +
    scale_y_continuous(label = label_number(suffix = " M", scale = 1e-6),
                       name = "Dollars (inflation and real GDP \nper capita adjusted, 2011)") +
    labs(x = "Year",
         title = "Figure 17: Private sector salaries exceed government counterparts",
         color = "") +
    theme_classic() +
    theme(legend.position = "top")
    

ggsave(path = fig.path, "17_personnelSalaries.png")
