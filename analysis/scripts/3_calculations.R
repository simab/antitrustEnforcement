# Agency budget and FTE data  -------------------------
##  Adjusting for inflation and real GDP per capita  ------

# Description: create real GDP per capita adjustment factor 
## Create a year-by-year adjustment factor by dividing each real GDP per capita value by the 2011 value
base_year_val_rGDPpc = real_GDP_per_capita_df$`Real GDP per capita`[real_GDP_per_capita_df$year == 2011]
real_GDP_per_capita_df = real_GDP_per_capita_df %>%
  mutate(rGDPpc_base_2011 =  (`Real GDP per capita`)/base_year_val_rGDPpc) 


agency_budgets_adjusted = agency_budgets %>%
  full_join(., inflation_df, by = "year") %>%
  left_join(., real_GDP_per_capita_df, by = "year") %>%
  filter(year > 1930 & year < 2019) %>%
  mutate(budget_allocated_amt_adj = (budget_allocated_amt/adj_factor)/rGDPpc_base_2011) 




# Federal employee and private legal salary data -------------------------
## Adjust combined df for inflation ---- 
#set the base year to 2011 for inflation data
base_year_val_cpi = inflation_df$cpi[inflation_df$year == 2011]

inflation_df = inflation_df %>%
  mutate(cpi_base_2011 = cpi*100/base_year_val_cpi) 

inflation_df = inflation_df %>%
  mutate(adj_factor = cpi_base_2011/base_year_val_cpi) 

#adjust the salaries for inflation
gov_law_salary_df_adjusted = gov_law_df %>%
  full_join(., inflation_df, by = "year") %>%
  filter(personnel_type != "District court judge" & personnel_type != "Associate Justice") %>%
  filter(year > 1944 & year < 2019) %>%
  mutate(salary_adj = salary_nominal /adj_factor) %>%
  filter(!is.na(salary_adj))


# Supreme Court data ------

## Creating scores ----

### Judge-level ----

#### Business-friendliness and NYT business-friendliness score ----
justice_bf_scores  = bf_scores_read %>%
  filter(term > 1951) %>%
  filter(BusinessLitigant == "1. business is Petitioner OR respondent") %>%
  group_by(justiceName, JVoteForBusiness) %>%
  summarise(votes = n()) %>%
  filter(JVoteForBusiness != "") %>%
  pivot_wider(names_from = JVoteForBusiness, values_from = votes) %>%
  mutate(total_votes = `j vote against business` + `j vote for business`,
         score = round(`j vote for business`/(`j vote against business` + `j vote for business`), 3)) %>%
  mutate(score_type = "bf") %>%
  rename(`pro-enforcement` = `j vote against business`,
         `anti-enforcement` = `j vote for business`) %>%
  filter(justiceName %in% justice_per_court$justiceName) 

justice_bf_nyt_scores = bf_scores_read %>%
  filter(term > 1951) %>%
  filter(BusinessLitigant == "1. business is Petitioner OR respondent" &
           nytSalience == 1) %>%
  group_by(justiceName, JVoteForBusiness) %>%
  summarise(votes = n()) %>%
  filter(JVoteForBusiness != "") %>%
  pivot_wider(names_from = JVoteForBusiness, values_from = votes) %>%
  replace(is.na(.), 0) %>%
  mutate(total_votes = `j vote against business` + `j vote for business`,
         score = round(`j vote for business`/(`j vote against business` + `j vote for business`), 3)) %>%
  mutate(score_type = "bf_nyt") %>%
  rename(`pro-enforcement` = `j vote against business`,
         `anti-enforcement` = `j vote for business`) %>%
  filter(justiceName %in% justice_per_court$justiceName) 

#### Monopoly scores (scores tied to antitrust SCT cases) ----

justice_monopoly_scores_raw = justice_votes %>% 
  select(starts_with("dir_vote_"), type_case_score) %>%
  pivot_longer(!type_case_score, names_to = "justice", values_to = "vote_direction") %>%
  filter(!is.na(vote_direction) & vote_direction != "NA") %>%
  group_by(justice, vote_direction, type_case_score) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = vote_direction, values_from = count) %>%
  mutate(across(`pro-enforcement`:`anti-enforcement`, ~ replace_na(., 0))) 


justice_monopoly_scores_anti_enforcement = justice_monopoly_scores_raw %>%
  summarise(justice = first(justice), type_case_score = 3, 
            `pro-enforcement` = sum(`pro-enforcement`),
            `anti-enforcement` = sum(`anti-enforcement`))

justice_monopoly_score = justice_monopoly_scores_raw %>%
  bind_rows(justice_monopoly_scores_anti_enforcement) %>%
  mutate(score = `pro-enforcement`/(`pro-enforcement`+ `anti-enforcement`)) %>%
  filter(type_case_score != 0) %>%
  mutate(type_case_score = recode(type_case_score, 
                                  '1' = "gov_score", 
                                  '2' = "psb_score", 
                                  '3' = "pe_score")) %>%
  ungroup() %>%
  mutate(justice = str_extract(justice, "[A-Z]+[a-z]+[0-9]*")) %>%
  rename(justiceName = justice)

### Court-level by justice ----

#### Business-friendliness and NYT business-friendliness score ----

court_bf_justice_score = justice_bf_scores %>%
  left_join(select(justice_per_court, justiceName, `1952`:`2019`)) %>%
  filter(!is.na(`1952`)) %>%
  mutate(across(`1952`:`2019`, ~ case_when(. == 0 ~ as.numeric(NA),
                                           score_type == "bf" ~ .*score), .names = "{col}_bf")) %>%
  select(ends_with("_bf")) %>%
  ungroup() %>%
  summarise(across(is.numeric, ~ round(mean(., na.rm = T), 3))) %>%
  pivot_longer(everything()) %>%
  mutate(court_term = as.numeric(str_extract(name, "\\d{4}"))) %>%
  rename(bf_score = value) %>%
  select(-name)



court_bf_nyt_justice_score = justice_bf_nyt_scores %>%
  left_join(dplyr::select(justice_per_court, justiceName, `1952`:`2019`)) %>%
  filter(!is.na(`1952`)) %>% 
  mutate(across(`1952`:`2019`, ~ case_when(. == 0 ~ as.numeric(NA),
                                           T ~ .*score), .names = "{col}_bf_nyt")) %>%
  select(ends_with("_bf_nyt")) %>%
  ungroup() %>%
  summarise(across(where(is.numeric), ~ round(mean(., na.rm = T), 3))) %>%
  pivot_longer(everything()) %>%
  mutate(court_term = as.numeric(str_extract(name, "\\d{4}"))) %>%
  rename(bf_nyt_score = value) %>%
  select(-name)


#### Monopoly scores (scores tied to antitrust SCT cases) ----
court_monopoly_justice_score = justice_monopoly_score %>% 
  left_join(select(justice_per_court, justiceName, `1952`:`2019`)) %>%
  mutate(across(`1952`:`2019`, ~ case_when(. == 0 ~ as.numeric(NA),
                                           T ~ .x*score), .names = "{col}_score")) %>%
  select(justiceName, type_case_score, ends_with("score")) %>%
  filter(justiceName != "ACBarrett") %>%
  select(-score, -justiceName) %>%
  group_by(type_case_score) %>%
  summarise(across(is.numeric, ~ mean(., na.rm = T))) %>%
  pivot_longer(!type_case_score) %>%
  pivot_wider(names_from = type_case_score, values_from = value) %>%
  mutate(name = as.integer(str_extract(name, "[0-9]{4}"))) %>%
  rename(court_term = name)

# Switching the direction of the score for figure purposes:
court_monopoly_justice_score_anti = justice_monopoly_scores_raw %>%
  bind_rows(justice_monopoly_scores_anti_enforcement) %>%
  mutate(score = `anti-enforcement`/(`pro-enforcement`+ `anti-enforcement`)) %>%
  filter(type_case_score != 0) %>%
  mutate(type_case_score = recode(type_case_score, 
                                  '1' = "ag_score", 
                                  '2' = "plb_score", 
                                  '3' = "ae_score")) %>%
  ungroup() %>%
  mutate(justice = str_extract(justice, "[A-Z]+[a-z]+[0-9]*")) %>%
  rename(justiceName = justice) %>%
  left_join(select(justice_per_court, justiceName, `1952`:`2019`)) %>%
  mutate(across(`1952`:`2019`, ~ case_when(. == 0 ~ as.numeric(NA),
                                           T ~ .x*score), .names = "{col}_score")) %>%
  select(justiceName, type_case_score, ends_with("score")) %>%
  filter(justiceName != "ACBarrett" & justiceName != "BMKavanaugh" &
           justiceName != "NMGorsuch") %>%
  select(-score, -justiceName) %>%
  group_by(type_case_score) %>%
  summarise(across(is.numeric, ~ mean(., na.rm = T))) %>%
  pivot_longer(!type_case_score) %>%
  pivot_wider(names_from = type_case_score, values_from = value) %>%
  mutate(name = as.integer(str_extract(name, "[0-9]{4}"))) %>%
  rename(court_term = name)

### Court-level by case ----
#### Business-friendliness and NYT business-friendliness score ----

court_bf_case_scores = bf_scores_read %>%
  filter(BusinessLitigant == "1. business is Petitioner OR respondent" &
           !is.na(JVoteForBusiness)) %>%
  group_by(term, JVoteForBusiness) %>%
  summarise(votes = n()) %>%
  filter(JVoteForBusiness != "") %>%
  pivot_wider(names_from = JVoteForBusiness, values_from = votes) %>%
  mutate(total_votes = `j vote against business` + `j vote for business`,
         score = round(`j vote for business`/(`j vote against business` + `j vote for business`), 3)) %>%
  mutate(score_type = "bf") %>%
  rename(`pro-enforcement` = `j vote against business`,
         `anti-enforcement` = `j vote for business`) 

court_bf_nyt_case_scores = bf_scores_read %>%
  filter(BusinessLitigant == "1. business is Petitioner OR respondent" &
           nytSalience == 1) %>%
  group_by(term, JVoteForBusiness) %>%
  summarise(votes = n()) %>%
  filter(JVoteForBusiness != "") %>%
  pivot_wider(names_from = JVoteForBusiness, values_from = votes) %>%
  replace(is.na(.), 0) %>%
  mutate(total_votes = `j vote against business` + `j vote for business`,
         score = round(`j vote for business`/(`j vote against business` + `j vote for business`), 3)) %>%
  mutate(score_type = "bf_nyt") %>%
  rename(`pro-enforcement` = `j vote against business`,
         `anti-enforcement` = `j vote for business`) 

#### Monopoly scores (scores tied to antitrust cases) -----
court_monopoly_scores_case_raw = justice_votes %>% 
  select(starts_with("dir_vote_"), type_case_score, year) %>%
  pivot_longer(!c(type_case_score, year), names_to = "justice", values_to = "vote_direction") %>%
  filter(!is.na(vote_direction) & vote_direction != "NA") %>%
  group_by(year, vote_direction, type_case_score) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = vote_direction, values_from = count) %>%
  mutate(across(`pro-enforcement`:`anti-enforcement`, ~ replace_na(., 0))) 

court_monopoly_scores_case_anti_enforcement = court_monopoly_scores_case_raw %>%
  summarise(year = first(year), type_case_score = 3, 
            `pro-enforcement` = sum(`pro-enforcement`),
            `anti-enforcement` = sum(`anti-enforcement`))

court_monopoly_case_score = court_monopoly_scores_case_raw %>%
  bind_rows(court_monopoly_scores_case_anti_enforcement) %>%
  mutate(score = `pro-enforcement`/(`pro-enforcement`+ `anti-enforcement`)) %>%
  filter(type_case_score != 0) %>%
  mutate(type_case_score = recode(type_case_score, 
                                  '1' = "gov_score", 
                                  '2' = "psb_score", 
                                  '3' = "pe_score")) 

## Combining scores into 1 dataframe ----

### Justice-level ----
justice_scores = justice_monopoly_score %>%
  rename(score_type = type_case_score) %>%
  bind_rows(justice_bf_scores, justice_bf_nyt_scores) %>%
  left_join(select(sc_judge, Party, era, justiceName, year_oath, schoolAttended)) %>%
  mutate(total_votes = `pro-enforcement` + `anti-enforcement`) %>%
  filter(justiceName != "WBRutledge" &  justiceName != "FMurphy" & justiceName != "ACBarrett") %>%
  mutate(justiceLastName = case_when(justiceName == "SDOConnor" ~ "O'Connor",
                                     T ~ str_extract(justiceName, "[A-Z][a-z]+"))) %>%
  left_join(justice_term_dates) %>%
  mutate(justiceTitle = paste0(justiceLastName, " ", termDates))

### Court-level - by justice ----
chiefJusticeLevels = c("Vinson", "Warren", "Burger", "Rehnquist", "Roberts")

court_justice_scores = court_monopoly_justice_score %>%
  left_join(court_bf_justice_score) %>%
  left_join(court_bf_nyt_justice_score) %>%
  ungroup() %>%
  pivot_longer(!court_term, names_to = "type_case_score", values_to = "score") %>%
  group_by(type_case_score) %>%
  mutate(rolling_score = rollmean(score, 3, align = "center", fill = 0)) %>%
  mutate(chief_term = (case_when(court_term < 1954 ~ "Vinson",
                                 court_term > 1953 & court_term < 1969 ~ "Warren",
                                 court_term > 1968 & court_term < 1986 ~ "Burger",
                                 court_term > 1985 & court_term < 2005 ~ "Rehnquist",
                                 court_term > 2004 ~ "Roberts"))) %>%
  mutate(chief_term = factor(chief_term, levels = chiefJusticeLevels))

### Court-level by case ----
court_monopoly_case_score_temp = court_monopoly_case_score %>%
  select(year, type_case_score, score) %>%
  pivot_wider(names_from = type_case_score, values_from = score) %>%
  full_join(., as.data.frame(1952:2021), by = c("year" = "1952:2021")) %>%
  left_join(rename(select(court_bf_case_scores, term, score), bf_score = score), by = c("year" = "term")) %>%
  full_join(., as.data.frame(1952:2021), by = c("year" = "1952:2021")) %>%
  left_join(rename(select(court_bf_nyt_case_scores, term, score), bf_nyt_score = score), by = c("year" = "term")) %>%
  ungroup() %>%
  pivot_longer(!year, names_to = "type_case_score", values_to = "score") %>%
  group_by(type_case_score) %>%
  mutate(rolling_score = rollapply(score, 3, mean, align = "right", fill = NA)) %>%
  mutate(decade = as.integer(paste0(str_extract(year, "[0-9]{3}"), "0"))) 

court_case_scores = court_monopoly_case_score_temp %>%
  left_join(summarise(group_by(court_monopoly_case_score_temp, decade, type_case_score),
                      decadeScore = mean(score, na.rm = T))) %>%
  rename(court_term = year) %>%
  mutate(chief_term = (case_when(court_term < 1954 ~ "Vinson",
                                 court_term > 1953 & court_term < 1969 ~ "Warren",
                                 court_term > 1968 & court_term < 1986 ~ "Burger",
                                 court_term > 1985 & court_term < 2005 ~ "Rehnquist",
                                 court_term > 2004 ~ "Roberts")))
## Finding top 5 scores on the court per year ----
### Anti-enforcement score (ends in 2015)
court_ae_score_top5_ends2015 = justice_monopoly_score %>%
  filter(type_case_score == "pe_score") %>%
  mutate(score = 1 - score) %>% #turn PE score to AE score
  left_join(select(justice_per_court, justiceName, `1952`:`2015`)) %>%
  mutate(across(`1952`:`2015`, ~ case_when(. == 0 ~ as.numeric(NA),
                                           T ~ .x*score), .names = "{col}_ae")) %>%
  select(`1952_ae`:`2015_ae`, justiceName) %>%
  mutate(across(is.numeric, ~ min_rank(desc(.x)), .names = "{col}_top")) %>%
  pivot_longer(!justiceName, names_to = "metric", values_to = "value") %>%
  mutate(term = as.integer(str_extract(metric, "[0-9]{4}")),
         type = str_extract(metric, "[a-z]{2}_?[a-z]?")) %>%
  select(-metric) %>%
  filter(!is.na(value)) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  filter(ae_t < 6) %>%
  group_by(term) %>%
  summarise(mean = mean(ae, na.rm = T)) 
### Pro-large company score (ends in 2015)
court_plb_score_top5_ends2015 = justice_monopoly_score %>%
  filter(type_case_score == "psb_score") %>%
  mutate(score = 1 - score) %>%
  left_join(select(justice_per_court, justiceName, `1952`:`2015`)) %>%
  mutate(across(`1952`:`2015`, ~ case_when(. == 0 ~ as.numeric(NA),
                                           T ~ .x*score), .names = "{col}_plb")) %>%
  select(`1952_plb`:`2015_plb`, justiceName) %>%
  mutate(across(is.numeric, ~ min_rank(desc(.x)), .names = "{col}_top")) %>%
  pivot_longer(!justiceName, names_to = "metric", values_to = "value")  %>%
  mutate(term = as.integer(str_extract(metric, "[0-9]{4}")),
         type = str_extract(metric, "[a-z]{3}_?[a-z]?")) %>%
  select(-metric) %>%
  filter(!is.na(value)) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  filter(plb_t < 6) %>%
  group_by(term) %>%
  summarise(mean = mean(plb, na.rm = T)) 

court_bf_score_top5_ends2015 = filter(justice_scores, score_type == "bf") %>%
  select(justiceName, score) %>%
  left_join(select(justice_per_court, justiceName, `1952`:`2015`)) %>%
  mutate(across(`1952`:`2015`, ~ case_when(. == 0 ~ as.numeric(NA),
                                           T ~ .x*score), .names = "{col}_bf")) %>%
  select(justiceName, `1952_bf`:`2015_bf`) %>%
  mutate(across(is.numeric, ~ min_rank(desc(.x)), .names = "{col}_top")) %>% 
  pivot_longer(!justiceName, names_to = "metric", values_to = "value") %>% 
  mutate(term = as.integer(str_extract(metric, "[0-9]{4}")),
         type = str_extract(metric, "[a-z]{3}_?[a-z]?")) %>%
  select(-metric) %>%
  filter(!is.na(value)) %>% 
  mutate(type = ifelse(is.na(type), "score", type)) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  filter(top < 6) %>%
  group_by(term) %>%
  summarise(mean = mean(score, na.rm = T)) 

# library(writexl)
# court_bf_score_top5_ends2015 %>%
#   left_join(court_ae_score_top5_ends2015, by = "term", suffix = c("_ae", "_bf")) %>%
#   rename(`Anti-enforcement` = mean_ae,
#          `Business friendliness`= mean_bf) %>%
#   write_xlsx(., "analysis/data/fig12_1_data.xlsx")
  
  


