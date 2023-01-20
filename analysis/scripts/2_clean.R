# Historical inflation and real GDP adjustments -------------------------

## Inflation ------

# Description: combining two versions of the CPI U (inflation index)
## Transform a quarterly index into an annual index 
## Add values from the annual index (starts in 1913)
## Arrange values by year
inflation_df = inflation_df_1947 %>%
  mutate(year = year(DATE)) %>%
  group_by(year) %>%
  summarise(cpi = mean(VALUE)) %>%
  bind_rows(., inflation_df_1913) %>%
  arrange(year) 


# Description: creating inflation adjustment for 2011 dollars
## Create a year-by-year adjustment factor by dividing each CPI value by the 2011 value

inflation_df = inflation_df %>%
  mutate(cpi_of_2011 = filter(inflation_df, year == 2011)$cpi) %>%
  mutate(adj_factor = cpi/cpi_of_2011)

## Real GDP adjustment ------
# Description:
## Removing unnecessary variables to the analysis
## Renaming variables for later merging
## Removing rows for later merging 
real_GDP_per_capita_df = real_gdp_per_capita_data %>%
  select(-Code, -`145446-annotations`, -Entity) %>%
  rename(year = Year, `Real GDP per capita` = `GDP per capita` ) %>%
  filter(year >= 1913)

# Agency budget and FTE data  -------------------------

## DOJ ----

# Cleaning DOJ Congressional appropriation and merger filing data
## Description: Multiplying monetary values by 1K because the original data was conveyed in thousands of dollars.
doj_df_clean = doj_df_read %>%
  select(-notes) %>%
  mutate(across(ends_with("amt"), ~ .x*1000)) 

## FTC -----
# Merging FTC Congressional appropriations data with DOJ appropriations data; creating additional FTC data from existing FTC/DOJ data
# Description: 
## Loading FTC and DOJ Cong. appropriations data then joining by appropriations year
## Creating a column for FTC merger filing fees by replicating DOJ merger filing fees because they are identical by law. 
## Creating a total FTC budget data column by combining the FTC's merger filing fee and Congressional appropriations amounts
ftc_df_clean = ftc_df_read %>%
  left_join(doj_df_clean) %>%
  rename(ftc_filing_fee_amt = doj_filing_fee_amt) %>%
  mutate(ftc_total_amt = case_when(is.na(ftc_filing_fee_amt) ~ ftc_appro_amt,
                                   T ~ ftc_appro_amt + ftc_filing_fee_amt)) %>%
  select(year, ftc_total_amt, ftc_appro_amt, ftc_filing_fee_amt) %>%
  left_join(select(ftc_breakdown_df_read, year, competitionFunds), 
            by = "year")


## Creating agency budget dataset ------

agency_budgets = doj_df_clean %>%
  left_join(ftc_df_clean) %>%
  pivot_longer(!year, names_to = "budget_id", values_to = "budget_allocated_amt") %>%
  mutate(agency_id = case_when(budget_id == "competitionFunds" |
                                 str_detect(budget_id, "ftc") ~ "FTC",
                               T ~ "DOJ"),
         budget_type_id = case_when(str_detect(budget_id, "fee") ~ "Filing fee",
                                    str_detect(budget_id, "total") ~ "Total",
                                    T ~ "Congressional appropriation"))





# Federal employee and private legal salary data -------------------------

## Federal employee salaries -------------------------


judge_df = judge_data %>%
  select(year, district_salary_nominal, appeals_salary_nominal,
         chief_justice_salary_nomial, associate_justice_salary_nominal) %>%
  complete(year = full_seq(year, period = 1)) %>%
  fill(district_salary_nominal, appeals_salary_nominal,
       chief_justice_salary_nomial, associate_justice_salary_nominal)

es_df = es_data %>%
  select(year_simp, ES_level_1_simp, ES_level_3_simp) %>%
  filter(!is.na(year_simp)) %>%
  complete(year_simp = full_seq(year_simp, period = 1)) %>%
  fill(ES_level_1_simp, ES_level_3_simp) %>%
  rename(year = year_simp)

president_df = president_data %>%
  filter(year > 1900) %>%
  complete(year = full_seq(year, period = 1)) %>%
  fill(president_salary_nominal) %>%
  select(year, president_salary_nominal)

gs_df = gs_data %>%
  select(year_simp, GS_15_avg_simp) %>%
  filter(!is.na(year_simp)) %>%
  complete(year_simp = full_seq(year_simp, period = 1)) %>%
  fill(GS_15_avg_simp) %>%
  rename(year = year_simp)


## Private law salaries: partners and first-year associates  -------------------------


law_df = law_data %>%
  rename(year = `Fiscal Year`) %>%
  mutate(net_income_per_partner = `Net Operating Income`/`Num Eq Partners`) %>%
  group_by(year) %>%
  summarise(avg_net_income_per_partner = mean(net_income_per_partner), avg_CAP = mean(`CAP`)) %>%
  add_row(year = 1947, avg_CAP = 27246, avg_net_income_per_partner = 35000) %>%
  add_row(year = 1954, avg_CAP = 36102, avg_net_income_per_partner = 44000) %>%
  pivot_longer(!year, names_to = "personnel_type", values_to = "salary_nominal") %>%
  arrange(year) %>%
  filter(personnel_type != "avg_CAP")

associates_clean = associates_read %>%
  filter(`NLJ250 Rank` < 101) %>%
  group_by(`Fiscal Year`) %>%
  summarise(first_year_low = mean(`First Year Salary Low`, na.rm = T), 
            first_year_high = mean(`First Year Salary High`, na.rm = T)) %>%
  rowwise() %>%
  mutate(first_year = mean(c(first_year_low, first_year_high), na.rm = T)) %>%
  filter(`Fiscal Year` != 2021, `Fiscal Year` != 2012) %>%
  select(`Fiscal Year`, first_year) %>%
  rename(year = `Fiscal Year`) %>%
  bind_rows(c(year = 1968, first_year = mean(14000, 13500*2, 12750*2, 11750, 12500, 
                                             12000*3, 11250, 11000))) %>%
  ungroup()


## Housing data -------------------------



dc_house_data_pre_90 = dc_house_data %>%
  pivot_longer(everything(), names_to = "year", values_to = "house_price_pre_90") %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(across(where(is.character), ~ gsub(",", "", .x))) %>% 
  mutate(across(where(is.character), as.numeric)) %>%
  complete(year = full_seq(year, period = 1)) 

### Description: Adjusting the Case-Schiller data to nominal amounts
#### The Case-Schiller data is reported in the year 2000 dollars. The following code uses the historical 
#### inflation index to readjust the Case-Schiller values to nominal amounts in order to have a similar base of comparison with salary data (all in 2011 dollars).  

dc_2000s_house_price = dc_house_data_pre_90[dc_house_data_pre_90$year == "2000",]$house_price_pre_90
base_year_val_cpi_2000 = inflation_df$cpi[inflation_df$year == 2000]

dc_house_data_post_90 = case_schiller %>%
  rename(cs_val = WDXRSA) %>%
  mutate(house_val = cs_val * dc_2000s_house_price/100) %>%
  mutate(year = as.double(format(observation_date, "%Y"))) %>%
  group_by(year) %>%
  summarise(house_val_post_90_real = mean(house_val)) %>%
  left_join(inflation_df) %>%
  mutate(house_val_post_90_nominal = house_val_post_90_real * cpi/base_year_val_cpi_2000)

dmv_house_data = dc_house_data_pre_90 %>%
  full_join(dc_house_data_post_90) %>%
  mutate(house_val = case_when(!is.na(house_val_post_90_nominal) ~ house_val_post_90_nominal,
                               T ~ house_price_pre_90)) %>%
  select(year, house_val)


## Combining federal employee salaries, private law salaries, and DC housing prices -------
gov_law_df = left_join(president_df, judge_df, by = "year") %>%
  left_join(., es_df, by = "year") %>%
  left_join(., gs_df, by = "year") %>%
  left_join(., associates_clean, by = "year") %>%
  left_join(., dmv_house_data, by = "year") %>%
  pivot_longer(!year, names_to = "personnel_type", values_to  = "salary_nominal") %>%
  bind_rows(law_df) %>%
  mutate(personnel_type = case_when(personnel_type == "president_salary_nominal" ~ "President",
                                    personnel_type == "district_salary_nominal"~"District court judge",
                                    personnel_type == "appeals_salary_nominal" ~ "Appeals court judge",
                                    personnel_type == 
                                      "chief_justice_salary_nomial" ~ "Chief Justice",
                                    personnel_type == 
                                      "associate_justice_salary_nominal" ~ "Associate Justice",
                                    personnel_type == "ES_level_1_simp" ~ "Federal Reserve Chair",
                                    personnel_type == "ES_level_3_simp" ~ "FTC Chair/AAG at DOJ",
                                    personnel_type == "GS_15_avg_simp" ~ "GS-15 staff average",
                                    personnel_type == "avg_net_income_per_partner" ~ 
                                      "Avg comp per equity partner",
                                    personnel_type == "avg_CAP" ~ 
                                      "Average compensation for all partners", 
                                    personnel_type == "house_val" ~ "DC median house price",
                                    personnel_type == "first_year" ~ "Law firm associate- first year")) 




# Personnel nominations  -------------------------

## Summarize personnel trajectories ---- 
personnel_nominations_summary_long = personnel_nominations_df %>%
  mutate(era = case_when(Year_started < 1976 ~ "Pre-1976",
                         T ~ "Post-1975")) %>%
  select(Type_of_previous_position, Type_of_following_position, era) %>%
  mutate(sum_type_prev_pos = case_when(
    Type_of_previous_position == "Industry" | Type_of_previous_position == "Law firm" ~ "Law firm &\nindustry",
    Type_of_previous_position == "Government" | 
      Type_of_previous_position == "Alternate" |
      Type_of_previous_position == "Academia" ~ "Government &\nacademia",
    T ~ "Other")) %>%
  mutate(sum_type_follow_pos = case_when(
    Type_of_following_position == "Industry" | 
      Type_of_following_position == "Law firm" ~ "Law firm &\nindustry",
    Type_of_following_position == "Government" | 
      Type_of_following_position == "Alternate" |
      Type_of_following_position == "Academia" ~ "Government &\nacademia",
    T ~ "Other")) %>%
  pivot_longer(cols = !era, values_to = "Type of positions") %>%
  filter(name != "Type_of_previous_position" & name != "Type_of_following_position") %>%
  mutate(direction = case_when(str_detect(name, "prev") ~ "Previous position",
                               T ~ "Following position")) %>%
  mutate(direction_f = factor(direction, levels = c('Previous position', 'Following position')),
         Era = factor(era, levels = c('Pre-1976', 'Post-1975'))) 

personnel_nominations_summary = personnel_nominations_summary_long %>%
  group_by(era, name, `Type of positions`, direction) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(era, direction) %>%
  mutate(freq = (count/sum(count))* 100) %>%
  mutate(direction = case_when(str_detect(name, "prev") ~ "Previous position",
                               T ~ "Following position")) %>%
  mutate(direction_f = factor(direction, levels = c('Previous position', 'Following position')),
         Era = factor(era, levels = c('Pre-1976', 'Post-1975'))) 



## Nomination hearings ----

### Clean nomination data
nomination_df_clean = personnel_nominations_df %>%
  filter(str_detect(hearingDocsObtained, "[Yy]es") |
           str_detect(hearingDocsObtained, "No - Recess appointment") |
           str_detect(hearingDocsObtained, "No Committee hearing") |
           str_detect(hearingDocsObtained, "Only have the documents, not the hearing itself.")) %>%
  mutate(enforcementEra = ifelse(Year_started < 1975, "High enforcement", "Low enforcement")) %>%
  mutate(era = case_when(Year_started > 1968 & Year_started < 1980 ~ "1969-1979",
                         Year_started > 1979 & Year_started < 1990 ~ "1980-1989",
                         Year_started > 1989 & Year_started < 2000 ~ "1990-1999",
                         Year_started > 1999 & Year_started < 2010 ~ "2000-2009",
                         Year_started > 2009 & Year_started < 2020 ~ "2010-",
                         T ~ as.character(NA))) %>%
  mutate(hearingMinutesInt = as.double(`Hearing duration (in minutes)`)) %>%
  mutate(substantiveDiscussionsYes = ifelse(str_detect(`Substantive discussions`, "[Yy]es"), 
                                            1, 0)) %>%
  mutate(substantiveDiscussionsNo = ifelse(str_detect(`Substantive discussions`, "[Yy]es"), 
                                           0, 1)) %>%
  mutate(proEnforcementYes = ifelse(str_detect(`Pro-Enforcement`, "Yes") &
                                      substantiveDiscussionsYes == 1, 1, 0)) %>%
  mutate(proEnforcementNo = ifelse(str_detect(`Pro-Enforcement`, "No") &
                                     substantiveDiscussionsYes == 1, 1, 0)) %>%
  mutate(proEnforcementMixed = ifelse(str_detect(`Pro-Enforcement`, "Mixed") & 
                                        substantiveDiscussionsYes == 1, 1, 0)) 

### Incorporate presidential information into nomination hearings data
pageNoms = read_html("https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States")

html_tbls_noms = pageNoms %>%
  html_elements("table") %>%
  html_table()

presidentialTermYears = html_tbls_noms[[1]] %>%
  select(`Name(Birth–Death)`, `Term[14]`) %>%
  rename(Term = `Term[14]`) %>%
  rowwise() %>%
  mutate(termStartYear = as.integer(str_extract_all(Term, ("[0-9]{4}"))[[1]][1])) %>%
  mutate(termEndYear = as.integer(str_extract_all(Term, ("[0-9]{4}"))[[1]][2])) %>%
  mutate(Name = case_when(str_detect(`Name(Birth–Death)`, "\\]\\(") ~ 
                            str_extract(`Name(Birth–Death)`, "\\s[A-z][a-z]+\\["),
                          T ~ str_extract(`Name(Birth–Death)`, "\\s[A-z][a-z]+\\("))) %>%
  mutate(Name = str_sub(Name, 2, nchar(Name)-1)) %>%
  filter(termStartYear > 1932) %>%
  group_by(Name, termStartYear) %>%
  summarize(Name = first(Name), 
            termStartYear = first(termStartYear), 
            termEndYear = first(termEndYear)) %>%
  ungroup() %>%
  mutate(Name = case_when(Name == "Roosevelt" ~ "FDR",
                          Name == "Kennedy" ~ "JFK",
                          Name == "Johnson" ~ "LBJ",
                          Name == "Bush" & termStartYear == 1989 ~ "GHWB",
                          Name == "Bush" & termStartYear == 2001 ~ "GWB",
                          T ~ Name))

# Supreme Court data -------------------------

## SCT justice biographic data ------
sc_judge = sc_judge_read %>%
  mutate(year_oath = format(`Date of Oath`, "%Y")) %>%
  mutate(across(c(CC_endorse, WSJ_endorse),  na_if, "Unavailable")) %>%
  mutate(era = case_when(year_oath > 1975 ~ "Low enforcement",
                         T ~ "High enforcement")) %>%
  mutate(across(.cols = ends_with("endorse"), .fns = ~ case_when(str_detect(., "y") ~ 1, 
                                                                 T ~ 0))) %>%
  mutate(name_edit = `Last Name, First Name`) %>%
  mutate(last_name = str_extract(name_edit, "^[a-zA-Z]*")) %>%
  mutate(row_num = 1:n()) %>%
  mutate(initials = str_extract_all(name_edit, "[A-Z]")[row_num]) %>%
  mutate(middle_initial = as.character(map(initials, 3))) %>%
  mutate(first_initial = as.character(map(initials, 2))) %>%
  mutate(across(middle_initial:first_initial, ~ replace(., . == "NULL", NA))) %>%
  mutate(name_final = case_when(is.na(middle_initial) ~ paste(first_initial, last_name, sep = ""),
                                T ~ paste(first_initial, middle_initial, last_name, sep = ""))) %>%
  mutate(name_final = case_when(name_edit == "O'Connor, Sandra Day" ~ "SDOConnor",
                                name_edit == "Harlan, John Marshall" ~ "JHarlan2",
                                name_edit == "Hughes, Charles Evans" ~ "CEHughes2",
                                T ~ name_final)) %>%
  rename(justiceName = name_final)

### Law school ------
page = read_html("https://en.wikipedia.org/wiki/List_of_law_schools_attended_by_United_States_Supreme_Court_justices")

listWiki = page %>% 
  html_nodes('ol') %>% 
  purrr::map(~html_nodes(.x, 'li') %>% 
               html_text() %>% 
               gsub(pattern = '\\t|\\r|\\n', replacement = '')
  )

harvardorYaleAttend = as.data.frame(listWiki[1], col.names = c("justice")) %>%
  bind_rows(  as.data.frame(listWiki[2], col.names = c("justice"))) %>%
  mutate(justice = str_remove(justice, " –.*")) %>%
  mutate(justice = str_remove(justice, " Jr.")) %>%
  mutate(lastName = str_extract(justice, "[A-z]*$"))


sc_judge = sc_judge %>%
  rowwise() %>%
  mutate(schoolAttended = ifelse(last_name %in% harvardorYaleAttend$lastName, 
                                 "Harvard/Yale", 
                                 "Other")) 

### Term dates ----

page = read_html("http://scdb.wustl.edu/documentation.php?var=majOpinWriter")

html_tbls = page %>%
  html_elements("table") %>%
  html_table()

justice_numbers_tbl = html_tbls[[6]] %>%
  rename(justiceNumber = X1, justiceName = X2, justiceDates = X3) 

justice_term_dates = justice_numbers_tbl %>%
  rowwise() %>%
  mutate(serviceStarts = str_extract_all(justiceDates, "[0-9]{4}")[[1]][[1]],
         serviceEnds = ifelse(str_detect(justiceDates, "0000"), 
                              " ",
                              str_extract_all(justiceDates, "[0-9]{4}")[[1]][2])) %>%
  mutate(termDates = case_when((serviceEnds == " ") ~  
      paste0("('", substr(serviceStarts, 
                          3, 
                          nchar(serviceStarts)),
             "-", serviceEnds, ")"),
    T ~  paste0("('", substr(serviceStarts, 3, nchar(serviceStarts)),
                "-'", substr(serviceEnds, 3, nchar(serviceEnds)),
                ")"))) 


## Clean SCT justice vote data ------

justice_per_court_raw = justice_per_court_read %>%
  select(term, justiceName, caseId) %>%
  distinct(justiceName, term, .keep_all = T) %>%
  filter(str_detect(caseId, "001")) %>%
  pivot_wider(names_from = term, values_from = caseId) %>%
  mutate(across(!justiceName, ~ case_when(!is.na(.) ~ 1, T ~ 0)))


## Combine SCT justice vote data and biographic information -----
justice_per_court =  justice_per_court_raw %>%
  left_join(sc_judge, keep = T) %>%
  rename(justiceName = justiceName.y) %>%
  select(!(`1946`:`1951`)) %>%
  filter(justiceName != "WBRutledge" &  justiceName != "FMurphy" &
           justiceName != "ACBarrett")


## Merging dataset of SCT justice votes with handcoded antitrust cases -----

justice_votes_per_case = justice_per_court_read %>%
  #merge list of antitrust cases with justice votes cases
  mutate(sctCite = gsub("[S]. ", "S.", sctCite)) %>%
  filter(usCite %in% filter(antitrust_westlaw_sc_cases, includeFinal == "Yes" 
                            | includeFinal == "yes")$Citation
         | sctCite %in% filter(antitrust_westlaw_sc_cases, includeFinal == "Yes" 
                               | includeFinal == "yes")$Citation) %>%
  mutate(Citation = ifelse(is.na(usCite), sctCite, usCite)) %>%
  select(voteId, dateDecision, Citation, caseName, majOpinWriter, majVotes:majority) %>%
  pivot_wider(names_from = justiceName, values_from = c(justice, vote:majority)) %>%
  group_by(Citation) %>%
  summarise(caseName = first(caseName), dateDecision = first(dateDecision), 
            majOpinWriter = mean(majOpinWriter),
            majVotes = mean(majVotes), minVotes = mean(minVotes),
            across(starts_with("vote_"), ~ mean(.x, na.rm = T))) %>%
  #add per curiam decision
  add_row(Citation = "352 U.S. 992", 
          caseName = "LAWLOR v. NATIONAL SCREEN SERVICE CORP.",
          dateDecision = "2/25/1957",
          majOpinWriter = NA,
          majVotes = 5,
          minVotes = 3,
          vote_HLBlack = 1,
          vote_SFReed = 1,
          vote_FFrankfurter = 2,
          vote_WODouglas = 1,
          vote_HHBurton = 2,
          vote_TCClark = 1,
          vote_JHarlan2 = 2,
          vote_WJBrennan = 1,
          vote_EWarren = 1) %>%
  # add handcoded court case characteristics
  left_join(., select(filter(antitrust_westlaw_sc_cases, 
                             Citation != "401 U.S. 1204"), Citation, includeFinal:sherman2),
            by = c("Citation")) 


justice_votes = justice_votes_per_case %>%
  mutate(year = as.integer(str_extract(dateDecision, "[0-9]{2}$"))) %>%
  mutate(year = case_when(year < 51 & year > 9 ~ as.integer(paste0("20", year)),
                          year < 51 & year < 10 ~ as.integer(paste0("200", year)),
                          T ~ as.integer(paste0("19", year)))) %>%
  mutate(party1_comp = case_when(str_detect(party1, "[Cc]ompan") ~ 1,
                                 T ~ 0)) %>%
  mutate(party2_comp = case_when(str_detect(party2, "[Cc]ompan") ~ 1,
                                 T ~ 0)) %>%
  mutate(party1_gov = case_when(str_detect(party1, "[Gg]ov") ~ 1,
                                T ~ 0)) %>%
  mutate(party2_gov = case_when(str_detect(party2, "[Gg]ov") ~ 1,
                                T ~ 0)) %>%
  mutate(party1_indiv = case_when(str_detect(party1, "[Ii]ndiv") ~ 1,
                                  T ~ 0)) %>%
  mutate(party2_indiv = case_when(str_detect(party2, "[Ii]ndiv") ~ 1,
                                  T ~ 0)) %>%  
  mutate(party1_comp_size_big = case_when(party1_comp == 1 & str_detect(party1, "^[Bb]ig") ~ T,
                                          T ~ F)) %>%
  mutate(party2_comp_size_big = case_when(party2_comp == 1 & str_detect(party2, "^[Bb]ig") ~ T,
                                          T ~ F)) %>%
  mutate(parties_comp = case_when(party1_comp == 1 & party2_comp == 1 ~ 1,
                                  T ~ 0)) %>%
  mutate(slash_or_comp_size_unclear = case_when(str_detect(party1, "/") | str_detect(party2, "/") ~ 1,
                                                str_detect(party1, "Company") & str_detect(party2, "Company") ~ 1,
                                                T ~ 0)) %>%
  mutate(type_case_score = case_when(scoreType == "plb" ~ 2,
                                     scoreType == "govCase" ~ 1,
                                     T ~ 0)) %>%
  mutate(across(starts_with("vote_"),
                ~ case_when(is.na(.) ~ "NA",
                            . == 1 & proAntitrustEnforcement == "Yes" ~ "pro-enforcement",
                            . == 1 & proAntitrustEnforcement == "No" ~ "anti-enforcement",
                            
                            . == 2 & proAntitrustEnforcement == "Yes" ~ "anti-enforcement",
                            . == 2 &  proAntitrustEnforcement == "No" ~ "pro-enforcement",
                            
                            (. == 3 | . == 4) & proAntitrustEnforcement == "Yes" ~ "pro-enforcement",
                            (. == 3 | . == 4) & proAntitrustEnforcement == "No" ~ "anti-enforcement",
                            
                            (. == 6 | . == 7) & proAntitrustEnforcement == "Yes" ~ "anti-enforcement",
                            (. == 6 | . == 7) & proAntitrustEnforcement == "No" ~ "pro-enforcement",
                            #no 5s or 8s in dataset
                            T ~ "NA"),
                .names = "dir_{.col}")) 




# Polling on antitrust -------------------------

## Roper Center: public attitude toward antitrust (all questions) -------------------------
dfRoper = dfRoper_load %>%
  bind_rows(dfRoperBusiness_load) %>%
  bind_rows(dfRoperGE_load) %>%
  mutate(century = case_when(as.integer(str_extract(BegDate, "[0-9]{1,2}$")) > 22 ~ "19",
                             T ~ "20")) %>%
  mutate(beginDate =  as.Date(paste0(str_extract(BegDate, "[0-9]{1,2}/[0-9]{1,2}/"),
                                     century,
                                     str_extract(BegDate, "[0-9]{1,2}$")), "%m/%d/%Y")) %>%
  mutate(endDate = as.Date(paste0(str_extract(EndDate, "[0-9]{1,2}/[0-9]{1,2}/"),
                                  century,
                                  str_extract(EndDate, "[0-9]{1,2}$")), , "%m/%d/%Y")) %>%
  mutate(year = as.integer(format(endDate, format="%Y"))) %>%
  mutate(RespPct = as.integer(RespPct)) %>%
  mutate(RespPct = replace_na(RespPct, 0))

## Gallup  -------------------------

dfBigBusiness_clean = html_tbls_Gallup[[5]] %>%
  slice(1:(n() - 2)) %>%
  filter(!row_number() == 2) %>% 
  mutate(X1 = case_when(X1 == "" ~ "year",
                        T ~ X1)) %>%
  janitor::row_to_names(1) %>%
  mutate(year = str_extract(year, "[0-9]{4}")) %>%
  mutate(across(where(is.character), as.integer)) %>%
  mutate(across(where(is.integer), ~replace_na(.x, 0))) 

## Combining survey responses into three groups (pro, anti, and ambigious) ------

### Roper --------
dfRoper_m = dfRoper %>%
  mutate(AntitrustDirectionCombo = case_when(str_detect(AntitrustDirection, "pro") ~ "pro",
                                             str_detect(AntitrustDirection, "anti") ~ "anti",
                                             T ~ AntitrustDirection)) %>%
  group_by(QuestionTxt, AntitrustDirectionCombo, questionType, QuestionID) %>%
  summarise(RespPct = mean(RespPct), year = first(year), SampleSize = mean(SampleSize)) 

### Gallup ----
dfBigBusiness_m = dfBigBusiness_clean %>%
  mutate(anti = `Great deal` + `Quite a lot`,
         ambig = Some + `No opinion`,
         pro = `Very little` + `None (vol.)`,
         QuestionID = paste("confidence in big business - Gallup", as.character(row_number()))) %>%
  select(year, anti:pro, QuestionID) %>%
  pivot_longer(!c(year, QuestionID),
               names_to = "AntitrustDirectionCombo",
               values_to = "RespPct") %>%
  mutate(questionType = "Gallup",
         QuestionTxt = "Now I am going to read you a list of institutions in American society. Please tell me how much confidence you, yourself, have in each one -- a great deal, quite a lot, some or very little? Big business")
