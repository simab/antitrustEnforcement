# Agency budget and FTE data  -------------------------

## DOJ data  ----------------
## Name: DOJ Congressional appropriation and merger filing data
## Description: DOJ Congressional appropriations and merger filing fees starting in 1903 
## Source: DOJ Antitrust Division website
## Note: The DOJ merger filing fees are also the FTC's merger filing fees
doj_df_read = read_excel("analysis/data/agency budgets/doj_atr-appropriation-figures1017.xls", sheet = "data")
names(doj_df_read) = c("year", "doj_direct_appro_amt", "doj_filing_fee_amt", "doj_total_amt", "notes")

## FTC data -----------------
## Name: FTC Congressional appropriations data
## Description: FTC Congressional appropriations data
## Source: Handcoded from Congressional Budget documents retrieved through FRASER. Copies of the original source materials can be found in "ftc appropriation docs / cong_appro". 
ftc_df_read = read_excel("analysis/data/agency budgets/ftc_appro.xlsx")


## Name: FTC fiscal and personnel budget by Mission
## Description: The FTC's budget broken down by type of Mission (ie. Promoting Competition (antitrust), Consumer Protection). 
## Source: Handcoded data from FTC Annual Reports. Copies of the original source materials can be found in *. 
ftc_breakdown_df_read = read_excel("analysis/data/agency budgets/ftc appropriations docs/ftc_financial_reports.xlsx")


# Historical inflation and real GDP adjustments -------------------------

# Data for inflation and GDP per capita adjustments ---
## Name: Inflation index
## Description: Loading two versions of the same inflation indexes from two different sources. One (from 1913) is an annual index and the other (starting in 1947) is a quarterly index.
## Sources: Bureau of Labor Statistics' CPI Index (U) website and FRED online portal
inflation_df_1913 = read_excel("analysis/data/CPI_index_u.xlsx")
inflation_df_1913 = inflation_df_1913 %>%
  filter(year < 1947)
inflation_df_1947 = read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
                               skip = 54, header = TRUE)

## Name: Historical GDP per capita data
## Description: US historical GDP per capita data
## Source: Maddison project (*)
real_gdp_per_capita_data = read_excel("analysis/data/gdp-per-capita-maddison-2020.xlsx", sheet = "data")

# Federal employee and private legal salary data -------------------------

## Federal employee salaries -------------------------

judge_data = read_excel("analysis/data/salaries/salary_data_gov_off.xlsx", sheet = "judges")
es_data = read_excel("analysis/data/salaries/salary_data_gov_off.xlsx", sheet = "es")
president_data = read_excel("analysis/data/salaries/salary_data_gov_off.xlsx", sheet = "president")
gs_data = read_excel("analysis/data/salaries/salary_data_gov_off.xlsx", sheet = "gs")


## Private law salaries  -------------------------

law_data = read_excel("analysis/data/salaries/AMLaw_salary_data_combo.xlsx")
associates_read = bind_rows(read_excel("analysis/data/salaries/NLJ250_202109301829578131.xlsx"),
                            read_excel("analysis/data/salaries/NLJ250_202109301829450006.xlsx"),
                            read_excel("analysis/data/salaries/NLJ250_202109301829304068.xlsx"),
                            read_excel("analysis/data/salaries/NLJ250_202109301829101412.xlsx"))

## Housing data -------------------------
 ## Source: ACS decennial survey on housing characteristics
dc_house_data = read.table("https://www2.census.gov/programs-surveys/decennial/tables/time-series/coh-values/values-unadj.txt", fill = T, header = F, skip = 14, nrow = 1, sep = "$")[,2:8]
dc_house_header = read.table("https://www2.census.gov/programs-surveys/decennial/tables/time-series/coh-values/values-unadj.txt", header = F, skip = 1, nrow = 1)
names(dc_house_data) = dc_house_header

case_schiller = read_excel("analysis/data/salaries/WDXRSA.xls", skip = 10)


# Personnel nominations  -------------------------

personnel_nominations_df = read_excel("analysis/data/nominations/personnel_data_220406.xlsx")


# Supreme Court data -------------------------
sct_hearings = read_excel("analysis/data/6_sctHearings.xlsx")

sc_judge_read = read_excel("analysis/data/sct/Supreme Court Chart.xlsx", sheet = "Business friendliness")

justice_per_court_read = fread("analysis/data/sct/SCDB_2021_01_justiceCentered_Citation.csv",
                                  select = c("caseId",
                                             "caseIssuesId",
                                             "voteId",
                                             "dateDecision",
                                             "decisionType",
                                             "usCite",
                                             "sctCite",
                                             "ledCite",
                                             "lexisCite",
                                             "term",
                                             "naturalCourt",
                                             "chief",
                                             "caseName",
                                             "dateArgument",
                                             "dateRearg",
                                             "petitioner",
                                             "petitionerState",
                                             "respondent",
                                             "respondentState",
                                             "jurisdiction",
                                             "adminAction",
                                             "adminActionState",
                                             "threeJudgeFdc",
                                             "caseOrigin",
                                             "caseOriginState",
                                             "caseSource",
                                             "caseSourceState",
                                             "lcDisagreement",
                                             "certReason",
                                             "lcDisposition",
                                             "lcDispositionDirection",
                                             "declarationUncon",
                                             "caseDisposition",
                                             "caseDispositionUnusual",
                                             "partyWinning",
                                             "precedentAlteration",
                                             "voteUnclear",
                                             "issue",
                                             "issueArea",
                                             "decisionDirection",
                                             "decisionDirectionDissent",
                                             "authorityDecision1",
                                             "authorityDecision2",
                                             "lawType",
                                             "lawSupp",
                                             "lawMinor",
                                             "majOpinWriter",
                                             "majOpinAssigner",
                                             "splitVote",
                                             "majVotes",
                                             "minVotes",
                                             "justice",
                                             "justiceName",
                                             "vote",
                                             "opinion",
                                             "direction",
                                             "majority",
                                             "firstAgreement",
                                             "secondAgreement"))

antitrust_westlaw_sc_cases = read_excel("analysis/data/sct/post51_sct_antitrust_cases_final.xlsx")

#antitrust_westlaw_sc_cases_sherman = read.csv("analysis/data/sct/03 2 2022 - post51_sct_antitrust_cases_sherman_coded.csv")
#antitrust_westlaw_sc_cases_clayton = read.csv("analysis/data/sct/03 2 2022 - post51_sct_antitrust_cases_clayton_coded.csv")

westlaw_sherman_search_df = fread("analysis/data/sct/Westlaw Edge - List of 677 results for advanced (Sherman Act OR Sherman Antitrust Act).csv",
                                     select = c(1:11))

bf_scores_read = fread("analysis/data/sct/Business2020.csv",
                               select = c("caseId",
                                          "usCite",
                                          "term",
                                          "caseName",
                                          "justiceName",
                                          "vote",
                                          "majority",
                                          "BusinessLitigant",
                                          "JVoteForBusiness",
                                          "nytSalience",
                                          "nytBusiness"))

# Polling on antitrust -------------------------

## Roper Center: public attitude toward antitrust (all questions) -------------------------
dir = as.array(list.files(path = paste0(getwd(), "/analysis/data/polling/roper polling data/overall")))
dfRoper_load = data.frame()

for(i in 1:length(dir)){
  fileDir = paste0(getwd(), 
                   "/analysis/data/polling/roper polling data/overall/",
                   as.character(dir[i]))
  temp = read.csv(fileDir, header = T, check.names = F) %>%
    mutate(QuestionID = as.character(QuestionID),
           RespPct = as.character(RespPct), 
           file = as.character(dir[i]),
           questionType = "overall")
  dfRoper_load = bind_rows(dfRoper_load, temp)
}

## Gallup  -------------------------

page = read_html("https://news.gallup.com/poll/1597/Confidence-Institutions.aspx")

html_tbls_Gallup = page %>%
  html_elements("table") %>%
  html_table()


## Roper Center: business power data ------
dir = as.array(list.files(path = paste0(getwd(), "/analysis/data/polling/roper polling data/business power")))
dfRoperBusiness_load = data.frame()

for(i in 1:length(dir)){
  fileDir = paste0(getwd(), 
                   "/analysis/data/polling/roper polling data/business power/",
                   as.character(dir[i]))
  temp = read.csv(fileDir, header = T, check.names = T) %>%
    mutate(QuestionID = as.character(QuestionID),
           RespPct = as.character(RespPct), 
           file = as.character(dir[i]),
           questionType = "powerDC")
  dfRoperBusiness_load = bind_rows(dfRoperBusiness_load, temp)
}
## Roper Center: regulation of large companies -----
dir = as.array(list.files(path = paste0(getwd(), "/analysis/data/polling/roper polling data/controlling large companies - GE survey")))
dfRoperGE_load = data.frame()

for(i in 1:length(dir)){
  fileDir = paste0(getwd(), 
                   "/analysis/data/polling/roper polling data/controlling large companies - GE survey/",
                   as.character(dir[i]))
  temp = read.csv(fileDir, header = T, check.names = T) %>%
    mutate(QuestionID = as.character(QuestionID),
           RespPct = as.character(RespPct), 
           file = as.character(dir[i]),
           questionType = "largeCompanyRegulation")
  dfRoperGE_load = bind_rows(dfRoperGE_load, temp)
}

# Executive policy -----

presidential_speeches = read_excel("analysis/data/4_presidentialSpeeches.xlsx")
party_platforms = read_excel("analysis/data/5_partyPlatforms.xlsx")
