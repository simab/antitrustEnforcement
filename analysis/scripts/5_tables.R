fig.path = here::here("analysis/results/tables")

# Nomination hearing stats ----
noms_by_pres_tbl = nomination_df_clean %>%
  left_join(presidentialTermYears, 
            by = c( "Administration_appointee" = "Name")) %>%
  filter(Year_started > 1968) %>%
  group_by(Administration_appointee) %>%
  summarise(Count = n(),
            Mean = round(mean(hearingMinutesInt, na.rm = T), 1),
            Median = median(hearingMinutesInt, na.rm = T),
            across(substantiveDiscussionsYes, ~(sum(., na.rm = T))),
            across(substantiveDiscussionsNo, ~(sum(., na.rm = T))),
            across(proEnforcementYes, ~(sum(., na.rm = T))),
            across(proEnforcementNo, ~(sum(., na.rm = T))),
            across(proEnforcementMixed, ~(sum(., na.rm = T)))) %>%
  ungroup() %>%
  mutate(percentSubstantive = 
           substantiveDiscussionsYes / (substantiveDiscussionsYes +
                                          substantiveDiscussionsNo)) %>%
  mutate(percentSubstantive = (100*round(percentSubstantive, 4))) %>%
  select(Administration_appointee: substantiveDiscussionsNo, 
         percentSubstantive,
         proEnforcementYes:proEnforcementMixed) %>%
  left_join(presidentialTermYears, 
            by = c( "Administration_appointee" = "Name")) %>%
  arrange(termStartYear) %>%
  rowwise() %>%
  mutate(across(termStartYear:termEndYear, ~(str_extract_all(., "[0-9]{2}")[[1]][2]))) %>%
  mutate(years = ifelse(is.na(termEndYear), 
                        paste0(" ('", termStartYear, "-)"),
                        paste0(" ('", termStartYear, "-'", termEndYear, ")"))) %>%
  mutate(Administration_appointee = paste0(Administration_appointee, 
                                           years))
  rename(`Appointing President (tenure)` = Administration_appointee,
         Yes = substantiveDiscussionsYes,
         No = substantiveDiscussionsNo,
         `%` = percentSubstantive,
         Pro = proEnforcementYes,
         Anti = proEnforcementNo,
         Mixed = proEnforcementMixed) 
  select(-termStartYear) %>%
 kable(caption = "Nomination hearing data by president",
       booktabs = T, linesep = "", longtable = T, "html", label = "test") %>%
 kable_styling(full_width = T,
   latex_options  = c("striped")
   ) %>%
 add_header_above(c(" " = 2, "Duration of nomination \nhearings (in minutes)" = 2,
                    "Substantive antitrust\n discussions?" = 3,
                    "Substantive attitudes\ntoward enforcement" = 3))

table1 = noms_by_pres_tbl %>%
  select()


file.name = paste0(fig.path, "/table1_noms_by_pres_tbl.pdf")
save_kable(noms_by_pres_tbl, 
           file = file.name)


