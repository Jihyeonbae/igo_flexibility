
library("pacman")
p_load(plyr, dplyr, tidyr, ggplot2, tidyverse, RColorBrewer, readxl,
       readr, haven, countrycode,
       rqog, igoR, modelsummary, knitr, kableExtra, flextable, plm)

qog<-rqog::read_qog(which_data = "standard", data_type = "time-series") 

qog<-qog%>%
  filter(year>=1950 & year<2015) 

COW<-igoR::igo_year_format3 %>%
  filter(year>=1950)

  
country <-qog %>%
  dplyr::select(cname, year, ccodecow, 
                # World Development Indicators
                wdi_gdpcapcon2015, wdi_pop, wdi_trade, 
                # judicial corruption decision
                vdem_jucorrdc, 
                ciri_assn, #freedom of association
                ciri_injud, # independence of the judiciary
                # Security variables
                atop_number, 
                # Democracy Indices 
                vdem_polyarchy, vdem_libdem, vdem_partipdem,
                # Democracy Indices (Alternatives for robustness check)
                p_polity2, bmr_dem, ht_regtype1,
                # Globalization Index (political)
                kofgi_dr_pg=dr_pg,
                # colonial legacy
                ht_colonial,
                # Political Terror Scale
                pts_ptsa=gd_ptsa) %>%  
  dplyr::mutate(wdi_log_gdpcapcon2015=log(wdi_gdpcapcon2015),
                wdi_log_pop=log(wdi_pop),
                wdi_log_trade=log(wdi_trade))%>%
  dplyr::rename(ccode=ccodecow)%>%
  dplyr::relocate(ccode, cname, year)

country <- country %>%
  mutate(ht_colonial=ifelse(ht_colonial>1, 1, 0),
         ht_regtype1=case_when(ht_regtype1==9 ~ 5, 
                               ht_regtype1==99 ~ 6,
                               ht_regtype1==100 ~ 7,
                               TRUE ~ ht_regtype1))



#Check rows that have NA values for VARIABLE_OF_INTEREST
country[is.na(country$ccode),]

#Fill in NAs
country$ccode[country$cname == "Ethiopia"] <- 530
country$ccode[country$cname == "Germany"] <- 255
country$ccode[country$cname == "Yemen Democratic"] <- 680
country$ccode[country$cname == "Yemen"] <- 679
country$ccode[country$cname == "Sudan"] <- 625
country$ccode[country$cname == "Cyprus"] <- 352
country$ccode[country$cname == "Pakistan"] <- 770
country$ccode[country$cname == "Vietnam, North"] <- 816
country$ccode[country$cname == "Vietnam, South"] <- 817
country$cname[country$cname == "USSR"] <- "Russian Federation (the)"


library(unvotes)
library(lubridate)

data(un_votes)
data(un_roll_calls)
data(un_roll_call_issues)

# --- Step 1: Build country-year resistance score from UNGA votes ---
un_resistance <- un_roll_calls %>%
  dplyr::left_join(un_roll_call_issues, by = "rcid") %>%
  dplyr::filter(issue %in% c("Human rights")) %>%
  dplyr::left_join(un_votes, by = "rcid") %>%
  dplyr::mutate(
    year = lubridate::year(date),
    # "no" vote = resisting democratic/HR norms
    resist = as.integer(vote == "no")
  ) %>%
  dplyr::mutate(
    ccode = countrycode::countrycode(country,
                                     origin      = "country.name",
                                     destination = "cown",
                                     warn        = FALSE)
  ) %>%
  dplyr::filter(!is.na(ccode)) %>%
  dplyr::group_by(ccode, year) %>%
  dplyr::summarise(
    pct_resist = mean(resist,        na.rm = TRUE),
    n_votes    = dplyr::n(),
    .groups    = "drop"
  )

un_resistance<-un_resistance %>% filter(is.na(ccode)==FALSE)

un_endorse <- un_roll_calls %>%
  dplyr::left_join(un_roll_call_issues, by = "rcid") %>%
  dplyr::filter(issue %in% c("Human rights")) %>%
  dplyr::left_join(un_votes, by = "rcid") %>%
  dplyr::mutate(
    year = lubridate::year(date),
    # "no" vote = resisting democratic/HR norms
    resist = as.integer(vote == "yes")
  ) %>%
  dplyr::mutate(
    ccode = countrycode::countrycode(country,
                                     origin      = "country.name",
                                     destination = "cown",
                                     warn        = FALSE)
  ) %>%
  dplyr::filter(!is.na(ccode)) %>%
  dplyr::group_by(ccode, year) %>%
  dplyr::summarise(
    pct_endorse = mean(resist,        na.rm = TRUE),
    n_votes    = dplyr::n(),
    .groups    = "drop"
  )


library(readr)
ideal <- read_csv("dataverse_files-3/IdealpointestimatesAll_Jun2024.csv", 
                                           col_types = cols(...1 = col_skip()))
ideal<-ideal%>%
  dplyr::select(ccode, session, "Q50%All") %>%
  dplyr::rename(point = "Q50%All")

session_info <- unvotes::un_roll_calls %>%
  mutate(year = lubridate::year(date)) %>%
  dplyr::select(year, session) %>%
  distinct()


ideal<-ideal %>%
  left_join(session_info, by="session") %>%
  dplyr::select(-session)

country <- country %>%
  left_join(ideal, by =  c("ccode", "year")) %>%
  left_join(un_resistance, by = c("ccode", "year"))%>%
  left_join(un_endorse, by = c("ccode", "year"))

COW <- COW %>%
  dplyr::rename(cow_igocode = ionum)%>%
  dplyr::select(-c(igocode, version, accuracyofpre1965membershipdates,sourcesandnotes, imputed)) %>%
  dplyr::relocate(cow_igocode, ioname, year, political, social, economic)%>%
  pivot_longer(c(`afghanistan`:`zimbabwe`),
               names_to="country",
               values_to="membership")%>%
  dplyr::filter(membership==1) #member states only


COW <- COW %>%
  dplyr::mutate(country = dplyr::case_when(
    country == "austriahungary" ~ "Austria-Hungary",
    country == "domrepublic" ~ "Dominican Republic",
    country == "etimor" ~ "East Timor",
    country == "hessegrand" ~ "Hesse Grand Ducal",
    country == "micronesiafs" ~ "Federated States of Micronesia",
    country == "nokorea" ~ "North Korea",
    country == "soafrica" ~ "South Africa",
    country == "sokorea" ~ "South Korea",
    country == "stlucia" ~ "St. Lucia",
    country == "wgermany" ~ "German Federal Republic",
    country == "syemen" ~ "Yemen People's Republic",
    TRUE ~ country  
    # keep original value for all other cases
  ))

#Attaching Country numeric code to character values
COW$ccode<-countrycode(COW$country, 
                       origin='country.name', 
                       destination='cown', 
                       warn = TRUE)

COW<-COW%>%
  filter(!is.na(ccode))


join_events <- COW %>%
  dplyr::group_by(ccode, ioname) %>%
  dplyr::summarise(join_year = min(year, na.rm = TRUE), .groups = "drop") 

join_counts <- join_events %>%
  dplyr::group_by(cname, join_year) %>%
  dplyr::summarise(n_igos = n(), .groups = "drop")

country_ts <- country %>%
  dplyr::select(ccode, cname, year, vdem_polyarchy)

selected_countries <- c("United States", "Germany", "India", "Brazil", "China", "Korea (the Republic of)")

country_ts_subset <- country_ts %>%
  filter(cname %in% selected_countries)

join_events <- join_events %>%
  filter(cname %in% selected_countries)

join_counts <- join_counts %>%
  filter(cname %in% selected_countries)
library(ggplot2)



igo_master <- COW %>%
  dplyr::inner_join(country, by=c("ccode", "year"))%>%
  dplyr::select(-c(orgname, longorgname, membership))


igo_master <- igo_master %>%
  group_by(cow_igocode, year) %>%
  mutate(gdp_share = 100*(wdi_gdpcapcon2015/sum(wdi_gdpcapcon2015, na.rm = TRUE)),
         poly_share = 100*(vdem_polyarchy/sum(vdem_polyarchy, na.rm = TRUE)))



igo <- igo_master %>%
  dplyr::group_by(cow_igocode, ioname, year) %>%
  dplyr::summarise(
    #average democracy scores
    polyarchy = mean(vdem_polyarchy, na.rm = TRUE),
    w.polyarchy = weighted.mean(vdem_polyarchy, w = wdi_log_gdpcapcon2015/sum(wdi_log_gdpcapcon2015), na.rm=TRUE),
    polyarchy_median = median(vdem_polyarchy, na.rm = TRUE),
    partipdem = mean(vdem_partipdem, na.rm=TRUE),
    partipdem_median = median(vdem_partipdem, na.rm=TRUE),
    libdem = mean(vdem_libdem, na.rm = TRUE),
    libdem_median = median(vdem_libdem, na.rm = TRUE),
    polity = mean(p_polity2, na.rm = TRUE),
    #economic variables
    gdp_cap = mean(wdi_log_gdpcapcon2015, na.rm = TRUE),
    population=mean(wdi_log_pop, na.rm=TRUE),
    trade=mean(wdi_log_trade, na.rm=TRUE),
    alliances = mean(atop_number, na.rm = TRUE),
    # Governance
    ciri_injud=mean(ciri_injud, na.rm=TRUE),
    #characteristics 
    number = n(),
    trade = mean(wdi_log_trade, na.rm = TRUE),
    percentage = sum(bmr_dem, na.rm = TRUE) / number,
    political = mean(political, na.rm = TRUE),
    social = mean(social, na.rm = TRUE),
    economic = mean(economic, na.rm = TRUE),
    colonial= sum(ht_colonial, na.rm=TRUE) / number,
    #asymmetry index
    polity_sd = sd(p_polity2, na.rm = TRUE),
    polyarchy_sd = sd(vdem_polyarchy, na.rm = TRUE),
    libdem_sd=sd(vdem_libdem, na.rm=TRUE),
    partipdem_sd=sd(vdem_partipdem, na.rm=TRUE),
    econ_sd = sd(wdi_log_gdpcapcon2015, na.rm = TRUE),
    # HH index
    hh_poly = sum(abs(poly_share), na.rm = TRUE) - (1 / n()),
    hh_gdp = sum(abs(gdp_share), na.rm = TRUE) - (1 / n()),
    resist_min  = ifelse(sum(!is.na(pct_resist)) > 0,
                         min(pct_resist,  na.rm = TRUE), NA),
    resist_mean = ifelse(sum(!is.na(pct_resist)) > 0,
                         mean(pct_resist, na.rm = TRUE), NA),
    resist_sd   = ifelse(sum(!is.na(pct_resist)) > 1,
                         sd(pct_resist,   na.rm = TRUE), NA),
    resist_pct  = ifelse(sum(!is.na(pct_resist)) > 0,
                         mean(pct_resist > 0.5, na.rm = TRUE), NA),
    endorse_min  = ifelse(sum(!is.na(pct_endorse)) > 0,
                         min(pct_endorse,  na.rm = TRUE), NA),
    endorse_mean = ifelse(sum(!is.na(pct_endorse)) > 0,
                         mean(pct_endorse, na.rm = TRUE), NA),
    endorse_sd   = ifelse(sum(!is.na(pct_endorse)) > 1,
                         sd(pct_endorse, na.rm = TRUE), NA),
    endorse_pct  = ifelse(sum(!is.na(pct_resist)) > 0,
                         mean(pct_endorse > 0.5, na.rm = TRUE), NA),
    # Coefficient of variation
    polity_cv = polity_sd / polity,
    polyarchy_cv = polyarchy_sd / polyarchy,
    ideal_mean = mean(point, na.rm = TRUE),
    ideal_sd = sd(point, na.rm=TRUE)
  )

write_rds(igo, "~/Desktop/gg_igo_data.rds")


MIA <- MIA %>%
  dplyr::select(ionumber, year, inception, typeI, pooling, delegation, delconstit, poolconstit)%>%
  dplyr::rename(cow_igocode = ionumber)

igo_dataset <- MIA %>% dplyr::left_join(igo, by = c("cow_igocode", "year"))
saveRDS(igo_dataset, "~/Desktop/igo_mia_2026.rds")
