library(eurostat)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(ggpubr)

##### Working directory
setwd("I:/Il mio Drive/Ricerca/Statistical analysis of the economic situation of Italy/Paper_Lucarelli_FSE")

##### Total Population, Thousand persons, 15-64 anni
# Population by sex, age, citizenship and labour status (1 000) 
pop <- get_eurostat(id = "lfsq_pganws")
pop_red <- pop %>%
  filter(unit == "THS_PER",
         geo %in% c("IT","DE","FR","ES","EA20"),
         age %in% c("Y15-64"),
         wstatus %in% c("POP"),
         citizen %in% c("TOTAL"),
         sex %in% c("T")) %>%
  mutate(geo = case_when(geo == "IT" ~ "Italy",
                         geo == "FR" ~ "France",
                         geo == "DE" ~ "Germany",
                         geo == "ES" ~ "Spain",
                         geo == "EA20" ~ "Euro Area 20",
                         TRUE ~ geo)) %>%
  select(Geo = geo, Time = time, Pop_1564_Ths = values)


##### GDP and main components
# Seasonally and calendar adjusted data
# output, expenditure, income, Gross fixed capital formation, Gross capital formation,
# Compensation of employees, Subsidies, Final consumption expenditure of general government,
# Final consumption expenditure of households
gdp <- get_eurostat(id = "namq_10_gdp")
gdp_red <- gdp %>%
  filter(unit %in% c("PD15_EUR","CP_MEUR"),
         geo %in% c("IT","DE","FR","ES","EA20"),
         s_adj %in% c("SCA","SA"),
         na_item %in% c("B1GQ","B1G","P3","P31_S14","P3_S13","P5G","P51G","D1","D3")) %>%
  mutate(geo = case_when(geo == "IT" ~ "Italy",
                         geo == "FR" ~ "France",
                         geo == "DE" ~ "Germany",
                         geo == "ES" ~ "Spain",
                         geo == "EA20" ~ "Euro Area 20",
                         TRUE ~ geo),
         na_item = case_when(na_item == "B1GQ" ~ "GDP",
                             na_item == "B1G" ~ "GVA",
                             na_item == "P3" ~ "Fin_cons_exp",
                             na_item == "P3_S13" ~ "Fin_cons_exp_gengov",
                             na_item == "P31_S14" ~ "Fin_cons_exp_house",
                             na_item == "P5G" ~ "GCF",
                             na_item == "P51G" ~ "GFCF",
                             na_item == "D3" ~ "Subsidies",
                             na_item == "D1" ~ "Comp_empl",
                             TRUE ~ na_item),
         unit = case_when(unit == "CP_MEUR" ~ "CurrPrc_MEuro",
                          unit == "PD15_EUR" ~ "PriceIdx_2015",
                          TRUE ~ unit)) %>%
  mutate(Key = paste(na_item,unit,sep = "_")) %>%
  select(Geo = geo, Time = time, Key, values) %>%
  # France and Germany: seasonal adjusted data
  # Italy and Spain: seasonal and calendar adjusted data
  # Europa 20: non fornisce dati su sussidi e spesa consumi famiglie
  group_by(Geo,Time,Key) %>%
  summarise(mean_val = mean(values,na.rm=T)) %>%
  pivot_wider(names_from = "Key", values_from = "mean_val")


##### Employment
# Employment by sex, age and citizenship 
emp1 <- get_eurostat(id = "lfsq_egan")
emp1_red <- emp1 %>%
  filter(unit == "THS_PER",
         geo %in% c("IT","DE","FR","ES","EA20"),
         age %in% c("Y15-64"),
         citizen %in% c("TOTAL"),
         sex %in% c("T")) %>%
  mutate(geo = case_when(geo == "IT" ~ "Italy",
                         geo == "FR" ~ "France",
                         geo == "DE" ~ "Germany",
                         geo == "ES" ~ "Spain",
                         geo == "EA20" ~ "Euro Area 20",
                         TRUE ~ geo),
         Var = "Employed_1564_Ths") %>%
  select(Geo = geo, Time = time, Var, values)
# Temporary employees by sex, age and educational attainment level 
emp2 <- get_eurostat(id = "lfsq_etgaed")
emp2_red <- emp2 %>%
  filter(unit == "THS_PER",
         geo %in% c("IT","DE","FR","ES","EA20"),
         age %in% c("Y15-64"),
         sex %in% c("T"),
         isced11 %in% c("TOTAL")) %>%
  mutate(geo = case_when(geo == "IT" ~ "Italy",
                         geo == "FR" ~ "France",
                         geo == "DE" ~ "Germany",
                         geo == "ES" ~ "Spain",
                         geo == "EA20" ~ "Euro Area 20",
                         TRUE ~ geo),
         Var = "TempEmpl_1564_Ths") %>%
  select(Geo = geo, Time = time, Var, values)
# Employees by sex, age and educational attainment level 
emp3 <- get_eurostat(id = "lfsq_eegaed")
emp3_red <- emp3 %>%
  filter(unit == "THS_PER",
         geo %in% c("IT","DE","FR","ES","EA20"),
         age %in% c("Y15-64"),
         sex %in% c("T"),
         isced11 %in% c("TOTAL")) %>%
  mutate(geo = case_when(geo == "IT" ~ "Italy",
                         geo == "FR" ~ "France",
                         geo == "DE" ~ "Germany",
                         geo == "ES" ~ "Spain",
                         geo == "EA20" ~ "Euro Area 20",
                         TRUE ~ geo),
         Var = "Employees_1564_Ths") %>%
  select(Geo = geo, Time = time, Var, values)
# Self-employed by sex, age and educational attainment level 
emp4 <- get_eurostat(id = "lfsq_esgan2")
emp4_red <- emp4 %>%
  filter(unit == "THS_PER",
         geo %in% c("IT","DE","FR","ES","EA20"),
         age %in% c("Y15-64"),
         sex %in% c("T"),
         nace_r2 %in% c("TOTAL"),
         wstatus %in% c("SELF")) %>%
  mutate(geo = case_when(geo == "IT" ~ "Italy",
                         geo == "FR" ~ "France",
                         geo == "DE" ~ "Germany",
                         geo == "ES" ~ "Spain",
                         geo == "EA20" ~ "Euro Area 20",
                         TRUE ~ geo),
         Var = "SelfEmployed_1564_Ths") %>%
  select(Geo = geo, Time = time, Var, values)
emp <- bind_rows(emp1_red,emp2_red,emp3_red,emp4_red)
emp <- emp %>%
  pivot_wider(names_from = Var,values_from = values)



##### General Government consolidated gross debt
# General Government consolidated gross debt
debt1 <- get_eurostat(id = "gov_10q_ggdebt")
debt1_red <- debt1 %>%
  filter(unit %in% c("MIO_EUR"),
         geo %in% c("IT","DE","FR","ES","EA20"),
         sector %in% c("S13"),
         na_item %in% c("GD")) %>%
  mutate(geo = case_when(geo == "IT" ~ "Italy",
                         geo == "FR" ~ "France",
                         geo == "DE" ~ "Germany",
                         geo == "ES" ~ "Spain",
                         geo == "EA20" ~ "Euro Area 20",
                         TRUE ~ geo)) %>%
  select(Geo = geo, Time = time, GenGov_ConsGrossDebt = values)
# General government deficit (-) and surplus (+) and total expenditure
debt2 <- get_eurostat(id = "gov_10q_ggnfa")
debt2_red <- debt2 %>%
  filter(unit %in% c("MIO_EUR"),
         geo %in% c("IT","DE","FR","ES","EA20"),
         sector %in% c("S13"),
         s_adj %in% c("NSA"),
         na_item %in% c("B9","TE")) %>%
  mutate(geo = case_when(geo == "IT" ~ "Italy",
                         geo == "FR" ~ "France",
                         geo == "DE" ~ "Germany",
                         geo == "ES" ~ "Spain",
                         geo == "EA20" ~ "Euro Area 20",
                         TRUE ~ geo),
         na_item = case_when(na_item == "B9" ~ "GenGov_DeficitSurplus",
                             na_item == "TE" ~ "GenGov_TotalExpend",
                             TRUE ~ na_item)) %>%
  select(Geo = geo, Time = time, na_item, values) %>%
  pivot_wider(names_from = "na_item", values_from = "values")

##### Join datasets
DataQuarter <- full_join(x = debt1_red, y = debt2_red, by = c("Geo","Time"))
DataQuarter <- full_join(x = DataQuarter, y = emp, by = c("Geo","Time"))
DataQuarter <- full_join(x = DataQuarter, y = gdp_red, by = c("Geo","Time"))
DataQuarter <- full_join(x = DataQuarter, y = pop_red, by = c("Geo","Time"))

DataQuarter <- DataQuarter %>%
  arrange(desc(Time),Geo)

write.xlsx(x = DataQuarter, file = "MacroData_quarter.xlsx")



########## Inflation
infl <- get_eurostat(id = "ei_cphi_m")
infl_red <- infl %>%
  filter(time >= "2007-01-01",
         geo %in% c("IT","DE","FR","ES","EA20"),
         indic %in% c("CP-HI00","CP-HIE","CP-HIF","CP-HIG","CP-HIIG")) %>%
  mutate(geo = case_when(geo == "IT" ~ "Italy",
                         geo == "FR" ~ "France",
                         geo == "DE" ~ "Germany",
                         geo == "ES" ~ "Spain",
                         geo == "EA20" ~ "Euro Area 20",
                         TRUE ~ geo),
         indic = case_when(indic == "CP-HI00" ~ "AllGoods",
                           indic == "CP-HIE" ~ "Energy",
                           indic == "CP-HIF" ~ "Food",
                           indic == "CP-HIG" ~ "TotalGoods",
                           indic == "CP-HIIG" ~ "IndustrGoods",
                           TRUE ~ indic),
         unit = case_when(unit == "HICP2015" ~ "HICP2015",
                          unit == "RT1" ~ "Growth_t_t1",
                          unit == "RT12" ~ "Growth_t_t12",
                          TRUE ~ unit)) %>%
  mutate(Key = paste(indic,unit,sep = "_")) %>%
  select(Geo = geo, Time = time, Key, values) %>%
  group_by(Geo,Time,Key) %>%
  summarise(mean_val = mean(values,na.rm=T)) %>%
  pivot_wider(names_from = "Key", values_from = "mean_val")

write.xlsx(x = infl_red, file = "InflationMonth.xlsx")

