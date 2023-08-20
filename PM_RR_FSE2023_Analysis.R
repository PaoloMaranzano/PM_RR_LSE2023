########## Libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(ggpubr)
library(forecast)

########## Setup
cols <- c("Energy" = "orange",
          "Food" = "green",
          "Industrial" = "blue",
          "General" = "red",
          "Italy" = "green",
          "France" = "blue",
          "Germany" = "orange",
          "Spain" = "red",
          "Euro Area 20" = "black",
          #####
          "Financial crisis 2007-2009" = "red",
          "COVID-19 pandemic 2020-2022" = "blue",
          "Ukraine invasion (since Feb. 2022)" = "green")


##### Working directory
setwd("I:/Il mio Drive/Ricerca/Statistical analysis of the economic situation of Italy/Paper_Lucarelli_FSE")


########## Quarterly macroeconomic data
Data <- read_excel("MacroData_quarter.xlsx")
DataInfl <- read_excel("InflationMonth.xlsx")

library(KFAS)
library(tsbox)
library(tsibble)
y <- Data %>%
  arrange(Geo,Time) %>%
  mutate(Time = ymd(Time),
         Var = log(GFCF_CurrPrc_MEuro)) %>%
  dplyr::select(Geo,Time,Var) %>%
  pivot_wider(names_from = Geo, values_from = Var) %>%
  pivot_longer(cols = 2:last_col(), names_to = "Geo", values_to = "Value") %>%
  as_tsibble(key = Geo, index = Time) %>%
  ts_ts()


########## Plots
##### GFCF
p_inv <- Data %>%
  group_by(Geo) %>%
  mutate(GFCF_CurrPrc_MEuro = unlist(hpfilter::hp1(data.frame(GFCF_CurrPrc_MEuro),lambda=1600))) %>%
  dplyr::select(Geo,Time,GFCF_CurrPrc_MEuro) %>%
  filter(Time >= "2007-01-01",
         Geo != "Euro Area 20") %>%
  ggplot(mapping = aes(x = Time)) + 
  geom_line(mapping = aes(y = GFCF_CurrPrc_MEuro/1000, col=Geo), linewidth=1.2) + 
  geom_rect(data = data.frame(Period  = factor(c("Financial crisis 2007-2009",
                                                 "COVID-19 pandemic 2020-2022"),
                                               levels = c("Financial crisis 2007-2009",
                                                          "COVID-19 pandemic 2020-2022")),
                              start = c(as.POSIXct("2007-01-01"),
                                        as.POSIXct("2020-01-01")),
                              end   = c(as.POSIXct("2010-01-01"),
                                        as.POSIXct("2023-01-01"))),
            inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf, fill = Period),
            alpha = 0.1) + 
  scale_color_manual("", values = cols) + 
  scale_fill_manual("Period", values = cols) + 
  labs(y = "Billion € (current prices)",x = "",
       title = "Gross Fixed Capital Formation",
       subtitle = latex2exp::TeX(paste0("One-Side Hodrick-Prescott Filter ($\\lambda$ = 1600)"))) + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p_inv),
         filename = "GFCF.png",width = 1000, height = 850,res = 100)

##### Subsidies
p_subs <- Data %>%
  group_by(Geo) %>%
  mutate(Subsidies_CurrPrc_MEuro = unlist(hpfilter::hp1(data.frame(Subsidies_CurrPrc_MEuro),lambda=1600))) %>%
  dplyr::select(Geo,Time,Subsidies_CurrPrc_MEuro) %>%
  filter(Time >= "2007-01-01",
         Geo != "Euro Area 20") %>%
  ggplot(mapping = aes(x = Time)) + 
  geom_line(mapping = aes(y = Subsidies_CurrPrc_MEuro/1000, col=Geo), linewidth=1.2) + 
  geom_rect(data = data.frame(Period  = factor(c("Financial crisis 2007-2009",
                                                 "COVID-19 pandemic 2020-2022"),
                                               levels = c("Financial crisis 2007-2009",
                                                          "COVID-19 pandemic 2020-2022")),
                              start = c(as.POSIXct("2007-01-01"),
                                        as.POSIXct("2020-01-01")),
                              end   = c(as.POSIXct("2010-01-01"),
                                        as.POSIXct("2023-01-01"))),
            inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf, fill = Period),
            alpha = 0.1) + 
  scale_color_manual("", values = cols) + 
  scale_fill_manual("Period", values = cols) + 
  labs(y = "Billion € (current prices)",x = "",
       title = "Public subsidies",
       subtitle = latex2exp::TeX(paste0("One-Side Hodrick-Prescott Filter ($\\lambda$ = 1600)"))) + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p_subs),
         filename = "Subsidies.png",width = 1000, height = 850,res = 100)

##### General gov. expenditure
p_govexp <- Data %>%
  filter(Time >= "2007-01-01",
         Geo != "Euro Area 20",
         !is.na(GenGov_TotalExpend)) %>%
  group_by(Geo) %>%
  mutate(GenGov_TotalExpend = unlist(hpfilter::hp1(data.frame(GenGov_TotalExpend),lambda=1600))) %>%
  dplyr::select(Geo,Time,GenGov_TotalExpend) %>%
  ggplot(mapping = aes(x = Time)) + 
  geom_line(mapping = aes(y = GenGov_TotalExpend/1000, col=Geo), linewidth=1.2) + 
  # facet_wrap(~ Geo, scales = "free") + 
  geom_rect(data = data.frame(Period  = factor(c("Financial crisis 2007-2009",
                                                 "COVID-19 pandemic 2020-2022"),
                                               levels = c("Financial crisis 2007-2009",
                                                          "COVID-19 pandemic 2020-2022")),
                              start = c(as.POSIXct("2007-01-01"),
                                        as.POSIXct("2020-01-01")),
                              end   = c(as.POSIXct("2010-01-01"),
                                        as.POSIXct("2023-01-01"))),
            inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf, fill = Period),
            alpha = 0.1) + 
  scale_color_manual("", values = cols) + 
  scale_fill_manual("Period", values = cols) + 
  labs(y = "Billion € (current prices)",x = "",
       title = "General government expenditure",
       subtitle = latex2exp::TeX(paste0("One-Side Hodrick-Prescott Filter ($\\lambda$ = 1600)"))) + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p_govexp),
         filename = "genGovExpend.png",width = 1000, height = 850,res = 100)

##### Employment
p_emp <- Data %>%
  dplyr::select(Geo,Time,Employed_1564_Ths,Employees_1564_Ths,TempEmpl_1564_Ths,SelfEmployed_1564_Ths) %>%
  filter(Time >= "2007-01-01",
         Geo != "Euro Area 20") %>%
  pivot_longer(cols = 3:last_col(), names_to = "Emp", values_to = "Values") %>%
  filter(!is.na(Values)) %>%
  mutate(Emp = case_when(Emp == "Employees_1564_Ths" ~ "Total employees",
                         Emp == "Employed_1564_Ths" ~ "Total employed",
                         Emp == "TempEmpl_1564_Ths" ~ "Total temporary employees",
                         Emp == "SelfEmployed_1564_Ths" ~ "Total self-employed")) %>%
  group_by(Geo,Emp) %>%
  mutate(Values = unlist(hpfilter::hp1(data.frame(Values),lambda=1600))) %>%
  ggplot(mapping = aes(x = Time)) + 
  geom_line(mapping = aes(y = Values/1000, col=Geo), linewidth=1.2) + 
  facet_wrap(~ Emp, scales = "free") + 
  geom_rect(data = data.frame(Period  = factor(c("Financial crisis 2007-2009",
                                                 "COVID-19 pandemic 2020-2022"),
                                               levels = c("Financial crisis 2007-2009",
                                                          "COVID-19 pandemic 2020-2022")),
                              start = c(as.POSIXct("2007-01-01"),
                                        as.POSIXct("2020-01-01")),
                              end   = c(as.POSIXct("2010-01-01"),
                                        as.POSIXct("2023-01-01"))),
            inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf, fill = Period),
            alpha = 0.1) + 
  scale_color_manual("", values = cols) + 
  scale_fill_manual("Period", values = cols) + 
  labs(y = "Million people 15-64 years",x = "",
       title = "Labor market composition",
       subtitle = latex2exp::TeX(paste0("One-Side Hodrick-Prescott Filter ($\\lambda$ = 1600)"))) + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p_emp),
         filename = "LaborMkt.png",width = 1000, height = 850,res = 100)


########## Inflation plots
p_infl <- DataInfl %>%
  filter(Time >= "2015-01-01") %>%
  mutate(Energy = Energy_HICP2015/AllGoods_HICP2015,
         Food = Food_HICP2015/AllGoods_HICP2015,
         Industrial = IndustrGoods_HICP2015/AllGoods_HICP2015) %>%
  dplyr::select(Time,Geo,Energy,Food,Industrial) %>%
  pivot_longer(cols = 3:last_col(), names_to = "Index", values_to = "Value") %>%
  ggplot(mapping = aes(x = Time)) + 
  geom_line(mapping = aes(y = Value*100, col=Index), linewidth=1.2) + 
  facet_wrap(~ Geo) + 
  geom_rect(data = data.frame(Period  = factor(c("Ukraine invasion (since Feb. 2022)",
                                                 "COVID-19 pandemic 2020-2022"),
                                               levels = c("Ukraine invasion (since Feb. 2022)",
                                                          "COVID-19 pandemic 2020-2022")),
                              start = c(as.POSIXct("2022-02-01"),
                                        as.POSIXct("2020-01-01")),
                              end   = c(as.POSIXct("2023-06-01"),
                                        as.POSIXct("2022-02-01"))),
            inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf, fill = Period),
            alpha = 0.1) + 
  geom_hline(yintercept = 100, col ="black", linewidth=1.2) + 
  scale_color_manual("", values = cols) + 
  scale_fill_manual("Period", values = cols) + 
  labs(y = "%",x = "",
       title = "Good-specific HICP over General HICP",
       subtitle = "Values are computed as ratio between the good-specific HICP and the overall HICP. Value = 100% means equal HICP.") + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p_infl),
         filename = "InflationRatio.png",width = 1000, height = 850,res = 100)


p_infl2 <- DataInfl %>%
  filter(Time >= "2015-01-01") %>%
  dplyr::select(Time,Geo,
         Energy = Energy_Growth_t_t12,
         Food = Food_Growth_t_t12,
         Industrial = IndustrGoods_Growth_t_t12,
         General = AllGoods_Growth_t_t12) %>%
  pivot_longer(cols = 3:last_col(), names_to = "Index", values_to = "Value") %>%
  ggplot(mapping = aes(x = Time)) + 
  facet_wrap(~ Geo) + 
  geom_rect(data = data.frame(Period  = factor(c("Ukraine invasion (since Feb. 2022)",
                                                 "COVID-19 pandemic 2020-2022"),
                                               levels = c("Ukraine invasion (since Feb. 2022)",
                                                          "COVID-19 pandemic 2020-2022")),
                              start = c(as.POSIXct("2022-02-01"),
                                        as.POSIXct("2020-01-01")),
                              end   = c(as.POSIXct("2023-06-01"),
                                        as.POSIXct("2022-02-01"))),
            inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf, fill = Period),
            alpha = 0.1) + 
  geom_line(mapping = aes(y = Value, col=Index), linewidth=1.2) + 
  scale_color_manual("", values = cols) + 
  scale_fill_manual("Period", values = cols) + 
  labs(y = "%",x = "",
       title = "Year-to-Year HICP by type of good",
       subtitle = "Values are HICP at time t minus the HICP at time t-12.") + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p_infl2),
         filename = "Inflation_YtoY.png",width = 1000, height = 850,res = 100)


p_infl3 <- DataInfl %>%
  filter(Time >= "2015-01-01") %>%
  select(Time,Geo,
         Energy = Energy_Growth_t_t1,
         Food = Food_Growth_t_t1,
         Industrial = IndustrGoods_Growth_t_t1,
         General = AllGoods_Growth_t_t12) %>%
  pivot_longer(cols = 3:last_col(), names_to = "Index", values_to = "Value") %>%
  ggplot(mapping = aes(x = Time)) + 
  facet_wrap(~ Geo) + 
  geom_line(mapping = aes(y = Value, col=Index), linewidth=1.2) + 
  scale_color_manual("", values = cols) + 
  labs(y = "%",x = "",
       title = "Month-to-Month HICP by type of good",
       subtitle = "Values are HICP at time t minus the HICP at time t-1.") + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p_infl3),
         filename = "Inflation_MtoM.png",width = 1000, height = 850,res = 100)




Data %>%
  # filter(Time >= "2008-01-01" & Time <= "2011-01-01") %>%
  arrange(Time) %>%
  select(Geo,Time,GDP_CurrPrc_MEuro) %>%
  pivot_wider(names_from = Geo, values_from = GDP_CurrPrc_MEuro) %>%
  mutate(across(!contains("Time"), ~ function(x) diff(.x,lag = 4)))


p1_lossesGDP <- Data %>%
  arrange(Time) %>%
  dplyr::select(Geo,Time,Value = GDP_CurrPrc_MEuro) %>%
  group_by(Geo) %>%
  mutate(Value = Value/1000,
         ValueL4 = lag(Value,n = 4),
         ValueD4 = Value - ValueL4) %>%
  filter(Time >= "2006-01-01") %>%
  # filter(Time >= "2006-01-01" & Time <= "2012-01-01") %>%
  # filter(Time >= "2018-01-01" & Time <= "2024-01-01") %>%
  ggplot(mapping = aes(x = Time)) + 
  geom_line(mapping = aes(y = ValueD4, col=Geo), linewidth=1.2) + 
  geom_hline(mapping = aes(yintercept = 0), col = "yellow", linewidth=1.2) + 
  geom_rect(data = data.frame(Period  = factor(c("Ukraine invasion (since Feb. 2022)",
                                                 "COVID-19 pandemic 2020-2022",
                                                 "Financial crisis 2007-2009"),
                                               levels = c("Ukraine invasion (since Feb. 2022)",
                                                          "COVID-19 pandemic 2020-2022",
                                                          "Financial crisis 2007-2009")),
                              start = c(as.POSIXct("2022-02-01"),
                                        as.POSIXct("2020-01-01"),
                                        as.POSIXct("2007-01-01")),
                              end   = c(as.POSIXct("2023-06-01"),
                                        as.POSIXct("2022-02-01"),
                                        as.POSIXct("2010-01-01"))),
            inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf, fill = Period),
            alpha = 0.1) + 
  scale_color_manual("", values = cols) + 
  scale_fill_manual("Period", values = cols) + 
  labs(y = "Billions €",x = "",
       title = "Year-to-Year variation of GDP by country",
       subtitle = "Values are GDP at time t minus the GDP at time t-4.") + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p1_lossesGDP),
         filename = "LossesGDP.png",width = 1000, height = 850,res = 100)


p2_lossesGFCF <- Data %>%
  arrange(Time) %>%
  # group_by(Geo) %>%
  # mutate(GFCF_CurrPrc_MEuro = unlist(hpfilter::hp1(data.frame(GFCF_CurrPrc_MEuro),lambda=1600))) %>%
  # ungroup() %>%
  dplyr::select(Geo,Time,Value = GFCF_CurrPrc_MEuro) %>%
  group_by(Geo) %>%
  mutate(Value = Value/1000,
         ValueL4 = lag(Value,n = 4),
         ValueD4 = Value - ValueL4) %>%
  filter(Time >= "2006-01-01") %>%
  # filter(Time >= "2006-01-01" & Time <= "2012-01-01") %>%
  # filter(Time >= "2018-01-01" & Time <= "2024-01-01") %>%
  ggplot(mapping = aes(x = Time)) + 
  geom_line(mapping = aes(y = ValueD4, col=Geo), linewidth=1.2) + 
  geom_hline(mapping = aes(yintercept = 0), col = "yellow", linewidth=1.2) + 
  geom_rect(data = data.frame(Period  = factor(c("Ukraine invasion (since Feb. 2022)",
                                                 "COVID-19 pandemic 2020-2022",
                                                 "Financial crisis 2007-2009"),
                                               levels = c("Ukraine invasion (since Feb. 2022)",
                                                          "COVID-19 pandemic 2020-2022",
                                                          "Financial crisis 2007-2009")),
                              start = c(as.POSIXct("2022-02-01"),
                                        as.POSIXct("2020-01-01"),
                                        as.POSIXct("2007-01-01")),
                              end   = c(as.POSIXct("2023-06-01"),
                                        as.POSIXct("2022-02-01"),
                                        as.POSIXct("2010-01-01"))),
            inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = +Inf, fill = Period),
            alpha = 0.1) + 
  scale_color_manual("", values = cols) + 
  scale_fill_manual("Period", values = cols) + 
  labs(y = "Billions €",x = "",
       title = "Year-to-Year variation of GFCF by country",
       subtitle = "Values are GFCF at time t minus the GFCF at time t-4.") + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p2_lossesGFCF),
         filename = "LossesGFCF.png",width = 1000, height = 850,res = 100)


# Data %>%
#   arrange(Time) %>%
#   select(Geo,Time,Value = GDP_CurrPrc_MEuro) %>%
#   group_by(Geo) %>%
#   mutate(Value = Value/1000,
#          ValueL4 = lag(Value,n = 4),
#          ValueD4 = Value - ValueL4) %>%
#   filter(Time >= "2020-01-01" & Time <= "2021-02-01") %>%
#   summarise(Loss = sum(ValueD4)) %>%
#   View()
# 
# Data %>%
#   arrange(Time) %>%
#   select(Geo,Time,Value = GFCF_CurrPrc_MEuro) %>%
#   group_by(Geo) %>%
#   mutate(Value = Value/1000,
#          ValueL4 = lag(Value,n = 4),
#          ValueD4 = Value - ValueL4) %>%
#   filter(Time >= "2008-10-01" & Time <= "2009-11-01") %>%
#   summarise(Loss = sum(ValueD4)) %>%
#   View()
#   
#     
#     
#   ggplot(mapping = aes(x = Time)) + 
#   geom_line(mapping = aes(y = ValueD4, col=Geo), linewidth=1.2) + 
#   geom_hline(mapping = aes(yintercept = 0), col = "yellow", linewidth=1.2) + 
#   scale_color_manual("", values = cols) + 
#   labs(y = "Billions €",x = "",
#        title = "Year-to-Year variation of GDP by country",
#        subtitle = "Values are GDP at time t minus the GDP at time t-4.") + 
#   theme(plot.title = element_text(size = 14,face = "bold"))
# ggexport(plotlist = list(p2_losses),
#          filename = "LossesGDP.png",width = 1000, height = 850,res = 100)
#   
# 
