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
          "Euro Area 20" = "black")


##### Working directory
setwd("I:/Il mio Drive/Ricerca/Statistical analysis of the economic situation of Italy/Paper_Lucarelli_FSE")


########## Quarterly macroeconomic data
Data <- read_excel("MacroData_quarter.xlsx")
DataInfl <- read_excel("InflationMonth.xlsx")


########## Plots
##### GFCF
p_inv <- Data %>%
  group_by(Geo) %>%
  mutate(GFCF_CurrPrc_MEuro = forecast::ma(GFCF_CurrPrc_MEuro,order=5,centre=T)) %>%
  select(Geo,Time,GFCF_CurrPrc_MEuro) %>%
  filter(Time >= "2007-01-01",
         Geo != "Euro Area 20") %>%
  ggplot(mapping = aes(x = Time)) + 
  geom_line(mapping = aes(y = GFCF_CurrPrc_MEuro/1000, col=Geo), linewidth=1.2) + 
  scale_color_manual("", values = cols) + 
  labs(y = "Billion € (current prices)",x = "",
       title = "Gross Fixed Capital Formation",
       subtitle = "4-period moving average") + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p_inv),
         filename = "GFCF.png",width = 1000, height = 850,res = 100)

##### Subsidies
p_subs <- Data %>%
  group_by(Geo) %>%
  mutate(Subsidies_CurrPrc_MEuro = ma(Subsidies_CurrPrc_MEuro,order=5,centre=T)) %>%
  select(Geo,Time,Subsidies_CurrPrc_MEuro) %>%
  filter(Time >= "2007-01-01",
         Geo != "Euro Area 20") %>%
  ggplot(mapping = aes(x = Time)) + 
  geom_line(mapping = aes(y = Subsidies_CurrPrc_MEuro/1000, col=Geo), linewidth=1.2) + 
  scale_color_manual("", values = cols) + 
  labs(y = "Billion € (current prices)",x = "",
       title = "Public subsidies",
       subtitle = "4-period moving average") + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p_subs),
         filename = "Subsidies.png",width = 1000, height = 850,res = 100)

##### General gov. expenditure
p_govexp <- Data %>%
  group_by(Geo) %>%
  mutate(GenGov_TotalExpend = ma(GenGov_TotalExpend,order=5,centre=T)) %>%
  select(Geo,Time,GenGov_TotalExpend) %>%
  filter(Time >= "2007-01-01",
         Geo != "Euro Area 20") %>%
  ggplot(mapping = aes(x = Time)) + 
  geom_line(mapping = aes(y = GenGov_TotalExpend/1000, col=Geo), linewidth=1.2) + 
  scale_color_manual("", values = cols) + 
  labs(y = "Million € (current prices)",x = "",
       title = "General government total expenditure",
       subtitle = "4-period moving average") + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p_govexp),
         filename = "genGovExpend.png",width = 1000, height = 850,res = 100)

##### Employment
p_emp <- Data %>%
  select(Geo,Time,Employed_1564_Ths,Employees_1564_Ths,TempEmpl_1564_Ths,SelfEmployed_1564_Ths) %>%
  filter(Time >= "2007-01-01",
         Geo != "Euro Area 20") %>%
  pivot_longer(cols = 3:last_col(), names_to = "Emp", values_to = "Values") %>%
  group_by(Geo) %>%
  mutate(Values = ma(Values,order=5,centre=T)) %>%
  mutate(Emp = case_when(Emp == "Employees_1564_Ths" ~ "Total employees",
                         Emp == "Employed_1564_Ths" ~ "Total employed",
                         Emp == "TempEmpl_1564_Ths" ~ "Total temporary employees",
                         Emp == "SelfEmployed_1564_Ths" ~ "Total self-employed")) %>%
  ggplot(mapping = aes(x = Time)) + 
  geom_line(mapping = aes(y = Values/1000, col=Geo), linewidth=1.2) + 
  facet_wrap(~ Emp, scales = "free") + 
  scale_color_manual("", values = cols) + 
  labs(y = "Million people 15-64 years",x = "",
       title = "Labor market composition",
       subtitle = "4-period moving average") + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p_emp),
         filename = "LaborMkt.png",width = 1000, height = 850,res = 100)



########## Inflation plots
p_infl <- DataInfl %>%
  filter(Time >= "2015-01-01") %>%
  mutate(Energy = Energy_HICP2015/AllGoods_HICP2015,
         Food = Food_HICP2015/AllGoods_HICP2015,
         Industrial = IndustrGoods_HICP2015/AllGoods_HICP2015) %>%
  select(Time,Geo,Energy,Food,Industrial) %>%
  pivot_longer(cols = 3:last_col(), names_to = "Index", values_to = "Value") %>%
  ggplot(mapping = aes(x = Time)) + 
  geom_line(mapping = aes(y = Value*100, col=Index), linewidth=1.2) + 
  facet_wrap(~ Geo) + 
  geom_hline(yintercept = 100, col ="black", linewidth=1.2) + 
  scale_color_manual("", values = cols) + 
  labs(y = "%",x = "",
       title = "Good-specific HICP over General HICP",
       subtitle = "Values are computed as ratio between the good-specific HICP and the overall HICP. Value = 100% means equal HICP.") + 
  theme(plot.title = element_text(size = 14,face = "bold"))
ggexport(plotlist = list(p_infl),
         filename = "InflationRatio.png",width = 1000, height = 850,res = 100)


p_infl2 <- DataInfl %>%
  filter(Time >= "2015-01-01") %>%
  select(Time,Geo,
         Energy = Energy_Growth_t_t12,
         Food = Food_Growth_t_t12,
         Industrial = IndustrGoods_Growth_t_t12,
         General = AllGoods_Growth_t_t12) %>%
  pivot_longer(cols = 3:last_col(), names_to = "Index", values_to = "Value") %>%
  ggplot(mapping = aes(x = Time)) + 
  facet_wrap(~ Geo) + 
  geom_line(mapping = aes(y = Value, col=Index), linewidth=1.2) + 
  scale_color_manual("", values = cols) + 
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


