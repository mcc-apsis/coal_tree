# Coal consumption in 2°C scenarios (AR5 database)

#---- Load libraries ------
library(openxlsx)
library(ar5data)
library(RcppRoll)
library(ggplot2)

#---- Get data --------
# Scenario historical data
v_dataHist <- read.csv2("data/SSP_History1970_5Regs_2015-04-20_manipu.csv") %>% 
  gather(period, value, -MODEL, -SCENARIO, -REGION, -VARIABLE, -UNIT) %>% 
  mutate(period = as.numeric(substr(period, 2, 5))) %>% 
  mutate(value = as.numeric(value))
names(v_dataHist) <- c("model", "scenario", "region", "variable", "unit", "period", "value")

# Compiled dataset
load("data/coal_data.RData")

# Exxon mobile scenarios
conv_QuadBTUtoEJ = 1.055
conv_MtoetoEJ    = 0.041868

v_dataExxon <- read.xlsx("data/ExxonMobil_2017_Energy_Outlook_Report_Data_Pages.xlsx")[23, 2:6]
names(v_dataExxon) <- c("2000", "2010", "2015", "2025", "2040")
v_dataExxon <- v_dataExxon %>% 
  gather(period, value) %>% 
  mutate(period = as.numeric(paste(period))) %>% 
  mutate(value = value*conv_QuadBTUtoEJ)

# Shell scenarios
v_dataShell <- data.frame(
  scenario = rep("Shell Energy scenarios to 2050 (2008)", 6),
  period   = c(2000, 2010, 2020, 2030, 2040, 2050),
  value    = c(97, 144, 199, 210, 246, 263)
) %>% 
  rbind(data.frame(
    scenario = rep("New Lens - Mountains (2013)", 11),
    period = c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2030, 2040, 2050, 2060),
    value = c(52.2, 61.6, 75.9, 94.2, 100.1, 146.2, 184.8, 199.0, 191.4, 211.8, 247.0)
  )) %>% 
  rbind(data.frame(
    scenario = rep("New Lens - Oceans (2013)", 11),
    period = c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2030, 2040, 2050, 2060),
    value = c(52.2, 61.6, 75.9, 94.2, 100.1, 146.2, 202.7, 222.3, 201.7, 218.6, 204.2)
  ))

# IEA scenario (WEOs)
v_dataIEA <- list()
v_dataIEA[["WEO"]] <- rbind(
  data.frame(
    scenario = rep("WEO 2006 - Alternative Policy Scenario", 3),
    period   = c(2004, 2015, 2030),
    value    = c(2773, 3431, 3512)*conv_MtoetoEJ
  ),
  data.frame(
    scenario = rep("WEO 2006 - Reference Scenario", 3),
    period   = c(2004, 2015, 2030),
    value    = c(2773, 3666, 4441)*conv_MtoetoEJ
  ), 
  data.frame(
    scenario = rep("WEO 2007 - Alternative Policy Scenario", 3),
    period   = c(2005, 2015, 2030),
    value    = c(2892, 3643, 3700)*conv_MtoetoEJ
  ),
  data.frame(
    scenario = rep("WEO 2007 - Reference Scenario", 3),
    period   = c(2005, 2015, 2030),
    value    = c(2892, 3988, 4994)*conv_MtoetoEJ
  ), 
  data.frame(
    scenario = rep("WEO 2008 - Reference Scenario", 5),
    period   = c(2006, 2015, 2020, 2025, 2030),
    value    = c(3053, 4023, 4374, 4719, 4908)*conv_MtoetoEJ
  ), 
  data.frame(
    scenario = rep("WEO 2009 - Reference Scenario", 5),
    period   = c(2007, 2015, 2020, 2025, 2030),
    value    = c(3184, 3828, 4125, 4522, 4887)*conv_MtoetoEJ
  ), 
  data.frame(
    scenario = rep("WEO 2010 - Current Policies Scenario", 5),
    period   = c(2008, 2015, 2020, 2030, 2035),
    value    = c(3315, 3892, 4307, 4932, 5281)*conv_MtoetoEJ
  ), 
  data.frame(
    scenario = rep("WEO 2010 - 450 Scenario", 5),
    period   = c(2008, 2015, 2020, 2030, 2035),
    value    = c(3315, 3892, 3743, 2714, 2496)*conv_MtoetoEJ
  )
)
v_dataIEA[["WEB"]] <- read.xlsx("X:\\stej\\Renaissance of Coal\\Analysis_2015\\IEA_WEB_2016_TPES.xlsx")[1,14:57] %>% 
  gather(period, value) %>% 
  mutate(period = as.numeric(period)) %>% 
  mutate(value = as.numeric(value)*1e-6)

# EIA scenario (IEO 2016)
v_dataEIA <- data.frame(
  period = c(2011, 2012, 2020, 2025, 2030, 2035, 2040),
  value = c(152.0, 153.3, 168.6, 173.2, 174.4, 176.9, 180.2)) %>% 
  mutate(value = value*conv_QuadBTUtoEJ)

#---- Process data --------
v_data <- ar5data %>% 
  filter(
    region   ==   "World",
    variable %in% c("Primary Energy|Coal", "Primary Energy|Coal|w/o CCS")) %>% 
  mutate(period = as.numeric(format(period, "%Y"))) %>% 
  ar5rm_2050models() %>% 
  ar5clean() %>% 
  ar5join() %>% 
  unite(model.scenario, model, scenario, sep=".", remove=FALSE)
  


#--- Plot data ------
p = ggplot()

#---- Baselines -----
p = p + 
  geom_ribbon(aes(x=period, ymin=value_min_rollmean, ymax=value_max_rollmean, fill="Baselines"),
              data=v_data %>% 
                filter(policy == "P0", variable == "Primary Energy|Coal") %>% 
                select(model, scenario, period, value) %>% 
                group_by(period) %>% 
                summarize(
                  value_min = min(value, na.rm=TRUE),
                  value_max = max(value, na.rm=TRUE)) %>% 
                ungroup() %>% 
                mutate(value_min_rollmean=roll_mean(value_min, n=3, fill=125)) %>% 
                mutate(value_max_rollmean=roll_mean(value_max, n=3, fill=125)) %>% 
                filter(period >= 2005, period < 2100),
              #fill = "#00000033", 
              colour="#00000000") +
  geom_line(aes(x=period, y=value, group=model.scenario),
            data=v_data %>% filter(policy == "P0", variable == "Primary Energy|Coal") %>% 
              filter(period >= 2005, period < 2100),
            color="#00000009")


# #---- Immediate CP/ Category 1 -----
# p = p + 
#   geom_ribbon(aes(x=period, ymin=value_min, ymax=value_max),
#               data=v_data %>% 
#                 filter(policy == "P1", technology == "T0", climate == "Category 1", variable == "Primary Energy|Coal") %>% 
#                 select(model, scenario, period, value) %>% 
#                 group_by(period) %>% 
#                 summarize(
#                   value_min = min(value, na.rm=TRUE),
#                   value_max = max(value, na.rm=TRUE)) %>% 
#                 ungroup(),
#               fill = "#FF000033", colour="#FF000000") #+
#   # geom_line(aes(x=period, y=value, group=model.scenario), 
#   #           data=v_data %>% filter(policy == "P1", technology == "T0", climate == "Category 1", variable == "Primary Energy|Coal"),
#   #           colour = "red") 
# 
# #---- Delayed CP/ Category 1 -----
# p = p + 
#   geom_ribbon(aes(x=period, ymin=value_min, ymax=value_max),
#               data=v_data %>% 
#                 filter(!policy %in% c("P1","P0"), technology == "T0", climate == "Category 1", variable == "Primary Energy|Coal") %>% 
#                 select(model, scenario, period, value) %>% 
#                 group_by(period) %>% 
#                 summarize(
#                   value_min = min(value, na.rm=TRUE),
#                   value_max = max(value, na.rm=TRUE)) %>% 
#                 ungroup(),
#               fill = "#00FF0033", colour="#00FF0000") #+
#   # geom_line(aes(x=period, y=value, group=model.scenario), 
#   #           data=v_data %>% filter(!policy %in% c("P1","P0"), technology == "T0", climate == "Category 1", variable == "Primary Energy|Coal"),
#   #           colour = "green")

#---- Category 1 / No CCS -----
p = p + 
  geom_ribbon(aes(x=period, ymin=value_min_rollmean, ymax=value_max_rollmean, fill="2°C scenarios"),
              data=v_data %>% 
                filter(climate == "Category 1", variable == "Primary Energy|Coal|w/o CCS") %>%   # technology == "T2", 
                select(model, scenario, period, value) %>% 
                group_by(period) %>% 
                summarize(
                  value_min = min(value, na.rm=TRUE),
                  value_max = max(value, na.rm=TRUE)) %>% 
                ungroup() %>% 
                mutate(value_min_rollmean=roll_mean(value_min, n=3, fill=125)) %>% 
                mutate(value_max_rollmean=roll_mean(value_max, n=3, fill=125)) %>% 
                filter(period >= 2005, period < 2100),
              #fill = "#0000FF66", 
              colour="#0000FF00") +
  geom_line(aes(x=period, y=value, group=model.scenario),
            data=v_data %>% filter(climate == "Category 1", variable == "Primary Energy|Coal|w/o CCS") %>%    # technology == "T2", 
              filter(period >= 2005, period < 2100),
            colour = "#0000FF22")

#---- Other scenarios -----
# p = p +
#   geom_line(aes(x=period, y=value, color="ExxonMobil"), data=v_dataExxon, size=1.25) +
#   geom_line(aes(x=period, y=value, color="Shell", group=scenario), data=v_dataShell, size=1.25) +
#   geom_line(aes(x=period, y=value, color="EIA"), data=v_dataEIA, size=1.25) +
#   geom_line(aes(x=period, y=value, color="IEA", group=scenario), data=v_dataIEA[["WEO"]], size=1.25)


p = p +
  geom_line(aes(x=period, y=value, color=scenario), data=v_dataIEA[["WEO"]] %>% filter(grepl("Reference|Current", scenario)), size=1.25) +
  geom_line(aes(x=period, y=value, group=scenario), data=v_dataIEA[["WEO"]] %>% filter(grepl("Alternative", scenario)), size=1.25, color="red") +
  geom_line(aes(x=period, y=value, group=scenario), data=v_dataIEA[["WEO"]] %>% filter(grepl("450", scenario)), size=1.25, color="green")

#---- Historical ------
# p = p +
#   geom_line(aes(x=period, y=value),
#             data=v_dataHist %>% filter(region == "World", scenario == "IEA", variable == "Primary Energy|Coal"),
#             colour = "#000000",
#             size=3)

p = p +
  geom_line(aes(x=year, y=value, colour="Historical"),
            data=p_data %>% 
              filter(variable == "E_CC", source == "BP 2016") %>% 
              group_by(year) %>% 
              summarize(value = sum(value, na.rm=TRUE)/1e9) %>% 
              ungroup(),
            # colour = "#000000",
            size=3)  +
  geom_point(aes(x=year, y=value, colour="Historical"),
            data=p_data %>% 
              filter(variable == "E_CC", source == "BP 2016") %>% 
              group_by(year) %>% 
              summarize(value = sum(value, na.rm=TRUE)/1e9) %>% 
              ungroup(),
            # colour = "#000000",
            fill = "#FFFFFF",
            size=3, pch=21)

# p = p +
#   geom_line(aes(x=year, y=value),
#             data=p_data %>% 
#               filter(variable == "E_CC", source == "IEA2016 - WEB") %>% 
#               group_by(year) %>% 
#               summarize(value = sum(value, na.rm=TRUE)/10000) %>% 
#               ungroup(),
#             colour = "#FF0000",
#             size=1.5)

#---- Legend -----
colfunc <- colorRampPalette(c("#ff0000", "#ffff00"))
#cols <- c("#000000", colfunc(length(levels(v_dataIEA$WEO$scenario)) + 3))
cols <- c("#000000", colfunc(4))
names(cols) <- c("Historical", "ExxonMobil", "Shell", "EIA", "IEA")
p = p +
  scale_fill_manual(name="AR5 scenarios",
                      values=c("Historical"="white", "Baselines"="#00000033", "2°C scenarios"="#0000FF66")) #+
  # scale_colour_manual(name="Other data",
  #                     values=cols)

#---- Cosmetics -----
p = p +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlim(2000, 2050) + ylim(0, 1000) +
  coord_cartesian(expand=FALSE) +
  xlab("") + ylab("Coal consumption [EJ/yr]")
p = p +
  guides(fill=guide_legend(nrow=2,byrow=TRUE), colour=guide_legend(nrow=2,byrow=TRUE))
  
print(p)

p = p +
  theme(
    plot.margin = unit(c(0.5,1.0,0.5,0.5), "cm"),
    text         = element_text(size=13),
    axis.text    = element_text(size=14),
    legend.title = element_text(size=10),
    legend.text  = element_text(size=10)
  )
ggsave(p, filename = "article/figures/coal_scenarios.pdf", width = 16, height = 16, units = "cm")
