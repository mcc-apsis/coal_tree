# Coal consumption plot

# Initialiase and get data
source("scripts/user_settings.R")
source("scripts/init.R")
source("scripts/process_data.R")

# Load library
library(tidyverse)
library(readxl)
library(ggplot2)

# Get WB country classification by income in 2017

country_class    <- read_xls("data/WorldBank/CLASS.xls", skip = 4)[-1,]

country_classOld <- read_xls("data/WorldBank/OGHIST.xls", skip = 4)[-1,]

v_data <- p_data %>% 
  filter(variable == "E_CC", source == "BP 2016") %>% 
  select(-country, -longname, -unit, -source, -variable) %>% 
  left_join(country_class %>% select(Code, `Income group`), by=c("iso"="Code")) %>% 
  mutate(value = value*1e-9)

# Plot data
p <- ggplot(data=p_data %>% 
              filter(variable == "E_CC", source == "BP 2016",
                     iso %in% c("CHN","USA", "DEU", "IND", "JPN", "POL", "GBR", "ZAF", "FRA", "KOR", "TUR", "CAN", "AUS", "BEL", "ITA", "BRA", "ESP", "IDN",
                                "VNM", "THA")) %>% 
              select(-country, -longname, -unit, -source, -variable) %>% 
              #mutate(iso = factor(iso, labels=rev(c("DEU", "USA", "AUS", "JPN", "IDN", "IND", "CHN")), ordered=TRUE)) %>% 
              arrange(iso, year)) +
  geom_area(aes(x=year, y=value, fill=iso), stat="identity", position="stack")
print(p)


p <- ggplot(data=v_data %>% 
              group_by(`Income group`,year) %>% 
              summarize(value=sum(value)) %>% 
              ungroup() %>% 
              mutate(`Income group` = factor(`Income group`, levels=c("Lower middle income", "Upper middle income", "High income"), ordered=TRUE))) +
  geom_area(aes(x=year, y=value, fill=`Income group`)) +
  facet_wrap(~`Income group`, ncol=3)  +
  xlab("") + ylab("Coal consumption [EJ/yr]") +
  theme_bw() +
  theme(text      = element_text(size=18),
        axis.text = element_text(size=16),
        legend.position="none")
print(p)

iso_class <- v_data %>% 
  group_by(`Income group`,iso) %>% 
  arrange(year) %>% 
  summarise(value=sum(value, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(value))

p <- ggplot(data=v_data %>% 
              group_by(`Income group`,year) %>% 
              summarize(value=sum(value, na.rm=TRUE)) %>% 
              ungroup() %>% 
              mutate(`Income group` = factor(`Income group`, levels=c("Lower middle income", "Upper middle income", "High income"), ordered=TRUE))) +
  geom_area(aes(x=year, y=value, fill=iso), stat="identity", position="stack", 
            data=rbind(
              v_data %>% 
                filter(iso %in% iso_class$iso[which(iso_class$`Income group` == "Lower middle income")[1:5]]),
              v_data %>% 
                filter(iso %in% iso_class$iso[which(iso_class$`Income group` == "Lower middle income")[6:length(iso_class$`Income group` == "Lower middle income")]]) %>% 
                group_by(`Income group`, year) %>% 
                summarise(value=sum(value, na.rm=TRUE)) %>% 
                ungroup() %>% 
                mutate(iso="Other LMI") %>% 
                select(iso,year,value,`Income group`)) %>% 
              mutate(`Income group` = factor(`Income group`, levels=c("Lower middle income", "Upper middle income", "High income"), ordered=TRUE))) +
  geom_area(aes(x=year, y=value, fill=iso), stat="identity", position="stack", 
            data=rbind(
              v_data %>% 
                filter(iso %in% iso_class$iso[which(iso_class$`Income group` == "Upper middle income")[1:5]]),
              v_data %>% 
                filter(iso %in% iso_class$iso[which(iso_class$`Income group` == "Upper middle income")[6:length(iso_class$`Income group` == "Upper middle income")]]) %>% 
                group_by(`Income group`, year) %>% 
                summarise(value=sum(value, na.rm=TRUE)) %>% 
                ungroup() %>% 
                mutate(iso="Other UMI") %>% 
                select(iso,year,value,`Income group`)) %>% 
              mutate(`Income group` = factor(`Income group`, levels=c("Lower middle income", "Upper middle income", "High income"), ordered=TRUE))) +
  geom_area(aes(x=year, y=value, fill=iso), stat="identity", position="stack", 
            data=rbind(
              v_data %>% 
                filter(iso %in% iso_class$iso[which(iso_class$`Income group` == "High income")[1:5]]),
              v_data %>% 
                filter(iso %in% iso_class$iso[which(iso_class$`Income group` == "High income")[6:length(iso_class$`Income group` == "High income")]]) %>% 
                group_by(`Income group`, year) %>% 
                summarise(value=sum(value, na.rm=TRUE)) %>% 
                ungroup() %>% 
                mutate(iso="Other HI") %>% 
                select(iso,year,value,`Income group`)) %>% 
              mutate(`Income group` = factor(`Income group`, levels=c("Lower middle income", "Upper middle income", "High income"), ordered=TRUE))) +
  geom_line(aes(x=year, y=value)) +
  facet_wrap(~`Income group`, ncol=3) +
  xlim(1990, 2015)  +
  xlab("") + ylab("Coal consumption [EJ/yr]") +
  theme_bw()
print(p)
  


ig = c("Lower middle income", "Upper middle income", "High income")
oig = c("Other LMI", "Other UMI", "Other HI")
for (kig in 1:3) {
  p <- ggplot(data=v_data %>% 
                group_by(`Income group`,year) %>% 
                summarize(value=sum(value, na.rm=TRUE)) %>% 
                ungroup() %>% 
                filter(`Income group` == ig[kig])) +
    geom_area(aes(x=year, y=value, fill=iso), stat="identity", position="stack", 
              data=rbind(
                v_data %>% 
                  filter(iso %in% iso_class$iso[which(iso_class$`Income group` == ig[kig])[1:5]]),
                v_data %>% 
                  filter(iso %in% iso_class$iso[which(iso_class$`Income group` == ig[kig])[6:length(iso_class$`Income group` == ig[kig])]]) %>% 
                  group_by(`Income group`, year) %>% 
                  summarise(value=sum(value, na.rm=TRUE)) %>% 
                  ungroup() %>% 
                  mutate(iso=oig[kig]) %>% 
                  select(iso,year,value,`Income group`)) %>% 
                mutate(iso = factor(iso, levels=c(iso_class$iso[which(iso_class$`Income group` == ig[kig])[1:5]], oig[kig]), ordered=TRUE)) %>% 
                mutate(`Income group` = factor(`Income group`, levels=c("Lower middle income", "Upper middle income", "High income"), ordered=TRUE))) +
    #geom_line(aes(x=year, y=value)) +
    facet_grid(iso~`Income group`, scales="free_y") +
    xlab("") + ylab("Coal consumption [EJ/yr]") +
    theme_bw() +
    theme(text      = element_text(size=18),
          axis.text = element_text(size=16),
          legend.position="none")
  print(p)
  
}

coal_ef = 14871.40556/(3911.18118022575*0.041868)
