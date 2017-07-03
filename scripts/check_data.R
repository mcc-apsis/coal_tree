# Understanding the log, demeaning and first-differences transformations
# Config1: 1971-2012 & 80 countries
# E_CC: 
# - No data   : "AFG", "BDI", "BFA", "CAF", "GIN", "GMB", "GNB", "LAO", "LBR", "LSO", "MDG", "MLI", "MRT", "MWI"
# - start 1982: "BWA", 
# - start 1985: "MNG"
# - start 1990: "PRK", "RUS", "UKR", "KZA", "SRB", "BIH", "KGZ", "UZB", "BLR", "LTU", "MDA", "HRV", "MKD", "LVA", "TJK", "SVN", "GEO", "EST", "TKM", "ARM", "AZE"
# - start 1991: "NAM"
# - start 1993: "ERI"
# - start 1995: "KHM"
# - start 2000: "NER"
# GDP : 
# - No data   : "CUB", "AFG", "ERI", "LBY"
# - start 1982: "BWA"
# - start 1985: "MNG"
# - start 1988: "YEM"
# - start 1990: "CZE", "SVK", "SRB", "BIH", "KGZ", "UZB", "BLR", "LTU", "MDA", "HRV", "MKD", "LVA", "TJK", "SVN", "GEO", "EST", "TKM", "ARM", "AZE"
# P : 
# - start 1990:
# E_CP :
# - start 1982: "BWA"
# - start 1985: "MNG"
# - start 1990: "PRK", "RUS", "UKR", "KZA", "SRB", "BIH", "KGZ", "UZB", "BLR", "LTU", "MDA", "HRV", "MKD", "LVA", "TJK", "SVN", "GEO", "EST", "TKM", "ARM", "AZE"
# - start 2000: "NER"
# - stops in 1995: "IRL"
# CO2, EF:
# - No data: SDN
u_period = c(1971, 2012)
u_iso    = c("CHN","USA", "DEU", "IND", "JPN", "POL", "GBR", "ZAF", "FRA", "KOR", "TUR", "CAN", "AUS", "BEL", "ITA", "BRA", "ESP", "IDN",
             "VNM", "THA", "HUN", "ROU", "AUT", "BGR", "NLD", "PAK", "MEX", "COL", "SWE", "FIN", "ZWE", "NOR", "NZL", "DNK", "GRC", "PHL", 
             "MYS", "CHL", "CHE", "PRT", "EGY", "PER", "IRN", "ALB", "HKG", "ARG", "BGD", "MAR", "ZMB", "ARE", "DZA", "MMR", "NPL", "VEN",
             "COD", "KEN", "MOZ", "LBN", "DOM", "NGA", "HND", "TUN", "SEN", "PAN", "JAM", "CYP", "LKA", "ETH", "MUS", "JOR", "CRI", "ISR", 
             "SGP", "URY", "TZA", "BOL", "HTI", "SYR", "GTM", "SLV")
u_var    = c("E_CC", "E_CCGDP", "E_CCP",
             "E_CIm", "E_CEx", "E_CP", "GDP", "GDPpc", "P", "P_Ndep", "URB", "LEx", "E_CRes", "E_CSeInd", "E_OC", "E_GC", "E_NC", "E_RenC", 
             "Elec_C", "Elec", "E_TotPeS", "E_TotFeC", "E_TotFeCpc", "EE", "EF", "CO2")
# Config2: 1990-2012 &  countries
u_period = c(1990, 2012)
u_iso    = c("CHN", "DEU", "IND", "JPN", "GBR", "ZAF", "FRA", "KOR", "TUR", "AUS", "ITA", "BRA", "IDN",
             "VNM", "THA", "ROU", "AUT", "BGR", "NLD", "PAK", "MEX", "COL", "SWE", "FIN", "NOR", "NZL", "DNK", "PHL", 
             "MYS", "CHL", "CHE", "EGY", "PER", "IRN", "ARG", "BGD", "MAR", "DZA",
             "COD", "KEN", "MOZ", "DOM", "NGA", "HND", "TUN", "SEN", "PAN", "CYP", "LKA", "MUS", "JOR", "CRI", 
             "SGP", "URY", "TZA", "BOL", "SLV")
u_var    = c("E_CC", "E_CCGDP", "E_CCP",
             "E_CIm", "E_CEx", "E_CP", "GDP", "GDPpc", "P", "P_Ndep", "URB", "LEx", "E_CRes", "E_CSeInd", "E_OC", "E_GC", "E_NC", "E_RenC", 
             "Elec_C", "Elec", "E_TotPeS", "E_TotFeC", "E_TotFeCpc", "EE", "EF", "CO2",
             "GDP_Ag", "GDP_Ind", "GDP_Ser", "GDP_Tra")

u_var  <- "E_CC"
u_niso <- 30
u_ncol <- u_niso %/% 10

source("scripts/user_settings.R")
source("scripts/init.R")
library("PerformanceAnalytics")
library("corrr")
source("scripts/process_data.R")

#===== Rank coal consuming countries by decreasing order ===================
tmp <- v_dataShort %>% 
  filter(variable == "E_CC") %>% 
  select(-variable) %>% 
  group_by(iso) %>% 
  arrange(year) %>% 
  summarize(value=sum(value, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(value != 0) %>% 
  arrange(desc(value))

iso_lvls = tmp$iso

#======= Variable availability  ======================
u_period = c(1991, 2012)
#"USA", "POL", "BEL", "CAN", "ARE", "ISR", "ETH", "HTI", "SYR", "ETH", "ESP", "HUN", "GRC", "PRT", "ALB", "MMR", "ZMB", "LBN", "NPL", "ZWE", "JAM", "VEN", "HKG", "GTM"
u_iso    = c("CHN", "DEU", "IND", "JPN", "GBR", "ZAF", "FRA", "KOR", "TUR", "AUS", "ITA", "BRA", "IDN",
             "VNM", "THA", "ROU", "AUT", "BGR", "NLD", "PAK", "MEX", "COL", "SWE", "FIN", "NOR", "NZL", "DNK", "PHL", 
             "MYS", "CHL", "CHE", "EGY", "PER", "IRN", "ARG", "BGD", "MAR", "DZA",
             "COD", "KEN", "MOZ", "DOM", "NGA", "HND", "TUN", "SEN", "PAN", "CYP", "LKA", "MUS", "JOR", "CRI", 
             "SGP", "URY", "TZA", "BOL", "SLV")
u_var    = c("E_CC", "E_CCGDP", "E_CCP",
             "E_CIm", "E_CEx", "E_CP", "GDP", "GDPpc", "P", "P_Ndep", "URB", "LEx", "E_CRes", "E_CSeInd", "E_OC", "E_GC", "E_NC", "E_RenC", 
             "Elec_C", "Elec", "E_TotPeS", "E_TotFeC", "E_TotFeCpc", "EE", "EF", "CO2",
             "GDP_Ag", "GDP_Ind", "GDP_Ser", "GDP_Tra")#,
             #"Manu", "Manu_Ex", "Steel", "Constr")
#"K", "Lab", "Debt_Ext"),
#)#,
#)

p = ggplot(v_dataShort %>% 
             filter(!grepl("d._|\\^2", variable)) %>%
             filter(year >= u_period[1], year <= u_period[2]) %>% 
             filter(iso %in% u_iso) %>% 
             filter(variable %in% u_var) %>% 
             mutate(iso = factor(iso, levels=iso_lvls, ordered=TRUE)) %>% 
             group_by(iso, variable) %>% 
             mutate(nans = ifelse(is.na(value),0,1)) %>% 
             summarize(count=sum(nans)) %>% 
             ungroup() %>% 
             mutate(count = count/max(count))) +
  geom_point(aes(x=iso, y=variable, colour=count, size=count)) +
  scale_color_gradient2(low="red", mid="green", high="green", midpoint=0.99, na.value = "grey") +
  ggtitle(paste0("Period: ", u_period[1], "-", u_period[2])) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))
print(p)

p = ggplot(v_data[[k_case]]$data %>% 
             gather(variable,value,-iso,-year) %>% 
             filter(year >= u_period[1], year <= u_period[2]) %>% 
             filter(iso %in% u_iso) %>% 
             mutate(iso = factor(iso, levels=iso_lvls, ordered=TRUE)) %>% 
             group_by(iso, variable) %>% 
             mutate(nans = ifelse(is.na(value),0,1)) %>% 
             summarize(count=sum(nans)) %>% 
             ungroup() %>% 
             mutate(count = count/max(count))) +
  geom_point(aes(x=iso, y=variable, colour=count, size=count)) +
  scale_color_gradient2(low="red", mid="green", high="green", midpoint=0.99, na.value = "grey") +
  ggtitle(paste0("Period: ", u_period[1], "-", u_period[2])) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))
print(p)


#======== Variable availability (bis) =========================
total = v_dataShort %>% 
  filter(variable == "E_CC") %>%
  filter(year >= u_period[1], year <= u_period[2]) %>% 
  mutate(region = "World") %>% 
  group_by(region) %>% 
  summarize(value = sum(value, na.rm=TRUE)) %>% 
  ungroup()
sum = v_dataShort %>% 
  filter(variable == "E_CC", iso %in% iso_lvls[which(iso_lvls %in% u_iso)]) %>%
  filter(year >= u_period[1], year <= u_period[2]) %>%
  mutate(region = "World") %>% 
  group_by(region) %>% 
  summarize(value = sum(value, na.rm=TRUE)) %>% 
  ungroup()
print(sum$value/total$value*100)  


tmp2 = v_dataShort %>% 
  filter(!grepl("d._|\\^2", variable), iso %in% iso_lvls[which(iso_lvls %in% u_iso)]) %>%
  filter(year >= u_period[1], year <= u_period[2]) %>% 
  mutate(iso = factor(iso, levels=iso_lvls[which(iso_lvls %in% u_iso)], ordered=TRUE)) %>% 
  group_by(iso, variable) %>% 
  arrange(year) %>% 
  mutate(nans = ifelse(is.na(value) | value == 0.0,0,1)) %>% 
  mutate(changes = nans - lag(nans))

tmp2 = tmp2 %>% 
  left_join( 
    left_join(
      tmp2 %>% 
        filter(changes == 1) %>% 
        filter(year == first(year)) %>% 
        rename(year_start = year) %>% 
        select(iso,variable,year_start),
      tmp2 %>% 
        filter(changes == -1) %>% 
        filter(year == last(year)) %>% 
        rename(year_end = year) %>% 
        select(iso,variable,year_end),
      by=c("iso", "variable")),
    by=c("iso", "variable")) %>% 
  mutate(year_start = ifelse(is.na(year_start), u_period[1], year_start)) %>% 
  mutate(year_end = ifelse(is.na(year_end), u_period[2], year_end)) %>% 
  filter(year == year_start) %>% 
  mutate(p1 = year_start   - u_period[1]) %>% 
  mutate(p2 = (year_end-1) - year_start) %>% 
  mutate(p3 = u_period[2]  - (year_end-1)) %>% 
  select(iso,variable,p1,p2,p3) %>% 
  ungroup() %>% 
  group_by(variable) %>%
  arrange(iso) %>% 
  mutate(x = row_number()) %>% 
  ungroup() %>% 
  group_by(iso) %>%
  arrange(variable) %>% 
  mutate(y = row_number()) %>% 
  ungroup()

p = ggplot() +
  geom_scatterpie(aes(x=x, y=y, r=0.4), data=tmp2 , cols=c("p1","p2","p3")) +
  theme_bw() +
  scale_x_continuous(breaks=1:max(tmp2$x), labels=levels(tmp2$iso)) +
  scale_y_continuous(breaks=1:max(tmp2$y), labels=sort(unique(tmp2$variable))) +
  ggtitle(paste0("Period: ", u_period[1], "-", u_period[2], " (first 80 most coal consuming countries)")) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))

print(p)


#==== Variable transformations (original data) - 10 countries ==============
u_niso <- 10
u_ncol <- u_niso %/% 1
#for (k_var in unique(v_dataShort$variable)) {
for (k_var in u_var) {
  
  p = ggplot(v_dataShort %>% 
               filter(variable == k_var, iso %in% iso_lvls[which(iso_lvls %in% u_iso)][1:u_niso]) %>% 
               select(-variable) %>%
               mutate(iso = factor(iso, levels=iso_lvls[which(iso_lvls %in% u_iso)], ordered=TRUE)) %>% 
               mutate(logval = log(value)) %>%
               left_join(
                 v_dataShort %>% 
                   filter(variable == k_var, iso %in% iso_lvls[which(iso_lvls %in% u_iso)][1:u_niso]) %>% 
                   select(-variable) %>%
                   mutate(iso = factor(iso, levels=iso_lvls[which(iso_lvls %in% u_iso)], ordered=TRUE)) %>% 
                   mutate(logval = log(value)) %>%
                   group_by(iso) %>% 
                   summarize(logval_mean=mean(logval, na.rm=TRUE)) %>% 
                   ungroup()
               ) %>% 
               group_by(iso) %>% 
               arrange(year) %>% 
               mutate(log_firstdiff = logval - lag(logval)) %>% 
               mutate(log_demean    = logval - logval_mean) %>% 
               ungroup() %>% 
               rename(none=value) %>% 
               select(iso,year,none,logval,log_demean,log_firstdiff) %>% 
               gather(transformation, value, -iso, -year) %>% 
               mutate(transformation = factor(transformation, levels=c("none", "logval", "log_demean", "log_firstdiff")))) +
    geom_line(aes(x=year, y=value)) +
    facet_grid(transformation~iso, scales="free") +
    ggtitle(k_var)
  print(p)
  
}

selectedVars <- c("E_CC", "E_CCGDP", "E_CSeInd", "E_CRes", "Elec", "E_GC", "E_RenC", "GDP", "GDPpc", "P", "URB", "LEx", "E_NC")
#"E_CIm", "E_CEx", "E_CP", "GDP", "GDPpc", "P", "P_Ndep", "URB", "LEx", "E_CRes", "E_CSeInd", "E_OC", "E_GC", "E_NC", "E_RenC", 
#"Elec_C", "Elec", "E_TotPeS", "E_TotFeC", "E_TotFeCpc", "EE", "EF"

p = ggplot(v_dataShort %>% 
             filter(year >= u_period[1], year <= u_period[2], variable %in% selectedVars, iso %in% iso_lvls[which(iso_lvls %in% u_iso)][1:u_niso]) %>% 
             group_by(variable) %>% 
             mutate(iso = factor(iso, levels=iso_lvls[which(iso_lvls %in% u_iso)], ordered=TRUE)) %>% 
             mutate(logval = log(value)) %>%
             left_join(
               v_dataShort %>% 
                 filter(year >= u_period[1], year <= u_period[2], variable %in% selectedVars, iso %in% iso_lvls[which(iso_lvls %in% u_iso)][1:u_niso]) %>% 
                 mutate(iso = factor(iso, levels=iso_lvls[which(iso_lvls %in% u_iso)], ordered=TRUE)) %>% 
                 mutate(logval = log(value)) %>%
                 group_by(iso,variable) %>% 
                 summarize(logval_mean=mean(logval, na.rm=TRUE)) %>% 
                 ungroup()
             ) %>% 
             group_by(iso) %>% 
             arrange(year) %>% 
             mutate(log_firstdiff = logval - lag(logval)) %>% 
             mutate(log_demean    = logval - logval_mean) %>% 
             ungroup() %>% 
             rename(none=value) %>% 
             select(iso,year,variable,log_demean) %>% 
             rename(value = log_demean)) +
  geom_line(aes(x=year, y=value)) +
  facet_grid(variable~iso, scales="free") +
  theme_bw()
print(p)


#===== Variable transformations (original data) - 30 countries ===========================
u_niso <- 30
u_ncol <- u_niso %/% 3
for (k_var in unique(v_dataShort$variable)) {
  
  p = ggplot(v_dataShort %>% 
               filter(variable == k_var, iso %in% iso_lvls[which(iso_lvls %in% u_iso)][1:u_niso]) %>% 
               select(-variable) %>%
               mutate(iso = factor(iso, levels=iso_lvls[which(iso_lvls %in% u_iso)], ordered=TRUE)) %>% 
               mutate(logval = log(value)) %>%
               left_join(
                 v_dataShort %>% 
                   filter(variable == k_var, iso %in% iso_lvls[which(iso_lvls %in% u_iso)][1:u_niso]) %>% 
                   select(-variable) %>%
                   mutate(iso = factor(iso, levels=iso_lvls[which(iso_lvls %in% u_iso)], ordered=TRUE)) %>% 
                   mutate(logval = log(value)) %>%
                   group_by(iso) %>% 
                   summarize(logval_mean=mean(logval, na.rm=TRUE)) %>% 
                   ungroup()
               ) %>% 
               group_by(iso) %>% 
               arrange(year) %>% 
               mutate(log_firstdiff = logval - lag(logval)) %>% 
               mutate(log_demean    = logval - logval_mean) %>% 
               ungroup() %>% 
               rename(none=value) %>% 
               select(iso,year,log_demean)) +
    geom_line(aes(x=year, y=log_demean)) +
    facet_wrap(~iso, ncol=u_ncol, scales="free") +
    ggtitle(k_var)
  print(p)
  
}



#====== Variable correlations - matrix simple (1) =======================
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
tmp <- v_dataShortProc$data %>% 
  #gather(variable, value, -iso, -year) %>% 
  #mutate(variable = gsub("log_", "", variable)) %>% 
  # filter(variable %in% c("log_CO2", "log_E_CC", "log_E_CEx", "log_E_CIm", "log_E_CP", "log_E_GC", "log_E_OC", "log_E_RenC",
  #                        "log_E_TotFeC", "log_E_TotPeS", "log_EE", "log_EF", "log_Elec_C", "log_GDP_Ag", "log_GDP_Ind", 
  #                        "log_GDP_Ser", "log_GDP_Tra", "log_GDPpc", "log_GINI", "log_LEx", "log_Manu_Ex", "log_ManuGDP",
  #                        "log_P", "log_P_Ndep", "log_PDen", "log_URB")) %>% 
  filter(year >= u_period[1], year <= u_period[2], iso %in% iso_lvls[which(iso_lvls %in% u_iso)]) %>% #variable %in% u_var, 
  # group_by(variable) %>% 
  # mutate(iso = factor(iso, levels=iso_lvls[which(iso_lvls %in% u_iso)], ordered=TRUE)) %>% 
  # mutate(logval = log(value)) %>%
  # left_join(
  #   v_dataShort %>% 
  #     filter(year >= u_period[1], year <= u_period[2], variable %in% selectedVars, iso %in% iso_lvls[which(iso_lvls %in% u_iso)][1:u_niso]) %>% 
  #     mutate(iso = factor(iso, levels=iso_lvls[which(iso_lvls %in% u_iso)], ordered=TRUE)) %>% 
  #     mutate(logval = log(value)) %>%
  #     group_by(iso,variable) %>% 
  #     summarize(logval_mean=mean(logval, na.rm=TRUE)) %>% 
  #     ungroup()
  # ) %>% 
  # group_by(iso) %>% 
  # arrange(year) %>% 
  # mutate(log_firstdiff = logval - lag(logval)) %>% 
  # mutate(log_demean    = logval - logval_mean) %>% 
  # ungroup() %>% 
  # rename(none=value) %>% 
  # select(iso,year,variable,log_demean) %>% 
  # rename(value = log_demean) %>% 
  # mutate(value = ifelse(is.na(value),0,value)) %>%
  spread(variable, value) %>% 
  select(-iso, -year)
mat <- cor(tmp, use="pairwise.complete.obs")
corrplot(mat, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = cor.mtest(mat), sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

#====== Variable correlations - matrix simple (2) =======================
png("output/data_check/corr_varpairs.png", width=2000, height=2000, res = 100)
v_dataShortProc$data %>% 
  gather(variable, value, -iso, -year) %>% 
  filter(variable %in% c("log_CO2", "log_E_CC", "log_E_CEx", "log_E_CIm", "log_E_CP", "log_E_GC", "log_E_OC", "log_E_RenC",
                         "log_E_TotFeC", "log_E_TotPeS", "log_EE", "log_EF", "log_Elec_C", "log_GDP_Ag", "log_GDP_Ind", 
                         "log_GDP_Ser", "log_GDP_Tra", "log_GDPpc", "log_GINI", "log_LEx", "log_Manu_Ex", "log_ManuGDP",
                         "log_P", "log_P_Ndep", "log_PDen", "log_URB")) %>% 
  spread(variable, value) %>% 
  select(-iso, -year) %>% 
  chart.Correlation(histogram=TRUE, pch=19)
dev.off()

#====== Variable correlations - network (3) =======================
v_dataShortProc$data %>% 
  gather(variable, value, -iso, -year) %>% 
  filter(variable %in% c("log_CO2", "log_E_CC", "log_E_CIm", "log_E_GC", "log_E_OC",
                         #"log_E_CEx", "log_E_CP", "log_EE", "log_E_RenC",
                         "log_E_TotFeC", "log_E_TotPeS", "log_EF", "log_Elec_C", "log_GDP_Ag", "log_GDP_Ind", 
                         "log_GDP_Ser", "log_GDP_Tra", "log_GDPpc", "log_GINI", "log_LEx", "log_Manu_Ex", "log_ManuGDP",
                         "log_P", "log_P_Ndep", "log_PDen", "log_URB")) %>% 
  spread(variable, value) %>% 
  select(-iso, -year) %>% 
  correlate() %>% 
  network_plot(min_cor=0.6)


#======== Variable correlations - Matrix pairs (4) ===============================
pdf("output/data_check/ggpairs.pdf", width=20, height=20)
tmp <- v_dataShortProc$data %>% 
  gather(variable, value, -iso, -year) %>% 
  filter(variable %in% c("log_CO2", "log_E_CC", "log_E_CEx", "log_E_CIm", "log_E_CP", "log_E_GC", "log_E_OC", "log_E_RenC",
                         "log_E_TotFeC", "log_E_TotPeS", "log_EE", "log_EF", "log_Elec_C", "log_GDP_Ag", "log_GDP_Ind", 
                         "log_GDP_Ser", "log_GDP_Tra", "log_GDPpc", "log_GINI", "log_LEx", "log_Manu_Ex", "log_ManuGDP",
                         "log_P", "log_P_Ndep", "log_PDen", "log_URB")) %>% 
  spread(variable, value) %>% 
  select(-iso, -year)
ggpairs(tmp)
dev.off()



#======= Data summary ==================================
yearcumsum = 2010
tmp_sumECC <- v_dataShort %>% 
  filter(variable == "E_CC", year == yearcumsum) %>% 
  select(iso, value) %>% 
  summarise(tot2010=sum(value, na.rm=TRUE))
dataSum <- v_dataShort %>% 
       filter(iso %in% iso_lvls) %>% 
       group_by(variable, iso) %>% 
       filter(!is.na(value) & value != 0) %>% 
       summarise(min_year=min(year), max_year=max(year)) %>% 
       ungroup() %>% 
       left_join(
         v_dataShort %>% 
           filter(variable == "E_CC", year == yearcumsum) %>% 
           select(iso, value) %>% 
           rename(valcumsum = value)) %>% 
       mutate(iso = factor(iso, levels=iso_lvls, labels=paste0(iso_lvls, "(", 1:length(iso_lvls), ")"), ordered=TRUE)) %>% 
       group_by(variable, min_year) %>% 
       arrange(iso) %>% 
       summarise(isos=paste0(iso, collapse=", "), nb_isos=length(iso), share=round(sum(valcumsum)/tmp_sumECC$tot2010*100, digits=2))  %>% 
       ungroup() %>% 
       group_by(variable) %>% 
       mutate(cumsum=cumsum(share)) %>% 
       ungroup() 

ggplot(data=dataSum %>% 
         filter(variable %in% c("E_CC", "P", "GDP", "URB", "LEx", "E_CP", "E_CIm", "E_CEx", "GDP_Ag", "GDP_Ind", "GDP_Ser", "GDP_Tra", "Manu", "Manu_Ex")) %>% 
         select(variable, min_year, cumsum)) + 
  geom_path(aes(x=min_year, y=cumsum)) + facet_wrap(~variable, ncol=2)


u_selectedVars      <- c("E_CC", "GDPpc", "P", "E_CCGDP")
u_selectedStartYear <- 1971

selectedCountries <- function(startYear, selectedVars){
  tmp <- v_dataShort %>% 
    filter(iso %in% iso_lvls, variable %in% selectedVars) %>% 
    group_by(variable, iso) %>% 
    filter(!is.na(value) & value != 0) %>% 
    summarise(min_year=min(year), max_year=max(year)) %>% 
    ungroup() %>% 
    left_join(
      v_dataShort %>% 
        filter(variable == "E_CC", year == yearcumsum) %>% 
        select(iso, value)) %>% 
    mutate(iso = factor(iso, levels=iso_lvls, ordered=TRUE)) %>% 
    filter(min_year <= startYear) %>% 
    select(-min_year, -max_year) %>% 
    spread(variable, value) %>% 
    gather(variable, value, -iso) %>%     
    group_by(iso) %>% 
    summarise(keep=ifelse(!is.na(sum(value)), 1, 0)) %>% 
    ungroup() %>% 
    filter(keep == 1) %>% 
    select(iso) %>% 
    left_join(
      v_dataShort %>% 
        filter(variable == "E_CC", year == yearcumsum) %>% 
        select(iso, value))
  
  return(data.frame(startyear=startYear, nbiso = dim(tmp)[1], share=sum(tmp$value)/tmp_sumECC$tot2010*100))
  
} 

tmplist <- lapply(1960:2000, selectedCountries, u_selectedVars)
balancedPanel <- do.call("rbind", tmplist)

ggplot(balancedPanel) + geom_path(aes(x=startyear, y=share))
