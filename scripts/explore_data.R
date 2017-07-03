isoclass <- read_xls("data/WorldBank/CLASS.xls", skip = 4)[c(-1),c("Code", "Income group")]

case1 <- "testingMethods6 - E_CC-SimpleSet - FE-MLinROLS - 1971-2012"
  

myvar="log_E_CC"
p = ggplot(data=left_join(v_data[[case1]][["0"]]$data %>% 
                       gather(variable, value, -iso, -year) %>% 
                       filter(variable == myvar), 
                     isoclass, 
                     by=c("iso"="Code")) %>% 
             filter(iso %in% u_iso[1:40])) +
  geom_segment(aes(x=1971, xend=2012, y=0, yend=0)) +
  geom_line(aes(x=year, y=value, colour=`Income group`, group=iso, label=iso)) + 
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("demeaned+logged value") +
  ggtitle(myvar)
#print(p)
plotly::ggplotly(p)

myvar=c("log_E_CC", "log_E_CP", "log_E_CIm", "log_E_CEx")
p = ggplot(data=left_join(v_data[[case1]][["0"]]$data %>% 
                            gather(variable, value, -iso, -year) %>% 
                            filter(variable %in% myvar), 
                          isoclass, 
                          by=c("iso"="Code")) %>% 
             filter(iso %in% u_iso[1:20])) +
  geom_segment(aes(x=1971, xend=2012, y=0, yend=0)) +
  geom_line(aes(x=year, y=value, colour=`Income group`, group=iso)) + 
  facet_wrap(~variable, ncol=2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("demeaned+logged value")
#print(p)
plotly::ggplotly(p)

myvar=c("log_GDPpc", "log_P", "log_URB", "log_EE")
p = ggplot(data=left_join(v_data[[case1]][["0"]]$data %>% 
                            gather(variable, value, -iso, -year) %>% 
                            filter(variable %in% myvar), 
                          isoclass, 
                          by=c("iso"="Code")) %>% 
             filter(iso %in% u_iso[1:20])) +
  geom_segment(aes(x=1971, xend=2012, y=0, yend=0)) +
  geom_line(aes(x=year, y=value, colour=`Income group`, group=iso)) + 
  facet_wrap(~variable, ncol=3, scales="free_y") +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("demeaned+logged value")
#print(p)
plotly::ggplotly(p)

myvar=c("E_CIm", "E_CP", "E_CEx", "GDPpc", "P", "URB", "EE")
p = ggplot(data=left_join(v_dataShort %>% 
                            filter(year >= 1971, year <= 2012) %>% 
                            filter(variable %in% c("E_CC", myvar)) %>% 
                            rename(original=value) %>% 
                            mutate(logged = log(original)) %>% 
                            group_by(iso,variable) %>% 
                            mutate(logdem = logged - mean(logged, na.rm=TRUE)) %>% 
                            ungroup() %>% 
                            gather(transform, value, -iso, -year, -variable), 
                          isoclass, 
                          by=c("iso"="Code")) %>% 
             filter(iso %in% u_iso[1:20])) +
  #geom_segment(aes(x=1971, xend=2012, y=0, yend=0)) +
  geom_line(aes(x=year, y=value, colour=`Income group`, group=iso)) + 
  facet_wrap(transform~variable, ncol=8, scales="free_y") +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("value")
#print(p)
plotly::ggplotly(p)

myvar=c("log_GDP_Ag", "log_GDP_Ind", "log_GDP_Ser", "log_GDP_Tra")
p = ggplot(data=left_join(v_data[[case1]][["0"]]$data %>% 
                            gather(variable, value, -iso, -year) %>% 
                            filter(variable %in% myvar), 
                          isoclass, 
                          by=c("iso"="Code")) %>% 
             filter(iso %in% u_iso[1:20])) +
  geom_segment(aes(x=1971, xend=2012, y=0, yend=0)) +
  geom_line(aes(x=year, y=value, colour=`Income group`, group=iso)) + 
  facet_wrap(~variable, ncol=2, scales="free_y") +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") + ylab("demeaned+logged value")
#print(p)
plotly::ggplotly(p)

v_dataShort %>% 
  filter(variable == "E_CC", iso %in% u_iso[1:20]) %>% 
  group_by(variable, iso) %>% 
  summarise(
    minval=min(value, na.rm=TRUE), 
    maxval=max(value, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(change = (maxval-minval)/minval) %>% 
  left_join(isoclass, by=c("iso"="Code"))

v_dataShort %>% 
  filter(variable == "E_CC", iso %in% u_iso[1:20], year %in% c(1971,2012)) %>%
  spread(year, value) %>% 
  mutate(change = (`2012`-`1971`)/`1971`) %>% 
  left_join(isoclass, by=c("iso"="Code"))