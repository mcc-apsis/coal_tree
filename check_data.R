# Understanding the log, demeaning and first-differences transformations

u_var  <- "E_CC"
u_niso <- 30
u_ncol <- u_niso %/% 10

source("scripts/user_settings.R")
source("scripts/init.R")
source("scripts/process_data.R")

#=========================================================
tmp <- v_dataShort %>% 
  filter(variable == u_var) %>% 
  select(-variable) %>% 
  group_by(iso) %>% 
  arrange(year) %>% 
  summarize(value=sum(value, na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(value))

iso_lvls = tmp$iso


#=========================================================
u_niso <- 10
u_ncol <- u_niso %/% 1
for (k_var in unique(v_dataShort$variable)) {
  
  p = ggplot(v_dataShort %>% 
               filter(variable == k_var, iso %in% iso_lvls[1:u_niso]) %>% 
               select(-variable) %>%
               mutate(iso = factor(iso, levels=iso_lvls, ordered=TRUE)) %>% 
               mutate(logval = log(value)) %>%
               left_join(
                 v_dataShort %>% 
                   filter(variable == k_var, iso %in% iso_lvls[1:u_niso]) %>% 
                   select(-variable) %>%
                   mutate(iso = factor(iso, levels=iso_lvls, ordered=TRUE)) %>% 
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
               select(iso,year,none,log_demean,log_firstdiff) %>% 
               gather(transformation, value, -iso, -year) %>% 
               mutate(transformation = factor(transformation, levels=c("none", "log_demean", "log_firstdiff")))) +
    geom_line(aes(x=year, y=value)) +
    facet_grid(transformation~iso, scales="free") +
    ggtitle(k_var)
  print(p)
  
}

#=========================================================
u_niso <- 30
u_ncol <- u_niso %/% 3
for (k_var in unique(v_dataShort$variable)) {
  
  p = ggplot(v_dataShort %>% 
               filter(variable == k_var, iso %in% iso_lvls[1:u_niso]) %>% 
               select(-variable) %>%
               mutate(iso = factor(iso, levels=iso_lvls, ordered=TRUE)) %>% 
               mutate(logval = log(value)) %>%
               left_join(
                 v_dataShort %>% 
                   filter(variable == k_var, iso %in% iso_lvls[1:u_niso]) %>% 
                   select(-variable) %>%
                   mutate(iso = factor(iso, levels=iso_lvls, ordered=TRUE)) %>% 
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

#=========================================================
u_period = c(1990, 2010)
p = ggplot(v_dataShort %>% 
             filter(!grepl("d._|\\^2", variable), iso %in% iso_lvls[1:80]) %>%
             filter(year >= u_period[1], year <= u_period[2]) %>% 
             group_by(iso, variable) %>% 
             mutate(nans = ifelse(is.na(value),0,1)) %>% 
             summarize(count=sum(nans)) %>% 
             ungroup() %>% 
             mutate(count = count/max(count))) +
  geom_point(aes(x=iso, y=variable, colour=count, size=count)) +
  scale_color_gradient2(low="white", mid="black", high="black", midpoint=0.8) +
  ggtitle(paste0("Period: ", u_period[1], "-", u_period[2]))
print(p)


#=========================================================
u_period = c(1971, 2012)
tmp2 = v_dataShort %>% 
  filter(!grepl("d._|\\^2", variable), iso %in% iso_lvls[1:80]) %>%
  filter(year >= u_period[1], year <= u_period[2]) %>% 
  mutate(iso = factor(iso, levels=iso_lvls[1:80], ordered=TRUE)) %>% 
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
