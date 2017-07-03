compare_regression_models <- function(i_mod1, i_mod2) {
  m1 <- as.matrix(i_mod1 %>% select(-Node))
  row.names(m1) <- i_mod1$Node
  m2 <- as.matrix(i_mod2 %>% select(-Node))
  row.names(m2) <- i_mod2$Node
  
  out <- matrix(0, nrow=nrow(m1), ncol=nrow(m2))
  
  for (k1 in 1:nrow(m1)) {
    d <- rowSums((m2-m1[k1,])^2)
    if (length(which(d == min(d))) > 1) print(d)
    out[k1,which(d == min(d))] = 1 #d[which(d == min(d))]
  }
  
  row.names(out) <- i_mod1$Node
  colnames(out)  <- i_mod2$Node
  
  return(out)
}

data_nbnodes <- data.frame(
  id      = 0:100, 
  nbnodes = as.numeric(lapply(v_regdata$`testingMethods6 - E_CC-SimpleSet - FE-MLinROLS - 1971-2012`, nrow)))


mod1 <- v_regdata$`testingMethods6 - E_CC-SimpleSet - FE-MLinROLS - 1971-2012`[[2]]
mod2 <- v_regdata$`testingMethods6 - E_CC-SimpleSet - FE-MLinROLS - 1971-2012`[[3]]

modmatch1 <- compare_regression_models(mod1, mod2)
modmatch2 <- compare_regression_models(mod2, mod1)

df = rbind(modmatch1 %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "mod1") %>% 
  gather(mod2,value,-mod1) %>% 
  mutate(comp="M1M2"),
  modmatch2 %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "mod2") %>% 
    gather(mod1,value,-mod2) %>% 
    mutate(comp="M2M1") %>% 
    select(mod1,mod2,value,comp)
)
ggplot(df) + geom_tile(aes(x=mod1, y=mod2, fill=value)) + facet_wrap(~comp) +
scale_fill_gradient2(low="white", mid="white", high="black", midpoint = 0.1)

df2 = left_join(modmatch1 %>% 
             as.data.frame() %>% 
             tibble::rownames_to_column(var = "mod1") %>% 
             gather(mod2,value,-mod1),
           modmatch2 %>% 
             as.data.frame() %>% 
             tibble::rownames_to_column(var = "mod2") %>% 
             gather(mod1,value,-mod2) %>% 
             select(mod1,mod2,value),
           by=c("mod1", "mod2")
) %>% 
  mutate(value=value.x+value.y)

ggplot(df2) + geom_tile(aes(x=mod1, y=mod2, fill=value))
  scale_fill_gradient2(low="white", mid="white", high="black", midpoint = 0.1)