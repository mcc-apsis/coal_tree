t2 <- Sys.time()

for (k_case in 1:Ncases) {
  
  k_caseName <- ifelse(nchar(k_case) < nchar(Ncases), paste0(rep("0", nchar(Ncases)-nchar(k_case)), k_case), paste0(k_case))
  
  cat(paste0("Plotting Experiment ", k_caseName , "\n"))
  
  if (!is.null(v_tree[[k_case]])) {
    # Tree
    tree <- as.fake.rpart.tree(v_tree[[k_case]])
    if (dim(tree$frame)[1] != 0) {
      plot_tree(tree, 
                TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_case]]$nodeType == "Terminal node")), "\n#Countries: ",length(unique(v_data[[k_case]]$data$iso)), " - period: ",min(v_data[[k_case]]$data$year),"-",max(v_data[[k_case]]$data$year)), 
                CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2,
                FILENAME = file.path("output", testingMethodName, "Plots Trees/", paste0(k_caseName, ".pdf")))
      plot_tree(tree, 
                TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_case]]$nodeType == "Terminal node")), "\n#Countries: ",length(unique(v_data[[k_case]]$data$iso)), " - period: ",min(v_data[[k_case]]$data$year),"-",max(v_data[[k_case]]$data$year)), 
                CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2,
                FILENAME = file.path("output", testingMethodName, "Plots Trees/", paste0(k_caseName, ".png")))
    }
    
    if (dim(v_alloc[[k_case]][[1]])[1] != 0) {
      # Country pathways
      png(filename=file.path("output", testingMethodName, "Plots country pathways/", paste0(k_caseName, ".png")), width=2500, height=2500, res = 150)
      par(mfrow=c(3,3))
      for (kiso in c("USA", "CHN", "DEU", "AUS", "IND", "IDN", "ZAF", "COL", "VNM")) {
        plot_ISOPathways(kiso,v_alloc[[k_case]][[1]], v_tree[[k_case]][[1]])
      }
      par(mfrow=c(1,1))
      dev.off()
      
      # Prediction plots
      dvar = paste0("log_", u_cases[[1]]$dvar)
      
      pred_data <- list()
      for (kn in v_regdata[[k_case]]$Node) {
        tmp_data = v_regdata[[k_case]] %>% 
          filter(Node == kn)
        tmp_cst = tmp_data$Constant
        tmp_reg = tmp_data %>% 
          select(-Node, -Constant) %>% 
          gather(variable, regcoeff)
        
        tmp_data <- v_alloc[[k_case]] %>% 
          filter(tnode == kn) %>% 
          select(-tnode) %>% 
          gather(variable, value, -iso, -year)
        original <- tmp_data %>% 
          filter(variable == dvar)
        predict <- tmp_data %>% 
          filter(variable != dvar) %>% 
          left_join(tmp_reg, by=c("variable")) %>% 
          #=== reproducing GUIDE approach to fill NAs (i.e. global average)
          group_by(variable) %>% 
          mutate(value = ifelse(is.na(value), mean(value, na.rm=TRUE), value)) %>% 
          ungroup() %>% 
          #=========
        mutate(tmp = value*regcoeff) %>%
          group_by(iso,year) %>% 
          summarize(estimate=sum(tmp, na.rm=TRUE)+tmp_cst) %>% 
          ungroup()
        
        pred_data[[paste(kn)]] <- left_join(original, predict, by=c("iso","year")) %>% 
          mutate(node = kn)
        
      }
      pred_data <- do.call("rbind", pred_data)
      
      p = ggplot(pred_data) +
        geom_segment(aes(x=-1.5, xend=1.5, y=-1.5, yend=1.5), color="red") +
        geom_point(aes(x=value, y=estimate)) +
        facet_wrap(~node, ncol=6) +
        xlab("original") + ylab("estimate")
      #print(p)
      ggsave(filename = file.path(paste0("output/", testingMethodName, "/Pred Plots/"), paste0(k_caseName, ".png")), width = 297, height = 210, units = c("mm"), dpi = 300)
    }
  }
}
plot_allexp <- Sys.time() - t2
cat(paste0("It took ",  plot_allexp, "(s) to plot all experiments.\n"))


isos = c("USA", "CHN", "DEU", "AUS", "IND", "IDN", "ZAF")
vars = c("log_E_CC", unique(tmp_tree$variable[which(tmp_tree$nodeType != "Terminal node")]))
#c("log_E_CC", "log_GDPpc", "log_pop", "log_GDPpc", "log_E_CIm", "log_GDP_Ag", "log_GDP_Ind", "log_GDP_Ser" )

tmp_data = v_dataShortProc$data %>% 
  gather(variable, value, -iso, -year) %>% 
  filter(
    iso %in% isos,
    variable %in% vars)
tmp_tree = v_tree$`testingMethods5 - E_CC-EnhancedSet - FE-MLinROLS - 1971-2012`[[1]]
tmp_alloc = v_alloc$`testingMethods5 - E_CC-EnhancedSet - FE-MLinROLS - 1971-2012`[[1]]

p = ggplot(data = tmp_data)

# 0 line
p = p + 
  geom_segment(aes(x=min(tmp_data$year), xend=max(tmp_data$year), y=0, yend=0), 
               data=NULL,
               col="black")

# Terminal nodes
p = p + geom_rect(
  aes(xmin=start, xmax=end, ymin=minval, ymax=maxval, fill=type),
  data=tmp_data %>% 
    group_by(iso, variable) %>% 
    summarise(minval = min(value, na.rm=TRUE), 
              maxval = max(value, na.rm=TRUE)) %>% 
    left_join(do.call("rbind", lapply(isos, function(x) {
      df <- getCountryPathway(tmp_alloc, x) %>% 
        mutate(iso = x) %>% 
        select(iso, start, end, node)
    })), by=c("iso")) %>% 
    group_by(iso) %>% 
    mutate(type = row_number() %% 2 == 1)
) +
  geom_label(aes(x=midpoint, y=max(maxval), label=node),
             size = 2,
             data=tmp_data %>% 
               group_by(iso, variable) %>% 
               summarise(minval = min(value, na.rm=TRUE), 
                         maxval = max(value, na.rm=TRUE)) %>% 
               left_join(do.call("rbind", lapply(isos, function(x) {
                 df <- getCountryPathway(tmp_alloc, x) %>% 
                   mutate(iso = x) %>% 
                   select(iso, start, end, node)
               })), by=c("iso")) %>% 
               group_by(iso) %>% 
               filter(variable == "log_E_CC") %>% 
               mutate(midpoint = (start+end)/2) %>% 
               mutate(type = row_number() %% 2 == 1))

# Intermediate nodes
p = p + 
  geom_segment(aes(x=ymin, xend=ymax, y=value, yend=value), 
               data=tmp_tree %>% 
                 filter(nodeType != "Terminal node", operator %in% c("<=")) %>% 
                 select(variable, value) %>% 
                 filter(variable %in% unique(tmp_tree$variable[which(tmp_tree$nodeType != "Terminal node")])) %>% 
                 mutate(ymin = min(tmp_data$year)) %>% 
                 mutate(ymax = max(tmp_data$year)),
               col="red", lty=3)
p = p +
  geom_line(aes(x=year, y=value), col="blue", lwd=1.25) +
  facet_grid(variable~iso, scales="free_y") +
  xlab("") + ylab("value") +
  scale_fill_manual(values=c("TRUE"="#99999966", "FALSE"="#eeeeee66")) +
  theme_bw()
print(p)


#==== Compare trees ==============
# TODO: Use edit distance instead? (Same for compare data function?)
t3 <- Sys.time()
# Data (country x year)
compare_trees <- function(kt1, kt2) {
  id1 = kt1
  id2 = kt2
  
  iso1  <- unique(v_alloc[[id1]]$iso)
  iso2  <- unique(v_alloc[[id2]]$iso)
  year1 <- unique(v_alloc[[id1]]$year)
  year2 <- unique(v_alloc[[id2]]$year)
  
  isoyear1 <- (v_alloc[[id1]] %>% mutate(isoyear=paste0(iso,"-",year)))$isoyear
  isoyear2 <- (v_alloc[[id2]] %>% mutate(isoyear=paste0(iso,"-",year)))$isoyear
  
  v_alloc1 <- v_alloc[[id1]] %>% mutate(isoyear=paste0(iso,"-",year)) %>% filter(isoyear %in% isoyear2) %>% select(-isoyear)
  v_alloc2 <- v_alloc[[id2]] %>% mutate(isoyear=paste0(iso,"-",year)) %>% filter(isoyear %in% isoyear1) %>% select(-isoyear)
  
  tmp <- v_tree[[id1]] %>% filter(nodeType == "Terminal node") %>% select(nodeID,n) %>% filter(nodeID %in% unique(v_alloc1$tnode)) %>% arrange(desc(n)) %>% mutate(levelsid1 = paste0(nodeID, " (", n, ")"))
  tNodeIDs1 <- tmp$nodeID
  levelsID1 <- tmp$levelsid1
  tmp <- v_tree[[id2]] %>% filter(nodeType == "Terminal node") %>% select(nodeID,n) %>% filter(nodeID %in% unique(v_alloc2$tnode)) %>% arrange(desc(n)) %>% mutate(levelsid2 = paste0(nodeID, " (", n, ")"))
  tNodeIDs2 <- tmp$nodeID
  levelsID2 <- tmp$levelsid2
  
  
  m_abs  <- matrix(NA, nrow = length(tNodeIDs1), ncol = length(tNodeIDs2))
  m_rel1 <- matrix(NA, nrow = length(tNodeIDs1), ncol = length(tNodeIDs2))
  m_rel2 <- matrix(NA, nrow = length(tNodeIDs1), ncol = length(tNodeIDs2))
  for (kr in 1:length(tNodeIDs1)) {
    id_dp1 <- which(v_alloc1$tnode == tNodeIDs1[kr])
    dp1    <- paste0(v_alloc1$iso[id_dp1], "-", v_alloc1$year[id_dp1])
    nbdp1  <- length(dp1)
    alldp2 <- c()
    for (kc in 1:length(tNodeIDs2)) {
      id_dp2 <- which(v_alloc2$tnode == tNodeIDs2[kc])
      dp2    <- paste0(v_alloc2$iso[id_dp2], "-", v_alloc2$year[id_dp2])
      alldp2 <- c(alldp2, dp2)
      nbdp2  <- length(dp2)
      
      m_abs[kr,kc]  <- length(which(dp1 %in% dp2))
      m_rel1[kr,kc] <- length(which(dp1 %in% dp2))/nbdp1
      m_rel2[kr,kc] <- length(which(dp2 %in% dp1))/nbdp2
    } 
    
    missing_dp1 <- dp1[which(!dp1 %in% alldp2)]
    if (length(missing_dp1) != 0) {
      print(tNodeIDs1[kr])
      print(tNodeIDs2[kr])
      print(missing_dp1)
      stop()
    }
    
  }
  
  process_m <- function(m, id1, id2) {
    colnames(m) <- paste(tNodeIDs2)
    m <- round(m, digits = 2) %>% 
      as.data.frame() %>% 
      mutate(TN_T1=tNodeIDs1) %>% 
      gather(TN_T2, value, -TN_T1) %>% 
      left_join(v_tree[[id1]] %>% select(nodeID,n), by=c("TN_T1"="nodeID")) %>% 
      rename(n1=n) %>% 
      mutate(TN_T1=paste0(TN_T1," (", n1, ")")) %>% 
      left_join(v_tree[[id2]] %>% select(nodeID,n), by=c("TN_T2"="nodeID")) %>% 
      rename(n2=n) %>% 
      mutate(TN_T2=paste0(TN_T2," (", n2, ")")) %>% 
      mutate(TN_T1=factor(TN_T1, levels=levelsID1 ,ordered=TRUE)) %>% 
      mutate(TN_T2=factor(TN_T2, levels=levelsID2 ,ordered=TRUE))
  }
  
  
  m_abs <- process_m(m_abs, id1, id2) %>% 
    mutate(type=ifelse(value == 0, "zero", "nonzero"))
  m_rel1 <- process_m(m_rel1, id1, id2)
  m_rel2 <- process_m(m_rel2, id1, id2)
  m_rel <- left_join(
    m_rel1 %>% 
      rename(rel1=value),
    m_rel2 %>% 
      rename(rel2=value) %>% 
      select(-n1,-n2)
  ) %>% 
    mutate(value = n1/(n1+n2)*rel1 + n2/(n1+n2)*rel2) 
  
  misalloc <- round((m_abs %>% group_by(TN_T1) %>% filter(value != max(value)) %>% summarise(sum=sum(value)) %>% ungroup() %>% summarise(sum=sum(sum)))$sum/sum(m_abs$value)*100, digits=1)
  
  ggplot(m_rel) + 
    geom_tile(aes(x=TN_T2, y=TN_T1, fill=value)) + 
    geom_text(aes(x=TN_T2, y=TN_T1, label=value, colour=type), data=m_abs) +
    scale_fill_gradient(low = "#ffffff", high="#ff0000") +
    scale_color_manual(values=c("nonzero"="#000000", zero="#eeeeee")) +
    ylab(paste0(names(u_cases)[id1], " (", length(tNodeIDs1), ")")) + xlab(paste0(names(u_cases)[id2], " (", length(tNodeIDs2), ")")) +
    ggtitle(paste0("Misallocation (%): ", misalloc))
  
  return(misalloc)
}

compare_treeStructure <- function(kt1, kt2) {
  id1 = kt1
  id2 = kt2
  
  tmp        <- v_tree[[id1]] %>% filter(nodeType == "Terminal node") %>% select(nodeID,n) %>% arrange(desc(n)) %>% mutate(levelsid1 = paste0(nodeID, " (", n, ")"))
  tNodeIDs1  <- tmp$nodeID
  levelsID1  <- tmp$levelsid1
  treepaths1 <- lapply(tNodeIDs1, function(x) {
    nodes <- getParentNodeID(x, v_tree[[id1]])[-1]
    out <- v_tree[[id1]] %>% 
      filter(nodeID %in% nodes) %>% 
      group_by(nodeID) %>% 
      filter(row_number() == 1) %>% 
      ungroup() %>% 
      select(variable, value) %>% 
      unite(col = "varval", variable, value, sep = "#")
    return(out$varval)
  })
  comb1 <- unique(unlist(treepaths1))
  
  tmp        <- v_tree[[id2]] %>% filter(nodeType == "Terminal node") %>% select(nodeID,n) %>% arrange(desc(n)) %>% mutate(levelsid2 = paste0(nodeID, " (", n, ")"))
  tNodeIDs2  <- tmp$nodeID
  levelsID2  <- tmp$levelsid2
  treepaths2 <- lapply(tNodeIDs2, function(x) {
    nodes <- getParentNodeID(x, v_tree[[id2]])[-1]
    out <- v_tree[[id2]] %>% 
      filter(nodeID %in% nodes) %>% 
      group_by(nodeID) %>% 
      filter(row_number() == 1) %>% 
      ungroup() %>% 
      select(variable, value) %>% 
      unite(col = "varval", variable, value, sep = "#")
    return(out$varval)
  })
  comb2 <- unique(unlist(treepaths2))
  
  return(length(which(comb1 %in% comb2))/length(comb1)*100)
}

m_comp  <- matrix(NA, nrow = Ncases, ncol = Ncases)
m_compS <- matrix(NA, nrow = Ncases, ncol = Ncases)

for (kt1 in 1:Ncases) {
  for (kt2 in 1:Ncases) {
    cat(paste0("Comparing T", kt1, " with T", k_t2 , "\n"))
    m_comp[kt1, kt2]  <- compare_trees(kt1, kt2)
    m_compS[kt1, kt2] <- compare_treeStructure(kt1, kt2)
  }
}

save(m_comp, file = file.path("output", testingMethodName, "data", paste0(testingMethodName, "_compData_", format(Sys.time(), "%Y-%m-%d"),".RData")))
save(m_compS, file = file.path("output", testingMethodName, "data", paste0(testingMethodName, "_compTreeStructure_", format(Sys.time(), "%Y-%m-%d"),".RData")))

tmp <- m_comp

colnames(tmp) <- paste(1:Ncases)

namenbtn <- do.call("rbind", lapply(paste(1:Ncases), function(x) data.frame(name=x, nbtn=length(unique(v_alloc[[as.numeric(x)]]$tnode)))))

tmp <- tmp %>% 
  as.data.frame() %>% 
  mutate(T1=paste(1:Ncases)) %>% 
  gather(T2, value, -T1) %>% 
  left_join(namenbtn, by=c("T1"="name")) %>% 
  mutate(T1 = paste0(T1, " (", nbtn, ")")) %>% 
  select(-nbtn) %>% 
  left_join(namenbtn, by=c("T2"="name")) %>% 
  mutate(T2 = paste0(T2, " (", nbtn, ")")) %>% 
  select(-nbtn)

ggplot(tmp) + 
  geom_tile(aes(x=T2, y=T1, fill=value)) + 
  geom_text(aes(x=T2, y=T1, label=value)) +
  scale_fill_gradient(low = "#ffffff", high="#ff0000") +
  theme(axis.text.x = element_text(angle=90))

# Tree structure
tmp <- m_compS

colnames(tmp) <- paste(1:Ncases)

namenbtn <- do.call("rbind", lapply(paste(1:Ncases), function(x) data.frame(name=x, nbtn=length(unique(v_alloc[[as.numeric(x)]]$tnode)))))

tmp <- tmp %>% 
  as.data.frame() %>% 
  mutate(T1=paste(1:Ncases)) %>% 
  gather(T2, value, -T1) %>% 
  left_join(namenbtn, by=c("T1"="name")) %>% 
  mutate(T1 = paste0(T1, " (", nbtn, ")")) %>% 
  select(-nbtn) %>% 
  left_join(namenbtn, by=c("T2"="name")) %>% 
  mutate(T2 = paste0(T2, " (", nbtn, ")")) %>% 
  select(-nbtn)

ggplot(tmp %>% mutate(fmt_val = format(value, digits = 2))) + 
  geom_tile(aes(x=T2, y=T1, fill=value)) + 
  geom_text(aes(x=T2, y=T1, label=fmt_val)) +
  scale_fill_gradient(low = "#ffffff", high="#ff0000") +
  theme(axis.text.x = element_text(angle=90))

comp_allexp <- Sys.time() - t3
cat(paste0("It took ",  comp_allexp, "(s) to compare all experiments.\n"))