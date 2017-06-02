u_mode   = "R"

source("scripts/user_settings.R")
source("scripts/init.R")
source("scripts/process_data.R")

testingMethodName = "testingMethod5"
Ncases   = 500
u_period = list(
  "LongTerm" = c(1971, 2012))
u_selectedDepVars = c("E_CC")
u_selectedIndepVars = list(
  "EnhancedSet" = c("E_CIm", "E_CEx", "E_CP", 
                    "GDPpc", "P", "P_Ndep", 
                    "URB", "LEx", "EE", 
                    "GDP_Ag", "GDP_Ind", "GDP_Ser", "GDP_Tra"))
u_treeModels = c("FE-MLinROLS")
# 80 countries
u_iso = c("CHN","USA", "DEU", "IND", "JPN", "POL", "GBR", "ZAF", "FRA", "KOR", "TUR", "CAN", "AUS", "BEL", "ITA", "BRA", "ESP", "IDN",
          "VNM", "THA", "HUN", "ROU", "AUT", "BGR", "NLD", "PAK", "MEX", "COL", "SWE", "FIN", "ZWE", "NOR", "NZL", "DNK", "GRC", "PHL", 
          "MYS", "CHL", "CHE", "PRT", "EGY", "PER", "IRN", "ALB", "HKG", "ARG", "BGD", "MAR", "ZMB", "ARE", "DZA", "MMR", "NPL", "VEN",
          "COD", "KEN", "MOZ", "LBN", "DOM", "NGA", "HND", "TUN", "SEN", "PAN", "JAM", "CYP", "LKA", "ETH", "MUS", "JOR", "CRI", "ISR", 
          "SGP", "URY", "TZA", "BOL", "HTI", "SYR", "GTM", "SLV")
# Clark2012 countries (66)
u_iso_clark2012 = c("AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA", "DEU", "ISL", "IRL", "ISR", "ITA", "JPN", "LUX", "NLD", "NZL", "NOR", "PRT", "ESP", "SWE", "CHE", "GBR", "USA",
                    "ALB", "DZA", "ARG", "BGD", "BWA", "BRA", "CHL", "CHN", "COL", "CRI", "CYP", "DOM", "EGY", "GRC", "HND", "HUN", "IND", "IDN", "IRN", "JAM", "KEN", "KOR", "MKD",
                    "MYS", "MEX", "MNG", "MAR", "NPL", "PAK", "PER", "PHL", "POL", "ROU", "ZAF", "LKA", "TZA", "THA", "TUR", "URY", "VEN", "VNM", "ZMB", "ZWE")
defineCases  <- function(projName, selectedDepVars, selectedIndepVars, period, treemod, SENSITIVITY_ANALYSIS=FALSE) {
  
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
  
  getCountries <- function(startYear, selectedVars, yearcumsum=2010){
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
    
    return(tmp$iso)
    
  } 
  
  
  out <- list()
  
  for (kp in names(period)) {
    for (kdv in selectedDepVars) {
      for (kiv in names(selectedIndepVars)) {
        for (ktm in treemod) {
          if (ktm == "Std-MLinROLS") {
            tmp_demean    <- FALSE
            tmp_firstdiff <- FALSE
          }
          if (ktm == "FE-MLinROLS") {
            tmp_demean    <- TRUE
            tmp_firstdiff <- FALSE
          }
          if (ktm == "FD-MLinROLS") {
            tmp_demean    <- FALSE
            tmp_firstdiff <- TRUE
          }
          
          if (kiv != "Clark2012") {
            tmp_iso <- u_iso #getCountries(period[[kp]][1], c(kdv, selectedIndepVars[[kiv]]))
          } else {
            tmp_iso <- u_iso_clark2012
          }
          
          print(paste0(projName, " - ", kdv, "-", kiv, " - ", ktm, " - ", period[[kp]][1], "-", period[[kp]][2]))
          
          out[[paste0(projName, " - ", kdv, "-", kiv, " - ", ktm, " - ", period[[kp]][1], "-", period[[kp]][2])]] <- 
            list(
              vardef    = generate_variableDefinition(v_dataShort, d=kdv, n=selectedIndepVars[[kiv]][which(selectedIndepVars[[kiv]] != kdv)]),
              period    = period[[kp]],
              iso       = tmp_iso,
              dvar      = kdv,
              demean    = tmp_demean,
              firstdiff = tmp_firstdiff,
              treetype  = "Single tree > LMS - Multilinear",
              maxsplits = 6, minnbnodes = 3,
              cex       = 0.3, legend.cex = 1.5
            )
          
          if (SENSITIVITY_ANALYSIS) {
            for (ky in 0:2) { # Start year
              for (kd in c(5,6,7)) {
                for (kn in c(3,10)) {
                  if (ky != 0 && kd != 6 && ky != 3){
                    print(paste0(projName, " - ", kdv, "-", kiv, " - ", ktm, " - ", period[[kp]][1]+ky, "-", period[[kp]][2], " - md", kd, "-mn", kn))
                    out[[paste0(projName, " - ", kdv, "-", kiv, " - ", ktm, " - ", period[[kp]][1]+ky, "-", period[[kp]][2], " - md", kd, "-mn", kn)]] <- 
                      list(
                        vardef    = generate_variableDefinition(v_dataShort, d=kdv, n=selectedIndepVars[[kiv]][which(selectedIndepVars[[kiv]] != kdv)]),
                        period    = c(period[[kp]][1]+ky, period[[kp]][2]),
                        iso       = tmp_iso,
                        dvar      = kdv,
                        demean    = tmp_demean,
                        firstdiff = tmp_firstdiff,
                        treetype  = "Single tree > LMS - Multilinear",
                        maxsplits = kd, minnbnodes = kn,
                        cex       = 0.3, legend.cex = 1.5
                      )
                  }
                }
              }
            }
          }
        }
      }
    }
    
  }
  return(out)
  
}
u_cases <- defineCases(testingMethodName, u_selectedDepVars,u_selectedIndepVars,u_period,u_treeModels, SENSITIVITY_ANALYSIS=FALSE)

#==== Initialise ========================
v_tree  <- list()


#==== Run experiments ===================
for (k_case in 1:Ncases) {
  k_caseName <- ifelse(nchar(k_case) < nchar(Ncases), paste0(paste(rep("0", nchar(Ncases)-nchar(k_case)+1), collapse=""), k_case), paste0("0", k_case))
  cat(paste0("Experiment ", k_caseName , "\n"))
  
  tryCatch(
    v_tree[[k_case]]  <- parse_output(file.path(outPath, "GUIDEfile_out.txt")),
    error = function(e) {
      print("Could not parse output file. Skipping...")
    }
  ) 
}

#==== Plot results ==============
t2 <- Sys.time()
dir.create(file.path("output", testingMethodName, "Plots Trees"))
dir.create(file.path("output", testingMethodName, "Plots country pathways"))
dir.create(file.path("output", testingMethodName, "Pred Plots"))

for (k_case in 1:Ncases) {
  
  k_caseName <- ifelse(nchar(k_case) < nchar(Ncases), paste0(paste0(rep("0", nchar(Ncases)-nchar(k_case)+1), collapse=""), k_case), paste0("0", k_case))
  
  cat(paste0("Plotting Experiment ", k_caseName , "\n"))
  
  if (!is.null(v_tree[[k_case]])) {
    # Tree
    tree <- as.fake.rpart.tree(v_tree[[k_case]])
    if (dim(tree$frame)[1] != 0) {
      # plot_tree(tree, 
      #           TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_case]]$nodeType == "Terminal node")), "\n#Countries: ",length(unique(v_data[[k_case]]$data$iso)), " - period: ",min(v_data[[k_case]]$data$year),"-",max(v_data[[k_case]]$data$year)), 
      #           CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2,
      #           FILENAME = file.path("output", testingMethodName, "Plots Trees/", paste0(k_caseName, ".pdf")))
      plot_tree(tree, 
                TITLE=paste0("RT - ", k_case, "\nLMS-Multilinear - Tn:", length(which(v_tree[[k_case]]$nodeType == "Terminal node"))), 
                CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2,
                FILENAME = file.path("output", testingMethodName, "Plots Trees/", paste0(k_caseName, ".png")))
    }
  }
}
plot_allexp <- Sys.time() - t2
cat(paste0("It took ",  plot_allexp, "(s) to plot all experiments.\n"))


#==== Compare trees ==============
t3 <- Sys.time()

# Data (country x year)
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
m_compS  <- matrix(NA, nrow = Ncases, ncol = Ncases)
for (kt1 in 1:Ncases) {
  for (kt2 in 1:Ncases) {
    cat(paste0("Comparing T", kt1, " with T", k_t2 , "\n"))
    m_compS[kt1, kt2] <- compare_treeStructure(kt1, kt2)
  }
}

save(m_compS, file = file.path("output", testingMethodName, "data", paste0(testingMethodName, "_compTreeStructure_", format(Sys.time(), "%Y-%m-%d"),".RData")))

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

ggplot(tmp %>% 
         filter(T1 < 100, T2 < 100) %>% 
         mutate(fmt_val = format(value, digits = 2))) + 
  geom_tile(aes(x=T2, y=T1, fill=value)) + 
  geom_text(aes(x=T2, y=T1, label=fmt_val)) +
  scale_fill_gradient(low = "#ffffff", high="#ff0000") +
  theme(axis.text.x = element_text(angle=90))

comp_allexp <- Sys.time() - t3
cat(paste0("It took ",  comp_allexp, "(s) to compare all experiments.\n"))