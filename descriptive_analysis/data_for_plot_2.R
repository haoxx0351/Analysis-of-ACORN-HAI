# Clean
rm(list = ls())

# Load package
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(readxl,
                 magrittr, 
                 dplyr,
                 tidyr,
                 stringr)
})

## Define working directory
wd <- "../"
setwd(wd)

## Load data
# AST 
df <- readRDS("data/clean_data_RData/all_each_ast.RData")
ast <- readRDS("data/clean_data_RData/ast_all_index.RData")

# Anti group
anti <- read_excel("data/anti_group/anti_group_used.xlsx")
org_anti_class <- read_excel("data/anti_group/organism_group.xlsx") %>% 
  as.data.frame()

#####
# Delete NA in org_new and infection types
ast[ast == ""] <- NA
na_rows <- which(is.na(ast$org_new) | is.na(ast$infection_types))

# AST all
ast_used <- ast[-na_rows,]

# AST each
df_del <- list()

system.time({
  df <- lapply(df, function(df) {
    df <- df %>% filter(redcap_repeat_instance == 1)
    df[-na_rows,]
  })
  
  df_del <- df
})

###
ris_col <- grep("^ris_", names(ast), value = TRUE)
#
count_list <- list()
system.time({
  for (i in seq_along(df_del)) {
    count_list[[i]] <- df_del[[i]] %>%
      group_by(!!sym(ris_col[i]), org_new, infection_types) %>%
      summarise(across(.cols = all_of(names(.)[names(.) %in% anti$anti_names]), ~sum(. == 1), .names = "{.col}_count"), 
                total = sum(c_across(all_of(names(.)[names(.) %in% anti$anti_names]))),
                across(all_of(names(.)[names(.) %in% anti$anti_names]), ~sum(.)/total, .names = "{.col}_percentage"),
                .groups = "drop")
  }
})
#
count_list_new <- list()
pie_result <- data.frame()

system.time({
  for (i in seq_along(count_list)) {
    
    anti_group <- str_remove(names(count_list[[i]])[1], "ris_")
    
    count_list_new[[i]] <- count_list[[i]] %>% 
      mutate(anti_group = anti_group) %>%
      select("anti_group", starts_with("ris_"), 
             "org_new", "infection_types", ends_with("_percentage")) %>%
      rename_all(~gsub("_percentage", "", .))
      
    colnames(count_list_new[[i]])[2] <- "ris"
    
    pie_result <- bind_rows(pie_result, count_list_new[[i]])
  }
})
#
str(pie_result)
for (i in c(2:4)) {pie_result[,i] = as.factor(pie_result[,i])}

# Set labels
pie_result <- within(pie_result, {c(
  org_new <- factor(org_new,
                    levels = c(1:5, 9:11, 13, 14),
                    labels = c("E.coli", "Klebsiella pneumoniae",
                               "Acinetobacter", "Pseudomonas", 
                               "Serratia/Proteus/Morganella/Enterobacter",
                               "Stenotrophomonas", "Staphylococcus aureus",
                               "Enterococcus", "Candida", "Others")),
  ris <- factor(ris,
                levels = c(5:1),
                labels = c("NA", "Unknown (U)", "Susceptible (S)",
                           "Susceptible (I)", "Resistance (R)"))
  
)})
#
#####
# To long
count_list_long <- df_list_long <- list()

system.time({
  for (i in seq_along(count_list)) {
    df_used <- count_list[[i]]
    
    anti_group <- str_remove(names(df_used)[1], "ris_")
    
    count_list_long[[i]] <- df_used %>%
      pivot_longer(cols = matches("_percentage"),
                   names_to = "antibiotic",
                   values_to = "percentage") %>%
      mutate(antibiotic = str_remove(antibiotic, "_percentage"),
             anti_group = anti_group) 
    
    df_list_long[[i]] <- count_list_long[[i]] %>% 
      select("anti_group", starts_with("ris_"), "org_new",
             "infection_types", "antibiotic", "percentage")
    
    colnames(df_list_long[[i]])[2] <- "ris"
  }
})
#
list_long <- do.call(rbind, df_list_long) %>% 
  as.data.frame()
for (i in 1:5) {list_long[,i] = as.factor(list_long[,i])}
# Set labels
list_long <- within(list_long, {c(
  org_new <- factor(org_new,
                    levels = c(1:5, 9:11, 13, 14),
                    labels = c("E.coli", "Klebsiella pneumoniae",
                               "Acinetobacter", "Pseudomonas", 
                               "Serratia/Proteus/Morganella/Enterobacter",
                               "Stenotrophomonas", "Staphylococcus aureus",
                               "Enterococcus", "Candida", "Others")),
  
  ris <- factor(ris,
                levels = c(5:1),
                labels = c("NA", "Unknown (U)", "Susceptible (S)",
                           "Susceptible (I)", "Resistance (R)"))
  
)})
#
# Add organism_group
colnames(org_anti_class)[2] <- "org_new"
pie_result <- inner_join(org_anti_class,
                         pie_result, 
                         by = c("org_new", "anti_group"))

list_long <- inner_join(list_long,
                        org_anti_class, 
                        by = c("org_new", "anti_group"))

# Save
saveRDS(pie_result, "data/clean_data_RData/pie_specific_anti_ast.RData")
saveRDS(list_long, "data/clean_data_RData/bar_specific_anti_ast.RData")
###