### Notes on Political Economy - MADE (2024)------------------------------------

## Packages---------------------------------------------------------------------

#install.packages("tidyverse")
library(tidyverse)

#install.packages("haven")
library(haven)

#install.packages("data.table")
library(data.table)

#install.packages("dplyr")
library(dplyr)

#install.packages("modi")
library(modi)

#install.packages("acid")
library(acid)

#install.packages("dineq")
library(dineq)

## Setting work directory-------------------------------------------------------

file_directory <- dirname(rstudioapi::getSourceEditorContext()$path)%>%
  gsub("\\Inequality analysis", "", .)

setwd(file_directory)

getwd()

## Opening databases from Brasmod-----------------------------------------------

## Reading files for households-------------------------------------------------

# Setting folder path

folder_path <- "Databases//"


# Opening Brasmod databases for year of reference

for(year in 2008:2022){
  
  file_path <- paste0(folder_path, "bra_", year, "_std_hh.txt")
  
  if (file.exists(file_path)) {
    
    df <- fread(file_path, sep = "\t", dec = ",")
    
    assign(paste0("bra_hh_", year), df)
    
    cat("File", file_path, "read successfully.\n")
    
  } else {
    
    cat("File", file_path, "not found.\n")
  }
}



# Opening Brasmod databases for simulations

sim_bra_hh_2020 <- fread("C:/Users/lauro/Documents/GitHub/NPE/Databases/bra_2020_sim_std_hh.txt", sep = "\t", dec = ",")
rbu_bra_hh_2020 <- fread("C:/Users/lauro/Documents/GitHub/NPE/Databases/bra_2020_rbu_std_hh.txt", sep = "\t", dec = ",")


### Analysis--------------------------------------------------------------------

## Gini-------------------------------------------------------------------------

# For disposable income
# Using dineq package

gini_list_dispy <- c()

for(year in 2008:2022){
  name_dispy <- paste0("gini_dispy_", as.character(year))
  
  
  df_dispy <- get(paste0("bra_hh_", as.character(year)))
  
  assign(name_dispy, 
         gini.wtd(df_dispy$ils_dispy, df_dispy$dwt))
  
  gini_list_dispy <- c(gini_list_dispy, get(name_dispy))
  
}

print(gini_list_dispy)

# For original income

gini_list_origy <- c()

for(year in 2008:2022){
  name_origy <- paste0("gini_origy_", as.character(year))
  
  
  df_origy <- get(paste0("bra_hh_", as.character(year)))
  
  assign(name_origy, 
         gini.wtd(df_origy$ils_origy, df_origy$dwt))
  
  gini_list_origy <- c(gini_list_origy, get(name_origy))
  
}

print(gini_list_origy)



years <- 2008:2022

color_list <- c("#FF7276", "#3366ff", "#feff41", "#45ff66")

income_concepts <- c("dispy", "origy")

plot <- ggplot()

for(income in income_concepts){
  
  df <- data.frame(year = 2008:2022,
                   gini = get(paste0("gini_list_", income)),
                   income_concept = case_when(
                     income == "dispy" ~ "Disposable income",
                     income == "origy" ~ "Original income"
                   ))
  
  
  plot <- plot +
    geom_line(data = df,
              aes(x = year, y = gini, color = income_concept)) +
    geom_point(data = df,
               aes(x = year, y = gini, color = income_concept))
  
  if(income == tail(income_concepts, 1)) {
    plot_final <- plot +
      scale_y_continuous(limits = c(0.25, 0.75))+
      scale_x_continuous(limits = c(2008, 2022),
                         breaks = seq(2008, 2022, 1)) +
      scale_color_manual(values = color_list) + 
      labs(title = "Gini index",
           x = "Year", 
           y = "Gini",
           color = "Income Type") +
      theme_bw()
    
    
    print(plot_final)
  }
  
}

# Contrafactual scenarios

# For disposable income
# Using dineq package

gini_dispy_2020 <- gini.wtd(sim_bra_hh_2020$ils_dispy, sim_bra_hh_2020$dwt)

gini_list_dispy <- c()

for(year in 2008:2022){
  name_dispy <- paste0("gini_dispy_", as.character(year))
  
  
  df_dispy <- get(paste0("bra_hh_", as.character(year)))
  
  assign(name_dispy, 
         gini.wtd(df_dispy$ils_dispy, df_dispy$dwt))
  
  gini_list_dispy <- c(gini_list_dispy, get(name_dispy))
  
}

index_2020 <- 2020 - 2008 + 1

# Assuming you have already executed the loop to create gini_list_dispy
# Update the value at the index corresponding to the year 2020
gini_list_dispy[index_2020] <- gini.wtd(sim_bra_hh_2020$ils_dispy, sim_bra_hh_2020$dwt)

print(gini_list_dispy)

# For original income

gini_list_origy <- c()

for(year in 2008:2022){
  name_origy <- paste0("gini_origy_", as.character(year))
  
  
  df_origy <- get(paste0("bra_hh_", as.character(year)))
  
  assign(name_origy, 
         gini.wtd(df_origy$ils_origy, df_origy$dwt))
  
  gini_list_origy <- c(gini_list_origy, get(name_origy))
  
}

index_2020 <- 2020 - 2008 + 1

# Assuming you have already executed the loop to create gini_list_dispy
# Update the value at the index corresponding to the year 2020
gini_list_origy[index_2020] <- gini.wtd(sim_bra_hh_2020$ils_origy, sim_bra_hh_2020$dwt)

print(gini_list_origy)



print(gini_list_origy)






gini_list_origy <- c()

for(year in 2008:2022){
  name_origy <- paste0("gini_origy_", as.character(year))
  
  
  df_origy <- get(paste0("bra_hh_", as.character(year)))
  
  assign(name_origy, 
         gini.wtd(df_origy$ils_origy, df_origy$dwt))
  
  gini_list_origy <- c(gini_list_origy, get(name_origy))
  
}

print(gini_list_origy)


years <- 2008:2022

color_list <- c("#FF7276", "#3366ff", "#feff41", "#45ff66")

income_concepts <- c("dispy", "origy")

plot <- ggplot()

for(income in income_concepts){
  
  df <- data.frame(year = 2008:2022,
                   gini = get(paste0("gini_list_", income)),
                   income_concept = case_when(
                     income == "dispy" ~ "Disposable income",
                     income == "origy" ~ "Original income"
                   ))
  
  
  plot <- plot +
    geom_line(data = df,
              aes(x = year, y = gini, color = income_concept)) +
    geom_point(data = df,
               aes(x = year, y = gini, color = income_concept))
  
  if(income == tail(income_concepts, 1)) {
    plot_final <- plot +
      scale_y_continuous(limits = c(0.25, 0.75))+
      scale_x_continuous(limits = c(2008, 2022),
                         breaks = seq(2008, 2022, 1)) +
      scale_color_manual(values = color_list) + 
      labs(title = "Gini index",
           x = "Year", 
           y = "Gini",
           color = "Income Type") +
      theme_bw()
    
    
    print(plot_final)
  }
  
}

