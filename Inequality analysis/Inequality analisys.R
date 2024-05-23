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

# Setting folder path

folder_path <- "Databases//"

## Households' databases--------------------------------------------------------
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

## Individuals' databases-------------------------------------------------------

for(year in 2008:2022){
  
  file_path <- paste0(folder_path, "bra_", year, "_std.txt")
  
  if (file.exists(file_path)) {
    
    df <- fread(file_path, sep = "\t", dec = ",")
    
    assign(paste0("bra_hh_", year), df)
    
    cat("File", file_path, "read successfully.\n")
    
  } else {
    
    cat("File", file_path, "not found.\n")
  }
}



# Simulations' databases--------------------------------------------------------

# For households
sim_bra_hh_2020 <- fread("C:/Users/lauro/Documents/GitHub/NPE/Databases/bra_2020_sim_std_hh.txt", 
                         sep = "\t", dec = ",")
sim_bra_ind_2020 <- fread("C:/Users/lauro/Documents/GitHub/NPE/Databases/bra_2020_sim_std.txt", 
                         sep = "\t", dec = ",")

# For individuals
rbu_bra_hh_2020 <- fread("C:/Users/lauro/Documents/GitHub/NPE/Databases/bra_2020_rbu_std_hh.txt", 
                         sep = "\t", dec = ",")
rbu_bra_ind_2020 <- fread("C:/Users/lauro/Documents/GitHub/NPE/Databases/bra_2020_rbu_std.txt", 
                         sep = "\t", dec = ",")

bra_ind_2020 <- fread("C:/Users/lauro/Documents/GitHub/NPE/Databases/bra_2020_std.txt", 
                      sep = "\t", dec = ",")

bra_hh_2020 <- fread("C:/Users/lauro/Documents/GitHub/NPE/Databases/bra_2020_std_hh.txt", 
                     sep = "\t", dec = ",")


### Analysis--------------------------------------------------------------------

## Gini - scenario 1------------------------------------------------------------

# Creating list for disposable income

gini_list_dispy <- c()

for(year in 2008:2022){
  name_dispy <- paste0("gini_dispy_", as.character(year))
  
  
  df_dispy <- get(paste0("bra_hh_", as.character(year)))
  
  assign(name_dispy, 
         gini.wtd(df_dispy$ils_dispy, df_dispy$dwt))
  
  gini_list_dispy <- c(gini_list_dispy, get(name_dispy))
  
}

print(gini_list_dispy)

# Creating list for original income

gini_list_origy <- c()

for(year in 2008:2022){
  name_origy <- paste0("gini_origy_", as.character(year))
  
  
  df_origy <- get(paste0("bra_hh_", as.character(year)))
  
  assign(name_origy, 
         gini.wtd(df_origy$ils_origy, df_origy$dwt))
  
  gini_list_origy <- c(gini_list_origy, get(name_origy))
  
}

print(gini_list_origy)

## Graphing disposable and original income--------------------------------------

years <- 2008:2022

color_list <- c("#FF7276", "#3366ff", "#feff41", "#45ff66")

income_concepts <- c("dispy", "origy")

plot <- ggplot()

for(income in income_concepts){
  
  df <- data.frame(year = 2008:2022,
                   gini = get(paste0("gini_list_", income)),
                   income_concept = case_when(
                     income == "dispy" ~ "Renda disponível",
                     income == "origy" ~ "Renda original"
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
      labs(title = "Cenário 1: Índice de Gini com Auxílio Emergencial",
           x = "Ano", 
           y = "Gini",
           color = "Conceito de renda") +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5)  # Center the title
      )
    
    print(plot_final)
  }
  
}

## Contrafactual scenarios------------------------------------------------------
## Gini - scenario 2------------------------------------------------------------

# Creating list for Disposable income, replacing for contrafactual data for 2020

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

# Creating list for original income

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



gini_list_origy <- c()

for(year in 2008:2022){
  name_origy <- paste0("gini_origy_", as.character(year))
  
  
  df_origy <- get(paste0("bra_hh_", as.character(year)))
  
  assign(name_origy, 
         gini.wtd(df_origy$ils_origy, df_origy$dwt))
  
  gini_list_origy <- c(gini_list_origy, get(name_origy))
  
}

print(gini_list_origy)

## Plotting graph---------------------------------------------------------------


years <- 2008:2022

color_list <- c("#FF7276", "#3366ff", "#feff41", "#45ff66")

income_concepts <- c("dispy", "origy")

plot <- ggplot()

for(income in income_concepts){
  
  df <- data.frame(year = 2008:2022,
                   gini = get(paste0("gini_list_", income)),
                   income_concept = case_when(
                     income == "dispy" ~ "Renda disponível",
                     income == "origy" ~ "Renda original"
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
      labs(title = "Cenário 2: Índice de Gini (sem Auxílio Emergencial)",
           x = "Ano", 
           y = "Gini",
           color = "Conceito de renda") +
      theme_bw()+
      theme(
        plot.title = element_text(hjust = 0.5)  # Center the title
      )
    
    
    print(plot_final)
  }
  
}


## Gini - scenario 3------------------------------------------------------------

# Creating list for Disposable income, replacing for contrafactual data for 2020

gini_dispy_2020 <- gini.wtd(rbu_bra_hh_2020$ils_dispy, rbu_bra_hh_2020$dwt)

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

gini_list_dispy[index_2020] <- gini.wtd(rbu_bra_hh_2020$ils_dispy, rbu_bra_hh_2020$dwt)

print(gini_list_dispy)

# Creating list for original income

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

gini_list_origy[index_2020] <- gini.wtd(rbu_bra_hh_2020$ils_origy, rbu_bra_hh_2020$dwt)

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

## Plotting graph---------------------------------------------------------------


years <- 2008:2022

color_list <- c("#FF7276", "#3366ff", "#feff41", "#45ff66")

income_concepts <- c("dispy", "origy")

plot <- ggplot()

for(income in income_concepts){
  
  df <- data.frame(year = 2008:2022,
                   gini = get(paste0("gini_list_", income)),
                   income_concept = case_when(
                     income == "dispy" ~ "Renda disponível",
                     income == "origy" ~ "Renda original"
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
      labs(title = "Cenário 3: Índice de Gini (com RBU)",
           x = "Ano", 
           y = "Gini",
           color = "Conceito de renda") +
      theme_bw()+
      theme(
        plot.title = element_text(hjust = 0.5)  # Center the title
      )
    
    
    print(plot_final)
  }
  
}

print(gini_list_dispy)


## Race and gender analysis-----------------------------------------------------

## Income shares (general)------------------------------------------------------

# Generating list with min and max of each decile + 1%

decis <- seq(0, 0.9, by = 0.1) %>%
  sapply(function(quantil) {
    weighted.quantile(bra_hh_2020$ils_dispy, bra_hh_2020$dwt, prob = quantil)
  }) %>%
  `-`(sapply(seq(0.1, 0.9, by = 0.1), function(quantil) {
    weighted.quantile(bra_hh_2020$ils_dispy, bra_hh_2020$dwt, prob = quantil)
  })) %>%
  c(max(bra_hh_2020$ils_dispy)) %>%
  as.list() %>%
  lapply(function(decil) {
    strsplit(decil, "-")
  }) %>%
  lapply(function(elemento) {
    as.numeric(elemento[[1]])
  })



decis <- lapply(lapply(as.list(c(paste0(sapply(seq(from = 0, to = .9, by = .1), 
                                               function(quantil) weighted.quantile(bra_hh_2020$ils_dispy,
                                                                                   bra_hh_2020$dwt,
                                                                                   prob = quantil)),
                                        "-",
                                        c(sapply(seq(0.1,.9,.1), 
                                                 function(quantil) weighted.quantile(bra_hh_2020$ils_dispy,
                                                                                     bra_hh_2020$dwt,
                                                                                     prob = quantil)),
                                          max(bra_hh_2020$ils_dispy))))),
                       function(decil) strsplit(decil, "-")), 
                function(elemento) as.numeric(elemento[[1]]))


# Chat gpt's version
decis_chat_gpt <- map(seq(from = 0, to = 0.9, by = 0.1), ~{
  quantil <- .x
  decil <- weighted.quantile(bra_hh_2020$ils_dispy, bra_hh_2020$dwt, prob = quantil) -
    max(bra_hh_2020$ils_dispy)
  decil
}) %>%
  map(~str_split(., "-")[[1]])




# Naming deciles
names(decis) <- paste0(seq(0,.9,.1), "-", seq(.1,1,.1))


# Calculating income appropriation

apropriacao <- data.frame()

for(dec in decis) {
  
  apropriacao <- rbind(apropriacao,
                       data.frame(Decil = paste(dec, collapse = "0"),
                                  Apro = sum(bra_hh_2020$ils_dispy[bra_hh_2020$ils_dispy >= dec[1] &
                                                                     bra_hh_2020$ils_dispy <= dec[2]]*
                                               bra_hh_2020$dwt[bra_hh_2020$ils_dispy >= dec[1] &
                                                                 bra_hh_2020$ils_dispy <= dec[2]])/
                                    sum(bra_hh_2020$dwt*bra_hh_2020$ils_dispy)))
  
}

apropriacao$Decil <- names(decis)

#Apropriação ils_origy

decis_origy <- lapply(lapply(as.list(c(paste0(sapply(seq(from = 0, to = .9, by = .1), 
                                                     function(quantil) weighted.quantile(bra_hh_2020$ils_origy,
                                                                                         bra_hh_2020$dwt,
                                                                                         prob = quantil)),
                                              "-",
                                              c(sapply(seq(0.1,.9,.1), 
                                                       function(quantil) weighted.quantile(bra_hh_2020$ils_origy,
                                                                                           bra_hh_2020$dwt,
                                                                                           prob = quantil)),
                                                max(bra_hh_2020$ils_origy))))),
                             function(decil) strsplit(decil, "-")), 
                      function(elemento) as.numeric(elemento[[1]]))


# Naming deciles
names(decis_origy) <- paste0(seq(0,.9,.1), "-", seq(.1,1,.1))


# Calculating income appropriation

apropriacao_origy <- data.frame()

for(dec in decis_origy) {
  
  apropriacao_origy <- rbind(apropriacao_origy,
                             data.frame(Decil = paste(dec, collapse = "0"),
                                        Apro = sum(bra_hh_2020$ils_origy[bra_hh_2020$ils_origy >= dec[1] &
                                                                           bra_hh_2020$ils_origy <= dec[2]]*
                                                     bra_hh_2020$dwt[bra_hh_2020$ils_origy >= dec[1] &
                                                                       bra_hh_2020$ils_origy <= dec[2]])/
                                          sum(bra_hh_2020$dwt*bra_hh_2020$ils_origy)))
  
}

apropriacao_origy$Decil <- names(decis_origy)

# Plotting graph
barplot(apropriacao_origy$Apro, names.arg = apropriacao_origy$Decil, 
        xlab = "Decil", ylab = "Income Appropriation (origy)", 
        col = "blue", main = "Income Appropriation by Deciles (origy)")


# Correção por dados do IRPF? Corrigir input com dados de IRPF?

## Average income per decile (general)------------------------------------------

rendimento.med <- data.frame()

for(dec in decis) {
  
  rendimento.med <- rbind(rendimento.med,
                          data.frame(Decil = paste(dec, collapse = "-"),
                                     RendMed = weighted.mean(x = bra_hh_2020$ils_dispy[bra_hh_2020$ils_dispy >= dec[1] &
                                                                                         bra_hh_2020$ils_dispy <= dec[2]],
                                                             w = bra_hh_2020$dwt[bra_hh_2020$ils_dispy >= dec[1] &
                                                                                   bra_hh_2020$ils_dispy <= dec[2]], na.rm = T)))
  
}

# Naming
rendimento.med$Decil <- names(decis)

# Plotting graph
barplot(rendimento.med$RendMed, names.arg = rendimento.med$Decil, xlab = "Decil", ylab = "Average Income", col = "red", main = "Average Income by Deciles")


# Generating list with min and max of quantiles 90%, 99% e 99,9%
decis.topo <- lapply(lapply(as.list(c(paste0(sapply(c(.9, .99, .999), 
                                                    function(quantil) weighted.quantile(x = bra_hh_2020$ils_dispy,
                                                                                        w = bra_hh_2020$dwt,
                                                                                        prob = quantil)),
                                             "-",
                                             rep(max(bra_hh_2020$ils_dispy),3)))),
                            function(decil) strsplit(decil, "-")), 
                     function(elemento) as.numeric(elemento[[1]]))

# Naming quantiles
names(decis.topo) <- c("10%", "1%", "0,1%")

## Average income per decile (by groups of interest)----------------------------

# Vector to identify variable "dgn"
genero <- c("Homem" = 1, "Mulher" = 2)

# Vector to identify variable "dra"
raca <- list("B" = c(1), "N" = c(2,4))


apropriacao_gen_race <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis_origy) {
      
      apropriacao_gen_race <- rbind(apropriacao_gen_race,
                                    data.frame(Genero = gen,
                                               Raca = paste(rc, collapse = "-"),
                                               Decil = paste(dec, collapse = "-"),
                                               Apro = sum(bra_ind_2020$ils_origy[bra_ind_2020$ils_origy >= dec[1] &
                                                                                   bra_ind_2020$ils_origy <= dec[2] &
                                                                                   bra_ind_2020$dgn == gen & 
                                                                                   (bra_ind_2020$dra %in% rc)]*
                                                            bra_ind_2020$dwt[bra_ind_2020$ils_origy >= dec[1] &
                                                                               bra_ind_2020$ils_origy <= dec[2] &
                                                                               bra_ind_2020$dgn == gen & 
                                                                               (bra_ind_2020$dra %in% rc)])/
                                                 sum(bra_ind_2020$dwt*bra_ind_2020$ils_origy)))
      
    }
    
  }
  
}

# Naming columns
apropriacao_gen_race$Genero <- c(rep("H",nrow(apropriacao_gen_race)/2), rep("M",nrow(apropriacao_gen_race)/2))
apropriacao_gen_race$Raca <- c(rep(c(rep("B",(nrow(apropriacao_gen_race)/4)), rep("N",(nrow(apropriacao_gen_race)/4))),2))
apropriacao_gen_race$Decil <- names(decis_origy)
apropriacao_gen_race$Grupo <- paste0(apropriacao_gen_race$Genero,"-", apropriacao_gen_race$Raca)


## Average income by decile by group--------------------------------------------

rendimento_gen_race <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis_origy) {
      
      rendimento_gen_race <- rbind(rendimento_gen_race,
                                   data.frame(Genero = gen,
                                              Raca = paste(rc, collapse = "-"),
                                              Decil = paste(dec, collapse = "-"),
                                              RendMed = weighted.mean(x = bra_ind_2020$ils_origy[bra_ind_2020$ils_origy >= dec[1] &
                                                                                                   bra_ind_2020$ils_origy <= dec[2] &
                                                                                                   bra_ind_2020$dgn == gen & 
                                                                                                   (bra_ind_2020$dra %in% rc)],
                                                                      w = bra_ind_2020$dwt[bra_ind_2020$ils_origy >= dec[1] &
                                                                                             bra_ind_2020$ils_origy <= dec[2] &
                                                                                             bra_ind_2020$dgn == gen & 
                                                                                             (bra_ind_2020$dra %in% rc)], na.rm = T)))
      
      
    }
    
  }
  
}

# Atributir o nome
rendimento_gen_race$Genero <- c(rep("H",20), rep("M",20))
rendimento_gen_race$Raca <- c(rep(c(rep("B",10), rep("N",10)),2))
rendimento_gen_race$Decil <- names(decis_origy)
rendimento_gen_race$Grupo <- paste0(rendimento_gen_race$Genero,"-", rendimento_gen_race$Raca)


## Participation by group by decile---------------------------------------------

participacao_gen_race <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis_origy) {
      
      participacao_gen_race <- rbind(participacao_gen_race,
                                     data.frame(Genero = gen,
                                                Raca = paste(rc, collapse = "-"),
                                                Decil = paste(dec, collapse = "-"),
                                                Part = sum(bra_ind_2020$dwt[bra_ind_2020$ils_origy >= dec[1] &
                                                                              bra_ind_2020$ils_origy <= dec[2] &
                                                                              bra_ind_2020$dgn == gen & 
                                                                              (bra_ind_2020$dra %in% rc)])/sum(bra_ind_2020$dwt[bra_ind_2020$ils_origy >= dec[1] &
                                                                                                                                  bra_ind_2020$ils_origy <= dec[2]])))
      
    }
    
  }
  
}

# Naming columns
participacao_gen_race$Genero <- c(rep("H",20), rep("M",20))
participacao_gen_race$Raca <- c(rep(c(rep("B",10), rep("N",10)),2))
participacao_gen_race$Decil <- names(decis_origy)
participacao_gen_race$Grupo <- paste0(participacao_gen_race$Genero,"-", participacao_gen_race$Raca)

## Ploting graph----------------------------------------------------------------
# Saving graph
png("CompDemoDecil.png", width = 4800, height = 3200, res = 300)

# Adjusting margins
par(mar=c(5, 5, 5, 11), xpd=TRUE)
par(mfrow = c(1,1))

info.grafico <- matrix(participacao_gen_race$Part[c(1:10,21:30,11:20,31:40)], nrow = 4, ncol = 10, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       1:10)) * 100

info.grafico <- cbind("Pop." = participacao.med$Part[c(1,3,2,4)]*100, info.grafico)

barplot(info.grafico,
        col = c("#45ff66","#EB52FF","#3366FF","#FEFF41"),
        border = "white",
        ylim = c(0,100),
        space = 0.04,
        font.axis = 2, 
        cex.axis = 1.5, 
        cex.names = 1.5,
        cex.lab=1.5,
        xlab = "Decil e total população",
        ylab = "Composição demográfica dos decis_origy de renda (em %)")

legend("right", 
       inset=c(-.09, 0), 
       cex = 1.5,
       legend = c("Mulheres negras", "Homens negros", "Mulheres brancas", "Homens brancos"),
       fill = rev(c("#45ff66","#EB52FF","#3366FF","#FEFF41")), 
       bty = "n")

dev.off()



## Average demographic composition of population--------------------------------

participacao.med <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    participacao.med <- rbind(participacao.med,
                              data.frame(Genero = gen,
                                         Raca = paste(rc, collapse = "-"),
                                         Part = sum(bra_ind_2020$dwt[bra_ind_2020$dgn == gen & 
                                                                       (bra_ind_2020$dra %in% rc)])/sum(bra_ind_2020$dwt)))
    
  }
  
}

## Demographic composition of the top-------------------------------------------

# Participation by group by decile

participacao.topo_gen_race <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis.topo) {
      
      participacao.topo_gen_race <- rbind(participacao.topo_gen_race,
                                          data.frame(Genero = gen,
                                                     Raca = paste(rc, collapse = "-"),
                                                     Decil = paste(dec, collapse = "-"),
                                                     Part = sum(bra_ind_2020$dwt[bra_ind_2020$ils_dispy >= dec[1] &
                                                                                   bra_ind_2020$ils_dispy <= dec[2] &
                                                                                   bra_ind_2020$dgn == gen & 
                                                                                   (bra_ind_2020$dra %in% rc)])/
                                                       sum(bra_ind_2020$dwt[bra_ind_2020$ils_dispy >= dec[1] &
                                                                              bra_ind_2020$ils_dispy <= dec[2]])))
      
      
    }
    
  }
  
}

# Atributir o nome
participacao.topo_gen_race$Genero <- c(rep("H",6), rep("M",6))
participacao.topo_gen_race$Raca <- c(rep(c(rep("B",3), rep("N",3)),2))
participacao.topo_gen_race$Decil <- names(decis.topo)
participacao.topo_gen_race$Grupo <- paste0(participacao.topo_gen_race$Genero,"-", participacao.topo_gen_race$Raca)

# Participation of each group in the top deciles--------------------------------

## Plotting graph---------------------------------------------------------------
#Saving graph
png("CompDemoTopo.png", width = 4800, height = 3200, res = 300)

# Adjusting margins
par(mar=c(5, 5, 5, 11), xpd=TRUE)
par(mfrow = c(1,1))

info.grafico <- matrix(participacao.topo_gen_race$Part[c(1:3,7:9,4:6,10:12)], nrow = 4, ncol = 3, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       c("10%", "1%", "0.1%"))) * 100

info.grafico <- cbind("Pop." = participacao.med$Part[c(1,3,2,4)]*100, info.grafico)

barplot(info.grafico,
        col = c("#45ff66","#EB52FF","#3366FF","#FEFF41"),
        border = "white",
        ylim = c(0,100),
        space = 0.1,
        font.axis = 2,
        cex.axis = 1.5, 
        cex.names = 1.5,
        cex.lab=1.5,
        xlab = "Quantis",
        ylab = "Composição demográfica dos quantis de renda (em %)")

legend("right", 
       inset=c(-0.8, 0),
       cex = 1.5,
       legend = c("Mulheres negras", "Homens negros", "Mulheres brancas", "Homens brancos"),
       fill = rev(c("#45ff66","#EB52FF","#3366FF","#FEFF41")), 
       bty = "n")

dev.off()

## Participation of the top 1% in the total income of the group-----------------

# Generating list with min and max of quantiles 90%, 99% e 99,9%
decis.topo <- lapply(lapply(as.list(c(paste0(sapply(c(.9, .99, .999), 
                                                    function(quantil) weighted.quantile(x = bra_hh_2020$ils_dispy,
                                                                                        w = bra_hh_2020$dwt,
                                                                                        prob = quantil)),
                                             "-",
                                             rep(max(bra_hh_2020$ils_dispy),3)))),
                            function(decil) strsplit(decil, "-")), 
                     function(elemento) as.numeric(elemento[[1]]))

# Naming quantiles
names(decis.topo) <- c("10%", "1%", "0,1%")



# Deciles
decis.topo.grupo <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in c(.9, .99, .999)) {
      
      decis.topo.grupo <- rbind(decis.topo.grupo,
                                data.frame(Genero = gen,
                                           Raca = paste(rc, collapse = "-"),
                                           Decil = dec,
                                           Renda = weighted.quantile(x = bra_ind_2020$ils_dispy[bra_ind_2020$dgn == gen & (bra_ind_2020$dra %in% rc)],
                                                                     w = bra_ind_2020$dwt[bra_ind_2020$dgn == gen & (bra_ind_2020$dra %in% rc)],
                                                                     prob = dec)))
      
    }
    
  }
  
}

# Naming columns
decis.topo.grupo$Genero <- c(rep("H",6), rep("M",6))
decis.topo.grupo$Raca <- c(rep(c(rep("B",3), rep("N",3)),2))
decis.topo.grupo$Decil <- names(decis.topo)
decis.topo.grupo$Grupo <- paste0(decis.topo.grupo$Genero,"-", decis.topo.grupo$Raca)

## Plotting graph---------------------------------------------------------------

# Income appropriation by demographic group by decile

png("ApropGrupoDecil.png", width = 4800, height = 3200, res = 300)

par(mar=c(5, 5, 5, 5), xpd=TRUE)
par(mfrow = c(2,2))

info.grafico <- matrix(apropriacao.grupo$Apro[c(1:10,21:30,11:20,31:40)], nrow = 4, ncol = 10, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       1:10)) * 100

cores <- c("#45ff66","#EB52FF","#3366FF","#FEFF41")
legenda <- c("Homens brancos", "Mulheres brancas", "Homens negros", "Mulheres negras")

for(i in 1:4) {
  
  barplot(info.grafico[i,],
          col = cores[i],
          border = "white",
          ylim = c(0,65),
          beside = T,
          font.axis = 2,
          cex.axis = 1.5, 
          cex.names = 1.5,
          cex.lab=1.5,
          xlab = "Decis",
          ylab = "%")
  
  text(x = seq(.7, 11.5, length.out = 10),
       y = info.grafico[i,]+3,
       labels = round(info.grafico[i,], digits = 0), font = 2)
  
  text(x = 1.9,
       y = 40,
       labels = legenda[i], font = 2, cex = 1.5)
  
}

dev.off()

# Calculating concentration in each group:

criterios <- list("HB" = list("Genero" = genero[1],
                              "Raca" = raca[[1]],
                              "Quantis" = decis.topo.grupo$Renda[1:3]),
                  "HN" = list("Genero" = genero[1],
                              "Raca" = raca[[2]],
                              "Quantis" = decis.topo.grupo$Renda[4:6]),
                  "MB" = list("Genero" = genero[2],
                              "Raca" = raca[[1]],
                              "Quantis" = decis.topo.grupo$Renda[7:9]),
                  "MN" = list("Genero" = genero[2],
                              "Raca" = raca[[2]],
                              "Quantis" = decis.topo.grupo$Renda[10:12]))

apropriacao.topo.grupo <- data.frame()

for (grupo in criterios) {
  
  for (decil in grupo$Quantis) {
    
    apropriacao.topo.grupo <- rbind(apropriacao.topo.grupo,
                                    data.frame(Genero = grupo$Genero,
                                               Raca = paste0(grupo$Raca, collapse = "-"),
                                               Quantil = decil,
                                               Part = sum(bra_ind_2020$ils_dispy[bra_ind_2020$ils_dispy >= decil &
                                                                                   bra_ind_2020$ils_dispy <= max(bra_ind_2020$ils_dispy) &
                                                                                   bra_ind_2020$dgn == grupo$Genero & 
                                                                                   (bra_ind_2020$dra %in% grupo$Raca)]*
                                                            bra_ind_2020$dwt[bra_ind_2020$ils_dispy >= decil &
                                                                               bra_ind_2020$ils_dispy <= max(bra_ind_2020$ils_dispy) &
                                                                               bra_ind_2020$dgn == grupo$Genero & 
                                                                               (bra_ind_2020$dra %in% grupo$Raca)])/
                                                 sum(bra_ind_2020$ils_dispy[bra_ind_2020$dgn == grupo$Genero & 
                                                                              (bra_ind_2020$dra %in% grupo$Raca)]*
                                                       bra_ind_2020$dwt[bra_ind_2020$dgn == grupo$Genero & 
                                                                          (bra_ind_2020$dra %in% grupo$Raca)])))
    
  }
  
}

# Naming columns
apropriacao.topo.grupo$Genero <- c(rep("H",6), rep("M",6))
apropriacao.topo.grupo$Raca <- c(rep(c(rep("B",3), rep("N",3)),2))
apropriacao.topo.grupo$Quantil <- names(decis.topo)
apropriacao.topo.grupo$Grupo <- paste0(apropriacao.topo.grupo$Genero,"-", apropriacao.topo.grupo$Raca)

## Income appropriation by the top by group-------------------------------------

apropriacao.topo <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis.topo) {
      
      apropriacao.topo <- rbind(apropriacao.topo,
                                data.frame(Genero = gen,
                                           Raca = paste(rc, collapse = "-"),
                                           Decil = paste(dec, collapse = "-"),
                                           Apro = sum(bra_ind_2020$ils_dispy[bra_ind_2020$ils_dispy >= dec[1] &
                                                                               bra_ind_2020$dgn == gen & 
                                                                               (bra_ind_2020$dra %in% rc)]*
                                                        bra_ind_2020$dwt[bra_ind_2020$ils_dispy >= dec[1] &
                                                                           bra_ind_2020$dgn == gen & 
                                                                           (bra_ind_2020$dra %in% rc)])/
                                             sum(bra_ind_2020$dwt*bra_ind_2020$ils_dispy)))
      
      
    }
    
  }
  
}

# Naming columns
apropriacao.topo$Genero <- c(rep("H",nrow(apropriacao.topo)/2), rep("M",nrow(apropriacao.topo)/2))
apropriacao.topo$Raca <- c(rep(c(rep("B",(nrow(apropriacao.topo)/4)), rep("N",(nrow(apropriacao.topo)/4))),2))
apropriacao.topo$Decil <- names(decis.topo)
apropriacao.topo$Grupo <- paste0(apropriacao.topo$Genero,"-", apropriacao.topo$Raca)

## Plotting graph---------------------------------------------------------------
# Saving image
png("ApropGrupoTopo.png", width = 4800, height = 3200, res = 300)

# Adjusting margins
par(mar=c(5, 5, 5, 5), xpd=TRUE)
par(mfrow = c(2,2))

info.grafico <- matrix(apropriacao.topo.grupo$Part[c(1:3,7:9,4:6,10:12)], nrow = 4, ncol = 3, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       c("10%", "1%", "0.1%"))) * 100

cores <- c("#45ff66","#EB52FF","#3366FF","#FEFF41")
legenda <- c("Homens brancos", "Mulheres brancas", "Homens negros", "Mulheres negras")

for(i in 1:4) {
  
  barplot(info.grafico[i,],
          col = cores[i],
          border = "white",
          ylim = c(0,70),
          beside = T,
          font.axis = 2,
          cex.axis = 1.5, 
          cex.names = 1.5,
          cex.lab=1.5,
          xlab = "Quantis do topo",
          ylab = "%")
  
  text(x = seq(.7, 3.1, length.out = 3),
       y = info.grafico[i,]+3,
       labels = round(info.grafico[i,], digits = 0), font = 2)
  
  text(x = 2.9,
       y = 60,
       labels = legenda[i], font = 2, cex = 1.5)
  
}

dev.off()

## Plotting graph---------------------------------------------------------------
# Income appropriation by the top
#Saving image
png("ApropTopo.png", width = 4800, height = 3200, res = 300)

#Adjusting margins
par(mar=c(5, 5, 5, 5), xpd=TRUE)
par(mfrow = c(1,1))

info.grafico <- cbind("0 - 90%" = rowSums(info.grafico[,-ncol(info.grafico)]),
                      matrix(apropriacao.topo$Apro[c(1:3,7:9,4:6,10:12)], nrow = 4, ncol = 3, byrow = T,
                             dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                               "Homens negros", "Mulheres negras"),
                                             c("10%", "1%", "0.1%"))) * 100)


barplot(info.grafico,
        col = c("#45ff66","#EB52FF","#3366FF","#FEFF41"),
        border = "white",
        ylim = c(0,60),
        space = 0.04,
        font.axis = 2,
        cex.axis = 1.5, 
        cex.names = 1.5,
        cex.lab=1.5,
        xlab = "Quantil",
        ylab = "Apropriação da renda total pelo topo (em %)")

legend("topright", 
       inset=c(.1, 0),
       cex = 1.5,
       legend = c("Mulheres negras", "Homens negros", "Mulheres brancas", "Homens brancos"),
       fill = rev(c("#45ff66","#EB52FF","#3366FF","#FEFF41")), 
       bty = "n")

dev.off()



# Generating list with max and min of deciles by group
decis.grupo <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in seq(0, .9, .1)) {
      
      decis.grupo <- rbind(decis.grupo,
                           data.frame(Genero = gen,
                                      Raca = paste(rc, collapse = "-"),
                                      Decil = dec,
                                      Renda = weighted.quantile(x = bra_ind_2020$ils_dispy[bra_ind_2020$dgn == gen & (bra_ind_2020$dra %in% rc)],
                                                                w = bra_ind_2020$dwt[bra_ind_2020$dgn == gen & (bra_ind_2020$dra %in% rc)],
                                                                prob = dec)))
      
    }
    
    decis.grupo <- rbind(decis.grupo,
                         data.frame(Genero = gen,
                                    Raca = paste(rc, collapse = "-"),
                                    Decil = "Max",
                                    Renda = max(bra_ind_2020$ils_dispy[bra_ind_2020$dgn == gen & (bra_ind_2020$dra %in% rc)])))
    
  }
  
}

# Naming columns
decis.grupo$Genero <- c(rep("H",22), rep("M",22))
decis.grupo$Raca <- c(rep(c(rep("B",11), rep("N",11)),2))
decis.grupo$Decil <- c(paste(seq(0, .9, .1)), "Max")
decis.grupo$Grupo <- paste0(decis.grupo$Genero,"-", decis.grupo$Raca)

# Calculating concentration inside groups---------------------------------------

criterios <- list("HB" = list("Genero" = genero[1],
                              "Raca" = raca[[1]],
                              "Quantis" = decis.grupo$Renda[1:11]),
                  "HN" = list("Genero" = genero[1],
                              "Raca" = raca[[2]],
                              "Quantis" = decis.grupo$Renda[12:22]),
                  "MB" = list("Genero" = genero[2],
                              "Raca" = raca[[1]],
                              "Quantis" = decis.grupo$Renda[23:33]),
                  "MN" = list("Genero" = genero[2],
                              "Raca" = raca[[2]],
                              "Quantis" = decis.grupo$Renda[34:44]))

apropriacao.grupo <- data.frame()

for (grupo in criterios) {
  
  for (decil in seq_along(grupo$Quantis)[-11]) {
    
    apropriacao.grupo <- rbind(apropriacao.grupo,
                               data.frame(Genero = grupo$Genero,
                                          Raca = paste0(grupo$Raca, collapse = "-"),
                                          Quantil = decil,
                                          Apro = sum(bra_ind_2020$ils_dispy[bra_ind_2020$ils_dispy >= grupo$Quantis[decil] &
                                                                              bra_ind_2020$ils_dispy <= grupo$Quantis[decil+1] &
                                                                              bra_ind_2020$dgn == grupo$Genero & 
                                                                              (bra_ind_2020$dra %in% grupo$Raca)]*
                                                       bra_ind_2020$dwt[bra_ind_2020$ils_dispy >= grupo$Quantis[decil] &
                                                                          bra_ind_2020$ils_dispy <= grupo$Quantis[decil+1] &
                                                                          bra_ind_2020$dgn == grupo$Genero & 
                                                                          (bra_ind_2020$dra %in% grupo$Raca)])/
                                            sum(bra_ind_2020$ils_dispy[bra_ind_2020$dgn == grupo$Genero & 
                                                                         (bra_ind_2020$dra %in% grupo$Raca)]*
                                                  bra_ind_2020$dwt[bra_ind_2020$dgn == grupo$Genero & 
                                                                     (bra_ind_2020$dra %in% grupo$Raca)])))
    
  }
  
}

# Naming columns
apropriacao.grupo$Genero <- c(rep("H",20), rep("M",20))
apropriacao.grupo$Raca <- c(rep(c(rep("B",10), rep("N",10)),2))
apropriacao.grupo$Grupo <- paste0(apropriacao.grupo$Genero,"-", apropriacao.grupo$Raca)

## Plotting graph---------------------------------------------------------------
# Income appropriation
#Saving image
png("ApropDecil.png", width = 4800, height = 3200, res = 300)

# Adjusting margins
par(mar=c(5, 5, 5, 5), xpd=TRUE)
par(mfrow = c(1,1))

info.grafico <- matrix(apropriacao_gen_race$Apro[c(1:10,21:30,11:20,31:40)], nrow = 4, ncol = 10, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       1:10)) * 100

barplot(info.grafico,
        col = c("#45ff66","#EB52FF","#3366FF","#FEFF41"),
        border = "white",
        ylim = c(0,60),
        space = 0.04,
        font.axis = 2,
        cex.axis = 1.5, 
        cex.names = 1.5,
        cex.lab=1.5,
        xlab = "Decil",
        ylab = "Apropriação da renda total (em %)")

legend("topleft", 
       inset=c(.1, 0),
       cex = 1.5,
       legend = c("Mulheres negras", "Homens negros", "Mulheres brancas", "Homens brancos"),
       fill = rev(c("#45ff66","#EB52FF","#3366FF","#FEFF41")), 
       bty = "n")

dev.off()




## Concentration coefficient----------------------------------------------------
# Ordering data
bra_hh_2020 <- bra_hh_2020[order(bra_hh_2020$ils_dispy),]
bra_ind_2020 <- bra_ind_2020[order(bra_ind_2020$ils_dispy),]

# Creating relevant column
bra_hh_2020$ordem <- 1:nrow(bra_hh_2020)
bra_ind_2020$ordem <- 1:nrow(bra_ind_2020)

bra_hh_2020$prop_pop <- cumsum(bra_hh_2020$dwt/sum(bra_hh_2020$dwt))
bra_ind_2020$prop_pop <- cumsum(bra_ind_2020$dwt/sum(bra_hh_2020$dwt))

# Calculating concentration coeffiecient
bra_hh_2020$cc <- cumsum(bra_hh_2020$ils_dispy*bra_hh_2020$dwt/sum(bra_hh_2020$ils_dispy*bra_hh_2020$dwt))

# Plotting graph
# Salvar o grafico a ser feito a seguir
png("CompDemoDecil.png", width = 4800, height = 3200, res = 300)

# Ajuste das margens para caber a legenda
par(mar=c(5, 5, 5, 11), xpd=TRUE)
par(mfrow = c(1,1))

x <- bra_hh_2020$prop_pop
y <- bra_hh_2020$cc
plot(x, y, col = "red")




