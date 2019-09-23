# list of packages used###############
list.of.packages <- c("sna", "broom", "shiny", "shinyjs","shinydashboard","shinydashboardPlus","DT", "tidyverse", "plotly", "shinycssloaders", "CINNA", "visNetwork","devtools", "plyr", "statnet", "intergraph", "igraph", "arcdiagram", "network")

#devtools::install_github('andrewsali/shinycssloaders')
# see if the packages are installed in the host################
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install.packages(c("shiny", "shinyjs","shinydashboard","shinydashboardPlus","DT", "tidyverse", "plotly", "visNetwork"))

#load multiple packages###########
lapply(list.of.packages, require, character.only = TRUE)

##############load authors data##############
 load("data/id_papers.RData")
 load("data/id_departamento.RData")
 load("data/id_journals.RData")
 load("data/id_autores.RData")
 load("data/master_publications_limpio_final.RData")
 load("data/department_palette.RData")
#load("C:/Users/Acer/Desktop/relations_publications/department_palette.RData")
#load("C:/Users/Acer/Desktop/relations_publications/id_autores.RData")
#load("C:/Users/Acer/Desktop/relations_publications/id_departamento.RData")
#load("C:/Users/Acer/Desktop/relations_publications/id_journals.RData")
#load("C:/Users/Acer/Desktop/relations_publications/id_papers.RData")
#load("C:/Users/Acer/Desktop/relations_publications/master_publications_limpio_final.RData")


############## header code ##############
header <- dashboardHeader(title = "Time Windows")

############## sidebar code ##############
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    sliderInput(
      inputId = "years",
      round = TRUE,
      h3("Years:"),
      sep = "",
      min = 1959,
      max = 2018,
      value = c(1959, 1962)
    ),
    actionButton(inputId = "run_cij",
                 label = "Calculate Window"),
    menuItem("Department network",
             tabName = "dep_net",
             icon = icon("building"))
  )
)

############## body code ##############
body <- dashboardBody(tabItems(
  tabItem(tabName = "dep_net",
          fluidPage(
            tabBox(
              width = 12,
              title = "Window Networks",
              tabPanel(
                "Select author",
                visNetworkOutput("visnetwork_author")  %>% withSpinner()
              ),
              tabPanel(
                "Select department",
                visNetworkOutput("visnetwork_department")  %>% withSpinner()
              ),
              tabPanel(
                "Select job",
                visNetworkOutput("visnetwork_job")  %>% withSpinner()
              )
            ),
            box(
              title = "All colabs between department",
              status = "primary",
              width = 12,
              solidHeader = TRUE,
              visNetworkOutput("vis_dep_unestable_net") %>% withSpinner()
            )
          ))
))

############## UI code ##############
ui <- dashboardPage(header, sidebar, body)

################ FUNCTIONS TO CALCULATE Vij ##################
#crea el dataframe de la ventana, recibe el df que se cargo,
#el aÃ±o inicial que es 1959, y el ultimo aÃ±o de la ventana
time_network_window <- function(data, window_year, last_year) {
  data <- master_publications_limpio_final
  window_df <- NULL
  n <- (last_year - window_year) + 1
  for (i in 1:n) {
    window_df_provisional <- data[data$anio == window_year,]
    window_df <- rbind(window_df, window_df_provisional)
    window_year <- window_year + 1
  }
  
  rownames(window_df) <- NULL
  return(window_df)
}

############### Functions for authors ###############
#generacion de la lista de autores exclusiva de la window
authors_window <- function(window_df) {
  window_df <- window_df[with(window_df, order(id_autor)), ]
  authors_list <- unique(window_df$id_autor)
  return(authors_list)
}

#en esta funcion se crea la lista de vi
vi_list <- function(id_autores_df, window_df, id_journals_df, authors_list) {
  
  vectores_autores <- NULL
  autor_var <- NULL
  journals_publicados <- NULL
  conteo_journals <- NULL
  indice <- NULL
  
  vectores_autores <- list(1:length(authors_list))
  for (i in 1:length(authors_list)) {
    vectores_autores[i] <- list(rep(0, length(id_journals_df$id_journal)))
  }
  
  for (i in 1:length(authors_list)) {
    journals_publicados <- NULL
    autor_var <- NULL
    conteo_journals <- NULL
    autor_var <- authors_list[i]
    journals_publicados <- window_df$id_journal[window_df$id_autor == autor_var]
    conteo_journals <- data.frame(table(journals_publicados), stringsAsFactors = FALSE)
    
    for (j in 1:length(conteo_journals$Freq)) {
      indice <- NULL
      indice <- match(conteo_journals$journals_publicados[j], id_journals_df$id_journal)
      vectores_autores[[i]][indice] <- as.integer(conteo_journals$Freq[j])
    }
  }
  return(vectores_autores)
}

#calculo de la norma de vi de los autores
norm_vector_calc <- function(product_vi_list, authors_list) {
  norma_vector <- NULL
  norma_journals_autor <- NULL
  
  
  norma_journals_autor <- data.frame(id_autor = authors_list, norma = 0, stringsAsFactors = FALSE)
  for (i in 1:length(authors_list)) {
    norma_vector <- NULL
    norma_vector <- unlist(product_vi_list[[i]])
    norma_journals_autor$norma[i] <- sqrt(sum(norma_vector^2))
  }
  return(norma_journals_autor)
}

#this function calculates the angle Cij between pairs of authors using the formula
#
cij_authors_calc <- function(product_vi_list, norm_window, list_authors, global_id_authors_df) {
  
  indice_vector1 <- NULL
  indice_vector2 <- NULL
  journals_vector1 <- NULL
  journals_vector2 <- NULL
  combinaciones_autores_journals <- NULL
  
  combinaciones_autores_journals <- t(combn(list_authors, 2))
  combinaciones_autores_journals <- data.frame(author1 = combinaciones_autores_journals[, 1], author2 = combinaciones_autores_journals[, 2], stringsAsFactors = FALSE)
  combinaciones_autores_journals$cij <- 0
  
  for (i in 1:length(combinaciones_autores_journals$cij)) {
    indice_vector1 <- NULL
    indice_vector2 <- NULL
    journals_vector1 <- NULL
    journals_vector2 <- NULL
    norma_1 <- NULL
    norma_2 <- NULL
    arriba <- NULL
    abajo <- NULL
    division <- NULL
    coseno <- NULL
    pie_var <- 2/pi
    cij <- NULL
    pre_cij <- NULL
    
    #vamos a sacar vector 1 y vector 2 para las operaciones de la lista
    indice_vector1 <- match(combinaciones_autores_journals$author1[i], list_authors)
    indice_vector2 <- match(combinaciones_autores_journals$author2[i], list_authors)
    journals_vector1 <- unlist(product_vi_list[[indice_vector1]])
    journals_vector2 <- unlist(product_vi_list[[indice_vector2]])
    norma_1 <- norm_window$norma[indice_vector1]
    norma_2 <- norm_window$norma[indice_vector2]
    #operation
    arriba <- sum(journals_vector1 * journals_vector2)
    abajo <- norma_1 * norma_2
    division <- arriba / abajo
    coseno <- acos(division)
    pre_cij <- pie_var * coseno
    #here you can add a 1- pre_cij to show the complement
    cij <- pre_cij
    combinaciones_autores_journals$cij[i] <- cij
  }
  #ahora cambiaremos los id por los nombres para tener la info bien
  for (j in 1:length(combinaciones_autores_journals$cij)) {
    combinaciones_autores_journals$author1[j] <- global_id_authors_df$autor[global_id_authors_df$id_autores == combinaciones_autores_journals$author1[j]]
    combinaciones_autores_journals$author2[j] <- global_id_authors_df$autor[global_id_authors_df$id_autores == combinaciones_autores_journals$author2[j]]
  }
  
  return(combinaciones_autores_journals)
} 

#this function tranforms the vi list to a matrix to get a visualization
vi_matrix_conversion <- function(product_vi_list, global_id_journals_df, global_id_authors_df, authors_list) {
  
  table_vi <- NULL
  sum_of_cols <- NULL
  list_to_delete <- NULL
  
  list_journals <- global_id_journals_df$journal
  
  for (i in 1:length(authors_list)) {
    authors_list[i] <- global_id_authors_df$autor[global_id_authors_df$id_autores == authors_list[i]]
  }
  
  table_vi <- matrix(nrow = length(authors_list), ncol = length(list_journals))
  
  
  
  for (j in 1:length(authors_list)) {
    for (k in 1:length(list_journals)) {
      table_vi[j,k] <- product_vi_list[[j]][k]
    }
  }
  
  colnames(table_vi) <- list_journals
  rownames(table_vi) <- authors_list
  sum_of_cols <- colSums(table_vi)
  m <- 1
  for (l in 1:length(sum_of_cols)) {
    if(sum_of_cols[l] == 0){
      list_to_delete[m] <- l
      m <- m + 1
    }
  }
  table_vi <- table_vi[, -list_to_delete]
  return(table_vi)
}

############### Functions for departments ###############
#in this function we create the department list of the window
window_departments <- function(list_of_authors_window, id_authors_df) {
  
  department_list <- NULL
  
  list_of_authors_window <- list_of_authors_window
  list_of_authors_window <- data.frame(author_id = list_of_authors_window, stringsAsFactors = FALSE)
  list_of_authors_window <- left_join(list_of_authors_window, id_authors_df, by = c("author_id" = "id_autores"))
  rownames(list_of_authors_window) <- NULL
  
  department_list <- unique(list_of_authors_window$departamento)
  department_list <- data.frame(department = department_list, authors = 1, stringsAsFactors = FALSE)
  
  for (i in 1:length(department_list$department)) {
    department_list$authors[i] <- list(which(list_of_authors_window$departamento == department_list$department[i]))
  }
  return(department_list)
}

#in this function we calculate the vi of the departments
departments_vi_calc <- function(department_list, vi_window_list, id_journals_df) {
  
  vectores_departamentos <- NULL
  sum_of_vectors <- NULL
  
  #sum of the vectors
  vectores_departamentos <- list(rep(0, length(id_journals_df$id_journal)))
  
  sum_of_vectors <- list(1:length(department_list$department))
  
  for (k in 1:length(department_list$department)) {
    unlisted <- unlist(department_list$authors[k])
    vector1 <- unlist(vectores_departamentos, use.names = FALSE)
    for (l in 1:length(unlisted)) {
      vector2 <- unlist(vi_window_list[unlisted[l]], use.names = FALSE)
      vector1 <- vector1 + vector2
      vector2 <- NULL
    }
    sum_of_vectors[[k]]<- vector1
    vector1 <- NULL
  }
  return(sum_of_vectors)
}

#calculo de la norma de vi de los departamentos
norm_department_vector_calc <- function(department_vi_list, department_list) {
  norma_vector <- NULL
  norma_journals_department <- NULL
  
  norma_journals_department <- data.frame(id_department = department_list$department, norma = 0, stringsAsFactors = FALSE)
  for (i in 1:length(department_list$department)) {
    norma_vector <- NULL
    norma_vector <- unlist(department_vi_list[[i]])
    norma_journals_department$norma[i] <- sqrt(sum(norma_vector^2))
  }
  return(norma_journals_department)
}

#cij for departments
cij_department_calc <- function(vi_departments, norm_department_window, department_list, id_autores) {
  
  indice_vector1 <- NULL
  indice_vector2 <- NULL
  journals_vector1 <- NULL
  journals_vector2 <- NULL
  combinaciones_departamentos_journals <- NULL
  
  combinaciones_departamentos_journals <- t(combn(department_list$department, 2))
  combinaciones_departamentos_journals <- data.frame(departamento1 = combinaciones_departamentos_journals[, 1], departamento2 = combinaciones_departamentos_journals[, 2], stringsAsFactors = FALSE)
  combinaciones_departamentos_journals$cij <- 0
  
  for (i in 1:length(combinaciones_departamentos_journals$cij)) {
    indice_vector1 <- NULL
    indice_vector2 <- NULL
    journals_vector1 <- NULL
    journals_vector2 <- NULL
    norma_1 <- NULL
    norma_2 <- NULL
    arriba <- NULL
    abajo <- NULL
    division <- NULL
    coseno <- NULL
    pie_var <- 2/pi
    cij <- NULL
    pre_cij <- NULL
    
    #vamos a sacar vector 1 y vector 2 para las operaciones de la lista
    indice_vector1 <- match(combinaciones_departamentos_journals$departamento1[i], department_list$department)
    indice_vector2 <- match(combinaciones_departamentos_journals$departamento2[i], department_list$department)
    journals_vector1 <- unlist(vi_departments[[indice_vector1]])
    journals_vector2 <- unlist(vi_departments[[indice_vector2]])
    norma_1 <- norm_department_window$norma[indice_vector1]
    norma_2 <- norm_department_window$norma[indice_vector2]
    #operation
    arriba <- sum(journals_vector1 * journals_vector2)
    abajo <- norma_1 * norma_2
    division <- arriba / abajo
    coseno <- acos(division)
    pre_cij <- pie_var * coseno
    #here you can add a 1- pre_cij to show the complement
    cij <- pre_cij
    combinaciones_departamentos_journals$cij[i] <- cij
  }
  return(combinaciones_departamentos_journals)
}

######## Functions to use with VisNetwork ##################
separate_authors <- function(window_df) {
  conteo_colabs <- NULL
  no_colabs <- NULL
  si_colabs <- NULL
  colabs_division <- NULL
  si_colabs_df <- NULL
  no_colabs_df <- NULL
  temp <- NULL
  
  conteo_colabs <- as.data.frame(table(window_df$paper), stringsAsFactors = FALSE)
  
  si_colabs <- conteo_colabs$Var1[conteo_colabs$Freq > 1]
  temp <- NULL
  for (j in 1:length(si_colabs)) {
    temp <- window_df[window_df$paper == si_colabs[j], ]
    si_colabs_df <- rbind(si_colabs_df, temp)
    temp <- NULL
  }
  rownames(si_colabs_df) <- NULL
  
  no_colabs <- conteo_colabs$Var1[conteo_colabs$Freq == 1]
  temp <- NULL
  for (i in 1:length(no_colabs)) {
    temp <- window_df[window_df$paper == no_colabs[i], ]
    no_colabs_df <- rbind(no_colabs_df, temp)
    temp <- NULL
  }
  rownames(no_colabs_df) <- NULL
  
  colabs_division <- list(si_colabs_df, no_colabs_df)
  return(colabs_division)
}

author_links <- function(colabs_list) {
  indexes <- NULL
  papers_window <- NULL
  authors_window <- NULL
  author_links1 <- NULL
  colabs_df <- NULL
  linkmatrix <- NULL
  duplicates <- NULL
  
  colabs_df <- as.data.frame(colabs_list[1], stringsAsFactors = FALSE)
  colabs_df <- colabs_df[with(colabs_df, order(id_autor)), ]
  
  papers_window <- unique(colabs_df$id_paper)
  authors_window <- unique(colabs_df$id_autor)
  
  for(i in 1:length(papers_window)){
    
    indexes <- colabs_df$id_autor[colabs_df$id_paper == papers_window[i]]
    indexes <- sort(indexes)
    linkmatrix <- t(combn(indexes, 2))
    linkmatrix <- rbind(data.frame(from = linkmatrix[, 1], to = linkmatrix[, 2], stringsAsFactors = FALSE))
    author_links1 <- rbind(author_links1, linkmatrix)
    indexes <- NULL
    
  }
  author_links1 <- unique(author_links1)
  rownames(author_links1) <- c()
  
  for (j in 1:length(author_links1$from)) {
    author_links1$from[j] <- as.numeric(gsub("a" ,"", author_links1$from[j]))
    author_links1$to[j] <- as.numeric(gsub("a" ,"", author_links1$to[j]))
  }
  
  l <- 1
  
  for (k in 1:length(author_links1$from)) {
    if(author_links1$from[k] == author_links1$to[k]){
      duplicates[l] <- k
      l <- l + 1
    }
  }
  
  if(!is.null(duplicates)) {
    author_links1 <- author_links1[-c(duplicates), ]
  }
  
  return(author_links1)
}

nodes_network <- function(colabs_list, data_id_authors_info_df) {
  
  colabs_df <- NULL
  id_number <- NULL
  name_label <- NULL
  department <- NULL
  grade <- NULL
  no_colabs_df <- NULL
  department_label <- NULL
  #lets take the colabs info
  colabs_df <- as.data.frame(colabs_list[1], stringsAsFactors = FALSE)
  #the next two lines helps to visualize the lone dots
  no_colabs_df <- as.data.frame(colabs_list[2], stringsAsFactors = FALSE)
  colabs_df <- rbind(no_colabs_df, colabs_df)
  list_id_nodes <- unique(colabs_df$id_autor)
  list_id_nodes <- list_id_nodes[order(list_id_nodes)]
  
  for (i in 1:length(list_id_nodes)) {
    id_number[i] <- which(data_id_authors_info_df$id_autores == list_id_nodes[i])
    name_label[i] <- data_id_authors_info_df$autor[id_number[i]]
    department_label[i] <- data_id_authors_info_df$departamento[id_number[i]]
    department[i] <- department_pallete$palette[which(department_pallete$department == department_label[i])]
    grade[i] <- data_id_authors_info_df$puesto[id_number[i]]
  }
  grade2 <- grade
  
  for (j in 1:length(grade)) {
    if(grade[j] == "Investigador") {
      grade[j] <- "circle"
    } else {
      if(grade[j] == "TA"){
        grade[j] <- "triangle"
      } else {
        if(grade[j] == "Posdoc"){
          grade[j] <- "star"
        }
      }
    }
  }
  nodes_info <- data.frame(id = id_number, title = name_label, shape = grade, group = grade2, color = department, department = department_label, stringsAsFactors = FALSE)
  return(nodes_info)
}

links_weight <- function(colabs_list) {
  indexes <- NULL
  papers_window <- NULL
  authors_window <- NULL
  author_links1 <- NULL
  colabs_df <- NULL
  linkmatrix <- NULL
  
  colabs_df <- as.data.frame(colabs_list[1], stringsAsFactors = FALSE)
  colabs_df <- colabs_df[with(colabs_df, order(id_autor)), ]
  
  papers_window <- unique(colabs_df$id_paper)
  authors_window <- unique(colabs_df$id_autor)
  
  for(i in 1:length(papers_window)){
    
    indexes <- colabs_df$id_autor[colabs_df$id_paper == papers_window[i]]
    indexes <- sort(indexes)
    linkmatrix <- t(combn(indexes, 2))
    linkmatrix <- rbind(data.frame(from = linkmatrix[, 1], to = linkmatrix[, 2], stringsAsFactors = FALSE))
    author_links1 <- rbind(author_links1, linkmatrix)
    indexes <- NULL
    
  }
  rownames(author_links1) <- c()
  
  for (j in 1:length(author_links1$from)) {
    author_links1$from[j] <- as.numeric(gsub("a" ,"", author_links1$from[j]))
    author_links1$to[j] <- as.numeric(gsub("a" ,"", author_links1$to[j]))
  }
  
  
  weight_df <- ddply(author_links1,.(from,to),nrow)
  
  
  return(weight_df)
}

##### department_as_nodes ##########
department_as_nodes <- function(colabs_list, data_id_department_info_df, department_color) {
  colabs_df <- NULL
  id_number <- NULL
  name_label <- NULL
  no_colabs_df <- NULL
  department <- NULL
  
  #lets take the colabs info
  colabs_df <- as.data.frame(colabs_list[1], stringsAsFactors = FALSE)
  #the next two lines helps to visualize the lone dots
  no_colabs_df <- as.data.frame(colabs_list[2], stringsAsFactors = FALSE)
  colabs_df <- rbind(no_colabs_df, colabs_df)
  list_id_nodes <- unique(colabs_df$id_departamento)
  list_id_nodes <- list_id_nodes[order(list_id_nodes)]
  
  for (i in 1:length(list_id_nodes)) {
    id_number[i] <- which(data_id_department_info_df$id_departamento == list_id_nodes[i])
    name_label[i] <- data_id_department_info_df$departamento[id_number[i]]
    department[i] <- department_color$palette[department_color$department == name_label[i]]
    
  }
  
  nodes_info <- data.frame(id = name_label, title = name_label, color = department, stringsAsFactors = FALSE)
  return(nodes_info)
}

department_links <- function(colabs_list) {
  indexes <- NULL
  papers_window <- NULL
  authors_window <- NULL
  department_links1 <- NULL
  colabs_df <- NULL
  linkmatrix <- NULL
  #duplicates <- NULL
  
  colabs_df <- as.data.frame(colabs_list[1], stringsAsFactors = FALSE)
  colabs_df <- colabs_df[with(colabs_df, order(id_departamento)), ]
  
  papers_window <- unique(colabs_df$id_paper)
  authors_window <- unique(colabs_df$id_autor)
  
  for(i in 1:length(papers_window)){
    
    indexes <- colabs_df$id_departamento[colabs_df$id_paper == papers_window[i]]
    indexes <- sort(indexes)
    linkmatrix <- t(combn(indexes, 2))
    linkmatrix <- unique(linkmatrix)
    linkmatrix <- rbind(data.frame(from = linkmatrix[, 1], to = linkmatrix[, 2], stringsAsFactors = FALSE))
    department_links1 <- rbind(department_links1, linkmatrix)
    indexes <- NULL
    
  }
  
  rownames(department_links1) <- c()
  
  for (j in 1:length(department_links1$from)) {
    department_links1$from[j] <- unique(colabs_df$departamento[colabs_df$id_departamento == department_links1$from[j]])
    department_links1$to[j] <- unique(colabs_df$departamento[colabs_df$id_departamento == department_links1$to[j]])
  }
  
  weight_df <- ddply(department_links1,.(from,to),nrow)
  
  colnames(weight_df) <- c("from", "to", "weight")
  weight_df$label <- weight_df$weight
  
  for (k in 1:length(weight_df$from)) {
    weight_df$weight[k] <- log(weight_df$weight[k])
  }
  
  #this chunk of code gets rid of collabs between the same department
  # for (k in 1:length(department_links1$from)) {
  #   if(department_links1$from[k] == department_links1$to[k]){
  #     duplicates[l] <- k
  #     l <- l + 1
  #   }
  # }
  
  # if(!is.null(duplicates)) {
  #   author_links1 <- author_links1[-c(duplicates), ]
  # }
  
  return(weight_df)
}
######## Functions to calculate recomendations ##################
####### check_past_colabs ################
check_past_colabs <- function(data, window_year) {
  if(window_year == 1959) {
    window_past_df <- "none"
  } else {
    window_past_df <- NULL
    n <- window_year - 1959
    first_year <- 1959
    for (n in 1:n) {
      window_df_provisional <- data[data$anio == first_year,]
      window_past_df <- rbind(window_past_df, window_df_provisional)
      first_year <- first_year + 1
    }
    rownames(window_past_df) <- NULL
    return(window_past_df)
  }
}

links_weight_past <- function(colabs_list, nodes_df) {
  indexes <- NULL
  papers_window <- NULL
  authors_window <- NULL
  author_links1 <- NULL
  colabs_df <- NULL
  linkmatrix <- NULL
  duplicates <- NULL
  
  colabs_df <- as.data.frame(colabs_list[1], stringsAsFactors = FALSE)
  colabs_df <- colabs_df[with(colabs_df, order(id_autor)), ]
  
  papers_window <- unique(colabs_df$id_paper)
  authors_window <- unique(colabs_df$id_autor)
  
  for(i in 1:length(papers_window)){
    
    indexes <- colabs_df$id_autor[colabs_df$id_paper == papers_window[i]]
    indexes <- sort(indexes)
    linkmatrix <- t(combn(indexes, 2))
    linkmatrix <- rbind(data.frame(from = linkmatrix[, 1], to = linkmatrix[, 2], stringsAsFactors = FALSE))
    author_links1 <- rbind(author_links1, linkmatrix)
    indexes <- NULL
    
  }
  rownames(author_links1) <- c()
  
  for (j in 1:length(author_links1$from)) {
    author_links1$from[j] <- as.numeric(gsub("a" ,"", author_links1$from[j]))
    author_links1$to[j] <- as.numeric(gsub("a" ,"", author_links1$to[j]))
  }
  
  #get their names right
  for (j in 1:length(author_links1$from)) {
    author_links1$from[j] <- nodes_df$title[nodes_df$id == author_links1$from[j]]
    author_links1$to[j] <- nodes_df$title[nodes_df$id == author_links1$to[j]]
  }
  
  
  weight_df <- ddply(author_links1,.(from,to),nrow)
  
  colnames(weight_df) <- c("from", "to", "weight")
  
  
  return(weight_df)
}

count_past_colabs <- function(cij_df, past_edges_df) {
  
  cij_df$past_colaboration <- ifelse(
    is.na(
      match(
        paste0(cij_df$Colaborator.1, cij_df$colaborator.2), as.vector(paste0(past_edges_df$from, past_edges_df$to)))),"No", "Yes")
  return(cij_df)
}
#recomendations

generate_recomendations <- function(cij_window, slider_cij, nodes_table, colabs_weight_df) {
  
  slider_pairs <- NULL
  slider_comparasion <- NULL
  
  #lets extract the pairs that coincide with the slider requirement
  slider_pairs <- cij_window[cij_window$cij <= slider_cij,]
  #then, we are going to asign the name of the author
  for (j in 1:length(colabs_weight_df$from)) {
    colabs_weight_df$from[j] <- nodes_table$title[nodes_table$id == colabs_weight_df$from[j]]
    colabs_weight_df$to[j] <- nodes_table$title[nodes_table$id == colabs_weight_df$to[j]]
  }
  #now, we are going to compare the pairs and see wich one isnt registered
  #to do so, we have to do a left join
  colnames(slider_pairs) <- c("from", "to", "cij")
  slider_comparasion <- left_join(slider_pairs, colabs_weight_df, by = c("from" = "from", "to" = "to"))
  
  recomendations <- slider_comparasion[is.na(slider_comparasion$V1),]
  recomendations <- recomendations[,c(-4)]
  rownames(recomendations) <- NULL
  colnames(recomendations) <- c("Colaborator 1", "colaborator 2", "Cij value")
  
  confirm_colab <- slider_comparasion[!is.na(slider_comparasion$V1),]
  rownames(confirm_colab) <- NULL
  colnames(confirm_colab) <- c("Colaborator 1", "colaborator 2", "cij_value", "number_of_colabs")
  
  recomendations_list <- list(recomendations, confirm_colab)
  return(recomendations_list)
}

#plot distribution function
distribution_cij <- function(cij_window, slider_cij, nodes_table, colabs_weight_df) {
  
  slider_pairs <- NULL
  slider_comparasion <- NULL
  
  #lets extract the pairs that coincide with the slider requirement
  slider_pairs <- cij_window[cij_window$cij <= slider_cij,]
  #then, we are going to asign the name of the author
  for (j in 1:length(colabs_weight_df$from)) {
    colabs_weight_df$from[j] <- nodes_table$title[nodes_table$id == colabs_weight_df$from[j]]
    colabs_weight_df$to[j] <- nodes_table$title[nodes_table$id == colabs_weight_df$to[j]]
  }
  #now, we are going to compare the pairs and see wich one isnt registered
  #to do so, we have to do a left join
  colnames(slider_pairs) <- c("from", "to", "cij")
  slider_comparasion <- left_join(slider_pairs, colabs_weight_df, by = c("from" = "from", "to" = "to"))
  slider_comparasion[is.na(slider_comparasion)] <- 0
  colnames(slider_comparasion) <- c("from", "to", "cij_value", "number_of_colabs")
  return(slider_comparasion)
}

########### fUNCTIONS STATISTICAL ############

count_papers <- function(window_df) {
  vec_pap <- unique(window_df$id_paper)
  cont_val <- length(vec_pap)
  return(cont_val)
}

papers_x_author <- function(window_df) {
  vec_count <- unique(window_df$id_autor)
  vec_val <- length(vec_count)
  vec_pap <- unique(window_df$id_paper)
  cont_val <- length(vec_pap)
  paps_auth <- cont_val / vec_val
  return(paps_auth)
}

author_x_papers <- function(window_df) {
  vec_count <- unique(window_df$id_autor)
  vec_val <- length(vec_count)
  vec_pap <- unique(window_df$id_paper)
  cont_val <- length(vec_pap)
  auth_paps <- vec_val / cont_val
  return(auth_paps)
}

specify_decimal <- function(x, k) {
  trimws(format(round(x, k), nsmall=k))
}

######## Functions to use with igraph ##################
igraph_network_object <- function(colabs_list, data_id_authors_info_df) {
  
  colabs_df <- NULL
  no_colabs_df <- NULL
  id_number <- NULL
  name_label <- NULL
  department_color <- NULL
  grade <- NULL
  department_label <- NULL
  author_links1 <- NULL
  indexes <- NULL
  papers_window <- NULL
  authors_window <- NULL
  weight_df <- NULL
  linkmatrix <- NULL
  duplicates <- NULL
  
  #lets take the colabs info
  colabs_df <- as.data.frame(colabs_list[1], stringsAsFactors = FALSE)
  #the next two lines helps to visualize the lone dots
  no_colabs_df <- as.data.frame(colabs_list[2], stringsAsFactors = FALSE)
  colabs_df <- rbind(no_colabs_df, colabs_df)
  list_id_nodes <- unique(colabs_df$id_autor)
  list_id_nodes <- list_id_nodes[order(list_id_nodes)]
  
  for (i in 1:length(list_id_nodes)) {
    id_number[i] <- which(data_id_authors_info_df$id_autores == list_id_nodes[i])
    name_label[i] <- data_id_authors_info_df$autor[id_number[i]]
    department_label[i] <- data_id_authors_info_df$departamento[id_number[i]]
    department_color[i] <- data_id_authors_info_df$color[id_number[i]]
    grade[i] <- data_id_authors_info_df$puesto[id_number[i]]
  }
  
  nodes_info <- data.frame(name = id_number, label = name_label, scholarship = grade, department_label = department_label, department_color = department_color, stringsAsFactors = FALSE)
  
  #finished the nodes df, moving to the edges
  colabs_df <- NULL
  colabs_df <- as.data.frame(colabs_list[1], stringsAsFactors = FALSE)
  colabs_df <- colabs_df[with(colabs_df, order(id_autor)), ]
  
  papers_window <- unique(colabs_df$id_paper)
  authors_window <- unique(colabs_df$id_autor)
  
  for(i in 1:length(papers_window)){
    
    indexes <- colabs_df$id_autor[colabs_df$id_paper == papers_window[i]]
    indexes <- sort(indexes)
    linkmatrix <- t(combn(indexes, 2))
    linkmatrix <- rbind(data.frame(from = linkmatrix[, 1], to = linkmatrix[, 2], stringsAsFactors = FALSE))
    author_links1 <- rbind(author_links1, linkmatrix)
    indexes <- NULL
    
  }
  
  rownames(author_links1) <- c()
  
  for (j in 1:length(author_links1$from)) {
    author_links1$from[j] <- as.numeric(gsub("a" ,"", author_links1$from[j]))
    author_links1$to[j] <- as.numeric(gsub("a" ,"", author_links1$to[j]))
  }
  
  weight_df <- ddply(author_links1,.(from,to),nrow)
  
  l <- 1
  
  for (k in 1:length(weight_df$from)) {
    if(weight_df$from[k] == weight_df$to[k]){
      duplicates[l] <- k
      l <- l + 1
    }
  }
  
  if(!is.null(duplicates)) {
    weight_df <- weight_df[-c(duplicates), ]
  }
  
  colnames(weight_df) <- c("from", "to", "weight")
  routes_igraph <- graph_from_data_frame(d = weight_df, vertices = nodes_info, directed = FALSE)
  return(routes_igraph)
}

bipartite_function <- function(window_df) {
  
  bipartite_edges <- NULL
  
  bipartite_edges <- data.frame(from = window_df$autor, to = window_df$paper, stringsAsFactors = FALSE)
  
  bipartite_papers <- NULL
  bipartite_papers <- graph.empty(directed = F)
  
  node.out <- data.frame(id = unique(bipartite_edges$from), stringsAsFactors = FALSE)
  node.in <- data.frame(id = unique(bipartite_edges$to), stringsAsFactors = FALSE)
  node.out <- left_join(node.out, id_autores, by = c("id" = "autor"))
  node.in <- left_join(node.in, id_papers, by = c("id" = "paper"))
  
  bipartite_papers <- add.vertices(bipartite_papers, nv = length(node.out$id), attr=list(name=node.out$id, department = node.out$departamento, job = node.out$puesto, colorDep = node.out$color), type = rep(FALSE, length(node.out)))
  bipartite_papers <- add.vertices(bipartite_papers, nv=length(node.in$id), attr=list(name=node.in$id),type=rep(TRUE,length(node.in)))
  edge.list.vec <- as.vector(t(as.matrix(data.frame(bipartite_edges))))
  bipartite_papers <- add.edges(bipartite_papers,edge.list.vec)
  
  
  return(bipartite_papers)
}

######## SERVER ##################
server <- function(input, output) { 
  ########### Reactive values lecture ############
  rv <- reactiveValues(years_input = NULL)
  
  ########### GET INPUTS ############
  
  observeEvent(input$run_cij, {
    rv$years_input <- c(input$years[1], input$years[2])
  })
  
  observeEvent(input$run_cij_graph, {
    rv$cij_less_move <- input$cij_slider
  })
  
  observeEvent(input$run_adjustment, {
    rv$cij_recomendations_move <- input$cij_adjustment
  })
  
  
  ########### FUNCTIONS Cij AND Vi ############
  generate_cij <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #get cij
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    list_of_authors_window <- authors_window(window_df_bar)
    vi_window_list <- vi_list(id_autores, window_df_bar, id_journals, list_of_authors_window) 
    norm_window <- norm_vector_calc(vi_window_list, list_of_authors_window)
    window_cij_table <- cij_authors_calc(vi_window_list, norm_window, list_of_authors_window, id_autores)
    return(window_cij_table)
  } )
  
  generate_department_cij <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #get cij
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    list_of_authors_window <- authors_window(window_df_bar)
    vi_window_list <- vi_list(id_autores, window_df_bar, id_journals, list_of_authors_window)
    #here we are going to sum all the diferent vectors vi accordingly to a department
    department_list_result <- window_departments(list_of_authors_window, id_autores)
    vi_list_departments <- departments_vi_calc(department_list_result, vi_window_list, id_journals)
    norm_department <- norm_department_vector_calc(vi_list_departments, department_list_result)
    department_cij_table <- cij_department_calc(vi_list_departments, norm_department, department_list_result, id_autores)
    return(department_cij_table)
  } )
  
  visual_vi_table <- reactive( {
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #get cij
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    list_of_authors_window <- authors_window(window_df_bar)
    vi_window_list <- vi_list(id_autores, window_df_bar, id_journals, list_of_authors_window) 
    #get a vi visual
    window_vi_matrix <- vi_matrix_conversion(vi_window_list, id_journals, id_autores, list_of_authors_window)
    colnames(window_vi_matrix)[window_vi_matrix[1, ] != 0]
    return(window_vi_matrix)
  } )
  
  ########### FUNCTIONS VisNetwork ############
  generate_nodes <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #generate time window
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    #separate nodes
    list_colabs <- separate_authors(window_df_bar)
    #creation of df
    nodes <- nodes_network(list_colabs, id_autores)
    return(nodes)
  } )
  
  generate_department_nodes <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #generate time window
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    #separate nodes
    list_colabs <- separate_authors(window_df_bar)
    #creation of df
    nodes <- department_as_nodes(list_colabs, id_departamento, department_pallete)
    return(nodes)
  } )
  
  generate_edges <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #generate time window
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    #separate nodes
    list_colabs <- separate_authors(window_df_bar)
    #generate nodes
    edges <- author_links(list_colabs)
    return(edges)
  } )
  
  generate_department_edges <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #generate time window
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    #separate nodes
    list_colabs <- separate_authors(window_df_bar)
    #generate nodes
    edges <- department_links(list_colabs)
    edges$value <- edges$weight
    return(edges)
  } )
  
  extract_recomendations <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    cij_value <- rv$cij_recomendations_move
    #get cij
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    list_of_authors_window <- authors_window(window_df_bar)
    vi_window_list <- vi_list(id_autores, window_df_bar, id_journals, list_of_authors_window) 
    norm_window <- norm_vector_calc(vi_window_list, list_of_authors_window)
    window_cij_table <- cij_authors_calc(vi_window_list, norm_window, list_of_authors_window, id_autores)
    list_colabs <- separate_authors(window_df_bar)
    #creation of df of colaborations before, need to take the functions nodes and weight to 
    nodes <- nodes_network(list_colabs, id_autores)
    weight_edges <- links_weight(list_colabs)
    colabs_list_table <- generate_recomendations(window_cij_table, cij_value, nodes, weight_edges)
    yes_colab <- as.data.frame(colabs_list_table[1], stringsAsFactors = FALSE)
    past_df <- check_past_colabs(master_publications_limpio_final, init_year)
    if(past_df == "none") {
      yes_colab$past_colaboration <- "no data found before 1959"
      recomendations_present_past <- yes_colab
    } else {
      list_of_past_authors <- authors_window(past_df)
      list_colabs_past <- separate_authors(past_df)
      nodes_past <- nodes_network(list_colabs_past, id_autores)
      weight_past_edges <- links_weight_past(list_colabs_past, nodes_past)
      recomendations_present_past <- count_past_colabs(yes_colab, weight_past_edges)
    }
    
    return(recomendations_present_past)
  } )
  
  extract_colaborating <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    cij_value <- 1
    #get cij
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    list_of_authors_window <- authors_window(window_df_bar)
    vi_window_list <- vi_list(id_autores, window_df_bar, id_journals, list_of_authors_window) 
    norm_window <- norm_vector_calc(vi_window_list, list_of_authors_window)
    window_cij_table <- cij_authors_calc(vi_window_list, norm_window, list_of_authors_window, id_autores)
    list_colabs <- separate_authors(window_df_bar)
    #creation of df
    nodes <- nodes_network(list_colabs, id_autores)
    weight_edges <- links_weight(list_colabs)
    colabs_list_table <- generate_recomendations(window_cij_table, cij_value, nodes, weight_edges)
    no_colaboration <- as.data.frame(colabs_list_table[2], stringsAsFactors = FALSE)
    return(no_colaboration)
  } )
  
  extract_cij_distribution <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    cij_value <- 1
    #get cij
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    list_of_authors_window <- authors_window(window_df_bar)
    vi_window_list <- vi_list(id_autores, window_df_bar, id_journals, list_of_authors_window) 
    norm_window <- norm_vector_calc(vi_window_list, list_of_authors_window)
    window_cij_table <- cij_authors_calc(vi_window_list, norm_window, list_of_authors_window, id_autores)
    list_colabs <- separate_authors(window_df_bar)
    #creation of df
    nodes <- nodes_network(list_colabs, id_autores)
    weight_edges <- links_weight(list_colabs)
    colabs_distribution <- distribution_cij(window_cij_table, cij_value, nodes, weight_edges)
    return(colabs_distribution)
  } )
  
  ########### FUNCTIONS STATISTICALS ############
  
  extract_papers_count <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #calc
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    papers_num <-count_papers(window_df_bar)
    return(papers_num)
  } )
  
  extract_pap_auth <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #calc
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    papers_authors <- papers_x_author(window_df_bar)
    return(papers_authors)
  } )
  
  extract_auth_pap <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #calc
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    authors_papers <- author_x_papers(window_df_bar)
    return(authors_papers)
  } )
  
  ########### FUNCTIONS IGRAPH ############
  extract_igraph_object <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #calc
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    list_colabs <- separate_authors(window_df_bar)
    igraph_network <- igraph_network_object(list_colabs, id_autores)
    return(igraph_network)
  } )
  
  extract_degree <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #calc
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    list_colabs <- separate_authors(window_df_bar)
    igraph_network <- igraph_network_object(list_colabs, id_autores)
    network_network <- asNetwork(igraph_network)
    network_degree <- data.frame(authors = network_network %v% 'label', degree = sna::degree(network_network, gmode="graph"), betweenness = sna::betweenness(network_network, gmode="graph"), stringsAsFactors = FALSE)
    
    return(network_degree)
  } )
  
  extract_bipartite <- reactive( {
    #get inputs
    years_list <- rv$years_input
    init_year <- as.integer(round(years_list[1]))
    last_year <- as.integer(round(years_list[2]))
    #calc
    window_df_bar <- time_network_window(master_publications_limpio_final, init_year, last_year)
    bip_igra <- bipartite_function(window_df_bar)
    V(bip_igra)$color = mapvalues(V(bip_igra)$type, from = c(0, 1), to = c("lightblue", "purple"))
    E(bip_igra)$color = "blue"
    return(bip_igra)
  } )
  
  
  
  
  
  ########### OUTPUTS ############
  
  # Colabs Data #
  output$window_years_show <- renderText( {
    if(is.null(rv$years_input)) return()
    years <- rv$years_input
    years <- paste0(years[1], "-", years[2], " Cij Distribution")
    
    return(years)
  } )
  
  
  output$visnetwork_department <- renderVisNetwork({
    if(is.null(rv$years_input)) return()
    nodes <- generate_nodes()
    edges <- generate_edges()
    visNetwork(nodes, edges)%>% 
      visInteraction(navigationButtons = TRUE)%>%
      visOptions(selectedBy = "department", 
                 highlightNearest = TRUE) %>%
      visPhysics(stabilization = FALSE)
  })
  
  output$visnetwork_author <- renderVisNetwork({
    if(is.null(rv$years_input)) return()
    nodes <- generate_nodes()
    edges <- generate_edges()
    visNetwork(nodes, edges)%>% 
      visInteraction(navigationButtons = TRUE)%>%
      visOptions(selectedBy = "title", 
                 highlightNearest = TRUE)
  })
  
  output$visnetwork_job <- renderVisNetwork({
    if(is.null(rv$years_input)) return()
    nodes <- generate_nodes()
    edges <- generate_edges()
    visNetwork(nodes, edges)%>% 
      visInteraction(navigationButtons = TRUE)%>%
      visOptions(selectedBy = "group", 
                 highlightNearest = TRUE)
  })
  

  
  
  output$vis_dep_net <- renderVisNetwork( {
    if(is.null(rv$years_input)) return()
    nodes <- generate_department_nodes()
    edges <- generate_department_edges()
    visNetwork(nodes, edges)%>% visIgraphLayout(layout = "layout_in_circle") %>%
      visOptions(highlightNearest = list(enabled = T, hover = T), 
                 nodesIdSelection = T)
  } )
  
  output$vis_dep_unestable_net <- renderVisNetwork( {
    if(is.null(rv$years_input)) return()
    nodes <- generate_department_nodes()
    edges <- generate_department_edges()
    visNetwork(nodes, edges)%>% 
      visInteraction(navigationButtons = TRUE)%>%
      visOptions(selectedBy = "title", 
                 highlightNearest = TRUE,
      ) %>%
      visPhysics(stabilization = TRUE)
  } )
}





######## Deploy function ##################
shinyApp(ui, server)