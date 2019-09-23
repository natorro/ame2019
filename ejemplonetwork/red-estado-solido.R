library(readr)
library(visNetwork)
setwd("/Volumes/EXTRASPACE/Dropbox/CURSOS DADOS/AME-2019/ame2019/ejemplonetwork")
nodes<-read_csv("nodes.csv")
edges<-read_csv("links.csv")
nodes$label <- nodes$title

#Con el nombre del tema en cada link (en caso de querer aumentar el tamaño de la letra aumentar 10px y el color cambiar orange)
visNetwork(nodes, edges) %>% 
  visLegend() %>% visEdges(font = "10px arial orange")

