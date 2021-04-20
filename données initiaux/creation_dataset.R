
#Creation de dataset

setwd("C:/Users/ashamza/Downloads/clust/données initiaux")
library(readxl)

#Nous chargeons le fichier synthetique que nous avons crée contenant les indicateurs sociaux
#Ainsi que des fichiers liés aux departements permettant d'agreger les données des departements

indicsociaux <-read.csv2("indicateurs.csv",encoding = "UTF-8")
population <-read.csv2("Population.csv",encoding = "UTF-8")
names(population) <- c("id","departement","population")
names(indicsociaux)[names(indicsociaux) == "X.U.FEFF.Departements"] <- "Departement"
departements_numero<-read.csv2("departements-region.csv",header = TRUE,sep = ",",encoding = "UTF-8")

#Nous chargeons les données liés aux crimes et delits de la police et de la gendarmerie
donneePolice <- read_excel(sheet = 10,"crimes-et-delits-enregistres-par-les-services-de-gendarmerie-et-de-police-depuis-2012.xlsx",col_names = FALSE)
donneeGendarmerie <- read_excel(sheet = 19,"crimes-et-delits-enregistres-par-les-services-de-gendarmerie-et-de-police-depuis-2012.xlsx",col_names = FALSE)

#Nous convertisons les numero de deparetment en numeric
departements_numero$num_dep <- sapply(departements_numero$num_dep,as.numeric)

#Nous enlevons les lignes et colonnes inutiles
donneePolice <- rbind(donneePolice[1,],donneePolice[4:nrow(donneePolice),])
donneePolice <- cbind(donneePolice[,2:ncol(donneePolice)])

donneeGendarmerie <- rbind(donneeGendarmerie[1,],donneeGendarmerie[3:nrow(donneeGendarmerie),])
donneeGendarmerie <- cbind(donneeGendarmerie[,2:ncol(donneeGendarmerie)])

#Nous transposer les données
donneePolice2 <- t(donneePolice)
donneeGendarmerie2 <- t(donneeGendarmerie)

#Nous definissons les indexs
colnames(donneePolice2) <- donneePolice2[1,]
rownames(donneePolice2) <- NULL
donneePolice2 <- donneePolice2[-c(1),]

colnames(donneeGendarmerie2) <- donneeGendarmerie2[1,]
rownames(donneeGendarmerie2) <- NULL
donneeGendarmerie2 <- donneeGendarmerie2[-c(1),]

#Nous transformer toutes les cellules des dataframes en numeric puis
#Nous agregeons par numero de departement
donneePolice2 <- as.data.frame(donneePolice2)

donneePolice3 <- as.data.frame(sapply(donneePolice2,as.numeric))
donneePolice3 <- aggregate(cbind(donneePolice3[,2:ncol(donneePolice3)]),by=list(Départements=donneePolice3$Départements),FUN=sum)

donneeGendarmerie2 <- as.data.frame(donneeGendarmerie2)

donneeGendarmerie3 <- as.data.frame(sapply(donneeGendarmerie2,as.numeric))
donneeGendarmerie3 <- aggregate(cbind(donneeGendarmerie3[,2:ncol(donneeGendarmerie3)]),by=list(Départements=donneeGendarmerie3$Départements),FUN=sum)

#Nous testons si des departements sont present dans un dataframe et pas dans l'autre
var <- setdiff( donneeGendarmerie3[,1],donneePolice3[,1])

#Nous les rajoutons manuellement dans donneesgendarmerie et donneespolice
new_row1 <- c(75,rep(0,107))
new_row2 <- c(90,rep(0,107))
new_row3 <- c(92,rep(0,107))
new_row4 <- c(93,rep(0,107))
new_row5 <- c(94,rep(0,107))
new_row6 <- c(976,rep(0,107))
new_row7 <- c(978,rep(0,107))


donneeGendarmerie3 <- rbind(donneeGendarmerie3,new_row1,new_row2,new_row3,new_row4,new_row5,new_row6)
donneePolice3 <- rbind(donneePolice3,new_row7)

#Nous trieons ces deux dataframes
donneeGendarmerie3 <- donneeGendarmerie3[order(donneeGendarmerie3$Départements),]
donneePolice3 <- donneePolice3[order(donneePolice3$Départements),]

#Nous les transformons en matrices
table1 <- as.table(as.matrix (donneePolice3[,2:ncol(donneePolice3)]))
table2 <- as.table(as.matrix (donneeGendarmerie3[,2:ncol(donneeGendarmerie3)]))

#Nous les sommons pour obtenir le total pour chaque crimes et delits par departement
table3 <- table1 + table2

#Nous retransformons la matrice en dataframe
mat <- matrix(table3, ncol=107, nrow=102)

table4 <- as.data.frame(mat)

#Nous recreons les indexs colnames et rownames
rownames(table4) <- NULL
colnames(table4) <- NULL

col <- colnames(donneeGendarmerie2)
row <- donneePolice3[,1]
  
colnames(table4) <- col[2:108]
table4$Départements <- row


#Nous rajoutons le nom des departements
tabmerge <- merge(departements_numero,table4,
      by.y = "Départements", by.x ="num_dep")

#Nous rajoutons la population des departements
tabmerge <- merge(tabmerge,population,
                  by.y = "id", by.x ="num_dep")

#Nous creons le dataframe final en groupant les variables(crimes ou delits) qui
#sont similaires , Nous passons ainsi de +100 variables a 10 synthetiques

tabfinal <- cbind(tabmerge[,1:3])
tabfinal$Homicide_et_tentative <- rowSums(tabmerge[,c(5,6,7,8,54)])
tabfinal$Coups_et_blessures <- rowSums(tabmerge[,c(4,9,10)])
tabfinal$Prisedotage_Sequestration_Attentat <- rowSums(tabmerge[,c(11,12,13,67,68)])
tabfinal$violences_Menaces <- rowSums(tabmerge[,c(14,15,76,55,75)])
tabfinal$viol_Harcelements <- rowSums(tabmerge[,c(48:53)])
tabfinal$Ventes_usage_stupefiant <- rowSums(tabmerge[,c(58:61)])
tabfinal$Falsification_escroquerie <- rowSums(tabmerge[,c(84:95)])
tabfinal$Destruction_Degradation <- rowSums(tabmerge[,c(65,66,70,71,72,81,82)])
tabfinal$Fraudes_Delitseconomiques <- rowSums(tabmerge[,c(63,108,109)])
tabfinal$Vols_Cambriolages <- rowSums(tabmerge[,c(18:47)])
tabfinal$Port_et_Detention_Armes_prohibe <- tabmerge[,77]
tabfinal$Population <- tabmerge[,112]
rownames(tabfinal) <- tabfinal[,2]
tabfinal <- tabfinal[,-c(1,2,3)]
tabfinal$Taux_Criminalité <- ((rowSums(tabfinal[,c(1:11)])/tabfinal[,12])*1000)

#Nous fusionnons puis creons le dataset final
dataset <- cbind(indicsociaux[,1],tabfinal[,1:11],indicsociaux[,2:14])
names(dataset)[names(dataset) == "indicsociaux[, 1]"] <- "Departement"

#Nous l'exportons
write.csv2(dataset,"../dataset.csv", row.names = FALSE)

