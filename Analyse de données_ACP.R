#definition du repertoire courant 
setwd('C:/Users/luc-a/Documents')

#importation des données
wine_red=read.csv("winequality-red.csv",sep=";")
wine_white=read.csv("winequality-white.csv",sep=";")
crime=read.csv("crime.csv")

#################### ACP Wine_red #############################
#Analyse descriptive des données
summary(wine_red)
sapply(wine_red,sd) #ecart-type

#ACP
#install.packages("FactoMineR")
#install.packages("factoextra")
library(FactoMineR)
library(factoextra)
res.pca=PCA(wine_red, scale.unit = TRUE, ncp = 5, graph =FALSE,quanti.sup =12)
res.pca

#Valeurs propres / Variances
eig.val=get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

##### Graphique des variables #######
var <- get_pca_var(res.pca)
var
# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)

#cercle de correlation
fviz_pca_var(res.pca, col.var = "black")

# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)


#### Graphique des individus #######
ind <- get_pca_ind(res.pca)
ind
# Coordonnées des individus
head(ind$coord)
# Qualité des individus
head(ind$cos2)
# Contributions des individus
head(ind$contrib)

#graphique
fviz_pca_ind (res.pca,geom="point")
# couleur en fonction du cos2
fviz_pca_ind (res.pca,geom="point", col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)




#################### ACP Wine_white #############################
#Analyse descriptive des données
summary(wine_white)
sapply(wine_white,sd) #ecart-type

#ACP
#install.packages("FactoMineR")
#install.packages("factoextra")
library(FactoMineR)
library(factoextra)
res.pca1=PCA(wine_white, scale.unit = TRUE, ncp = 5, graph =FALSE,quanti.sup =12)
res.pca1

#Valeurs propres / Variances
eig.val=get_eigenvalue(res.pca1)
eig.val
fviz_eig(res.pca1, addlabels = TRUE, ylim = c(0, 50))

##### Graphique des variables #######
var <- get_pca_var(res.pca1)
var
# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)

#cercle de correlation
fviz_pca_var(res.pca1, col.var = "black")

# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)


#### Graphique des individus #######
ind <- get_pca_ind(res.pca1)
ind
# Coordonnées des individus
head(ind$coord)
# Qualité des individus
head(ind$cos2)
# Contributions des individus
head(ind$contrib)

#graphique
fviz_pca_ind (res.pca1,geom="point")
# couleur en fonction du cos2
fviz_pca_ind (res.pca1,geom="point", col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)



#################### ACP crime #############################

row.names(crime)=crime$communityname.string #erreur ligne non unique
length(unique(crime$communityname.string))
crime$communityname.string[duplicated(crime$communityname.string)]
View(crime[crime$communityname.string=="Hendersoncity",])

#Gestion des duplicats les duplicats
names_duplicats=crime$communityname.string[duplicated(crime$communityname.string)]
for (var in names_duplicats){
  crime[crime$communityname.string==var,1]=communityname.string=paste(crime[crime$communityname.string==var,1],1:length(crime[crime$communityname.string==var,1]))
}

row.names(crime)=crime$communityname.string
crime=crime[,-1]
#verifications
library(dplyr)
View(crime %>% filter(grepl("Hendersoncity",communityname.string)))


#Analyse descriptive des données
summary(crime)
sum(is.na(crime)) #valeursmanquantes
sapply(crime,sd)

res.pca1=PCA(crime, scale.unit = TRUE, ncp = 5, graph =FALSE)
res.pca1

Valeurs propres / Variances
eig.val=get_eigenvalue(res.pca1)
eig.val
fviz_eig(res.pca1, addlabels = TRUE, ylim = c(0, 50))

##### Graphique des variables #######
var <- get_pca_var(res.pca1)
var
# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)

#cercle de correlation
fviz_pca_var(res.pca1, col.var = "black")

# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)


#### Graphique des individus #######
ind <- get_pca_ind(res.pca1)
ind
# Coordonnées des individus
head(ind$coord)
# Qualité des individus
head(ind$cos2)
# Contributions des individus
head(ind$contrib)

#graphique
fviz_pca_ind (res.pca1,geom="point")
# couleur en fonction du cos2
fviz_pca_ind (res.pca1,geom="point", col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)


#regrouper les variables 
#distance 
D_coef=get_dist(t(crime),method="pearson")
D=as.dist(1-cor(crime))

D_1=as.data.frame(D_coef)
sum(is.na(D))

cah<-hclust(D,method="average")
plot(cah)
d=rect.hclust(cah, 10)
cl=cutree(cah,10)
crimevariables=crimevariables[-c(1,2),]

for (i in 1:10)
groupe4=names(d[[4]])
Groupe4=crimevariables %>% filter(crimevariables$X.1 %in% groupe4)

               
groupe5=names(d[[5]])
Groupe5=crimevariables %>% filter(crimevariables$X.1 %in% groupe5)

groupe6=names(d[[6]])
Groupe6=crimevariables %>% filter(crimevariables$X.1 %in% groupe6)

groupe10=names(d[[10]])
Groupe10=crimevariables %>% filter(crimevariables$X.1 %in% groupe10)

groupe7=names(d[[7]])
Groupe7=crimevariables %>% filter(crimevariables$X.1 %in% groupe7)

groupe1=names(d[[1]])
Groupe1=crimevariables %>% filter(crimevariables$X.1 %in% groupe1)


cl=as.factor(cl)
fviz_pca_var(res.pca1,
             geom="arrow",
             col.var=cl,
             legend.title = "Groups",
             ellipse.type="convex",
             addEllipses = TRUE,
)


###################### Classifification sur les individus ####################################
#install.packages("cluster")
library(cluster)
library(FactoMineR)
library(factoextra)

#first method
crime
classif=agnes(crime,method="ward")
plot(classif,which.plots = 2)

#CAH WARD
D_coef=get_dist(crime,method="euclidean")

cah_ward<-hclust(D_coef,method="ward.D")
plot(cah_ward)
d=rect.hclust(cah_ward,5)
cl=cutree(cah_ward,5)

cl=as.factor(cl)
fviz_pca_ind(res.pca1,
             axes=c(1,3),
             geom="point",
             col.ind=cl,
             legend.title = "Groups",
             ellipse.type="convex",
             addEllipses = TRUE,
)

#CAH WARD
D_coef=get_dist(crime,method="euclidean")

cah_ward<-hclust(D_coef,method="ward.D")
plot(cah_ward)
d=rect.hclust(cah_ward,5)
cl=cutree(cah_ward,5)
cl=as.factor(cl)
fviz_pca_ind(res.pca1,
             axes=c(1,2),
             geom="point",
             col.ind=cl,
             legend.title = "Groups",
             ellipse.type="convex",
             addEllipses = TRUE,
)

#CAH Average
cah_average<-hclust(D_coef,method="average")
plot(cah_average)
d=rect.hclust(cah_average,5)
cl=cutree(cah_average,5)

cl=as.factor(cl)
fviz_pca_ind(res.pca1,
             axes=c(1,2),
             geom="point",
             col.ind=cl,
             legend.title = "Groups",
             ellipse.type="convex",
             addEllipses = TRUE,
)


#CAH Single
cah_single<-hclust(D_coef,method="single")
plot(cah_single)
d=rect.hclust(cah_single,5)
cl=cutree(cah_single,5)
cl=as.factor(cl)
fviz_pca_ind(res.pca1,
             axes=c(1,2),
             geom="point",
             col.ind=cl,
             legend.title = "Groups",
             ellipse.type="convex",
             addEllipses = TRUE,
)

#CAH max
cah_max<-hclust(D_coef,method="complete")
plot(cah_max)
d=rect.hclust(cah_max,5)
cl=cutree(cah_max,5)
cl=as.factor(cl)
fviz_pca_ind(res.pca1,
             axes=c(1,2),
             geom="point",
             col.ind=cl,
             legend.title = "Groups",
             ellipse.type="convex",
             addEllipses = TRUE,
)


# Cah sur les resultats des 5 premieres axes
new_data=res.pca1$ind$coord[,1:5]
D_coef=get_dist(new_data,method="euclidean")

cah_ward<-hclust(D_coef,method="ward.D")
plot(cah_ward)
d=rect.hclust(cah_ward,5)
cl=cutree(cah_ward,5)

cl=as.factor(cl)
a=fviz_pca_ind(res.pca1,
             axes=c(1,2),
             geom="point",
             col.ind=cl,
             legend.title = "Groups",
             ellipse.type="convex",
             addEllipses = TRUE,
             title="avec dimension ACP, Methode=ward")

# Cah sur les resultats des 5 premieres axes (average)
new_data=res.pca1$ind$coord[,1:5]
D_coef=get_dist(new_data,method="euclidean")
plot(cah_average)
d=rect.hclust(cah_average,5)
cl=cutree(cah_average,5)

cl=as.factor(cl)
b=fviz_pca_ind(res.pca1,
             axes=c(1,2),
             geom="point",
             col.ind=cl,
             legend.title = "Groups",
             ellipse.type="convex",
             addEllipses = TRUE,
             title="Avec Dimension ACP, Methode=Average"
)

# Cah sur les resultats des 5 premieres axes (single)
new_data=res.pca1$ind$coord[,1:5]
D_coef=get_dist(new_data,method="euclidean")
plot(cah_single)
d=rect.hclust(cah_single,5)
cl=cutree(cah_single,5)
cl=as.factor(cl)
c=fviz_pca_ind(res.pca1,
             axes=c(1,2),
             geom="point",
             col.ind=cl,
             legend.title = "Groups",
             ellipse.type="convex",
             addEllipses = TRUE,
             title="Avec Dimension ACP, Methode=Single"
)



#Cah sur les resultats des 5 premieres axes (max)
new_data=res.pca1$ind$coord[,1:5]
D_coef=get_dist(new_data,method="euclidean")
cah_max<-hclust(D_coef,method="complete")
plot(cah_max)
d=rect.hclust(cah_max,5)
cl=cutree(cah_max,5)
cl=as.factor(cl)
d=fviz_pca_ind(res.pca1,
             axes=c(1,2),
             geom="point",
             col.ind=cl,
             legend.title = "Groups",
             ellipse.type="convex",
             addEllipses = TRUE,
             title="Avec Dimension ACP, Methode=max"
)
library("cowplot")
plot_grid(a, b,c,d, ncol = 2, nrow = 2)
# classification Kmeans 




