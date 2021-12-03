###########TP1 - ACP  ############

library(rgl)
library(FactoMineR)
library(factoextra)
library(plot3D)
library(car)
library(ggplot2)
library(MASS)
library(gridExtra)
library(cowplot)
### Question 1

data(survey)
help(survey)
summary(survey)                                

#%%%%%%%% Partie I: ACP à la main %%%%%%%%#


### Question 2 ###


base.0=survey[,c(1,2,3,10)]
head(base.0)

### Question 3 ###

base.1=na.omit(base.0)
# on peut utiliser aussi : base.1= base.0[complete.cases(base.0), ]
# on vérifie les dimensions du tableau dans l'environnement
head(base.1)

### Question 4 ###

sexe =base.1$Sex
couleur= ifelse(base.1$Sex=="Male","blue","red")
### Question 5 ###
X=base.1[,c(2,3,4)]
head(X)
plot(X,col=couleur,pch=16)

### Question 6 ###
#Chaque individu est caractérisé par trois variables, soit par un point dans R3.
#La fonction plot3d() de la bibliothèque rgl nous permet d'explorer facilement
#un nuage de points en 3 dimensions.
plot3d(X, type = "s", col = couleur, size = 2)
#a) On a l'impression que les distances entres les points changent ...
#b) La représentation graphique précédente est trompeuse (malgrès que les variables ont
#la même unité) parce que nous n'avons pas utilisé la même échelle en x, y et z.
# Refaites une autre en imposant une échelle commune aux trois axes. Que constatez-vous ?
lims <- range(X)
open3d()
plot3d(X, type = "s", col = couleur, xlim = lims, ylim = lims, zlim = lims,size = 2)
#Voici donc à quoi ressemblent réellement les données. On voit tout de suite le problème,
#comme les tailles sont en moyenne beaucoup plus grandes (u3=172,38 cm)
#que les empans (u1 =18.74 et u2 = 18,67 cm), les points se trouvent complètement collés
#sur le plan des empans.
mean(X$Wr.Hnd)
mean(X$NW.Hnd)
mean(X$Height)
#Nous allons voir comment remédier à cette situation.

###Question 7 ###

X.c=scale(X,center = TRUE,scale = FALSE)
lims <- range(X.c)
open3d()
plot3d(X.c, type = "s", col = couleur, xlim = lims, ylim = lims, zlim = lims, size=2)
# Est-ce que ça va ?  C'est mieux. Le nuage est maintenant centré autour de l'origine mais
#il a l'aspect d'une galette complètement aplatie.
#la variabilité est beaucoup plus forte pour la taille (sigma1 = 9.87 cm) que pour les
#empans(sigma2=1.90 sigma3= 1.98 cm)
sd(X$Wr.Hnd)
sd(X$NW.Hnd)
sd(X$Height)
#Quel remède proposez-vous à ce problème ? Réduire !

### Question 8 ###

X.cr=scale(X,center=TRUE,scale=TRUE)
# ATTENTION ! "scale" considère que l'écart-type est estimé à partir d'un échantillon!
lims <- range(X.cr)
open3d()
plot3d(X.cr, type = "s", col = couleur, xlim = lims, ylim = lims, zlim = lims, size=2)

#Quand on fait une ACP normée, on travaille avec les données après centrage et
#réduction. Il est donc important de bien comprendre à quoi correspondent ces
#opérations.

### Question 9 ###

# lorsque les données sont centrées réduites, la forme générale du nuage de points est celui d'une dragée
#(le terme technique est un ellipsoïde).
#À partir de la matrice de corrélation, pour mieux visualiser, nous pouvons définir une régionde confiance à 95% (équivalent IC en analyse unidimensionnelle)
#(c'est-à-dire que, sous l'hypothèse de normalité des données, moins de 5% des données seront en dehors de l'ellipsoïde).
#pour calculer la matrice de corrélation
R=cor(X.cr)
R
plot3d(ellipse3d(R), col = "grey",alpha=0.50,add = TRUE,level=0.95)
#alpha: désigne le degrès de transparence

### Question 10 ###

#Si vous aviez à choisir entre les trois plans que nous avons envisagés ci-dessus,
#vous n'hésiteriez pas à prendre le plan défini par les deux premiers axes de la
#dragée parce que c'est dans cette représentation que l'on a le moins de perte
#d'information par rapport au nuage de points dans R3 : c'est dans cette projection
#que les points sont les plus étalés dans le plan (on dit aussi que l'on a conservé
#le maximum possible de l'inertie initiale du nuage de points). Ce
#faisant, vous avez en fait réalisé une ACP à la main.

### Question 11 ###

#Symétries des solutions:
#Nous allons faire ici une remarque qui est valable pour toutes les méthodes
#d'analyse multivariées quand on cherche à interpréter un plan factoriel.
#Toutes ces représentation sont parfaitement équivalente du point de vue de la minimisation de la perte d'information par rapport au nuage de points initial dans R3
#Ceci est la traduction graphique du fait que si u est le vecteur propre associé à la valeur propre ??
# de la matrice R, alors automatiquement ???u est également un vecteur propre :  Ru=??u <=> R(-u)=??(-u)
#ce qu'il faut retenir c'est que l'orientation des axes des analyses multivariées n'a pas de sens puisqu'elle est arbitraire. Les algorithmes utilisés pour calculer
#les vecteurs propres sont de type itératif et alternent entre u et -u. Le critère de convergence sera atteint à un moment qui est fonction de la précision des
#calculs et peut donc différer d'un ordinateur à l'autre

