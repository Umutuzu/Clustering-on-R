library(factoextra)
library(cluster)
library(ggplot2)
library(clValid)

#Veri ekleme
data <- read.csv(file.choose())

#Veri dC<zenleme

data <- data[,-c(1,2)]
data <- na.omit(data)
st.data <- scale(data)


#Optimal KC<me SayD1sD1 Belirleme

fviz_nbclust(st.data,kmeans,"wss",linecolor = "blue")+
  theme(axis.text = element_text(size=14),element_line(size = 14),
        panel.background = element_rect("lightgray"))

fviz_nbclust(st.data,pam,"silhouette",
          linecolor = "blue")+
  theme(axis.text = element_text(size=14),element_line(size = 14),
        panel.background = element_rect("lightgray"))


#Kmeans

km.res <- kmeans(st.data,2)
km.res1 <- kmeans(st.data,4)
km.res2 <- kmeans(st.data,6)


fviz_cluster(km.res,data=data)
fviz_cluster(km.res1,data=data)
fviz_cluster(km.res2,data=data)

#PAM

pamk_2 <- pam(st.data,2)
pamk_4 <- pam(st.data,4)
pamk_6 <- pam(st.data,6)


fviz_cluster(pamk_2,data=data)
fviz_cluster(pamk_4,data=data)
fviz_cluster(pamk_6,data=data)


#HiyerarEik

agnes. <- agnes(st.data,metric = "euclidean", method = "ward")
diana. <- diana(st.data,metric = "euclidean")

fviz_dend(agnes.,k=4)
fviz_dend(diana.,k=4)

agnes.cl <- cutree(agnes., k=4)
diana.cl <- cutree(diana., k=4)


fviz_cluster(list(data=st.data,cluster=agnes.cl),
             show.clust.cent=T)

fviz_cluster(list(data=st.data,cluster=diana.cl),
             show.clust.cent=T)




#KC<meleme D0Elemleri KarED1laEtD1rma 
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(st.data, nClust = 4,
                  clMethods = clmethods, validation = "internal")
summary(intern)


stab<-clValid(st.data,nClust=4,clMethods=clmethods,
              validation="stability")
summary(stab)





