
neur <- read.csv('neur.csv')
Y<- as.matrix(neur)
fa<-factanal(Y,factors=2,scores="regression")
fs<-fa$scores
#plot(fs[,1],fs[,2],xlab="X-component of Velocity",ylab="Y-component of Velocity",main = "Intended Velocities")

#Do k means clustering on these two components. 
xcoord <- fs[,1]
ycoord <- fs[,2]

find_closest<-function(x,y,x_centers,y_centers)
{
    min = Inf
    min_pos = -1
    for (i in c(1:length(x_centers))){
        distance = (x_centers[i]-x)^2 +(y_centers[i]-y)^2
        if(distance<min){min = distance
                         min_pos = i}
    }
    return(min_pos)
}
kmeans2d<- function(n, iter, datax, datay){
    len <- length(datax)
    #This should be equal to datay
    #Generate n random (x,y) points selected from the data - initialization
    centers<- sample(c(1:len),n,replace=F)
    center_x<-c()
    center_y<-c()
    for (index in centers)
     {
        center_x<- c(center_x,datax[index])
        center_y<- c(center_y,datay[index])
    }
    #run k-means iteratively
    for(t in c(1:iter)){
        positions <- c()
        for(i in c(1:len))
        {
            positions <- c(positions,find_closest(datax[i],datay[i],center_x,center_y))
        }
        counts = rep(c(0),n)
        center_x<-rep(c(0),n)
        center_y<-rep(c(0),n)
        for(i in c(1:n))
        {
            true_vec = (positions==i)
            center_x[i]<-sum(datax[true_vec])/sum(true_vec)
            center_y[i]<-sum(datay[true_vec])/sum(true_vec)
        }
    }
    return (positions)
}
#position vector for each point , e.g [1,3,4,...], cluster 1, cluster 3, cluster 4...
pos<-kmeans2d(8,50,xcoord,ycoord)
#Plot (x,y) points, change color based on which cluster they belong to. 
plot(fs[,1],fs[,2],xlab="X-component of Velocity",ylab="Y-component of Velocity",main = "Intended Velocities",col=pos)



