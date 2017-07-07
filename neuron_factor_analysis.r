
neur <- read.csv('neur.csv')
head(neur)

Y<- as.matrix(neur)

##Problem 2
fa<-factanal(Y,factors=2,scores="regression")
fa

#Factor Scores  (Intended Velocities)
fs<-fa$scores
plot(fs[,1],fs[,2],xlab="X-component of Velocity",ylab="Y-component of Velocity",main = "Intended Velocities")


b<- as.matrix(fa$loadings)
# B contains factor loadings, i.e preferred directions
plot(b[,1],b[,2],xlab = "x component", ylab = "ycomponent", main = "Preferred Directions for 96 neurons")
#complicated code for positioning the numbers - making sure the graph is pretty
pos_vec = rep(c(4),96)
pos_vec[c(10,14,90,35,83,47,73,88)]=1
pos_vec[c(18,60,28,25,92,40,71,31,85,61)]=3
pos_vec[c(15,30,76,80,87)]=2
text(b[,1],b[,2],labels = c(1:96),cex=0.7,pos=pos_vec)


#Plot the angle - arctangent of the y component of intended velocity (fs[,2]) over the x component (fs[,1])
plot(c(1:65),atan(fs[,2]/fs[,1])[c(1:65)],xlab = "Time(x100ms)", ylab = "Angle (Theta)", main = "Plot of Angle vs. Time")


