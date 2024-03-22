#Problem 1
A = matrix( c(5, 1, 0,
            + 3,-1, 2), nrow=2, byrow=TRUE)


B = matrix( c(2, 9, 6,
              + 7,-3, 2), nrow=2, byrow=TRUE)
print(A)
print(B)
C=A+B
print(C)
D=A-B
print(D)
E=A%*%t(B)
print(E)
I=solve(E)
print(I)
F=cbind(A,B)
print(F)

#problem 2
m1=matrix(1:10,4,4)
m1[,3]
m1[2,]
#extracting a submatrix who has entries > 8
submat=c()
check=c()
for (i in 1:4){
  check=c(m1[i,]>8)
  for (j in 1:4){
    if (check[j]){
      submat=rbind(submat,m1[i,])
       break}
  }
}
print(submat)



#problem 3
hmdf <- data.frame( ID=c("John","Hayley","Tim","Frank"),
                    Cancertype=c("Prostrate","Breast","Bladder","Kidney"),
                    Age=c(56,72,76,98), 
                    Ageofonset=c(28,53,44,75) )
YearSonset= hmdf[,3]-hmdf[,4]

#problem 4
l1 <- list(course=c("combinatorics","dyanamical system","networks","Algebra"),numbers=1:4,matrix=A) 
l2 = list(logicalvalues=c("True","False"))
l3=list(l1,l2,students=c("arthure","Fadi","Abdul"))
#problem 5
x1=rnorm(50, mean = 1, sd = 1)
y1=rnorm(50, mean = 1, sd = 1)
x2=rnorm(50, mean = 2, sd = 1)
y2=rnorm(50, mean = 2, sd = 1)
plot(x1,y1,col="red",lty=1, pch=19,xlim=c(-5,5),ylim=c(-1,5))
par(new=TRUE)
plot(x2,y2,col="blue",lty=2, pch=19,xlim=c(-5,5),ylim=c(-1,5))

#problem 6
x=seq(-5,5,length.out = 100)
plot(x,exp(x),col="red",type="l", lty=1, pch=19,xlim=c(-5,5),ylim=c(-1,5))
par(new=TRUE)
lines(x,sin(x),col="blue",type="l",lty=2, pch=19,xlim=c(-5,5),ylim=c(-1,5))
legend("topright",legend=c("e^x","sin(x)"),col=c("red","blue"),lty=1:2)

#Problem 7
n=200
cube_set=c()
for (i in 2:n){
  for (j in 2:n^(1/3)){ 
    if (i==(j^3)){        #check if i equals the cube of j
      cube_set=c(cube_set,i)  #if yes, then add i into the set
    }
  }
}
length(cube_set)

#Problem 8

self_div=c()
for (x in 10:50){
  ones=(x/10-trunc(x/10))*10
  tens = trunc(x/10)
  if (ones!=0){
    if (x%%ones==0 & x%%tens==0){
      print(x)
      self_div=c(self_div,x)
      
    }
  }
  
}
 print(self_div)

#part 2
install.packages("igraph")
library(igraph)
gmat690st <- graph_from_literal(Fadi-Mitchell:Stephanie:Justin:Quentin:Ryan:Abdul, Jillian-Shaughn:Mitchell:Christian:Justin, Arthur-Janathon:Ye, Shaughn-Mitchell:Christian:Stephanie, Mitchell-Christian:Stephanie:Justin:Quentin:Ryan:Bright, Christian-Ryan, Stephanie-Quentin:Ryan:Bright, Justin-Ryan:Bright:Ye, Xavier-Quentin, Quentin-Ryan:Abdul, Bright-Abdul, Ryan, Abdul, Janathon) #h is linked to every vertex in the set {c,f,i}
plot(gmat690st)
V(gmat690st)$gender <- c("male","male","female","male","male","male","male","female","male","male","male","male","male","male","female")
V(gmat690st)$year <- c("2nd","1st","1st","1st","1st","2nd","2nd","1st","2nd","2nd","2nd","2nd","1st")
plot(gmat690st, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5,
     vertex.color=c( "pink", "skyblue")[1+(V(gmat690st)$gender=="male")] )

plot(gmat690st, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5,
         vertex.color=c( "green", "red")[1+(V(gmat690st)$year=="1st")] )


#part 3
l1 <- layout_with_fr(gmat690st)
plot(gmat690st, layout=l1)

l2 <- layout_with_drl(gmat690st)
plot(gmat690st, layout=l2)


l3 <- layout_on_sphere(gmat690st)
 plot(gmat690st, layout=l3)



