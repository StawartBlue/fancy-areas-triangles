#Using the trig formula for the area of a triangle, 
#we find the values that have integer sides, angles
#and almost integer or half integer areas.
ls()
rm(list=ls())

Area <- function(a,b,C) {
  theta <- pi/180*C
  0.5*a*b*sin(theta)
}

Area(1,2,30)

NiceQ <- function(x){
  abs(2*x-round(2*x))<0.03
}

NiceQ(2.52)

GenerateAreas <- function(n) {
  m <- NULL
  for(C in 10:70)
    for(a in 1:n)
      for(b in a:n) {
        check <- Area(a,b,C) 
        if(NiceQ(check)) m <- rbind(m,c(a,b,C,round(2*check)/2))
      }
  colnames(m) <- c("a","b","C","Area")
  m
}

GenerateAreas(5)

Area(3,3,13)

LawOfCosines <- function(a,b,C) {
  theta <- C*pi/180
  sqrt(a^2+b^2-2*a*b*cos(theta))
}

LawOfCosines(4,5,37)

NiceForm <- function(x) {
  round(2*x)/2
}

NiceForm(2.49)

PrettyQ <- function(a,b,C) {
  NiceQ(Area(a,b,C)) & NiceQ(LawOfCosines(a,b,C))
}

PrettyQ(4,5,36.9)

NewRow <- function(a,b,C) {
  c(C,a,b,NiceForm(LawOfCosines(a,b,C)),NiceForm(Area(a,b,C)))
}

NewRow(4,5,37)

PrettyTriangles <- function(n) {
  m <- NULL
  for(C in 10:70)
    for(a in 1:n)
      for(b in a:n) {
        if(PrettyQ(a,b,C)) {
          m <- rbind(m,NewRow(a,b,C))
        }
      }
  colnames(m) <- c("C","a","b","c","Area")
  m
}
PrettyTriangles(10)

triangle_list <- PrettyTriangles(10)

getwd()
write.csv(triangle_list,"triangles, nice areas.csv")
