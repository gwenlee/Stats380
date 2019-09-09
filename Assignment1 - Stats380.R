#Question1
#1(a)

#(i)
rep(7:9,length.out=10)

#(ii)
rep(0:4,4)[2:16]

#(iii)
seq(pi/2, 6*pi, length.out = 12)

#(iv)
(0.2^seq(3, 39, 3)*(0.8^seq(1, 37, 3)))

#(v) 
1+sum(cumprod(seq(2,38,by=2)/seq(3,39,by=2)))

#(b)
x=numeric(0)
sum(exp(seq(along=x)))


#Question2
#(a)


pdf_ = function(x,a,b,c) {
if(x<a || x>b) {
  0
} else if (x>=a && x<c) {
  (2*(x-a))/((b-a)*(c-a))
} else if (x==c) {
  2/(b-a)
} else if (x>c && x<=b){
  (2*(b-x))/((b-a)*(b-c))
}else {
  1
  }
}

pdf_(2.5,2,8,5)

#(b) 
#Vectorize

vector= seq(-1,10, by=0.5)

pdf_2 = function(vector, a ,b, c){
  len = numeric(length(vector))
  for(i in 1:length(vector))
    len[i] = pdf_(vector[i],a, b, c)
  len 
}


pdf_2(vector, 2,8,5)

#(c)
#Vectorize
sapply(vector, pdf_, a=2, b=8, c=5)

#(d)
#potential problem: 
#Process may take a lot of times if they increase the size of data. Especailly, when we want to use for...loops it may take longer than sapply in general.
#Plus both functions are highly rely on pdf_ function so any changes on its function will be critical to output of both functions.


#Question 3

#(a)

path=function(n){
  x=c(-1,1)
  b=sample(x,n,replace=TRUE)
  a=c(0,cumsum(b))
  a
}


d=path(n)
d

#(b)

step=function(d){
  sum(tabulate(d))
}

step(d)