#### some silly functions, conditionals and loops to demonstrate their structure in R
sum.of.squares <- function(x,y) {#paste in demo function that calculates sum of squares
  x^2 + y^2 #add square of x to square of y
}

sum.of.squares(3,4)#demo sum of squares function

iszero<-function(number){#create a function that determines if a value is zero
  if(number==0){
    print("yes")#if the number is zero, print yes
  }else{
    print("no")#otherwise print no
  }
}

iszero("cherry pie")#demonstrate iszero function

vec<-c(1,2,3,0)#define a test vector for demonstrating a function on a loop

for(i in 1:length(vec)){#demonstrate a simple loop 
  iszero(vec[i])#is each item in the vector zero?
}