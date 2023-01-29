# Quant_Methods_HW

#' ---
#' title: HW2
#' author: Briar Ownby-Connolly
#' date: ' `r paste("created on", Sys.Date())`'
#' output: html_document
#' ---
#' 

data(iris)
head(iris)
sp_ids <- unique(iris$Speices)
output <- matrix(0, nrow=length(sp_ids), ncol=ncol(iris)-1)
rownames(trait_average) <- sp_ids
colnames(trait_average) <- names(iris[ , -ncol(iris)])

for(i in seq_along(sp_ids)) {
    iris_sp <- subset(iris, subset=Species == sp_ids[i], select=-Species)
# loop through traits
    for(j in 1:(ncol(iris_sp))) {
        x <- 0 
        y <- 0 
#  'loop through rows for values greater than 0        
        if (nrow(iris_sp) > 0) {
#'loop through rows,x is running through traits, y is running down the rows
            for(k in 1:nrow(iris_sp)) {
                x <- x + iris_sp[k, j]
                y <- y + 1
            }
#'generating averages of traits 
            output[i, j] <- x / y 
        }
    }
}
output

# 1) Describe the values stores in object output. In other words what did the loop create?
the output is creating a two-dimensional array of the list of the unique species in the iris data. The loop generated trait average.
# 2) Describe using pseudo-code how output was calculated.

# 3)The variables in the loop were named so as to be vague. How can the objects output, x, and y be renamed such that it is clearer what is occurring in the loop.

output = trait average
x = trait_sum
y = sample_size 

# 4)It is possible to accomplish the same task using fewer lines of code? Please suggest one other way to calculate output that decreases the number of loops by 1.

for(i in seq_along(sp_ids)) {
  iris_sp <- subset(iris, subset=Species == sp_ids[i], select=-Species)
  # loop through traits
  for(j in 1:(ncol(iris_sp))) {
    for(k in 1:nrow(iris_sp)) {
      trait_average[i, j] <- mean(iris_sp[ , j])
    }
  }
}

trait_average


#'Sum of a Sequence'

#5) You have a vector x with the numbers 1:10. Write a for loop that will produce a vector y that contains the sum of x up to that index of x. So for example the elements of x are 1, 2, 3, and so on and the elements of y would be 1, 3, 6, and so on.

x <- 1:10
y <- cumsum(x)
for(i in 1:10) {
  y <- cumsum(1:10)
}
y

#6) Modify your for loop so that if the sum is greater than 10 the value of y is set to NA

x <- 1:10
y <- cumsum(1:10)
y <- ifelse(y > 10, NA, y)


for(i in 1:10) {
  y <- cumsum(1:10)
  if(cumsum(1:10) > 10) {
     print('NA')
  }
}


# 7) Place your for loop into a function that accepts as its argument any vector of arbitrary length and it will return y.


func1 <- function(y){
    y + seq_along(y)
}


