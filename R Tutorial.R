read.csv(file = "data/inflammation-01.csv", header = FALSE)
weight_kg <- 55
weight_kg * 2.2
weight_lb <- 2.2 * weight_kg
weight_lb
weight_kg <- 60
weight_kg
weight_lb <- 2.2 * weight_kg
coffee_g <- 30
drink_g <- 500
coffee_oz <- coffee_g / 28.3495
drink_oz <- drink_g / 28.3495
coffee_oz
drink_oz




dat <- read.csv(file = "data/inflammation-01.csv", header = FALSE)
class(dat)
dim(dat)
dat[1, 1]
patient <- 30
day <- 20
dat[patient, day]
dat[30, 20]
dat[30, 21]
dat[30, 22]
dat[c(1, 3, 7), c(5, 10, 20, 30, 40)]
dat[c(1, 3, 7, 15, 30, 35, 55), c(5, 10, 20, 30, 40)]
dat[20:50, c(5, 10, 20, 30, 40)]
dat[20:30, 15:25]
dat[5, ]
dat[20, ]
dat[16:18, ]
dat[ , 20]
dat[ , 40]
dat[c(16, 17, 18), ]
dat[c(20,21,22,23,24,)]
dat[20:50]


pat_1 <- dat[1, ]
min(pat_1)
max(pat_1)
std(pat_1)
sd(pat_1)
mean(pat_1)
mean(dat[ ,5])
mean(as.numeric(pat_1))
mean(as.numeric(dat[5, ]))
mean(as.numeric(dat[10, ]))

summary(dat[ ,c(5, 10, 15, 20, 25, 30, 35, 40)])

avg_pat_inflammation <- apply(dat, 1, mean)
avg_day_inflammation <- apply(dat, 2, mean)

avg_pat_inflammation
max(avg_pat_inflammation)

avg_day_inflammation

apply(dat, 2, sd)

plot(avg_day_inflammation)
plot(avg_pat_inflammation)

max_day_inflammation <- apply(dat, 2, max)
plot(max_day_inflammation)

min_day_inflammation <- apply(dat, 2, min)
plot(min_day_inflammation)

animal <- c("e", "l", "e", "p", "h", "a", "n", "t")
animal
animal[c(1,4,5)]
animal[2:5]


max(dat[5, ])
max(dat[3:7, 5])
max(dat[5, 3:7])
max(dat[5, 3, 7])
dat[5, 3]

apply(dat[1:5, ],1, mean)

apply(dat[ , 1:10], 2, mean)

seq(2, 60, 2)
seq(2, 60, 3)
seq(15,30,3)

plot(apply(dat[ , seq(2, 40, 2)], 2, mean))

fahrenheit_to_celcius <- function(temp_F) {
  temp_c <- (temp_F - 32) * 5/9
  return(temp_c)
}

fahrenheit_to_celcius(70)
fahrenheit_to_celcius(32)
fahrenheit_to_celcius(212)

# Converts degrees celcius to degrees kelvin
celcius_to_kelvin <- function(temp_C){
  
  temp_k <- temp_C + 273.15
  return(temp_k)
}

celcius_to_kelvin(0)
celcius_to_kelvin(100)

# Converts degrees fahrenheit to degrees kelvin
fahrenheit_to_kelvin <- function(temp_F){
  temp_c <- fahrenheit_to_celcius(temp_F)
  temp_k <- celcius_to_kelvin(temp_c)
  return(temp_k)
}
fahrenheit_to_kelvin(32)

best_practice <- c("Write", "programs", "for", "people", "not", "computers")
wrap <- "!!!!"

highlight <- function(sentence, wrapper) {
  new_sentence <- c(wrapper, sentence, wrapper)
  return(new_sentence)
}

highlight(best_practice, wrap)

length(c(1,2,3,4,5))
length(dat[5, ])
dat[5, length(dat[5, ])]
dat[5, 40]

# Returns a vector of first and last elements of input
edges <- function(input_vector){
  vector_out <- c( input_vector[1] , input_vector[length(input_vector)] )
  return(vector_out)
}


edges(best_practice)
edges(c(3,7,3,7,9,34,8,3,7))
edges(c("It", "is", "windy", "in", "Nebraska!"))


#Centers a dataset around a new value
center <- function(data, new_midpoint){
  new_data <- (data - mean(data)) + new_midpoint
  return(new_data)
}

t <- c(0,0,0,0)
center(t, 5)
t <- c(1,5,7,10)
center(t,5)
mean(t)


centered <- center(dat[ ,4], 0)
head(centered)

mean(dat[ , 4])
mean(centered)

sd(dat[ ,4])
sd(centered)
all.equal(sd(dat[ ,4]), sd(centered))



read.csv(file = "data/inflammation-0csv", header = FALSE)

read.csv(file = "data/commadec.txt", sep = "|", de1.c = ",")



center <- function(data, midpoint = 0){
  # returns a new vector containing the original data centered around the new midpoints
  # Example: center(c(1,2,3),0) --> c(-1, 0, 1)
  new_data <- (data - mean(data)) + midpoint
  return(new_data)
}

analyze <- function(filename){
  #Generate plots from a data input over different
  #numeric analyses from the given input file
  
  dat <- read.csv(file = filename, header = FALSE)
  #Min
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
  #Max
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  #Mean
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
}

analyze("data/inflammation-01.csv")

analyze("data/inflammation-02.csv")

best_practice <- c("Let", "the", "computer", "do", "the", "work", "in", "a", "very", "efficient", "manner")
print_words <- function(sentence) {
  print(sentence[1])
  print(sentence[2])
  print(sentence[3])
  print(sentence[4])
  print(sentence[5])
  print(sentence[6])
  print(sentence[7])
  print(sentence[8])
  print(sentence[9])
  print(sentence[10])
  print(sentence[11])
}

print_words(best_practice)

print_words <- function(sentence) {
  for (purple_monkey in sentence) { #do
    print(purple_monkey)
  } #done
}

print_words(dat[5, ])


for (bucket in truck){ 
  fill_bucket()
}


vec_len <- 0
vowels <- c("a", "e", "i", "o", "u")
for (v in vowels) { #do
  print(v)
  vec_len <- vec_len + 1
  print(vec_len)
} #done

print("Vector Length:")
print(length(vowels))


print_N <- function(n){
  #prints each number in a sequence from 'n'
  for (num in seq(1, n)) {#do
    print(num)
    }#done
}
print_N(20)


total <- function(sum_data){
  #Sums the values of a vector
  
  #initialize variable for answer
  result <- 0
  
  #add everything line by line
  for (num in sum_data) {
    result <- result + num
  }
  
  return(result)
}


v = c(4, 8, 15, 16, 23, 42)
total(v)

list.files()
list.files(path = "data")
list.files(path = "data", pattern = "csv")
list.files(path = "data", pattern = "inflammation")
list.files(path = "data", pattern = "inflammation", full.names = TRUE)


analyze_all <- function(data_path, pattern){
  print("Entered Function")
  our_files <- list.files(path = data_path, pattern=pattern, full.names = TRUE)
  print(our_files)
  print("our files generated")
  for (f in our_files){
    print("Now Analyzing file:")
    print(f)
    analyze(f)
  }
}

analyze_all <- function(direct, patt) {
  #run analyze function on all files in a directory that match a pattern
  #direct = directory
  #patt = pattern
  files_to_analyze <- list.files(path= direct, pattern= patt, full.names= TRUE)
  for (f in files_to_analyze) {
    print("Now Analyzing file:")
    print(f)
    analyze(f)
  }
}

analyze_all("data", "inflammation")

a <- 37
a > 100
a < 100
a <= 100
a >= 37


a <- 84769
if (a > 100) {#then
  print("A is greater than")
} else {
  print("A is less than")
}
print("Complete")

if (a <= 99) {
  print("a is greater than or equal to")
}
print("DOne")

a <- -3.14
if (a > 0){
  print(1)
} else if (a == 0) {
  print(0)
} else {
  print(-1)
}

# AND && - True if both sides are true
if (1 > 0 && -1 > 0){
  print("Both are true")
} else{
  print("Not all are true")
}

if (1 > 0){
  if (-1 > 0){
    print("Both are true")
  }
} else {
  print("Not all are true")
}




# OR || - True if atleast one side is true
if (1 > 0 || -1 > 0) { 
  print("Atleast there is some truth!")
} else {
  print("There is no truth!")
}

a <- 12
a == NA
if (a==NA){ # Fails because "NA" is non-existant
  print("Coconut")
}

if (is.na(a)) {
 print("Coconut")
}

plot_dist <- function(data_in, threshold){
  
  # If the data length is atleast the threshold, then boxplot, else stripchart
  if (length(data_in) >= threshold){
    boxplot(data_in)
  } else {
    stripchart(data_in)
  }
}

plot_dist(dat[1:5 , 10], threshold=10)



analyze_all <- function(direct, patt) {
  #run analyze function on all files in a directory that match a pattern
  #direct = directory
  #patt = pattern
  files_to_analyze <- list.files(path= direct, pattern= patt, full.names= TRUE)
  for (f in files_to_analyze) {
    print("Now Analyzing file:")
    print(f)
    analyze(f)
  }
}

pdf("new_results.pdf")
analyze("data/inflammation-01.csv")
dev.off()

analyze <- function(filename, output=NULL){
  #Generate plots from a data input over different
  #numeric analyses from the given input file
  
  if (!is.null(output)){
    pdf(output)
  }
  
  dat <- read.csv(file = filename, header = FALSE)
  #Min
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
  #Max
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  #Mean
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  if (!is.null(output)){
    dev.off()
  }
}

f_input <- "inflammation-01.csv"
sub("csv", "pdf", f_input)
file.path("results", sub("csv", "pdf", f_input))

analyze_all <- function(data_path, results_path, patt) {
  #run analyze function on all files in a directory that match a pattern
  #direct = data_path
  #results = results_path
  #patt = pattern
  files_to_analyze <- list.files(path= data_path, pattern= patt)
  for (f in files_to_analyze) {
    print("Now Analyzing file:")
    print(f)
    output_file_name <- sub("csv", "pdf", f)
    analyze( file.path(data_path, f), file.path(results_path, output_file_name) )
  }
}

analyze_all("red_rainbows", "blue_umbrella", "inflammation")


















