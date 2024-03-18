# Class_6

## Quarto

Quarto enables you to weave together content and executable code into a
finished document. To learn more about Quarto see <https://quarto.org>.

## Running Code

Read in gradebook csv from url

``` r
url <- "https://bioboot.github.io/bggn213_S19/class-material/student_homework.csv"
grade_book <- read.csv(url)
colnames(grade_book)[1] <- "Students"
```

You can add options to executable code like this

This section is the work through of the function in each step, the steps
were then taken and put into the function in the order they were
recorded here.

``` r
#convert all na's to zero
grade_book[is.na(grade_book)] <- 0
#Identify what the lowest score is for every student in the class
lowest_score <- apply(grade_book[, -1], 1, FUN = min)
#Add lowest score to the grade sheet
grade_book$lowest_score <- lowest_score
#Sum all of the scores for the homework for each student in the class
sum_grades <- apply(grade_book[,2:6], 1, FUN = sum)
#Add the summed grades to the grade sheet
grade_book$sum_grades <- sum_grades
#The max score for the homework would be 400 with the lowest score dropped
max_score <- 400
#Calculate grades, with the lowest score subtracted from the sum total score
grade_book$score <- ((grade_book$sum_grades - grade_book$lowest_score) / max_score) * 100
```

The grade function itself

The return function was added to show the score as part of the class
data frame, rather than just returning a score vector

``` r
grade <- function(r){
  r <- data.frame(r)
  r[is.na(r)] <- 0
  lowest_score <- apply(r[, -1], 1, FUN = min)
  r$lowest_score <- lowest_score
  sum_grades <- apply(r[,2:6], 1, FUN = sum)
  r$sum_grades <- sum_grades
  max_score <- 400
  r$score <- ((r$sum_grades - r$lowest_score) / max_score) * 100
  return(r)
}
```

Use of the function

Should be executed after loading the first chunk of code (the grade_book
csv)

``` r
grade_book_graded <- grade(grade_book)
grade_book_graded
```

         Students hw1 hw2 hw3 hw4 hw5 lowest_score sum_grades score
    1   student-1 100  73 100  88  79           73        440 91.75
    2   student-2  85  64  78  89  78           64        394 82.50
    3   student-3  83  69  77 100  77           69        406 84.25
    4   student-4  88   0  73 100  76            0        337 84.25
    5   student-5  88 100  75  86  79           75        428 88.25
    6   student-6  89  78 100  89  77           77        433 89.00
    7   student-7  89 100  74  87 100           74        450 94.00
    8   student-8  89 100  76  86 100           76        451 93.75
    9   student-9  86 100  77  88  77           77        428 87.75
    10 student-10  89  72  79   0  76            0        316 79.00
    11 student-11  82  66  78  84 100           66        410 86.00
    12 student-12 100  70  75  92 100           70        437 91.75
    13 student-13  89 100  76 100  80           76        445 92.25
    14 student-14  85 100  77  89  76           76        427 87.75
    15 student-15  85  65  76  89   0            0        315 78.75
    16 student-16  92 100  74  89  77           74        432 89.50
    17 student-17  88  63 100  86  78           63        415 88.00
    18 student-18  91   0 100  87 100            0        378 94.50
    19 student-19  91  68  75  86  79           68        399 82.75
    20 student-20  91  68  76  88  76           68        399 82.75

``` r
#Who was the top scoring student?
grade_book_graded[which.max(grade_book_graded$score), ]
```

         Students hw1 hw2 hw3 hw4 hw5 lowest_score sum_grades score
    18 student-18  91   0 100  87 100            0        378  94.5

``` r
#Student 18 was the highest scoring student with a grade of 94.5 %
```

``` r
#Which homework was the hardest for the students, ie which had the lowest average score
grade_book[is.na(grade_book)] <- 0
home_work_avg <- apply(grade_book[,2:6], 2, FUN = mean)
home_work_avg
```

      hw1   hw2   hw3   hw4   hw5 
    89.00 72.80 80.80 85.15 79.25 

``` r
which.min(home_work_avg)
```

    hw2 
      2 

``` r
#Homework 2 was the hardest for the students with the average score being 72.8 out of 100
```

``` r
#Which homework was most predictive of overall performance?
cor_scores <- cor(grade_book_graded[, c(2:6, 9)])
cor_scores[, "score"]
```

          hw1       hw2       hw3       hw4       hw5     score 
    0.4250204 0.1767780 0.3042561 0.3810884 0.6325982 1.0000000 

``` r
max(cor_scores[-6, "score"])
```

    [1] 0.6325982

``` r
#Homework 5 was most predictive with a corelation to total score of 0.6325982
```
