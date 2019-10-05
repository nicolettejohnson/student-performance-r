# Students Performance in Exams
# Examines student performance based on different variables

# Load Packages
library(dplyr)
library(ggplot2)

# Import raw data
rawdata <- read.csv(file = "StudentsPerformance.csv", header = TRUE)

# Cleaning the data
# Checking for missing values:
cat("There are", sum(is.na(rawdata)), "missing values.")

# Get a feel for the data
str(rawdata)
head(rawdata, n = 10)
summary(rawdata)



# Data Manipulation #

# Converting raw data into a tibble data frame for easier data analysis
spdata <- as_tibble(rawdata)
# Converting appropriate categorical data to ordinal data
paredu <- ordered(spdata$parental.level.of.education, levels = c("some high school", "high school", "some college", "associate's degree", "bachelor's degree", "master's degree"))

#Grade Scale is as follows:
#A: 90-100
#B: 80-89
#C: 70-79
#D: 60-69
#F: 0-59

# Creating new grade columns based on corresponding scores:
spdata_with_grades <- spdata %>%
  mutate(math.grade = case_when(math.score < 60 ~ "F",
                                math.score >= 60 & math.score <= 69 ~ "D",
                                math.score >= 70 & math.score <= 79 ~ "C",
                                math.score >= 80 & math.score <= 89 ~ "B",
                                math.score >= 90 & math.score <= 100 ~ "A"),
         reading.grade = case_when(reading.score < 60 ~ "F",
                                   reading.score >= 60 & reading.score <= 69 ~ "D",
                                   reading.score >= 70 & reading.score <= 79 ~ "C",
                                   reading.score >= 80 & reading.score <= 89 ~ "B",
                                   reading.score >= 90 & reading.score <= 100 ~ "A"),
         writing.grade = case_when(writing.score < 60 ~ "F",
                                   writing.score >= 60 & writing.score <= 69 ~ "D",
                                   writing.score >= 70 & writing.score <= 79 ~ "C",
                                   writing.score >= 80 & writing.score <= 89 ~ "B",
                                   writing.score >= 90 & writing.score <= 100 ~ "A"))


# Checking for missing values again. This time, using the newly-created dataframe:
cat("There are", sum(is.na(spdata_with_grades)), "missing values.")

# Let's look at our data with the newly-added columns:
str(spdata_with_grades)
head(spdata_with_grades, n = 10)

# The new columns need to be converted to factors with levels using lapply:
grades <- c("math.grade", "reading.grade", "writing.grade")
spdata_with_grades[grades] <- lapply(spdata_with_grades[grades], factor)
str(spdata_with_grades)

# Writing to a new file:
write.csv(spdata_with_grades, file = "StudentsPerformance-wrangled.csv", row.names = FALSE, col.names = TRUE)


# Data Visualization #

# Let's look at math scores
ggplot(spdata_with_grades, aes(x = math.score)) +
  geom_histogram() +
  ggtitle("Math Scores Distribution")
# We can see the math scores follow a fairly normal distribution with some outliers (left-skewed).

# Here are the math grades vs. parental level of education
ggplot(spdata_with_grades, aes(x = paredu , fill = math.grade)) +
  geom_bar(position = "dodge") +
  ggtitle("Math Grades Grouped by Parental Level of Education") +
  xlab("Parental Level of Education") +
  theme(axis.text.x = element_text(angle = 90))

# Here are the math grades vs. race/ethnicity
ggplot(spdata_with_grades, aes(x = race.ethnicity , fill = math.grade)) +
  geom_bar(position = "dodge") +
  ggtitle("Math Grades Grouped by Race/Ethnicity") +
  xlab("Parental Level of Education") +
  theme(axis.text.x = element_text(angle = 90))


# Let's look at reading scores
ggplot(spdata_with_grades, aes(x = reading.score)) +
  geom_histogram() +
  ggtitle("Reading Scores Distribution")
# We can see the reading scores follow a fairly normal distribution and is slightly left-skewed.
  
# Here are the reading grades vs. parental level of education
ggplot(spdata_with_grades, aes(x = paredu , fill = reading.grade)) +
  geom_bar(position = "dodge") +
  ggtitle("Reading Grades Grouped by Parental Level of Education") +
  xlab("Parental Level of Education") +
  theme(axis.text.x = element_text(angle = 90))
  
# Here are the reading grades vs. race/ethnicity
ggplot(spdata_with_grades, aes(x = race.ethnicity , fill = reading.grade)) +
  geom_bar(position = "dodge") +
  ggtitle("Reading Grades Grouped by Race/Ethnicity") +
  xlab("Parental Level of Education") +
  theme(axis.text.x = element_text(angle = 90))  
  
  
# Let's look at writing scores
  ggplot(spdata_with_grades, aes(x = writing.score)) +
    geom_histogram() +
    ggtitle("Writing Scores Distribution")
# We can see the writing scores follow a fairly normal distribution and is slightly left-skewed.

# Here are the writing grades vs. parental level of education
ggplot(spdata_with_grades, aes(x = paredu , fill = writing.grade)) +
  geom_bar(position = "dodge") +
  ggtitle("Writing Grades Grouped by Parental Level of Education") +
  xlab("Parental Level of Education") +
  theme(axis.text.x = element_text(angle = 90))
  
# Here are the writing grades vs. race/ethnicity
ggplot(spdata_with_grades, aes(x = race.ethnicity , fill = writing.grade)) +
  geom_bar(position = "dodge") +
  ggtitle("Writing Grades Grouped by Race/Ethnicity") +
  xlab("Parental Level of Education") +
  theme(axis.text.x = element_text(angle = 90))    
  
 




 
### Future Plans Below ###  
  
# Here, we can see the students' parental level of education based on the type of lunch the student has:
  ggplot(spdata_with_grades, aes(x = paredu)) + ##I want to make this a percentage chart, proprotion might also be useful
    geom_bar() +
    facet_wrap(~lunch) +
    ggtitle("Parental education vs. lunch type") +
    theme(axis.text.x = element_text(angle = 90))

  
##later, I want to facet-wrap these scores together based on score type.
  ##I also want to add results interpretations
  #test#
