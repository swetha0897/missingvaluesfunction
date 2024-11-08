README-count_all_missing_by_group
================
2024-11-08

# Overview

This repository contains R functions designed to analyze missing data
within datasets. The main function, `count_all_missing_by_group`, allows
users to count the number of missing values (NAs) for all columns in a
dataset, grouped by a specified column.

\#Installation To install the package from GitHub, you can use the
devtools package:
devtools::install_github(“swetha0897/missingvaluesfunction”)

# Contents

- **R Function**: The `count_all_missing_by_group` function for counting
  missing values.
- **Example Dataset**: Utilizes the built-in `airquality` dataset to
  demonstrate functionality.
- **Usage Examples**: Code snippets show how to use the function for
  different grouping scenarios.
- **Description** gives a detailed description of the missing values function.
- **Function**  can be found as a `missingvalues.R` file in the R folder
- **tests** folder which includes the tests done on a sample dataset.

# Key Features

- **Group-wise Counting**: Counts missing values for all columns based
  on specified groups.
- **Flexible Grouping**: Supports both numeric and categorical grouping
  variables.
- **Clear Output**: Returns a data frame summarizing missing values by
  group.

# Pre-Requisites for the Repository

A basic understanding of STAT545 course would be helpful, as it will
help to effectively use and adapt the functions provided in this
repository.

# Getting Started

To use the files in this repository:

1.  **Install Required Packages and dataset**: Ensure you have the
    following packages installed:

    - `dplyr`
    - `tidyverse`
    - `airquality`

2.  **Load the Function**: Source or copy the
    `count_all_missing_by_group` function into your R environment.

3.  **Load Example Data**: You can use the built-in `airquality` dataset
    or any other dataset of your choice.

4.  **Count Missing Values**: Use the function to count missing values
    based on your grouping variable.

# Contents of the Repository

- **RMD File**: Contains the code for the `count_all_missing_by_group`
  function along with examples demonstrating its usage.
- **missingvalues.R**: This R file contains the function code located
  within the ‘R’ folder.
- **tests**:Different tests performed for a sample Dataset using the
  `count_all_missing_by_group` function
- **DESCRIPTION**:Contains all the necessary details of the package.

# Usage demonstration

``` r
# Load the package
library(missingvalues)

# Example 1- Using the airquality dataset to count missing values by 'Month'
result <- count_all_missing_by_group(airquality, "Month")

# View the results
print(result)
```

    ##   Month Ozone Solar.R Wind Temp Day
    ## 1     5     5       4    0    0   0
    ## 2     6    21       0    0    0   0
    ## 3     7     5       0    0    0   0
    ## 4     8     5       3    0    0   0
    ## 5     9     1       0    0    0   0

``` r
# Count Missing Values by Wind Category
#First, create a new column to categorize wind speeds, and then count the missing values by the new Wind_category column:

# Create a new column for Wind categories
airquality$Wind_category <- cut(airquality$Wind, breaks=c(0, 5, 10, 15, 20),
                                 labels=c("Low", "Medium", "High", "Very High"))

# Example 2: Count missing values by Wind category
result2 <- count_all_missing_by_group(airquality, "Wind_category")

# View the result
print(result2)
```

    ##   Wind_category Ozone Solar.R Wind Temp Month Day
    ## 1             2    19       6    2    2     2   2
    ## 2             3    18       4    2    2     2   2
    ## 3            NA   153     153  153  153   153 153
    ## 4             4     4       2    2    2     2   2
    ## 5             1     4       3    2    2     2   2

``` r
#Count Missing Values by Temp Category
##In this example, we categorize temperature values and then count the missing values by the Temp_category column

# Create a new column for Temperature categories
airquality$Temp_category <- cut(airquality$Temp, breaks=c(50, 70, 80, 90, 100),
                                 labels=c("Cool", "Moderate", "Warm", "Hot"))

# Example 3: Count missing values by Temp category
result3 <- count_all_missing_by_group(airquality, "Temp_category")

# View the result
print(result3)
```

    ##   Temp_category Ozone Solar.R Wind Temp Month Day Wind_category
    ## 1             1     6       3    0    0     0   0             1
    ## 2             2    17       1    0    0     0   0             1
    ## 3             3    10       3    0    0     0   0             0
    ## 4             4     4       0    0    0     0   0             0

# License

This project is licensed under the MIT License
