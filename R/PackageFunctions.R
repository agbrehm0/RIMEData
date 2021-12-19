library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)


#'
#' Separate Function
#'
#' @param df Data frame of one year RIME data
#' @param sep_by Variable to separate by
#'
#' @return List of Data Frames by the possible Responses of sep_by
#' @export
#'
#' @examples
#' totDf <- read.csv("Data Info/2021 IMEB Data.csv")
#'
#' dfList <- separate(totDf, Hospitalcode)
#'
separate <- function(df, sep_by) {
  quoted_sep_by <- enquo(sep_by)
  res <- df %>% group_split(!!quoted_sep_by)
  return(res)
}


#' 2 Dimensional Contingency Table
#'
#' @param df Data frame of one year RIME data
#' @param var1 Variable along the top that will be shown in the output
#' @param var2 Variable along the side that will not be shown
#'
#' @return 2 Dimensional Contingency Table
#' @export
#'
#' @examples
#' totDf <- read.csv("Data Info/2021 IMEB Data.csv")
#' agegroups5 <- c("25-29", "30-34", "35–39", "40-44", "45+")
#' agegrp5 <-cut(totDf$Age, c(24,29,34,39,44,100),
#'               labels = agegroups5)
#' totDf$AgeGroup <- agegrp5
#'
#' contTbl(df = totDf, var1 = AgeGroup, var2 = Q25a_balance)
contTbl <- function(df, var1, var2) {
  quoted_var1 <- enquo(var1)
  quoted_var2 <- enquo(var2)
  res <- df %>%
    tabyl(!!quoted_var1, !!quoted_var2) %>%
    adorn_totals("row") %>%
    adorn_totals("col")
  return(res)
}

#' Pearson's Chi Square Test
#'
#' @param df Data frame of one year RIME data
#' @param var1 Variable 1
#' @param var2 Variable 2
#' @param idvar Identifying variable
#'
#' @return p-value of the Pearson Chi-Square test
#' @export
#'
#' @examples
#' totDf <- read.csv("Data Info/2021 IMEB Data.csv")
#' agegrp2 <- cut(totDf$Age, c(24,29,44), labels = c("<30","30+"))
#' totDf$AgeGroup2 <- agegrp2
#'
#' chiSqTest(df = totDf, var1 = AgeGroup2, var2 = Q25a_balance, idvar = ID)
chiSqTest <- function(df, var1, var2, idvar = ID) {
  quoted_var1 <- enquo(var1)
  quoted_var2 <- enquo(var2)
  quoted_idvar <- enquo(idvar)
  res <- df %>%
    group_by(!!quoted_idvar) %>%
    distinct(!!quoted_idvar, !!quoted_var1, !!quoted_var2) %>%
    ungroup() %>%
    summarise(pval = chisq.test(!!quoted_var1, !!quoted_var2)$p.value)
}

#' Normalize Year-by-year data
#'
#' @param df Unstructured year-by-year RIME data of one response from the
#' survey
#'
#' @return List of responses possible responses and structured data with
#' only the
#' @export
#'
#' @examples
normalize <- function(df) {
  # First replace with correct NAs
  df <- na_if(df, "n/a")

  # Get the column indexes for the possible responses
  response_cols <- df[str_which(df, pattern = 'Table')]
  response_cols_index <- colnames(response_cols) %>%
    str_replace("...", "")
  response_cols_index <- as.numeric(response_cols_index)

  # Get possible responses
  a=1
  responses <- list()
  for (a in 1:length(response_cols)) {
    tmp <- response_cols[rowSums(is.na(response_cols[a]))==0,a]
    responses[a] <- tmp[3:(nrow(tmp)-2),]
  }
  names(responses) <- rep("Responses", a)

  # Initialize and populate a list of dataframes for each time the
  # possible responses change. If the possible responses haven't changes
  # it's okay because the loop will then only run once.
  i=1
  N = length(response_cols_index)
  smallDf_list <- list()

  for (i in 1:N) {
    if (i < N) {
      smallDf_list[[i]] <- df[,response_cols_index[i]:(response_cols_index[i+1]-2)]
    } else {
      smallDf_list[[i]] <- df[response_cols_index[i]:length(df)]
    }
  }

  # Iterate through the list of dataframes and normalize them
  j=1
  resList <- list()
  for (dfs in smallDf_list) {
    # Get the columns where the data is
    tmp1 <- dfs[str_which(dfs, pattern = 'n=')]
    # Change column names to the year
    col_tmp <- str_split(tmp1[2,], pattern = " ")
    k=1
    col_names_tmp <- NULL
    for (k in 1:length(col_tmp)) {
      col_names_tmp <- append(col_names_tmp, col_tmp[[k]][1])
    }

    tmp2 <- tmp1[rowSums(is.na(tmp1))==0,]


    # still need to figure out missings
    # missingCases <- as.numeric(t(tmp1[nrow(tmp2),]))


    # Don't need the top rows anymore
    if (j > 1 & length(tmp2)==1){
      resDf <- tmp2 %>% slice(4:(n()-2))
    } else {
      resDf <- tmp2 %>% slice(3:(n()-2))
    }

    colnames(resDf) <- col_names_tmp
    #resDf <- cbind(as.data.frame(responses[j]), resDf)
    resList[[j]] <- resDf
    j=j+1
  }
  # print(missingCases)
  res <- list("Responses" = responses, "Normalized" = resList)
  return(res)
}

#' Totals or average by response
#'
#' @param df unstructured data frame
#' @param stat choice of either c("total", "average")
#'
#' @return structured data frame with descriptive statistic column
#' @export
#'
#' @examples
descStats <- function(df, stat = c("total", "average")) {
  norm <- normalize(df)
  normdf <- norm$Normalized[[1]]
  normdf[] <- lapply(normdf, function(x) as.integer(as.character(x)))

  if(stat == "total") {
    normdf %>% rowwise() %>%
      mutate(total = sum(c_across(colnames(normdf))))
  } else if(stat == "average") {
    normdf %>% rowwise() %>%
      mutate(average = mean(c_across(colnames(normdf))))
  }
}


#' Add Next Year to the year-by-year RIME data
#'
#' @param df Unstructured year-by-year RIME data of one response from the
#' survey
#'
#' @return Structured year-by-year data with next year added to the final
#' column. Also writes this to a .csv file so the data can be added later.
#' @export
#'
#' @examples
addYear <- function(df) {
  norm <- normalize(df)
  normdf <- norm$Normalized[[1]]
  normdf[] <- lapply(normdf, function(x) as.integer(as.character(x)))

  nextYear <- as.integer(names(normdf[length(normdf)]))+1
  normdf$ny <- NA
  colnames(normdf) <-
    c(colnames(normdf)[1:(length(normdf)-1)], as.character(nextYear))
  write.csv(normdf, "newYearTable.csv")
  return(normdf)
}
