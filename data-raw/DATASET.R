## code to prepare `DATASET` goes here
library(readxl)

totDf <- read.csv("C:/Users/agbre/OneDrive - Indiana University/Desktop/GradSchool/YearTwo/PBHL B581/Final Project/Data/2021 IMEB Data.csv")
chnAge <- read_excel("C:/Users/agbre/OneDrive - Indiana University/Desktop/GradSchool/YearTwo/PBHL B581/Final Project/Data/CHN Age.xlsx", col_names = F)
chnRace <-read_excel("C:/Users/agbre/OneDrive - Indiana University/Desktop/GradSchool/YearTwo/PBHL B581/Final Project/Data/CHN Race.xlsx", col_names = F)
chnGender <- read_excel("C:/Users/agbre/OneDrive - Indiana University/Desktop/GradSchool/YearTwo/PBHL B581/Final Project/Data/CHN Gender.xlsx", col_names = F)
chnIncome <- read_excel("C:/Users/agbre/OneDrive - Indiana University/Desktop/GradSchool/YearTwo/PBHL B581/Final Project/Data/CHN Gross Income.xlsx", col_names = F)

usethis::use_data(totDf, overwrite = TRUE)
usethis::use_data(chnAge, overwrite = TRUE)
usethis::use_data(chnRace, overwrite = TRUE)
usethis::use_data(chnGender, overwrite = TRUE)
usethis::use_data(chnIncome, overwrite = TRUE)

