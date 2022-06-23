require(tidyverse)

path <- 'Data/contact/'

cntry <- 'South Africa'
use_year <- 2017

get_pop<- function(x){
  as.numeric(x) * 1000
}

un <- readxl::read_excel('Data/contact/WPP2019_POP_F15_1_ANNUAL_POPULATION_BY_AGE_BOTH_SEXES.xlsx', sheet = 1, skip = 16)

un <- dplyr::rename(un,
             region = `Region, subregion, country or area *`,
             year = `Reference date (as of 1 July)`
)

sa <- (
  un
  %>% filter(region == cntry, year == use_year)
  %>% select(9:ncol(un))
  %>% mutate_if(is.character,get_pop)
  %>% mutate(`75+` = `75-79`+ `80-84`+`85-89`+`90-94`+`95-99`+`100+`)
  %>% select(-c(`75-79`,`80-84`,`85-89`,`90-94`,`95-99`,`100+`))
)
sa <- as.numeric(sa)
