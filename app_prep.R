tuesdata <- tidytuesdayR::tt_load('2022-03-29')

#data treatment
sports <- tuesdata$sports |> 
  filter(year == 2019)


states <- sports |> distinct(state_cd) |> pull()


city_counter <- function(x){
  sports |> 
    filter(state_cd == x) |> 
    distinct(city_txt) |> 
    nrow()
}



n_cities_per_state <- tibble(
  state = states,
  n_city = purrr::map_dbl(states,city_counter)
) |> 
  arrange(n_city)


#take a look at the mile high
c('sum_partic_men','sum_partic_women','rev_men',"rev_women")
milehigh<-sports |> filter(state_cd == "ID") |> 
  select(year, institution_name, city_txt, starts_with("sum"),
         starts_with("rev"), starts_with("exp"), sports) |> 
  filter(!(sum_partic_men ==0 & sum_partic_women ==0  )) |> 
  #reshape the dataset to an adequate plotting format
  tidyr::pivot_longer(cols = ends_with("men")) |> 
  #add variables to make it tidier
  mutate(sex = case_when(stringr::str_detect(name, "women") ~ "Women",
                         TRUE ~ "Men"),
         meaning_value = case_when(stringr::str_detect(name, "partic") ~ "number of participants",
                           stringr::str_detect(name, "rev")  ~ "revenue",
                           stringr::str_detect(name, "exp") ~ "expenses"))


#now test two plots


hchart(milehigh |> filter(meaning_value =="number of participants",
                          city_txt == "Boise",
                          institution_name =='Boise State University',
                          sports == "All Track Combined")
       , "column", hcaes(x = sex, 
                            y=value))



       