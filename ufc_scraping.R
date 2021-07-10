
# Load Libraries ----------------------------------------------------------
library(xml2)
library(rvest)
library(tidyverse)
library(purrr)
library(magick)
library(glue)
library(gt)


# Scrape UFC Rankings from ESPN API ---------------------------------------
ufc_rankings_url <-  "http://site.api.espn.com/apis/site/v2/sports/mma/ufc/rankings"
raw_ranking_json <- rjson::fromJSON(file = ufc_rankings_url)
mma_logo <- pluck(raw_ranking_json, "sports",1, "logos",1,"href")
ufc_league_full_name <- pluck(raw_ranking_json, "leagues", 1, "name") 
ufc_league_abbr <- pluck(raw_ranking_json, "leagues", 1, "abbreviation") 
ufc_league_short_name <- pluck(raw_ranking_json, "leagues", 1, "shortName") 

# Rankings 
rankings <- pluck(raw_ranking_json, "rankings") %>%  
    enframe()
rankings <- rankings %>% 
    unnest_wider(value, names_sep = "_") %>% 
    unnest_wider(value_weightClass, names_sep = "_")


#  Clean it 
rankings <- rankings%>% 
    unnest_longer(value_ranks) %>% 
    unnest_wider(value_ranks) %>% 
    unnest_wider(athlete, names_sep = "_") %>% 
    select(starts_with("athlete"),everything(),-where(is.list), -c(name, value_id)) 

# Example : HeavyWeight fighters standing
( hw_fighters <- rankings %>%
        filter(value_weightClass_slug == "heavyweight"))


# Scrape All MMA fighters profil from ESPN Website ------------------------
(fighters_rows <- str_c("http://www.espn.com/mma/fighters?search=", letters) %>% 
     map( ~read_html(.x) %>% 
              html_nodes("tr.evenrow")
     ) %>% 
     append(
         str_c("http://www.espn.com/mma/fighters?search=", letters) %>% 
             map( ~read_html(.x) %>% 
                      html_nodes("tr.oddrow")
             )
     )
)

# https://stackoverflow.com/questions/43427726/concatenate-vectors-from-a-lists-of-lists
# concatenate vectors from a lists of lists
vecs <- unlist(fighters_rows, recursive = F)
(lapply(unique(names(vecs)), function(name) do.call(c, vecs[name == names(vecs)])))

# Retrieve Player Links
retrieve_player_infos <- function(player) {
    (player_infos <- player %>% 
         html_nodes("td")
    )
    (player_name <- player_infos %>% 
            pluck(1) %>% 
            html_node("a") %>% 
            html_text()
    )  
    (player_link <- player_infos %>% 
            pluck(1) %>% 
            html_node("a") %>% 
            html_attr("href") 
    )  
    (player_country <- player_infos %>% 
            pluck(2) %>% 
            html_text()
    )
    list("player_name" = player_name, "player_link" = player_link, "player_country" = player_country)
}

# Retrieve links for all fighters
fighters_dataset <- vecs %>% 
    map(retrieve_player_infos) %>% 
    bind_rows() %>% 
    arrange(player_name) %>% 
    mutate(
        id = as.integer(str_extract(player_link, "(\\d+)" )), # Extract id 
        player_link = str_c("https://www.espn.com", player_link)
    ) 



# Example : French fighters 
(fighters_dataset %>% 
        filter(player_country == "FRA") 
)

# Save the dataset 
# write_csv(fighters_dataset, "Dataset/mma_fighters.csv")
# saveRDS(fighters_dataset,"Dataset/mma_fighters.rds")

# Get a specifical fighter info
get_fighter_infos <- function(fighter_name){
    fighters_dataset %>% 
        filter(player_name == fighter_name)
}

# Get Fighter Individual Stats
get_fighter_stats <- function(fighter_id) {
    fighter_url <- glue("https://www.espn.com/mma/fighter/stats/_/id/{fighter_id}")
    tables <- fighter_url %>% 
        read_html() %>% 
        html_nodes("table") %>% 
        html_table(header = T) %>% 
        enframe()
    
    stats <- tables %>% 
        pull(value) %>% 
        map_dfc(pluck) %>%
        select(-c(17:20,33:36))  # Remove date 
    
    colnames(stats) <-  names(stats) %>% 
        str_replace("\\.+\\d","")
    
    stats
}

# Summarise KD BODY HEAD LEG
summarise_player_stats <- function(stats) {
    stats %>% 
        summarise(
            last_5_kds = sum(head(as.numeric(KD),5)),
            last_5_body_pct = mean(head(parse_number(`%BODY`),5)),
            last_5_head_pct = mean(head(parse_number(`%HEAD`),5)),
            last_5_leg_pct = mean(head(parse_number(`%LEG`),5)),
            nb_wins = sum(Res == "W"),
            nb_loses = sum(Res == "L")
        )
    
}
