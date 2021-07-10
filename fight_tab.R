source("ufc_scraping.R")

# function to combine player name + country
combine_word <- function(name, country){
  glue::glue(
    "<span style='line-height:14px'><span style='font-weight:bold;font-size:16px'>{name}</span></span>
        <span style='line-height:16px'><span style ='font-weight:bold;color:grey;font-size:9px'>{country}</span></span>"
  )
}


# Graphical Table ---------------------------------------------------------
figthers_tab <- function(event_num, weight_class,fighters_stats) {
  fighters_stats %>% 
    mutate(
      combo = combine_word(player_name_ordered,player_country),
      combo = map(combo,gt::html)
    ) %>% 
    select(player_headshot, combo, everything(), -c(player_name_ordered, player_country)) %>% 
    gt() %>% 
    text_transform(
      locations = cells_body(c(player_headshot)),
      fn = function(x) {
        web_image(url =x, 
                  height = px(40)) 
      }
    ) %>% 
    cols_label(
      player_headshot = "",
      combo = "", 
      odds = "Odds",
      nb_wins = "W",
      nb_loses = "L",
      last_5_kds = "KD",
      last_5_body_pct = "%BODY",
      last_5_head_pct = "%HEAD",
      last_5_leg_pct = "%LEG"
    ) %>% 
    tab_spanner(
      label = "Career Record",
      columns = c(nb_wins, nb_loses)
    ) %>% 
    tab_spanner(
      label = "Last 5 fights", 
      columns = 6:9
    ) %>% 
    data_color(
      c(odds),
      colors = scales::col_numeric(
        palette = c("#489bca","white","#ff2d21" ),
        domain = NULL
      ),
      autocolor_text = FALSE
    ) %>% 
    tab_style(
      style = cell_borders(sides = "bottom", color = "transparent", weight = px(3)),
      locations = cells_body(
        columns = 1
      )
    )  %>% 
    tab_style(
      style = cell_borders(sides = "bottom", color = "transparent", weight = px(3)),
      locations = cells_body(
        rows = 2
      )
    ) %>% 
    tab_style(
      style = cell_text(font = "Inconsolata"), 
      locations = cells_body(columns = c(3:9))
    ) %>% 
    tab_style(
      style =  cell_text(weight = "bold"),
      locations = cells_body(columns = c(nb_wins,nb_loses))
    ) %>% 
    fmt_percent(c(last_5_body_pct,last_5_head_pct,last_5_leg_pct), scale_values = FALSE, decimals = 1) %>% 
    tab_header(
      title = html(glue('<b> <img width = 50, style = "vertical-align: middle;" src = "{mma_logo}"/>  UFC {event_num} - {weight_class}</b>'))
    ) %>% 
    tab_source_note(
      html("<span style='float:left';><b>Data</b>: ESPN & Draftkings</span><span style = 'float:right';> <span style= 'font-family:\"Font Awesome 5 Brands\"'>&#xf099;</span>@issa_madjid")
    ) %>% 
    tab_footnote(footnote = "Knockdowns",
                 locations = cells_column_labels(
                   columns = c(last_5_kds)
                 )) %>% 
    tab_footnote(footnote = "Target Breakdown Body",
                 locations = cells_column_labels(
                   columns = c(last_5_body_pct)
                 )) %>% 
    tab_footnote(footnote = "Target Breakdown Head",
                 locations = cells_column_labels(
                   columns = c(last_5_head_pct)
                 )) %>% 
    tab_footnote(footnote = "Target Breakdown Leg",
                 locations = cells_column_labels(
                   columns = c(last_5_leg_pct)
                 )) %>% 
    opt_table_font(font = "Lato") %>% 
    tab_options(
      heading.align = "center",
      heading.border.bottom.width = px(3),
      heading.border.bottom.color = "black",
      table.border.top.width = px(3),
      table.border.top.color =  "transparent",
      table.border.bottom.color =  "transparent",
      table.border.bottom.width =  px(3),
      table.background.color = "#f9fbfc",
      column_labels.font.size = 14,
      table.width = px(600),
      heading.title.font.weight = "bold",
      data_row.padding = px(1),
      source_notes.padding = px(8),
      footnotes.font.size = px(10)
    )
}


fighters_stats <- function(fighter_1, fighter_2, fighter_1_odd, fighter_2_odd) { 
  fighters_dataset %>% 
    filter(player_name %in% c(fighter_1, fighter_2)) %>% 
    mutate(
      odds = c(fighter_1_odd, fighter_2_odd),
      player_name_ordered = str_replace(player_name,"(.+), (.+)","\\2 \\1"),
      player_headshot = glue("https://a.espncdn.com/combiner/i?img=/i/headshots/mma/players/full/{id}.png"),
      summarize = map(map(id, get_fighter_stats), summarise_player_stats)
    ) %>% 
    unnest_wider(summarize) %>% 
    select(player_headshot, player_name_ordered, player_country, odds,nb_wins, nb_loses,contains("5")) 
  
}

# Gather all
event_tab <- function(event_num, weight_class, fighter_1, fighter_2, fighter_1_odd, fighter_2_odd) {
  fighters_stats <- fighters_stats(fighter_1, fighter_2, fighter_1_odd, fighter_2_odd)
  figthers_tab(event_num, weight_class,fighters_stats)
}

# Example
event_tab(261,"Welterweight","Masvidal, Jorge","Usman, Kamaru",+320,-435)
ZSSSS
# UFC 264 
mcgregor_poirier <- event_tab(264,"Lightweight","McGregor, Conor","Poirier, Dustin",+105,-134)
gtsave(mcgregor_poirier,"mcgregor_poirier.png",path = "Graphics")

burns_thompson <- event_tab(264,"Welterweight","Burns, Gilbert","Thompson, Stephen",+130,-162)
gtsave(burns_thompson,"burns_thompson.png",path = "Graphics")

