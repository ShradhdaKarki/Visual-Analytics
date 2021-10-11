load('data/states_sf.rda')
#loading reprocessed beer award
load('data/beer_awards.Rda')


#### lists ####
state_list <- list("All states" = "USA","Alabama" = "AL","Alaska" = "AK",
                   "Arizona" = "AZ","Arkansas" = "AR","California" = "CA","Colorado" = "CO",
                   "Connecticut" = "CT","Delaware" = "DE","Florida" = "FL","Georgia" = "GA",
                   "Idaho" = "ID","Illinois" = "IL","Indiana" = "IN","Iowa" = "IA",
                   "Kansas" = "KS","Kentucky" = "KY","Louisiana" = "LA","Maine" = "ME",
                   "Maryland" = "MD","Massachusetts" = "MA","Michigan" = "MI","Minnesota" = "MN",
                   "Mississippi" = "MS","Missouri" = "MO","Montana" = "MT","Nebraska" = "NE",
                   "Nevada" = "NV","New Hampshire" = "NH","New Jersey" = "NJ","New Mexico" = "NM",
                   "New York" = "NY","North Carolina" = "NC","North Dakota" = "ND","Ohio" = "OH",
                   "Oklahoma" = "OK","Oregon" = "OR","Pennsylvania" = "PA","Rhode Island" = "RI",
                   "South Carolina" = "SC","South Dakota" = "SD","Tennessee" = "TN","Texas" = "TX",
                   "Utah" = "UT","Vermont" = "VT","Virginia" = "VA","Washington" = "WA","West Virginia" = "WV",
                   "Wisconsin" = "WI",
                   "Wyoming" = "WY")

state_list_r <- list("USA"="All states","AL"="Alabama" , "AK"="Alaska" ,
                     "AZ"="Arizona", "AR"="Arkansas" , "CA"="California" , "CO"="Colorado" ,
                     "CT"="Connecticut" , "DE"="Delaware" , "FL"="Florida" , "GA"="Georgia" ,
                     "ID"="Idaho" , "IL"="Illinois" , "IN"="Indiana" , "IA"="Iowa" ,
                     "KS"="Kansas", "KY"="Kentucky" , "LA"="Louisiana" , "ME"="Maine",
                     "MD"="Maryland" , "MA"="Massachusetts" , "MI"="Michigan", "MN"="Minnesota" ,
                     "MS"="Mississippi" , "MO"="Missouri" , "MT"="Montana" , "NE"="Nebraska" ,
                     "NV"="Nevada" ,"NH"="New Hampshire", "NJ"="New Jersey" , "NM"="New Mexico" ,
                     "NY"="New York" ,"NC"="North Carolina" , "ND"="North Dakota" , "OH"="Ohio" ,
                     "OK"="Oklahoma" , "OR"="Oregon" , "PA"="Pennsylvania" ,"RI"="Rhode Island" ,
                     "SC"="South Carolina" , "SD"="South Dakota" , "TN"="Tennessee" ,"TX"="Texas" ,
                     "UT"="Utah" , "VT"="Vermont","VA"="Virginia" , "WA"="Washington" , "WV"="West Virginia" ,
                     "WI"="Wisconsin" ,
                     "WY"="Wyoming" )

medal_list <- list("Gold" = "Gold","Silver"="Silver","Bronze"="Bronze")
cat_list <- unique(beer_awards$new_cat)

token_list <- list('One word'='one','Two words'='two','Three words'='three')

#### text analysis part ####
get_corpus <- function(data_df,n_gram,stop_click){
  if(n_gram == 'one'){
    if (stop_click == TRUE) {
      mycorp <- corpus(data_df$beer_name)
      toks <- tokens(mycorp, remove_punct = TRUE)
      toks_ngram <- tokens_ngrams(toks, n = 1:2)
      myDfm <- dfm(toks_ngram, remove = c(stopwords("english")), remove_punct = TRUE,remove_symbols = TRUE,
                   stem =T)
      return(myDfm)
    }
    else{
      mycorp <- corpus(data_df$beer_name)
      toks <- tokens(mycorp, remove_punct = TRUE)
      toks_ngram <- tokens_ngrams(toks, n = 1:2)
      myDfm <- dfm(toks_ngram, remove_punct = TRUE,remove_symbols = TRUE,
                   stem =T)
      return(myDfm)
    }
  }
  if(n_gram == 'two'){
    if (stop_click == TRUE) {
      mycorp <- corpus(data_df$beer_name)
      toks <- tokens(mycorp, remove_punct = TRUE)
      toks_ngram <- tokens_ngrams(toks, n = 2:4)
      myDfm <- dfm(toks_ngram, remove = c(stopwords("english")), remove_punct = TRUE,remove_symbols = TRUE,
                   stem =T)
      return(myDfm)
    }
    else{
      mycorp <- corpus(data_df$beer_name)
      toks <- tokens(mycorp, remove_punct = TRUE)
      toks_ngram <- tokens_ngrams(toks, n = 2:4)
      myDfm <- dfm(toks_ngram, remove_punct = TRUE,remove_symbols = TRUE,
                   stem =T)
      return(myDfm)
    }
    
  }
  else{
    if (stop_click == TRUE) {
      mycorp <- corpus(data_df$beer_name)
      toks <- tokens(mycorp, remove_punct = TRUE)
      toks_ngram <- tokens_ngrams(toks, n = 3:6)
      myDfm <- dfm(toks_ngram, remove = c(stopwords("english")), remove_punct = TRUE,remove_symbols = TRUE,
                   stem =T)
      return(myDfm)
    }
    else{
      mycorp <- corpus(data_df$beer_name)
      toks <- tokens(mycorp, remove_punct = TRUE)
      toks_ngram <- tokens_ngrams(toks, n = 3:6)
      myDfm <- dfm(toks_ngram, remove_punct = TRUE,remove_symbols = TRUE,
                   stem =T)
      return(myDfm)
    }
  }
}
#### make word cloud ####

get_wordcloud <- function(data_dfm,min_word){
  pal <- brewer.pal(8,"Dark2")
  textplot_wordcloud(data_dfm,min_count = min_word,color = pal)
  return(a)
}
#### ngram a and b ####
func_a <- function(n_gram){
  if(n_gram == 'one'){
    a <- 1
    return(a)
  }
  if (n_gram == 'two') {
    a <- 2
    return(a)
  }
  else{
    a <- 3
    return(a)
  }
} 

func_b <- function(n_gram){
  if(n_gram == 'one'){
    b <- 2
    return(b)
  }
  if (n_gram == 'two') {
    b <- 4
    return(b)
  }
  else{
    b <- 5
    return(b)
  }
}
#### Function Filter ####
# for specific cells
df_filter <- function(data_df,state_df,medal_df,date_df,cat_select){
  if(state_df == 'USA'){
    
    df_ready <- data_df %>% filter((medal %in% medal_df)&(`year`>=date_df[1] & `year` <=date_df[2])&
                                     new_cat %in% cat_select)
         }
  else{
    df_ready <- data_df %>% filter( medal %in% medal_df & state == state_df &
                                      (`year`>=date_df[1] & `year` <=date_df[2])&
                                      new_cat %in% cat_select)}
  return(df_ready)
}

#dont know what this does but looks important
beer_filter <- function(data_df,cat_beer = c("Lager","Pilsner","Wheat beer","Pale ale")){
  a <- data_df %>% filter(new_cat %in% cat_beer)
  return(a)
}

#th efunction to join map with dataframe
map_fun <- function(a){
  temp <- a %>% group_by(state,new_cat) %>% count(new_cat)
  
  temp1 <- inner_join(states_sf,temp,by=c('state_abbv'='state'))
  return(temp1)
}

##
map_fun_g7 <- function(a){
  
  temp <- a %>% group_by(state,medal) %>% count()
  temp1 <- inner_join(states_sf,temp,by=c('state_abbv'='state'))
  return(temp1)
}



#reactive top_n nor used but I keep it
g_click <- function(a){
  return(a)
}
# function for the map g5
g5_func<- function(data_df,beer_cat){
  data_df <- data_df %>% filter(new_cat %in% beer_cat)
  gg <- data_df %>% group_by(state,new_cat) %>% count(new_cat)
  gg <- map_fun(a = data_df)
  return(gg)
}

# function for the map g5
g7_func<- function(data_df){
  gg <- map_fun_g7(a = data_df)
  return(gg)
}



#### Plot1 (edit the texts)####

g1 <- function(n_df,data_df,title_name,year_d){
  jj <- state_list_r[[title_name]]
  a <-  data_df %>%
    group_by(medal,brewery) %>% 
    count(medal)%>% group_by(medal) %>% top_n(n_df) %>%  
    ggplot(aes(x =reorder_within(brewery,n,medal),y=n,width=0.75)) + 
    geom_bar(stat = 'identity',aes(fill=medal)) +
    scale_fill_manual(values=c('Gold'="#FFCC33", 'Silver'="#999999",'Bronze'= "#CC6600")) +
    labs(title = paste("Top breweries in",jj,'from',year_d[1],'to',year_d[2]), 
         x ='Brewery city',
         y = "Number of Medal Awards",
         
         fill = 'Medal') +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 15),
      plot.caption = element_text( size = 10),
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 12),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20),
      axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
      plot.background = element_rect(fill = "#ffffff"),
      axis.title.x = element_text(size =12,face='bold'),
      axis.title.y = element_text(size =12,face='bold'),
      strip.text.y = element_blank()
    ) +
    coord_flip()+
    facet_grid(medal~.,scales = "free")+
    scale_x_reordered()
  return(a)
}

#### Plot 2 ####
g2 <- function(n_df,data_df,title_name,year_d){
  jj <- state_list_r[[title_name]]
  a <-  data_df %>% 
    group_by(medal,city) %>% 
    count(medal)%>% group_by(medal) %>% top_n(n_df) %>%  
    ggplot(aes(x =reorder_within(city,n,medal),y=n,width=0.75)) + 
    geom_bar(stat = 'identity',aes(fill=medal)) +
    scale_fill_manual(values=c('Gold'="#FFCC33", 'Silver'="#999999",'Bronze'= "#CC6600")) +
    labs(title = paste("Cities with most winners in",jj,'from',year_d[1],'to',year_d[2]), 
         subtitle = "Texas Edition",
         x ='Brewery city',
         y = "Number of Medal Awards",
         fill = 'Medal') +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 15),
      plot.caption = element_text( size = 10),
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 12),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20),
      axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
      plot.background = element_rect(fill = "#ffffff"),
      axis.title.x = element_text(size =12,face='bold'),
      axis.title.y = element_text(size =12,face='bold'),
      strip.text.y = element_blank()
    ) +
    coord_flip()+
    facet_grid(medal~.,scales = "free")+
    scale_x_reordered()
  return(a)
}

#### Beer type ####
g3 <- function(data_df,n_df,title_name,year_d){ 
  jj <- state_list_r[[title_name]]
  b <- data_df  %>%
    group_by(brewery) %>% 
    count(medal)%>% group_by(medal) %>% top_n(n_df)
  
  b <- unique(b$brewery)
  
  a <- data_df %>% filter(brewery %in% b) %>% group_by(brewery,medal,new_cat) %>% count(medal) %>%  
    ggplot(aes(x=new_cat,y=n))+
    coord_flip()+
    facet_wrap(~brewery,scales = 'free')+
    geom_col(aes(fill=medal),position = position_dodge2(preserve = "single"))+
    scale_fill_manual(values=c('Gold'="#FFCC33", 'Silver'="#999999",'Bronze'= "#CC6600"))+
    labs(title = paste("Total medals won by selected beer categories",'from',year_d[1],'to',year_d[2]),
         y='Number of awards',x='Beer type',fill='Medals won'
         )+
    theme_classic()+
    theme(strip.text.x = element_text(size = 12,face = 'bold'),
          plot.title = element_text(size = 20),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          plot.caption = element_text( size = 10),
          plot.subtitle = element_text(size = 12),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          plot.background = element_rect(fill = "#ffffff"),
          axis.title.x = element_text(size =12,face='bold'),
          axis.title.y = element_text(size =12,face='bold'))
  return(a)
  
}

#### Beer Type ####

g4 <- function(data_df,title_name,year_d){
  jj <- state_list_r[[title_name]]
  a <- data_df  %>% group_by(year,new_cat) %>% count(new_cat)%>% 
    ggplot(aes(x=year,y=n))+geom_bar(stat = 'identity',aes(fill=new_cat))+
    facet_wrap(~new_cat,scale='free',ncol = 3)+
    scale_x_continuous(limits=c(year_d[1],year_d[2]),breaks = seq(year_d[1],year_d[2],by = 5))+
    labs(title = paste("Medals won by category in",jj,'from',year_d[1],'to',year_d[2]),
         y='Number of awards',x=element_blank()
         )+
    theme_classic()+
    theme(strip.text.x = element_text(size = 12,face = 'bold'),
          plot.title = element_text(size = 20),
          legend.position = "none")
  return(a)
}

#### Total medals won by map ####

g5 <- function(data_df,title_name,year_d){
  jj <- state_list_r[[title_name]]

  a <- ggplot()+ theme_bw()+
    geom_sf(data = states_sf,fill='#ffffff',size = 0.05)+
    geom_sf(data = data_df,aes(fill=n))+
    geom_sf_text(data = data_df,aes(label=state_abbv),size=2)+
    scale_fill_gradient(low = "#e5ffcc", high = "#3d700b", na.value = NA)+
    facet_wrap(~new_cat,ncol = 2)+ coord_sf(datum = NA)+
    labs(title = paste("Total medals won in",jj,'based on categories selected','from',year_d[1],'to',year_d[2]),
         x=element_blank(),y=element_blank(),fill='Medals won\nscale')+
    theme(strip.text.x = element_text(size = 12,face = 'bold'),
          plot.title = element_text(size = 20))
  return(a)
}

#### total beers by state ####
g6 <- function(data_df,title_name,year_d){
  
  jj <- state_list_r[[title_name]]
  a <- ggplot()+ theme_bw()+
    geom_sf(data = states_sf,fill='#ffffff',size = 0.05)+
    geom_sf(data = data_df,aes(fill=n))+
    geom_sf_text(data = data_df,aes(label=state_abbv),size=2)+
    scale_fill_gradient(low = "#e5ffcc", high = "#3d700b", na.value = NA)+
    labs(title = paste("Total medals won in",jj,'from',year_d[1],'to',year_d[2]),
         x=element_blank(),y=element_blank(),fill='Medals won\nscale')+
    facet_wrap(.~medal,ncol=1)+
    theme(panel.grid.major = element_blank(),
          strip.text.x = element_text(size = 12,face = 'bold'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 20),axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  return(a)
}

# g6 for single state
g6_one <- function(data_df,title_name,year_d){
  jj <- state_list_r[[title_name]]
  a <-  data_df %>%
    group_by(medal) %>% 
    count(medal)%>%  
    ggplot(aes(x =reorder_within(medal,n,medal),y=n,width=0.75)) + 
    geom_bar(stat = 'identity',aes(fill=medal)) +
    scale_fill_manual(values=c('Gold'="#FFCC33", 'Silver'="#999999",'Bronze'= "#CC6600")) +
    labs(title = paste("Total medals won in",jj,'from',year_d[1],'to',year_d[2]), 
         x ='Medal',
         y = "Number of Medal Awards",
         fill = 'Medal') +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 15),
      plot.caption = element_text( size = 10),
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 12),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20),
      axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
      plot.background = element_rect(fill = "#ffffff"),
      axis.title.x = element_text(size =12,face='bold'),
      axis.title.y = element_text(size =12,face='bold'),
      strip.text.y = element_blank()
    ) +
    coord_flip()+
    facet_grid(medal~.,scales = "free")+
    scale_x_reordered()
  return(a)
}










#### medals won by cat barplot ####
g8 <- function(data_df,title_name,year_d){
  jj <- state_list_r[[title_name]]
  a <-  data_df %>%
    group_by(new_cat) %>% 
    count(medal) %>%  
    ggplot(aes(x =reorder_within(new_cat,n,medal),y=n,width=0.75)) + 
    geom_bar(stat = 'identity',aes(fill=medal)) +
    scale_fill_manual(values=c('Gold'="#FFCC33", 'Silver'="#999999",'Bronze'= "#CC6600")) +
    labs(title = paste("Medals won based on category in",jj,'from',year_d[1],'to',year_d[2]), 
         
         x ='Beer Category',
         y = "Number of Medal Awards",
         
         fill = 'Medal') +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 15),
      plot.caption = element_text( size = 10),
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 12),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20),
      axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
      plot.background = element_rect(fill = "#ffffff"),
      axis.title.x = element_text(size =12,face='bold'),
      axis.title.y = element_text(size =12,face='bold'),
      strip.text.y = element_blank()
    ) +
    coord_flip()+
    facet_grid(medal~.,scales = "free")+
    scale_x_reordered()

  return(a)
}
