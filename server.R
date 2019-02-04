library(shiny)
library(DT)
library(dplyr)
library(tidyr)
#library(ggmosaic)
library(markdown)
library(viridis)
#library(dendextend)
library(tools)
#library(scales)

df <- read.csv('artistdata.csv')
levels(df$gender) = c('Man','Woman')
df$gender = as.character(df$gender)
df$gender[is.na(df$gender)] = 'Not Inferred'
df$gender = as.factor(df$gender)
df$gender = factor(df$gender, levels = c('Man','Woman','Not Inferred'))

levels(df$ethnicity) = c('Asian','Black','Hispanic or Latino/a','Other','White')
df$ethnicity = as.character(df$ethnicity)
df$ethnicity[is.na(df$ethnicity)] = 'Not Inferred'
df$ethnicity = as.factor(df$ethnicity)
df$ethnicity = factor(df$ethnicity, levels = rev(c('Asian','Black','Hispanic or Latino/a','Other','White','Not Inferred')))


df$birthyear = cut(df$year,c(-400,499, 1499,1599, 1699, 1799,1899,2000) )
df$birthyear= as.character(df$birthyear)
df$birthyear[is.na(df$birthyear)] = 'Not Inferred'
df$birthyear = as.factor(df$birthyear)
levels(df$birthyear) = c('Before 500',"1500's","1600's","1700's","1800's","1900's",'500-1500','Not Inferred')
df$birthyear = factor(df$birthyear, levels = rev(c('Before 500','500-1500',"1500's","1600's","1700's","1800's","1900's",'Not Inferred')))


df$nationality = df$GEO3major
df$nationality = as.character(df$nationality )
df$nationality [is.na(df$nationality )] = 'Not Inferred'
df$nationality  = as.factor(df$nationality)
levels(df$nationality) = c("Africa", "Asia/Pacific", "Europe", "Latin America/Caribbean","North America","Not Inferred","West Asia"  )
df$nationality = factor(df$nationality, levels = rev(c("Africa", "Asia/Pacific","West Asia", "Latin America/Caribbean","Europe","North America","Not Inferred")))

MLevels = levels(df$museum)
df$museum = factor(df$museum,levels = MLevels[c(4,7,9,12,14,1,5,13,15,18,2,3,6,10,8,11,16,17)])


shinyServer(function(input, output) {
  observe({
    filters = ifelse(input$filter,'top','none')
    
    output$artistdata <- renderDT(
      df[,c('museum','artist','gender','ethnicity','nationality','birthyear')] %>% arrange(museum,artist), 
      colnames = c( 'Museum','Artist', 'Gender', 'Ethnicity', 'Regional Origin', 'Birth Year'),
      rownames = FALSE,
      extensions = 'FixedHeader',
      filter = filters,
      options = list(fixedHeader = TRUE)
    )
  })
  
  
  output$demoplot <- renderPlot({
    dftmp = df
    if(input$unknownfilter){
      dftmp = dftmp %>% filter_(paste0(input$demovar,"!= 'Not Inferred'")) %>% droplevels()
    }
    Levels = levels(eval(parse(text=paste0('dftmp$',input$demovar))))
    
    barplot <-  dftmp %>%
      count(get(input$demovar),museum) %>%
      group_by(museum)%>%
      mutate(percent = n/sum(n)*100) %>%
      rename(demo = `get(input$demovar)` ) %>%
      ggplot( aes(x = demo)) + 
      geom_bar(aes(y = percent, fill = factor(..x..)),stat='identity')  + facet_wrap(~museum, ncol=3) +
      scale_fill_viridis_d(name=paste0(tools::toTitleCase(input$demovar),":"),
        breaks=length(Levels):1,
        labels=rev(Levels)) +
      xlab('') + #tools::toTitleCase(input$demovar)
      ylab('Percent') +
      coord_flip() +
      theme_classic(base_size = 14) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),axis.text.y = element_text(angle = 0, hjust = 1)) + 
      theme(legend.position="top")
    
    mosaicplot <- dftmp %>%
      count(get(input$demovar),museum) %>%
      group_by(museum)%>%
      mutate(percent = n/sum(n)*100) %>%
      rename(demo = `get(input$demovar)` ) %>%
      ggplot() + 
      geom_bar(aes( y = percent,fill =  demo, x = museum), stat='identity') +
      scale_fill_viridis_d(name=paste0(tools::toTitleCase(input$demovar),":"),
        breaks=rev(Levels))+
      xlab('') +
      ylab('Percent') +
      coord_flip() +
      theme_classic(base_size = 18) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1),axis.text.y = element_text(angle = 0, hjust = 1)) +
      theme(legend.position="top") 
    
    
    ifelse(input$barplot,return(barplot),return(mosaicplot))
  })
  
  
  #output$demoplot <- renderPlot({
  #  museumdata <- df %>% group_by(museum) %>% summarize(
  #    Male=sum(gender=="man", na.rm=TRUE)/n(), 
  #    Female=sum(gender=="woman", na.rm=TRUE)/n(), 
  #    Unknown=sum(is.na(gender))/n()
  #  )
  #  museumdata.long <-  gather(museumdata, "Gender", "Proportion", c("Male","Female","Unknown"), factor_key=TRUE) 
  # %>% group_by(museum) %>% arrange(Proportion, .by_group=TRUE)
  #  plot <- ggplot(museumdata.long, aes(x=Gender, y=Proportion, fill=Gender)) +
  #    geom_bar(stat="identity", width=0.7) + facet_wrap(~museum, ncol=3) +
  #    guides(fill = guide_legend(title="Gender:")) + 
  #    theme(legend.position="top")
  #  
  #  return(plot)
  #})
  
  
})