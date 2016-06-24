getTitle <- function(code) {
  #This function creates title for graphs with comprehensive information - cityname, region, country  
  #it requires city.codes data set from emibase()
  paste0('City: ', city.codes[city.codes$CityCode==code, 'CityName'], ' ',
         code, ' ',
         '(',city.codes[city.codes$CityCode==code, 'Region'], ', ',
         city.codes[city.codes$CityCode==code, 'RegionCode'], ') ',
         city.codes[city.codes$CityCode==code, 'CountryName'])
}


drawPlot <- function(df,ypav,citycode) {
  #actually draw and create data for each separate city, compare old and new data. 
  #good fit for Tier1 indicators
  #please note function creates and prints graph, it does not open pdf file. Pdf should be called prior to calling this   #function
  ## Input
  # df - dataset with the following columns
  # -> variable - should be years, the whole time series
  # -> value - corresponding value for each year
  # -> ProductName - product name. Usually it is a set of related product names - e.g. all tourism indicators, all vehicle
  #    indicators (number of passenger cars/motorcycles/commercial), all air pollution indicators.., all education indicators
  #    it is a good idea to have no more than five different product names.
  # -> period - should take values 'estimate' (= newly calculated data), 'old' (=previous Passport data), 
  #   'new' (=original data from research file with no calculations
  # ypav - some general title for graph, for example 'Transport indicators', 'Tourism indicators'
  # citycode - which city is being generated, needed for creating title.
  

  # create some initial settings, most importantly split dataset according to product name and assign
  # strict colours for old/new/estimate data.
  gg <- ggplot(df,aes(x=variable,y=value)) + 
               facet_grid(ProductName ~ .,scales="free") +
               xlab("Year") +
               ylab(ypav) +
               # scale_y_continuous(labels = comma) + 
               theme(strip.text.y = element_text(size = 8)) +
               scale_colour_manual(values = c("new" = "red",
                                              "old" = "blue",
                                              "estimate" = "green"))
  
  # skip adding estimated data if no estimated data exists (otherwise error)
  if (nrow(df[df$period=='estimate',])>0) {
    gg <- gg + geom_line(aes(group=1,colour=period),
                         data=df[df$period=='estimate',])
  }
  # same for new data
  if (nrow(df[df$period=='new',])>0) {
    gg <- gg + geom_point(aes(colour=period),
                          data=df[df$period=='new',],
                          shape=3,
                          size = 5)
  }
  # same for old data
  if (nrow(df[df$period=='old',])>0) {
    gg <- gg + geom_point(aes(colour=period),
                          data=df[df$period=='old',],
                          size=3)
  }
  
  # call function to create appropriate title
  graphTitle <- getTitle(citycode)
  
  # add title and set general theme for graph
  gg <- gg + ggtitle(label = graphTitle) +
    theme(plot.title = element_text(size = rel(1.4), 
                                    face = "bold",
                                    hjust = 0,
                                    vjust = 0)) +
    theme(legend.position="bottom", legend.text=element_text(size=12)) +
    theme(axis.title = element_text(size = rel(1.8))) +
    theme(axis.text = element_text(size = rel(1.4))) +
    theme(legend.text=element_text(size=rel(1.4))) +
    theme(panel.margin = unit(2, "lines"))
  print(gg)
}