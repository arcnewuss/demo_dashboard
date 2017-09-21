## app.R ##
# setwd('/home/johan/public/shiny-server/sample-apps/')
# shiny::runApp('dashboard')


library(shinydashboard)


uni_df  <- readRDS('uni_df.rds')
uni_df <<-uni_df # not sure why we need this
#############
# WORLD MAP #
#############

library(ggplot2)
library(ggmap)

# Sandhu's
library(rsconnect)
library(quantmod)

#library(googleway)


# google service:
# geocoding-backend.googleapis.com 
# Google Maps Geocoding API

# vector of countries, each student counted only once                            

# use package ggmap to get coordinates

# creating a function to load map by location
# first argument is location second is zoom
#university_map <- function(loc,z=10){
#    region <- get_map(location = loc, 
#        maptype = 'toner', #source = 'stamen',
#        zoom = z,
#        api_key = key)
#    map_region <- ggmap(region, extent='panel', base_layer=ggplot(uni_df, 
#        aes(x=longitude, y=latitude,label=name)),na.rm=TRUE)
#    map.uni.region <- map_region + geom_point(color = "blue", size = 2)
#    map.uni.region +  geom_text(hjust=0, vjust=0,angle=45,colour="blue")
#}

university_map <- function(loc='Shanghai', z=12, updateProgress=NULL){
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- "Geolocalizing..."
      updateProgress(detail = text)
    }
    region <- get_map(location = loc, 
        maptype = 'roadmap', #source = 'stamen',
        zoom = z,
        api_key = key)
    if (is.function(updateProgress)) {
      text <- "Fetching map..."
      updateProgress(detail = text)
    }
    map_region <- ggmap(region, extent='panel', base_layer=ggplot(uni_df, 
        aes(x=longitude, y=latitude,label=name)),na.rm=TRUE)
    map.uni.region <- map_region + geom_point(color = "blue", size = 3)
    map.uni.region +  geom_text(hjust=0, vjust=0,angle=45,colour="blue",
        label.size=.5)
}


shinyServer(function(input, output, session) { 

    # A temp file to save the output.
    # This file will be removed later by renderImage
    


    # Geolocalize
    observeEvent(input$show, {
        output$myImage <- renderImage({
            progress <- shiny::Progress$new(style = 'old')
            progress$set(message = "Computing data", value = 0)
            # Close the progress when this reactive exits (even if there's an error)
            on.exit(progress$close())
        
            # Create a closure to update progress.
            # Each time this is called:
            # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
            #   distance. If non-NULL, it will set the progress to that value.
            # - It also accepts optional detail text.
            updateProgress <- function(value = NULL, detail = NULL) {
              if (is.null(value)) {
                value <- progress$getValue()
                value <- value + (progress$getMax() - value) / 5
              }
              progress$set(value = value, detail = detail)
            }
            outfile <- tempfile(fileext='.png')
            paste("\"",paste(input$region, collapse=' '),"\"",collapse='')
            png(outfile, width=700, height=800)
            print(university_map(input$region,input$zoom,updateProgress))
            dev.off()
    
            # Return a list containing the filename
            # modify the .img css
            list(src = outfile,
            contentType = 'image/png',
            width = '100%',
            height = '100%')#'calc(width*8/7)')

        }, deleteFile = TRUE)
   })
    
    # Stocks
    output$graph <- renderPlot({
    
    if (input$stock=='Apple' )
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("AAPL", src = "yahoo", from = start, to = end)
      class(AAPL)
      
      candleChart(AAPL, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("AAPL", src = "yahoo", from = start, to = end)
        class(AAPL)
        candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("AAPL", src = "yahoo", from = start, to = end)
        class(AAPL)
        candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("AAPL", src = "yahoo", from = start, to = end)
          class(AAPL)
          
          candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Google' )
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("GOOG", src = "yahoo", from = start, to = end)
      class(GOOG)
      
      candleChart(GOOG, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("GOOG", src = "yahoo", from = start, to = end)
        class(GOOG)
        candleChart(GOOG, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("GOOG", src = "yahoo", from = start, to = end)
        class(GOOG)
        candleChart(GOOG, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("GOOG", src = "yahoo", from = start, to = end)
          class(GOOG)
          
          candleChart(GOOG, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Microsoft' )
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("MSFT", src = "yahoo", from = start, to = end)
      class(MSFT)
      
      candleChart(MSFT, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("MSFT", src = "yahoo", from = start, to = end)
        class(MSFT)
        candleChart(MSFT, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("MSFT", src = "yahoo", from = start, to = end)
        class(MSFT)
        candleChart(MSFT, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("MSFT", src = "yahoo", from = start, to = end)
          class(MSFT)
          
          candleChart(MSFT, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Tesla' )
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("TSLA", src = "yahoo", from = start, to = end)
      class(TSLA)
      
      candleChart(TSLA, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("TSLA", src = "yahoo", from = start, to = end)
        class(TSLA)
        candleChart(TSLA, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("TSLA", src = "yahoo", from = start, to = end)
        class(TSLA)
        candleChart(TSLA, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("TSLA", src = "yahoo", from = start, to = end)
          class(TSLA)
          
          candleChart(TSLA, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Amazon' )
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("AMZN", src = "yahoo", from = start, to = end)
      class(AMZN)
      
      candleChart(AMZN, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("AMZN", src = "yahoo", from = start, to = end)
        class(AMZN)
        candleChart(AMZN, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("AMZN", src = "yahoo", from = start, to = end)
        class(AMZN)
        candleChart(AMZN, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("AMZN", src = "yahoo", from = start, to = end)
          class(AMZN)
          
          candleChart(AMZN, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    
    else if (input$stock=='Accenture' )
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("ACN", src = "yahoo", from = start, to = end)
      class(ACN)
      
      candleChart(ACN, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("ACN", src = "yahoo", from = start, to = end)
        class(ACN)
        candleChart(ACN, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("ACN", src = "yahoo", from = start, to = end)
        class(ACN)
        candleChart(ACN, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("ACN", src = "yahoo", from = start, to = end)
          class(ACN)
          
          candleChart(ACN, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Coca-Cola')
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("KO", src = "yahoo", from = start, to = end)
      class(KO)
      
      candleChart(KO, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("KO", src = "yahoo", from = start, to = end)
        class(KO)
        candleChart(KO, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("KO", src = "yahoo", from = start, to = end)
        class(KO)
        candleChart(KO, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("KO", src = "yahoo", from = start, to = end)
          class(KO)
          
          candleChart(KO, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Exxon Mobil')
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("XOM", src = "yahoo", from = start, to = end)
      class(XOM)
      
      candleChart(XOM, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("XOM", src = "yahoo", from = start, to = end)
        class(XOM)
        candleChart(XOM, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("XOM", src = "yahoo", from = start, to = end)
        class(XOM)
        candleChart(XOM, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("XOM", src = "yahoo", from = start, to = end)
          class(XOM)
          
          candleChart(XOM, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Top Hundred Stocks')
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("NUS", src = "yahoo", from = start, to = end)
      class(NUS)
      
      candleChart(NUS, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("NUS", src = "yahoo", from = start, to = end)
        class(NUS)
        candleChart(NUS, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("NUS", src = "yahoo", from = start, to = end)
        class(NUS)
        candleChart(NUS, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("NUS", src = "yahoo", from = start, to = end)
          class(NUS)
          
          candleChart(NUS, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Walt Disney')
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("DIS", src = "yahoo", from = start, to = end)
      class(DIS)
      
      candleChart(DIS, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("DIS", src = "yahoo", from = start, to = end)
        class(DIS)
        candleChart(DIS, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("DIS", src = "yahoo", from = start, to = end)
        class(DIS)
        candleChart(DIS, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("DIS", src = "yahoo", from = start, to = end)
          class(DIS)
          
          candleChart(DIS, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='General Electric')
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("GE", src = "yahoo", from = start, to = end)
      class(GE)
      
      candleChart(GE, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("GE", src = "yahoo", from = start, to = end)
        class(GE)
        candleChart(GE, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("GE", src = "yahoo", from = start, to = end)
        class(GE)
        candleChart(GE, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("GE", src = "yahoo", from = start, to = end)
          class(GE)
          
          candleChart(GE, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Bank of America')
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("BA", src = "yahoo", from = start, to = end)
      class(BA)
      
      candleChart(BA, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("BA", src = "yahoo", from = start, to = end)
        class(BA)
        candleChart(BA, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("BA", src = "yahoo", from = start, to = end)
        class(BA)
        candleChart(BA, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("BA", src = "yahoo", from = start, to = end)
          class(BA)
          
          candleChart(BA, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Ford Motor')
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("F", src = "yahoo", from = start, to = end)
      class(F)
      
      candleChart(F, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("F", src = "yahoo", from = start, to = end)
        class(F)
        candleChart(F, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("F", src = "yahoo", from = start, to = end)
        class(F)
        candleChart(F, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("F", src = "yahoo", from = start, to = end)
          class(F)
          
          candleChart(F, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='At&T' )
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("T", src = "yahoo", from = start, to = end)
      class(T)
      
      candleChart(T, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("T", src = "yahoo", from = start, to = end)
        class(T)
        candleChart(T, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("T", src = "yahoo", from = start, to = end)
        class(T)
        candleChart(T, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("T", src = "yahoo", from = start, to = end)
          class(T)
          
          candleChart(T, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Pfizer')
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("PFE", src = "yahoo", from = start, to = end)
      class(PFE)
      
      candleChart(PFE, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("PFE", src = "yahoo", from = start, to = end)
        class(PFE)
        candleChart(PFE, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("PFE", src = "yahoo", from = start, to = end)
        class(PFE)
        candleChart(PFE, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("PFE", src = "yahoo", from = start, to = end)
          class(PFE)
          
          candleChart(PFE, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Morgan Stanley' )
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("MS", src = "yahoo", from = start, to = end)
      class(MS)
      
      candleChart(MS, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("MS", src = "yahoo", from = start, to = end)
        class(MS)
        candleChart(MS, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("MS", src = "yahoo", from = start, to = end)
        class(MS)
        candleChart(MS, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("MS", src = "yahoo", from = start, to = end)
          class(MS)
          
          candleChart(MS, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='JP Morgan Chase' )
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("JPM", src = "yahoo", from = start, to = end)
      class(JPM)
      
      candleChart(JPM, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("JPM", src = "yahoo", from = start, to = end)
        class(JPM)
        candleChart(JPM, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("JPM", src = "yahoo", from = start, to = end)
        class(JPM)
        candleChart(JPM, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("JPM", src = "yahoo", from = start, to = end)
          class(JPM)
          
          candleChart(JPM, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Alibaba' )
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("BABA", src = "yahoo", from = start, to = end)
      class(BABA)
      
      candleChart(BABA, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("BABA", src = "yahoo", from = start, to = end)
        class(BABA)
        candleChart(BABA, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("BABA", src = "yahoo", from = start, to = end)
        class(BABA)
        candleChart(BABA, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("BABA", src = "yahoo", from = start, to = end)
          class(BABA)
          
          candleChart(BABA, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Twitter' )
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("TWTR", src = "yahoo", from = start, to = end)
      class(TWTR)
      
      candleChart(TWTR, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("TWTR", src = "yahoo", from = start, to = end)
        class(TWTR)
        candleChart(TWTR, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("TWTR", src = "yahoo", from = start, to = end)
        class(TWTR)
        candleChart(TWTR, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("TWTR", src = "yahoo", from = start, to = end)
          class(TWTR)
          
          candleChart(TWTR, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
    else if (input$stock=='Verizon' )
      
    {
      if (input$ana=='Moving Average')
      {title <- "Moving Average Plot"
      start <- input$startdate
      end <- input$enddate
      
      getSymbols("VZ", src = "yahoo", from = start, to = end)
      class(VZ)
      
      candleChart(VZ, up.col = "black" ,dn.col = "red", theme = "white", subset = "2016-09-01/")
      addSMA(n = c(input$days))
      }
      else if(input$ana=='Moving Average Convergence Divergence (MACD)')
      {
        title <- "Moving Average Convergence Divergence (MACD)"
        start <- input$startdate
        end <- input$enddate
        getSymbols("VZ", src = "yahoo", from = start, to = end)
        class(VZ)
        candleChart(VZ, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addMACD()
      }
      else if (input$ana=='Rate of Change')
      {
        title <- "Rate of Change"
        start <- input$startdate
        end <- input$enddate
        getSymbols("VZ", src = "yahoo", from = start, to = end)
        class(VZ)
        candleChart(VZ, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/")
        addROC(n = 1, type = c("discrete", "continuous"), col = "red")
      }
      else if (input$ana=='Price Envelop and Relative Strength Index')
      {
        {
          title <- "Price Envelop and Relative Strength Index and Relative Sensitivity Index"
          start <- input$startdate
          end <- input$enddate
          getSymbols("VZ", src = "yahoo", from = start, to = end)
          class(VZ)
          
          candleChart(VZ, up.col = "black", dn.col = "red", theme = "white", subset = "2016-09-01/", TA="addVo(); addRSI()")
          addBBands()
          
          
        }
      }
    }
    
  })
})
