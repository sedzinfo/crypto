##########################################################################################
# ENVIRO
##########################################################################################
#' @title crypto
#' @import shiny plotly shinyWidgets crypto2 coinmarketcapr
#' @keywords crypto timeseries
#' @export
#' @examples
#' crypto()
crypto<-function() {
  ##########################################################################################
  # LOAD
  ##########################################################################################
  library(shiny)
  library(plotly)
  library(shinyWidgets)
  library(crypto2)
  library(coinmarketcapr)
  options(scipen=999)

  lsd<-Sys.Date()
  setup(api_key="97a7389c-cdae-4c11-a2e7-e3653f1a333c",sandbox=FALSE)
  df_listings<-get_crypto_listings(currency="USD",latest=TRUE)
  df_listings$name_symbol<-paste0(df_listings$name," ","(",df_listings$symbol,")")
  df_listings<-df_listings[,c("name_symbol","USD_price","USD_volume_24h","USD_volume_change_24h","USD_percent_change_24h",
                              "USD_percent_change_7d","USD_percent_change_30d","USD_percent_change_60d","USD_percent_change_90d",
                              "USD_market_cap","USD_market_cap_dominance","USD_fully_diluted_market_cap","USD_last_updated")]

  crypto_names<-crypto_list(only_active=TRUE,add_untracked=FALSE)
  crypto_names$name_symbol<-paste0(crypto_names$name," ","(",crypto_names$symbol,")")
  ##########################################################################################
  # UI
  ##########################################################################################
  ui<-tagList(tags$head(tags$head(HTML("<script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>")),
                        tags$script('
                        var dimension=[0,0];
                        $(document).on("shiny:connected",function(e) {
                        dimension[0]=window.innerWidth;
                        dimension[1]=window.innerHeight;
                        Shiny.onInputChange("dimension",dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0]=window.innerWidth;
                        dimension[1]=window.innerHeight;
                        Shiny.onInputChange("dimension",dimension);
                        });
                        ')),

              navbarPage("",
                         tabPanel("Timeseries",
                                  pickerInput(inputId="name_symbol",
                                              label="Live search",
                                              choices=unique(crypto_names$name_symbol),
                                              options=list(`live-search`=TRUE)),
                                  plotlyOutput("plot_coin_ts",width="100%",height="1080px",)),
                         tabPanel("Top 100",DT::dataTableOutput("listings"))
              ))
  ##########################################################################################
  # SERVER
  ##########################################################################################
  server<-function(input,output) {
    output$plot_coin_ts<-renderPlotly({
      coin_list<-crypto_names[crypto_names$name_symbol%in%input$name_symbol,]
      temp_df<-crypto_history(start_date="20130101",limit=NULL,coin_list=coin_list,convert="USD")
      temp_df$timestamp<-as.Date(as.character(temp_df$timestamp))
      p1<-plot_ly(temp_df,
                  x=temp_df$timestamp,
                  open=temp_df$open,
                  high=temp_df$high,
                  low=temp_df$low,
                  close=temp_df$close,
                  yaxis="y",
                  text=~paste0("\nDate=",temp_df$timestamp,
                               "\nClose=",temp_df$close,
                               "\nOpen=",temp_df$open,
                               "\nHigh=",temp_df$high,
                               "\nLow=",temp_df$low),
                  name="Range",
                  type="candlestick") %>%
        add_lines(x=temp_df$timestamp,
                  y=temp_df$close,
                  line=list(width=1),
                  mode="lines",
                  inherit=FALSE,
                  text=~paste0("\nDate=",temp_df$timestamp,
                               "\nClose=",temp_df$close,
                               "\nOpen=",temp_df$open,
                               "\nHigh=",temp_df$high,
                               "\nLow=",temp_df$low),
                  name="close") %>%
        layout(title=unique(temp_df$name),
               legend=list(orientation="h",xanchor="center"),
               yaxis=list(title="USD"),
               xaxis=list(title="Date",
                          rangeslider=list(type="date",visible=TRUE),
                          rangeselector=list(buttons=list(
                            list(count=1,
                                 label="1 year",
                                 step="year",
                                 stepmode="backward"),
                            list(count=1,
                                 label="year to date",
                                 step="year",
                                 stepmode="todate"),
                            list(count=6,
                                 label="6 months",
                                 step="month",
                                 stepmode="backward"),
                            list(count=3,
                                 label="3 months",
                                 step="month",
                                 stepmode="backward"),
                            list(count=1,
                                 label="1 month",
                                 step="month",
                                 stepmode="backward"),
                            list(count=7,
                                 label="7 days",
                                 step="day",
                                 stepmode="backward"),
                            list(step="all")))))
      p2<-plot_ly(x=temp_df$timestamp,
                  y=temp_df$volume,
                  mode="lines",
                  name="Volume")%>%
        layout(yaxis=list(title="Volume"))
      subplot(p1,p2,heights=c(0.8,0.2),nrows=2,shareX=TRUE,titleY=TRUE)
    })
    output$keepAlive<-renderText({
      req(input$count)
      paste("keep alive",input$count)
    })
    output$listings=DT::renderDataTable({
      names(df_listings)<-gsub("USD_","",names(df_listings))
      names(df_listings)<-gsub("percent_","",names(df_listings))
      df_listings<-data.frame(df_listings,check.names=FALSE)
      df_listings[,names(which(sapply(df_listings,is.numeric)))]<-round(df_listings[,names(which(sapply(df_listings,is.numeric)))],2)
      brks<-quantile(df_listings[,c("change_24h","change_7d","change_30d","change_60d","change_90d")],probs=seq(.05,.95,.001),na.rm=TRUE)
      brksp<-brks[brks>=0]
      brksm<-brks[brks<0]
      clrsm<-rev(seq(255,0,length.out=length(brksm)))%>%{paste0("rgb(255,", ., ",", ., ")")}
      clrsp<-seq(255,0,length.out=length(brksp)+1)%>%{paste0("rgb(", ., ",255,", ., ")")}
      bc<-c(clrsm,clrsp)
      listings<-DT::datatable(df_listings,options=list(pageLength=100,lengthMenu=list(c(20,50,100,-1), c("20","50","100","All"))))
      # listings<-DT::formatStyle(table=listings,columns=c("volume_change_24h","change_24h","change_7d","change_30d","change_60d","change_90d"),
      #                           color=DT::styleInterval(cuts=0,values=c("red","green")))
      listings<-DT::formatStyle(table=listings,columns=c("change_24h","change_7d","change_30d","change_60d","change_90d"),backgroundColor=DT::styleInterval(brks,bc))
      listings<-DT::formatCurrency(table=listings,columns="price",currency="$",interval=3,mark=",",dec.mark=getOption("OutDec"),before=TRUE,digits=4)
      listings<-DT::formatCurrency(table=listings,columns=c("change_24h","change_7d","change_30d","change_60d","change_90d"),currency="%",before=FALSE,digits=2)
      return(listings)
    })
  }
  ##########################################################################################
  # RUN
  ##########################################################################################
  shinyApp(ui = ui, server = server)
}
