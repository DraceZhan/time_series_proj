
shinyServer(function(input, output) {
  
  output$dygraph1 <-renderDygraph(dygraph(air_reserve_xts[,c(2,3)]) %>% dySeries('visitors', label = 'Total Visitors')  %>%dySeries(
    'bc_visitors', label = 'Box-Cox Transformed Total Visitors') %>% dyRangeSelector(
      height = 20) %>%dyOptions(
        logscale=F) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)))
  output$dygraph2 <-renderDygraph({
    dygraph(air_reserve_xts[,c(3)]) %>% dySeries(
      'bc_visitors', label = 'Box-Cox Transformed Total Visitors') %>%dyRangeSelector(height = 20) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>% dyOptions(logscale=F)})
  output$rolling_average <-renderPlot(ggplot(air_reserve, aes(x= Date)) + geom_line(
    aes(y= visitors, color = "#00ba38"), size =1) + geom_line(
    aes(y= rolling_week, color = "#f8766d"), size =1)+ theme_bw()+scale_color_discrete(name = "Value",
      labels = c("Visitors", "Rolling Weekly")))

  datasetInput <- reactive({switch(input$dataset, "Original" = air_reserve_ts,
                                   "Differenced at lag = 7" = ts_diff)})
  output$summary <- renderPrint({
    ds <- datasetInput()
    summary(ds)
  })
  
  output$acf <- renderPlot({acf(datasetInput()[,as.numeric(input$val_)], main='ACF')})
  output$pacf <- renderPlot({pacf(datasetInput()[,as.numeric(input$val_)], main='PACF')})
  
    
  output$period_ <- renderPlot(periodogram(
    air_reserve_ts[,c(2)],log='yes',plot=TRUE,ylab="Periodogram", xlab="Frequency",lwd=2))
  output$time_freq <- renderTable({time_conv}, colnames = F)
  

  output$adf_ <- renderPrint({
    adf.test(datasetInput()[,as.numeric(input$val_)])
    }
    )
  output$kpss_ <- renderPrint({kpss.test(datasetInput()[,as.numeric(input$val_)])})
  
  output$unit_root <-renderPlot(autoplot(ar(ts_diff[,c(3)], method ='yule-walker', aic=input$bool_, order.max = input$p_)))
  output$box_lj <-renderPrint(Box.test(ar(ts_diff[,c(3)], method ='yule-walker', aic=input$bool_, order.max = input$p_)$resid))
  output$var_expl <- renderPrint(1-(ar(ts_diff[,c(3)], method ='yule-walker', aic=input$bool_, order.max = input$p_)$var.pred))
  
  output$aic_plot <- renderPlot(plot(ar_max$aic, xlab = 'Orders', ylab = 'AIC', main = 'AIC Orders'))
  
  output$resids_plot <- renderPlot(ggplot(ar(ts_diff[,c(3)], method ='yule-walker', aic=input$bool_, order.max = input$p_)$resid, aes(
    x=as.numeric(ar(ts_diff[,c(3)], method ='yule-walker', aic=input$bool_, order.max = input$p_)$resid))) + geom_density(
    alpha =.2) + geom_histogram(fill='lightblue', aes(y=..density..), alpha=.5) +labs(
      title="Distribution of Residuals",x="Residual Value", y = "Density"))
  
  output$arima_plots <- renderPlot(sarima(air_reserve_ts[,c(3)], p = 15,d = 1, q = 0, P=0,D=1,Q=1,S =7, Model = F))

  output$RSME <- renderPrint(rmse(air_reserve_holdout$visitors, preds_))
  
  output$dygraph4 <-renderDygraph({
    dygraph(xts_holdout[,c(2,4)])  %>%dySeries('visitors', label = 'Total Visitors') %>% dySeries(
      'as.numeric(preds_)', label = 'Predicted Values') %>%dyRangeSelector(height = 20) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>% dyOptions(logscale=F)})

  output$resids_Arima_plt<- renderPlot(ggplot(pred_errors) + geom_point(aes(x = seq_along(pred_errors$x), y=abs(as.numeric(pred_errors$x))))+ labs(
    title = "Forecast Errors", x='Forward Delta', y= 'Error'))
  
  output$table <-DT::renderDataTable({air_reserve_full})
  
  })