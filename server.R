library(shiny);library(ggplot2)

source("bias_variance.R")


shinyServer(function(input, output){
  
     dat<-reactive(fdata(n=input$n,
                         x_min=input$x_min,
                         x_max=input$x_max,
                         sigma=input$sigma
                         )) 
     

     res<-reactive(fres(training_km_sim=dat()$training_km_sim,testing_km_sim=dat()$testing_km_sim, 
                    training=dat()$training_km[[1]],testing=dat()$testing_km[[1]],
                    res_tr=dat()$res_tr,res_ts=dat()$res_ts,
                    res_y_hat=dat()$res_y_hat,res_y_hat_ts=dat()$res_y_hat_ts,
                    res_y_hat_sim=dat()$res_y_hat_sim,res_y_hat_ts_sim=dat()$res_y_hat_ts_sim,
                    ks=input$ks))
     
    
     output$res_g<-renderDataTable({res()$res_g},options=list(pageLength=5))
     output$kso<-renderPrint({dat()$kso})    
     output$g1<-renderPlot({res()$g1})   
     output$g3<-renderPlot({res()$g3})
     output$g4<-renderPlot({res()$g4})
})








