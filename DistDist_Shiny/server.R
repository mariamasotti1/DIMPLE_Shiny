#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(ggplot2);library(dplyr);library(here);library(spatstat);library(tidyr)

library(DistDist)

source("helpers.R")

options(shiny.maxRequestSize = 100000*1024^2)


function(input, output, session) {
  
  
  
  data <- reactive({ 
    req(input$file1) 
    
    inFile <- input$file1 
    
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    updateSelectInput(session, inputId = 'xcol', label = 'X',
                      choices = names(df), selected = "X")
    updateSelectInput(session, inputId = 'ycol', label = 'Y',
                      choices = names(df), selected = "Y")
    updateSelectInput(session, inputId = 'image_id', label = 'Image ID',
                      choices = names(df), selected = "spots")
    updateSelectInput(session, inputId = 'type_id', label = 'Cell Type',
                      choices = names(df), selected = "type")
    
    
    
    return(df)
  })
  
  
  output$contents <- renderTable({ 
    head(data())
  })
  
  metadata <- reactive({ 
    req(input$file2) 
    
    inFile <- input$file2 
    
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    updateSelectInput(session, inputId = 'md_image_id', label = 'Image ID',
                      choices = names(df), selected = "spots")
    updateSelectInput(session, inputId = 'patient_id', label = 'Patient ID',
                      choices = names(df), selected = "Patient")
    updateSelectInput(session, inputId = 'surv_time', label = 'Survival Time',
                      choices = names(df), selected = "OS")
    updateSelectInput(session, inputId = 'surv_event', label = 'Survival Event',
                      choices = names(df), selected = "OS_Censor")
    
    return(df)
  })
  
  output$contents_md <- renderTable({ 
    head(metadata())
  })
  
  
  data.frame<-reactive({
    
    df<-data()%>%select(.data[[input$xcol]],.data[[input$ycol]],.data[[input$type_id]],.data[[input$image_id]]) %>% group_by(.data[[input$image_id]])%>%
      mutate(x=as.numeric(.data[[input$xcol]]),y=as.numeric(.data[[input$ycol]]),cell_type=as.factor(.data[[input$type_id]]),image_id=as.factor(.data[[input$image_id]]))
    
    updateSelectInput(session, inputId = 'image_to_plot', label = 'Select image to plot',choices = unique(df$image_id), selected = "1_A")
    updateSelectInput(session, inputId = 'intens_to_plot', label = 'Select image to plot intensities',choices = unique(df$image_id), selected = "1_A")
    updateSelectInput(session, inputId = 'dm_to_plot', label = 'Select image id to plot distance matrix',choices = unique(df$image_id))
    updateSelectInput(session, inputId = 't1', label = 'Select cell type 1',choices = unique(df$cell_type),selected="tumor cells")
    updateSelectInput(session, inputId = 't2', label = 'Select cell type 2',choices = unique(df$cell_type),selected="CD4+ T cells")
    updateSelectInput(session, inputId = 'quant_cell_type', label = 'Select cell type',choices = unique(df$cell_type),selected="tumor cells")
    updateSelectInput(session, inputId = 'quantile_to_plot', label = 'Select image to plot quantiles of intensities',choices =  unique(df$image_id), selected = "1_A")
    return(df)
  })
  
  md.data.frame<-reactive({
    
    df<-metadata()%>%mutate(slide_id=as.factor(.data[[input$md_image_id]]),patient_id=as.factor(.data[[input$patient_id]]),surv_time=as.numeric(.data[[input$surv_time]]),surv_event=as.factor(.data[[input$surv_event]]))
    updateSelectInput(session, inputId = 'grouping_var', label = 'Select grouping variable',choices = names(df),selected="Patient")
    return(df)
  })
  
  
  
  output$pp_contents <- renderPlot({   
    
    df<-data.frame()%>%filter(image_id==input$image_to_plot)
    req(input$image_to_plot%in%df$image_id)
    obj<-new_MltplxObject(
      x = df$x,
      y = df$y,
      marks = df$cell_type,
      slide_id = df$image_id)
    
    
    plot(obj$mltplx_image)
    # req(data.frame())
    # 
    # df<-data.frame()
    # req(input$image_to_plot%in%df$image_id)
    # obj<-new_MltplxExperiment(
    #   x = df$x,
    #   y = df$y,
    #   marks = df$cell_type,
    #   slide_id = df$image_id)
    # plot_ppp(obj,input$image_to_plot)
    
    
  })
  
  
  output$intens_plot <- renderPlot({
    df<-data.frame()%>%filter(image_id==input$intens_to_plot)
    req(input$intens_to_plot%in%df$image_id)
    intens_data<-new_MltplxObject(
      x = df$x,
      y = df$y,
      marks = df$cell_type,
      slide_id = df$image_id,ps=input$eps,bw=input$bw )
    as.data.frame(intens_data$mltplx_intensity$intensities)%>%pivot_longer(-c("X","Y"))%>%
      ggplot(aes(X,Y))+geom_raster(aes(fill=value))+facet_wrap(name~.)
    
    
  })
  
  func_list<-list(jsd_unnormalized,KL_div)
  names(func_list)<-c("JSD","KLD")
  
  output$one_dm_plot<- renderPlot({
    df<-data.frame()%>%filter(image_id==input$dm_to_plot)
    req(input$dm_to_plot%in%df$image_id)
    distance_data<-new_MltplxObject(
      x = df$x,
      y = df$y,
      marks = df$cell_type,
      slide_id = df$image_id,ps=input$eps,bw=input$bw, dist_metric=func_list[[input$dist_metric]])
    
    plot_dist(distance_data,input$dm_to_plot, mode = "heatmap")
    
    
  })
  
  output$quantile_intens_plot<-renderPlot({
    
    df<-data.frame()%>%filter(image_id==input$quantile_to_plot)
    req(input$quantile_to_plot%in%df$image_id)
    intens_data<-new_MltplxObject(
      x = df$x,
      y = df$y,
      marks = df$cell_type,
      slide_id = df$image_id,ps=input$eps,bw=input$bw)
    
    
    
    
    
  })
  
  out <- reactive({
    req(data.frame())
    shinyjs::disable("submit")
    df<-data.frame()
    if(!is.null(input$file2)){
      meta<-md.data.frame()
      
      obj<-new_MltplxExperiment(
        x = df$x,
        y = df$y,
        marks = df$cell_type,
        slide_id = df$image_id,metadata=meta)
      
      obj<-update_intensity(obj,ps=input$eps,bw=input$bw)
      
      obj<-update_dist(obj,func_list[[input$dist_metric]])
      
      #   new_MltplxExperiment(
      #   x = df$x,
      #   y = df$y,
      #   marks = df$cell_type,
      #   slide_id = df$image_id,ps=input$eps,bw=input$bw,dist_metric = func_list[[input$dist_metric]])
      # obj<-update_metadata(obj)
    }else{
      obj<-new_MltplxExperiment(
        x = df$x,
        y = df$y,
        marks = df$cell_type,
        slide_id = df$image_id,ps=input$eps,bw=input$bw,dist_metric = func_list[[input$dist_metric]]) 
    }
    
    
    
    return(obj)
    
  })
  
  
  observeEvent(input$submit, {
    withCallingHandlers({
      shinyjs::html("text", "")
      req(out())
    },
    message = function(m) {
      shinyjs::html(id = "text", html = paste0(m$message), add = FALSE)
    })
  })
  
  
  output$dm_plot <- renderPlot({
    req( input$dm_to_plot%in%data.frame()$image_id)
    plot_dist(out(), input$dm_to_plot, mode = "heatmap")
    
  })
  
  output$patient_boxplot<-renderPlot(({
    req(out)
    patient_boxplots(out(),input$t1,input$t2,grouping_var=input$grouping_var,label_spots=TRUE)
    
  }))
  
}
