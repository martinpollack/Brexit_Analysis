

shinyServer(function(input, output){

  factor_index <- reactive({grep(paste0("^",input$group_choice, "$"),names(ulti_dat))})
  y_index <- reactive({grep(paste0("^", input$y, "$"), names(ulti_dat))}) 
  data <- reactive({
     if(input$y != "all"){
      d<-ulti_dat %>% 
        select(c(factor_index(),16,y_index())) %>%
        filter(!bi_voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(bi_voter_type))


    }else{
      ulti_plot %>%
        
        filter(!bi_voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(bi_voter_type))
    }
   })
  
   ct<- ulti_dat %>%
     select(bi_voter_type) %>%
     filter(!bi_voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(bi_voter_type))%>%
     group_by(bi_voter_type) %>%
     summarise(count = n())

   
 
  output$plot <- renderPlotly({
    
   
    
    if(input$group_choice == "none"){
      if(input$y == "all"){
        p1<-ggplot(data(), aes_string(x = "bi_voter_type", y = "value", fill = "measure")) + 
          geom_boxplot() + 
          scale_x_discrete(labels=c("Always-Stay", "Switched-Stay", "Switched-Leave", "Always-Leave"))+
          annotate("text", x = 1:4, y = -0.1, label = c(as.character(ct$count)))+
          xlab("Voter Type")
          scale_fill_manual(
                            name="Voter Type",
                            labels=c("Always Stay", "Switched-Stay",
                                     "Switched-Leave", "Always Leave")) 
        
        ggplotly(p1) %>%
          layout(height = input$plotHeight, autosize=TRUE) %>%
          layout(boxmode = "group")
      }else{
        p2<-ggplot(data(), aes_string(x = "bi_voter_type", y = input$y, fill = "bi_voter_type")) + 
          geom_boxplot() + 
          scale_x_discrete(labels=c("Always-Stay", "Switched-Stay", "Switched-Leave", "Always-Leave"))+
          annotate("text", x = 1:4, y = -0.1, label = c(as.character(ct$count)))+
          scale_fill_manual(values=c("#2c7fb8","#7fcdbb","#fdae6b","#e6550d"),
                            name="Voter Type",
                            labels=c("Always Stay", "Switched-Stay", 
                                     "Switched-Leave", "Always Leave"))+
          xlab("   ")+
          theme(legend.position="none",
                axis.text.x = element_text(size = 15))
        
        
        ggplotly(p2) %>%
          layout(height = input$plotHeight, autosize=TRUE) %>%
          layout(boxmode = "group")
      }
    }else{
      p3<-data()%>%
        ggplot(aes_string(x = "bi_voter_type", y = input$y, fill = input$group_choice)) + 
        geom_boxplot() + 
        scale_x_discrete(labels=c("Always-Stay", "Switched-Stay", "Switched-Leave", "Always-Leave"))+
        xlab("Voter Type")
        #annotate("text", x = 1:4, y = 0, label = c(as.character(ct$count)))
      
      ggplotly(p3) %>%
        layout(height = input$plotHeight, autosize=TRUE) %>%
        # plot_ly(data(), y = ~immig_index5, x = ~voter_type, color = paste0("~",input$group_choice), type = "box") %>%
        layout(boxmode = "group")
    }
      })
 
})
