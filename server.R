function(input, output) { 
  
  tab_list <- NULL
  
  
  showNotification("Interative Plotting Project", duration = NULL, type = "message")
  
  
  
  
  # ___________________________________________________________________________________________________________________
  # ____________________________________________________ VALUE BOX ____________________________________________________
  
  
  
  # Jumlah Orang Meninggal
  
  output$sum_meninggal <- renderValueBox({
    mng <- disaster_clean %>% 
      
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Meninggal = sum(Meninggal)) 
    
    valueBox(subtitle = "Jumlah Orang Meninggal", 
               value = sum(mng$Jumlah_Meninggal),
               icon = icon("bed-pulse"),
               color = "red")
  })
  
  
  # Jumlah Orang Hilang
  
  
  output$sum_hilang <- renderValueBox({
    hlg <- disaster_clean %>% 
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Hilang = sum(Hilang)) 
    
  
      valueBox(subtitle = "Jumlah Orang Hilang", 
               value = sum(hlg$Jumlah_Hilang),
               icon = icon("person-circle-exclamation"),
               color = "yellow")
  })
  
  

  # Jumlah Orang Terluka
  
  output$sum_terluka <- renderValueBox({
    trl <- disaster_clean %>% 
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Terluka = sum(Terluka)) 
      
      valueBox(subtitle = "Jumlah Orang Terluka", 
               value = sum(trl$Jumlah_Terluka),
               icon = icon("user-injured"),
               color = "green")
  })
  
  # Jumlah Rumah Rusak
  
  output$sum_rusak <- renderValueBox({
    rrsk <- disaster_clean %>% 
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Rusak = sum(Rumah_Rusak)) 
      
      valueBox(subtitle = "Jumlah Rumah Rusak", 
               value = sum(rrsk$Jumlah_Rusak),
               icon = icon("house-circle-xmark"),
               color = "blue")
  })
  
  
  # valueBoxOutput("sum_terendam")
  
  output$sum_terendam <- renderValueBox({
    rtrn <- disaster_clean %>% 
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Terendam = sum(Rumah_Terendam)) 
      
      
      valueBox(subtitle = "Jumlah Rumah Terendam", 
               value = sum(rtrn$Jumlah_Terendam),
               icon = icon("house-flood-water"),
               color = "teal")
  })
  
  
  # valueBoxOutput("sum_fasum"))
  
  output$sum_fasum <- renderValueBox({
    fsmr <- disaster_clean %>% 
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Fasum = sum(Fasum_Rusak)) 
      
      valueBox(subtitle = "Jumlah Fasum Rusak", 
               value = sum(fsmr$Jumlah_Fasum),
               icon = icon("building-shield"),
               color = "purple")
  })
  
  
  
  # -------------------------- PLOT 1 --------------------------------------
  
  
  # Plot 1: Rangking -> Top Provinsi 
  
  output$plot_rangking <- renderPlotly({
    # Data Prep Plot 1
    plot_agg_1 <- disaster_clean %>%
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Provinsi, Kejadian) %>% 
      summarise(Jumlah_Kejadian = n()) %>% 
      ungroup() %>% 
      arrange(desc(Jumlah_Kejadian)) %>% 
      top_n(10)
    
    plot_agg_1 <-  plot_agg_1 %>% 
      mutate(label=glue("Provinsi : {Provinsi}
      Jumlah Kejadian : {Jumlah_Kejadian} Kali"))
    
    # Plot Rangking Statis
    plot_rangking <- plot_agg_1 %>% 
      top_n(10) %>% 
      ggplot(mapping = aes(x= Jumlah_Kejadian , y= reorder(Provinsi, Jumlah_Kejadian), text = label)) +
      geom_col(aes(fill = Jumlah_Kejadian)) +
      
      scale_x_continuous(breaks = seq(0, {max(plot_agg_1$Jumlah_Kejadian)}, 1)) +
      scale_fill_gradient(high = "#ff0a54" , low = "#ff99ac") +
      
      labs(title = glue("TOP 10 Provinsi berdasarkan Jumlah Kejadian {input$input_kejadian}  Bulan {input$input_bulan}") ,
           x = NULL,
           y = NULL) +
      theme(legend.position = "none") +
      
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 1, vjust = 3),
            plot.title.position = "center",
            axis.ticks.y = element_blank(),
            plot.background = element_rect(fill = "#e2eafc"),
            panel.background = element_rect(fill = "#e2eafc"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            axis.text = element_text(color="black"))
            
    
    # Plot Rangking Interaktif
    ggplotly(plot_rangking, tooltip = "text")
  })
  
  
  
  
  # -------------------------- PLOT 2 --------------------------------------
  
  output$plot_trend <- renderPlotly({
   
    plot_agg_2 <- disaster_clean %>% 
      filter(Kejadian == input$input_kejadian) %>%
      filter(Bulan == input$input_bulan) %>% 
      group_by(Tanggal, Kejadian) %>% 
      summarise(Jumlah_Kejadian =n())
  
    plot2 <- plot_agg_2 %>% 
      ggplot(mapping = aes( x= Tanggal, 
                            y = Jumlah_Kejadian)) +
      geom_area(aes(group = Kejadian), fill= "#e0b1cb", color= "#bd4f6c", linetype = "dashed") +
      geom_point(aes(group = Kejadian,
                     text = glue("Tanggal :  {Tanggal}
                                    Jumlah Kejadian : {Jumlah_Kejadian} Kali")), color =  "#f48498") +
     
      
      scale_x_continuous(breaks = seq(1,31,1)) +
      scale_y_continuous(breaks = seq(0,{max(plot_agg_2$Jumlah_Kejadian)},1)) +
      
      labs(title = glue("Trend Jumlah Kejadian Bencana {input$input_kejadian} Bulan {input$input_bulan} di Seluruh Indonesia"),
           x = "Tanggal",
           y = "Jumlah Kejadian") +
      
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.05, vjust = 0.05),
            plot.title.position = "center",
            axis.ticks.y = element_blank(),
            plot.background = element_rect(fill = "#e2eafc"),
            panel.background = element_rect(fill = "#e2eafc"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            axis.text = element_text(color="black"))
    
    
    ggplotly(plot2, tooltip = "text")

  
  })
  
  
  # ___________________________________________________________________________________________________________________
  # _____________________________________________________ PAGE 3 ______________________________________________________
  
  
  
  # --------------------------------------- PLOT 3.1 ------------------------------------
  
  
  output$prop_dis <- renderPlotly({
    
   


    plot_agg_3.1 <- disaster_clean %>%
      filter(Provinsi == input$input_prov) %>%
      group_by(Bulan) %>%
      summarise(Jumlah_Kejadian = n())
    
    plot_agg_3.1 <- plot_agg_3.1 %>% 
      mutate(label= glue("Bulan : {Bulan} 
                          Jumlah Kejadian : {Jumlah_Kejadian}"))
    
    plot3 <- plot_agg_3.1 %>% 
      ggplot(mapping = aes(x = Bulan, y = Jumlah_Kejadian, text = label)) +
      geom_col(fill= "#a7c957") +
      geom_text(aes(label = Jumlah_Kejadian), nudge_y = 1) +
    
      scale_y_continuous() +
      
      
      labs(title = glue("Jumlah Semua Kejadian Bencana di Provinsi {input$input_prov}"),
           x = NULL,
           y = "Jumlah Kejadian") +
      
      
      theme(legend.position = "none") +
      
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.05, vjust = 0.05),
            plot.title.position = "center",
            axis.ticks.y = element_blank(),
            plot.background = element_rect(fill = "#e2eafc"),
            panel.background = element_rect(fill = "#e2eafc"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            axis.text = element_text(color="black"))
      
      ggplotly(plot3, tooltip = "text")
    

  })
  
  
  
  
  
  
  # ---------------------------------------- PLOT 3.2 --------------------------------------
    
  
  
  output$prop_sum <- renderPlotly({
    
    
    plot_agg_3.2 <- disaster_clean %>%
      filter(Provinsi == input$input_prov) %>% 
      group_by(Bulan, Kejadian) %>% 
      summarise(Jumlah_Kejadian = n())
    
    plot_agg_3.2 <- plot_agg_3.2 %>% 
      mutate(label= glue("Bulan : {Bulan} 
                          Jenis Bencana : {Kejadian}
                          Jumlah Kejadian : {Jumlah_Kejadian}"))
    
    plot3.2 <- plot_agg_3.2 %>% 
      ggplot(mapping = aes(x = Bulan, y = Jumlah_Kejadian, text = label)) +
      geom_col(aes(fill = Kejadian), position = "fill") +
      
      
      scale_y_continuous() +
      
      
      labs(title = glue("Proporsi Kejadian Bencana di Provinsi {input$input_prov}"),
           x = NULL,
           y = "Proporsi") +
      
      
      theme(legend.position = "none") +
      
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.05, vjust = 0.05),
            plot.title.position = "center",
            axis.ticks.y = element_blank(),
            plot.background = element_rect(fill = "#e2eafc"),
            panel.background = element_rect(fill = "#e2eafc"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.line.y = element_line(color = "black"),
            axis.text = element_text(color="black"))
    
    ggplotly(plot3.2, tooltip = "text")
    
  })
  
  
  
  
  
  
  
  # ___________________________________________________________________________________________________________________
  # ____________________________________________________ DATA TABLE ____________________________________________________
  
  
  
  output$table_data <- renderDataTable({
    disaster_table <- disaster_clean %>% 
      select(-Penyebab)
    
    datatable(data = disaster_table,
              options = list(scrollX = TRUE)
    )
  })
  
  
  
  
}