library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(stringr)
library(leaflet)
library(sf)
library(scales)
library(plotly)
library(janitor)
library(DT)
library(shinydashboard)
library(shinythemes)
library(shiny)

resident_data_w_geo_code <- read_rds("2020_10_31_resident_data_w_geo_code.rds")

wide_sum_data_program<- read_rds("2020_10_31_wide_sum_data_program.rds")
wide_sum_data_med_school <- read_rds("2020_10_31_wide_sum_data_med_school.rds")

combined_program_school_geo_data <- wide_sum_data_program%>%
    mutate(data_type = "By Program") %>%
    select(data_type,
           primary_name = program,
           primary_lat = program_lat,
           primary_long = program_long,
           secondary_name = medical_school,
           secondary_lat = medical_school_lat,
           secondary_long = medical_school_long,
           num_residents_2,
           sum_data = data,
           sum_label = label) %>%
    bind_rows(wide_sum_data_med_school %>%
                  mutate(data_type = "By Medical School") %>%
                  select(data_type,
                         primary_name = medical_school,
                         primary_lat = medical_school_lat,
                         primary_long = medical_school_long,
                         secondary_name = program_w_label,
                         secondary_lat = program_lat,
                         secondary_long = program_long,
                         num_residents_2,
                         sum_data = data,
                         sum_label = label))

long_sum_data_program <- read_rds("2020_10_31_long_sum_data_program.rds")

combined_geo_long <- long_sum_data_program %>%
    mutate(data_type = "By Program") %>%
    select(data_type,
           primary_name=program,
           secondary_name=medical_school,
           long,
           lat) %>%
    bind_rows(long_sum_data_program %>%
                  mutate(data_type = "By Medical School") %>%
                  select(data_type,
                         primary_name=medical_school,
                         secondary_name=program,
                         long,
                         lat))


program_data_details <- read_rds("2020_10_31_program_data_details.rds")
medical_school_data_details <- read_rds("2020_10_31_medical_school_data_details.rds")

combined_data_details <- program_data_details %>%
    bind_rows(medical_school_data_details) %>%
    filter(!is.na(primary_name))


data_w_geo_code_sum_program <- read_rds("2020_10_31_data_w_geo_code_sum_program.rds")
data_w_geo_code_sum_medical_school <- read_rds("2020_10_31_data_w_geo_code_sum_medical_school.rds")
  
combined_summary_data <- data_w_geo_code_sum_program %>%
    bind_rows(data_w_geo_code_sum_medical_school)

center_of_us <- tibble(long = -98.5795,
                       lat=39.8283)


overtime_data <- read_rds("2020_10_31_overtime_data.rds")



ui <- fluidPage(
    # Sidebar with a slider input for number of bins
    navbarPage("Neurosurgery Residency Data", theme=shinytheme("flatly"),
               tabPanel("Individual Data",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("data_type",
                                            "Data Type:",
                                            choices = c("By Program","By Medical School"),
                                            selected = "By Program"),
                                selectInput("individual_selected",
                                            "Program:",
                                            choices = unique(resident_data_w_geo_code$program),
                                            selected = "Cleveland Clinic"),
                                htmlOutput("genericSummaryTable")
                            ),
                            mainPanel(
                                tabsetPanel(type="tabs",
                                            #tabPanel("Map",plotlyOutput("mapPlot")),
                                            #tabPanel("Map",leafletOutput("leafletMap")),
                                            #tabPanel("Map",leafletOutput("leafletMapGeneric"))
                                            tabPanel(uiOutput("maptitle_panel"),leafletOutput("leafletMapGeneric"))
                                            #tabPanel("Overtime",plotlyOutput("programTrendOvertime"))
                                ),
                                DT::dataTableOutput("genericDataTable")
                            )
                        )
               ),
               tabPanel("Comparison Data",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("data_type_2",
                                            "Data Type:",
                                            choices = c("By Program","By Medical School"),
                                            selected = "By Program"),
                                selectInput("individual_selected_2",
                                            "Program:",
                                            choices = unique(resident_data_w_geo_code$program),
                                            selected = "Cleveland Clinic"),
                                selectInput("quant_cat_2",
                                            "Category:",
                                            choices = c("# of Residents",
                                                        "% of Home Students",
                                                        "# of Home Students",
                                                        "% of International Students",
                                                        "# of International Students",
                                                        "% of Female Residents",
                                                        "# of Female Residents",
                                                        "% of Male Residents",
                                                        "# of Male Residents",
                                                        "% w/ Advanced Degree",
                                                        "# w/ Advanced Degree",
                                                        "% w/ PhD",
                                                        "# w/ PhD"),
                                            selected = "% of Home Students")
                                # selectInput("program",
                                #             "Program:",
                                #             choices = unique(data_w_geo_code$program),
                                #             selected = "Cleveland Clinic"),
                            ),
                            mainPanel(
                                plotlyOutput("summaryGenericCharBarPlot"),
                                DT::dataTableOutput("summaryGenericCompTable")
                            )
                        )
               ),
               tabPanel("Trends Over Time",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("quant_cat_3",
                                        "Category:",
                                        choices = c("% of Home Students",
                                                    "% of International Students",
                                                    "% of Female Residents",
                                                    #"% of Male Residents",
                                                    "% w/ Advanced Degree",
                                                    "% w/ PhD"),
                                         selected = "% of Home Students")
                          ),
                          mainPanel(
                             plotlyOutput("trendLinePlot"),
                             DT::dataTableOutput("trendTable")
                          )
                        )
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

    observeEvent(input$data_type,{
        updateSelectInput(session,"individual_selected",
                          label = paste0(str_replace(input$data_type,"By ",""),":"),
                          choices = combined_program_school_geo_data %>%
                              filter(data_type == input$data_type) %>%
                              distinct(primary_name) %>%
                              pull(primary_name),
                          selected = ifelse(input$data_type == "By Program",
                                            "Cleveland Clinic",
                                            "Case Western Reserve University School of Medicine"))
        })
    
    observeEvent(input$data_type_2,{
        updateSelectInput(session,"individual_selected_2",
                          label = paste0(str_replace(input$data_type_2,"By ",""),":"),
                          choices = combined_program_school_geo_data %>%
                              filter(data_type == input$data_type_2) %>%
                              distinct(primary_name) %>%
                              pull(primary_name),
                          selected = ifelse(input$data_type_2 == "By Program",
                                            "Cleveland Clinic",
                                            "Case Western Reserve University School of Medicine"))
        choices_df = tibble(data_type = c("By Program",
                                          "By Medical School"),
                            choices = c(list(c("# of Residents",
                                               "% of Home Students",
                                               "# of Home Students",
                                               "% of International Students",
                                               "# of International Students",
                                               "% of Female Residents",
                                               "# of Female Residents",
                                               "% of Male Residents",
                                               "# of Male Residents",
                                               "% w/ Advanced Degree",
                                               "# w/ Advanced Degree",
                                               "% w/ PhD",
                                               "# w/ PhD")),
                                        list(c("# of Residents",
                                               "% of Home Students",
                                               "# of Home Students",
                                               "% of Female Residents",
                                               "# of Female Residents",
                                               "% of Male Residents",
                                               "# of Male Residents",
                                               "% w/ Advanced Degree",
                                               "# w/ Advanced Degree",
                                               "% w/ PhD",
                                               "# w/ PhD"))))
        updateSelectInput(session,"quant_cat_2",
                          choices = choices_df %>%
                              filter(data_type == input$data_type_2) %>%
                              pull(choices) %>%
                              unlist())
    })
    test_df <- reactive(combined_data_details %>%
                            filter(data_type == input$data_type_2) %>%
                            #filter(primary_name==input$individual_selected_2) %>%
                            mutate(y_total = case_when(input$quant_cat_2 == "# of Residents" ~ as.double(num_residents),
                                                       input$quant_cat_2 == "% of Home Students" ~ pct_home_students,
                                                       input$quant_cat_2 == "# of Home Students" ~ as.double(num_home_students),
                                                       input$quant_cat_2 == "% of International Students" ~ pct_international_student,
                                                       input$quant_cat_2 == "# of International Students" ~ as.double(num_international_student),
                                                       input$quant_cat_2 == "% of Female Residents" ~ pct_female_residents,
                                                       input$quant_cat_2 == "# of Female Residents" ~ as.double(num_female_residents),
                                                       input$quant_cat_2 == "% of Male Residents" ~ pct_male_residents,
                                                       input$quant_cat_2 == "# of Male Residents" ~ as.double(num_male_residents),
                                                       input$quant_cat_2 == "% w/ PhD" ~ pct_phd,
                                                       input$quant_cat_2 == "# w/ PhD" ~ as.double(num_phd),
                                                       input$quant_cat_2 == "% w/ Advanced Degree" ~ pct_advanced_degree,
                                                       input$quant_cat_2 == "# w/ Advanced Degree" ~ as.double(num_advanced_degree)
                            )) %>%
                            distinct(primary_name,y_total) %>%
                            mutate(primary_name_y_total = fct_reorder(primary_name,desc(y_total))) %>%
                            arrange(primary_name_y_total))

        
#Generic leaflet Map
output$leafletMapGeneric <- renderLeaflet({
            primary_point <- combined_program_school_geo_data %>%
                filter(data_type == input$data_type) %>%
                filter(primary_name==input$individual_selected) %>%
                distinct(primary_name,
                         primary_lat,
                         primary_long)
            
            combined_program_school_geo_data %>%
                filter(data_type == input$data_type) %>%
                filter(primary_name==input$individual_selected) %>%
                leaflet() %>%
                addTiles() %>%
                #addProviderTiles("CartoDB.Positron") %>%
                addPolylines(data=combined_geo_long %>%
                                 filter(data_type == input$data_type) %>%
                                 filter(primary_name==input$individual_selected),
                             lng=~long,
                             lat=~lat,
                             group=~secondary_name,
                             opacity = 0.01,
                             stroke=T,
                             weight=2) %>%
                addCircleMarkers(lat=~secondary_lat,
                                 lng=~secondary_long,
                                 popup = ~sum_label,
                                 radius = ~num_residents_2*2) %>%
                addCircles(data = primary_point,
                           lat=~primary_lat,
                           lng=~primary_long,
                           color="red",
                           popup = ~primary_name) %>%
                setView(lng=center_of_us$long,
                        lat=center_of_us$lat,
                        zoom=4)
            
            
        })

    output$genericSummaryTable <- renderUI({
       sum_table<-combined_summary_data %>% 
            filter(data_type == input$data_type) %>%
            filter(primary_name==input$individual_selected) %>% 
            pull(label)
        HTML(paste(sum_table))
    })
    
    output$genericDataTable <- DT::renderDataTable({
        combined_program_school_geo_data %>%
            filter(data_type == input$data_type) %>%
            filter(primary_name==input$individual_selected) %>%
            mutate(num = num_residents_2,
                   pct = round_half_up(num_residents_2/sum(num_residents_2)*100,digits=1)) %>%
            select(secondary_name,num,pct) %>%
            arrange(desc(num)) %>%
            datatable(options=list(pageLength=3,
                                   searching=F,
                                   lengthChange=F),
                      escape=F,
                      colnames = c(ifelse(str_detect(input$data_type,"Program"),
                                          "Medical School",
                                          "Program"),"n","%"))
    })
    


output$summaryGenericCharBarPlot <- renderPlotly({
    

    test_df <- combined_data_details %>%
        filter(data_type == input$data_type_2) %>%
        #filter(primary_name==input$individual_selected_2) %>%
        mutate(y_total = case_when(input$quant_cat_2 == "# of Residents" ~ as.double(num_residents),
            input$quant_cat_2 == "% of Home Students" ~ pct_home_students,
            input$quant_cat_2 == "# of Home Students" ~ as.double(num_home_students),
            input$quant_cat_2 == "% of International Students" ~ pct_international_student,
            input$quant_cat_2 == "# of International Students" ~ as.double(num_international_student),
            input$quant_cat_2 == "% of Female Residents" ~ pct_female_residents,
            input$quant_cat_2 == "# of Female Residents" ~ as.double(num_female_residents),
            input$quant_cat_2 == "% of Male Residents" ~ pct_male_residents,
            input$quant_cat_2 == "# of Male Residents" ~ as.double(num_male_residents),
            input$quant_cat_2 == "% w/ PhD" ~ pct_phd,
            input$quant_cat_2 == "# w/ PhD" ~ as.double(num_phd),
            input$quant_cat_2 == "% w/ Advanced Degree" ~ pct_advanced_degree,
            input$quant_cat_2 == "# w/ Advanced Degree" ~ as.double(num_advanced_degree)
        )) %>%
        distinct(primary_name,primary_name_wo_label,y_total) %>%
        mutate(primary_name_y_total = fct_reorder(primary_name,desc(y_total))) %>%
        arrange(primary_name_y_total) %>%
        mutate(y_total_color = colorRampPalette(c("blue","gray"))(nrow(.)),
               y_total_color = ifelse(str_detect(primary_name,input$individual_selected_2),
                                      "#FF0000", #red
                                      y_total_color))
    

    plot_bar <- test_df %>%
        filter(primary_name_y_total!="Not Reported") %>% 
        ggplot(aes(x=primary_name_y_total,y=case_when(y_total==0 & str_detect(input$quant_cat_2,"#")~0.1,
                                                 y_total==0 & str_detect(input$quant_cat_2,"%")~0.5,
                                                 T~y_total)))+
        geom_col(aes(fill=y_total_color,
                     text=paste0(primary_name_wo_label,
                                 "<br>",input$quant_cat_2,": ",y_total)))+
        scale_y_continuous(expand=expand_scale(mult=c(0,0.1)),
                           breaks = scales::pretty_breaks(n=5))+
        scale_fill_identity()+
        labs(y=input$quant_cat_2,
             x=str_replace(input$data_type_2,"By ",""),
             title = paste0(str_replace(input$data_type_2,"By ",""),"s by ",input$quant_cat_2))+
        #scale_x_discrete(dlabels = function(x) str_wrap(x, width = 30))+
        theme_bw()+
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) 
    
    ggplotly(plot_bar,
             tooltip="text") %>%
        config(displayModeBar = F) %>%
        layout(xaxis=list(fixedrange=TRUE),
               yaxis=list(fixedrange=TRUE),
               showlegend=F) 

})

output$summaryGenericCompTable <- DT::renderDataTable({
    test_df <- combined_data_details %>%
        filter(data_type == input$data_type_2) %>% 
        distinct(primary_name,
                 num_residents,
                 pct_home_students,num_home_students,
                 pct_female_residents,num_female_residents,
                 pct_male_residents,num_male_residents,
                 pct_international_student,num_international_student,
                 pct_phd,num_phd,
                 pct_advanced_degree,num_advanced_degree) %>%
        mutate(cat_pct = case_when(input$quant_cat_2 == "# of Residents" ~ 100,
            str_detect(input$quant_cat_2,"Home") ~ pct_home_students,
            str_detect(input$quant_cat_2,"International") ~ pct_international_student,
            str_detect(input$quant_cat_2,"Female") ~ pct_female_residents,
            str_detect(input$quant_cat_2,"Male") ~ pct_male_residents,
            str_detect(input$quant_cat_2,"PhD") ~ pct_phd,
            str_detect(input$quant_cat_2,"Advanced") ~ pct_advanced_degree,
            T ~ pct_home_students),
            cat_n = case_when(input$quant_cat_2 == "# of Residents" ~ num_residents,
                str_detect(input$quant_cat_2,"Home") ~ num_home_students,
                str_detect(input$quant_cat_2,"International") ~ num_international_student,
                str_detect(input$quant_cat_2,"Female") ~ num_female_residents,
                str_detect(input$quant_cat_2,"Male") ~ num_male_residents,
                str_detect(input$quant_cat_2,"PhD") ~ num_phd,
                str_detect(input$quant_cat_2,"Advanced") ~ num_advanced_degree,
                T ~ num_home_students),
            y_total = case_when(input$quant_cat_2 == "# of Residents" ~ as.double(num_residents),
                input$quant_cat_2 == "% of Home Students" ~ pct_home_students,
                input$quant_cat_2 == "# of Home Students" ~ as.double(num_home_students),
                input$quant_cat_2 == "% of International Students" ~ pct_international_student,
                input$quant_cat_2 == "# of International Students" ~ as.double(num_international_student),
                input$quant_cat_2 == "% of Female Residents" ~ pct_female_residents,
                input$quant_cat_2 == "# of Female Residents" ~ as.double(num_female_residents),
                input$quant_cat_2 == "% of Male Residents" ~ pct_male_residents,
                input$quant_cat_2 == "# of Male Residents" ~ as.double(num_male_residents),
                input$quant_cat_2 == "% w/ PhD" ~ pct_phd,
                input$quant_cat_2 == "# w/ PhD" ~ as.double(num_phd),
                input$quant_cat_2 == "% w/ Advanced Degree" ~ pct_advanced_degree,
                input$quant_cat_2 == "# w/ Advanced Degree" ~ as.double(num_advanced_degree)
            )) %>%
        distinct(primary_name,num_residents,cat_pct,cat_n,y_total) %>%
        #mutate(program_y_total = fct_reorder(program,desc(y_total))) %>%
        arrange(desc(y_total)) %>%
        select(primary_name,num_residents,cat_n,cat_pct) %>%
        datatable(options=list(pageLength=3,
                               searching=F,
                               lengthChange=F),
                  escape=F,
                  colnames = c(str_replace(input$data_type_2,"By ",""),"Total Residents","n","%"))
    
    })



    output$summaryProgramCharTable <- DT::renderDataTable({
        
      
        resident_data_w_geo_code %>% 
            distinct(program,num_residents,pct_w_missing_med_school_info,pct_w_med_school_info,
                     pct_home_students,num_home_students,
                     pct_female_residents,num_female_residents,
                     pct_male_residents,num_male_residents,
                     pct_international_student,num_international_student,
                     pct_phd,num_phd,
                     pct_advanced_degree,num_advanced_degree) %>%
            mutate(cat_pct = case_when(#input$quant_cat == "# of Residents" ~ NA,
                                       str_detect(input$quant_cat,"Home") ~ pct_home_students,
                                       str_detect(input$quant_cat,"International") ~ pct_international_student,
                                       str_detect(input$quant_cat,"Female") ~ pct_female_residents,
                                       str_detect(input$quant_cat,"Male") ~ pct_male_residents,
                                       str_detect(input$quant_cat,"PhD") ~ pct_phd,
                                       str_detect(input$quant_cat,"Advanced") ~ pct_advanced_degree,
                                       T ~ pct_home_students),
                   cat_n = case_when(#input$quant_cat == "# of Residents" ~ num_residents,
                                     str_detect(input$quant_cat,"Home") ~ num_home_students,
                                     str_detect(input$quant_cat,"International") ~ num_international_student,
                                     str_detect(input$quant_cat,"Female") ~ num_female_residents,
                                     str_detect(input$quant_cat,"Male") ~ num_male_residents,
                                     str_detect(input$quant_cat,"PhD") ~ num_phd,
                                     str_detect(input$quant_cat,"Advanced") ~ num_advanced_degree,
                                     T ~ num_home_students),
                   y_total = case_when(#input$quant_cat == "# of Residents" ~ as.double(num_residents),
                                       input$quant_cat == "% of Home Students" ~ pct_home_students,
                                       input$quant_cat == "# of Home Students" ~ as.double(num_home_students),
                                       input$quant_cat == "% of International Students" ~ pct_international_student,
                                       input$quant_cat == "# of International Students" ~ as.double(num_international_student),
                                       input$quant_cat == "% of Female Residents" ~ pct_female_residents,
                                       input$quant_cat == "# of Female Residents" ~ as.double(num_female_residents),
                                       input$quant_cat == "% of Male Residents" ~ pct_male_residents,
                                       input$quant_cat == "# of Male Residents" ~ as.double(num_male_residents),
                                       input$quant_cat == "% w/ PhD" ~ pct_phd,
                                       input$quant_cat == "# w/ PhD" ~ as.double(num_phd),
                                       input$quant_cat == "% w/ Advanced Degree" ~ pct_advanced_degree,
                                       input$quant_cat == "# w/ Advanced Degree" ~ as.double(num_advanced_degree)
                   )) %>%
            distinct(program,num_residents,cat_pct,cat_n,y_total) %>%
            #mutate(program_y_total = fct_reorder(program,desc(y_total))) %>%
            arrange(desc(y_total)) %>%
            select(program,num_residents,cat_n,cat_pct) %>%
            datatable(options=list(pageLength=3,
                                   searching=F,
                                   lengthChange=F),
                      colnames = c("Program","Total Residents","n","%"))
})
    
    output$trendLinePlot <- renderPlotly({
      
      
      test_df <- overtime_data %>%
        #filter(intern_year != 2020) %>%
        mutate(y_total = case_when(input$quant_cat_3 == "% of Home Students" ~ pct_home_student,
                                   input$quant_cat_3 == "% of International Students" ~ pct_international_student,
                                   input$quant_cat_3 == "% of Female Residents" ~ pct_female_residents,
                                   #input$quant_cat_3 == "% of Male Residents" ~ pct_male_residents,
                                   input$quant_cat_3 == "% w/ PhD" ~ pct_phd,
                                   input$quant_cat_3 == "% w/ Advanced Degree" ~ pct_advanced_degree)
        ) 

      
      plot_bar <- test_df %>%
        filter(intern_year!=2013) %>%
        ggplot(aes(x=intern_year,y=y_total,text=paste0("Year: ",intern_year,
                                                                "<br>",input$quant_cat_3,": ",y_total),
                   group=1))+
        geom_line()+
        geom_point()+
        scale_y_continuous(expand=expand_scale(mult=c(0,0.1)),
                           limits = c(0,max(test_df$y_total)),
                           breaks = scales::pretty_breaks(n=5)
                           )+
        scale_x_continuous(breaks=seq(2014,2020,1))+
        labs(y=input$quant_cat_3,
             x="Intern Class Year",
             title = paste0("Trend in ",input$quant_cat_3, " Over Time"))+
        #scale_x_discrete(dlabels = function(x) str_wrap(x, width = 30))+
        theme_bw()+
        theme() 
      
      ggplotly(plot_bar,
               tooltip="text") %>%
        config(displayModeBar = F) %>%
        layout(xaxis=list(fixedrange=TRUE),
               yaxis=list(fixedrange=TRUE),
               showlegend=F) 
    
    
})
    
    output$trendTable <- DT::renderDataTable({
      
      test_df <- overtime_data %>%
        filter(#intern_year != 2020,
               !is.na(intern_year)) %>%
        mutate(cat_pct = case_when(input$quant_cat_3 == "% of Home Students" ~ pct_home_student,
                                   input$quant_cat_3 == "% of International Students" ~ pct_international_student,
                                   input$quant_cat_3 == "% of Female Residents" ~ pct_female_residents,
                                   #input$quant_cat_3 == "% of Male Residents" ~ pct_male_residents,
                                   input$quant_cat_3 == "% w/ PhD" ~ pct_phd,
                                   input$quant_cat_3 == "% w/ Advanced Degree" ~ pct_advanced_degree),
               cat_n = round_half_up(cat_pct*total_n/100))  %>%
        arrange(desc(intern_year)) %>%
        select(intern_year,total_n,cat_n,cat_pct) %>%
        datatable(options=list(pageLength=3,
                               searching=F,
                               lengthChange=F),
                  rownames=FALSE,
                  colnames = c("Year","Total Residents","n","%"))

    })
    
    output$maptitle_panel = renderText({
      case_when(input$data_type == "By Program" ~ paste0("Medical Schools of ", input$individual_selected,"'s Residents"),
                input$data_type == "By Medical School" ~ paste0("Programs of ",input$individual_selected,"'s Graduates"))
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
