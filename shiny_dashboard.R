library(tidyverse)
players <- read.csv("Players_2019.csv")
salaries <- read.csv("WNBA player salary 20192.csv")
all_data <- players %>% inner_join(salaries, by = "Player") %>% 
    mutate(Std_Salary = (Salary - mean(Salary, na.rm = TRUE))/sd(Salary, na.rm = TRUE))
unique_teams <- unique(all_data$Tm)
unique_stats <- names(all_data)

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(shiny)

ui <- dashboardPage(
    dashboardHeader(title = "WNBA Player Value"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Player Information", tabName = "player-info", icon = icon("dashboard")),
            menuItem("Salaries", tabName = "Salaries", icon = icon("dollar-sign")),
            menuItem("Statistic Relationship", tabName = "stat-corr", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "player-info",
                    fluidRow(
                        box(
                            selectInput('team2', 'Choose a team', unique_teams, "ATL", selectize = TRUE),
                            selectizeInput('unique_stats', 'Stat Options',
                                           unique_stats, selected = "Player", multiple = TRUE),
                            actionButton('update2', 'Update')
                        ),
                        box(
                            downloadButton("downloadData", "Download")
                            ),
                        
                        tableOutput("selected"),
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "Salaries",
                    fluidRow(
                        box(
                            checkboxGroupInput(inputId = "team",
                                               label = "Choose a team/teams",
                                               unique_teams,
                                               selected = "ATL", inline = TRUE),
                            actionButton('update1', 'Update')
                        ),
                            plotOutput('words')
                    )
            ),
            
            tabItem(tabName = "stat-corr",
                    fluidRow(
                        box(
                            selectInput('stat', 'Choose a stat', unique_stats, "G", selectize = TRUE),
                            actionButton('update3', 'Update')
                        ),
                        plotOutput('lmplot')
                    )
            )
            
        )
    )
)

server <- function(input, output, session){
    rplot_words <- eventReactive(input$update1, {
        plot1 <- plot1 <- all_data %>%
            filter(Tm %in% input$team) %>%
            ggplot(aes(FG., Std_Salary)) +
            geom_point(fill="#003058", aes(color = Tm)) +
            ggtitle("Salary scale") +
            xlab("Field Goal Percentage") +
            ylab("Salary") +
            theme_classic() +
            theme(axis.text.y = element_text(size = 16),
                  axis.title = element_text(size = 16))
        plot1 + scale_color_brewer("Team", palette = "Set3")
    })
    rplot_stats <- eventReactive(input$update3, {
        plot2 <- all_data %>%
            ggplot(aes_string(x = input$stat, y = 'WS')) + 
            geom_point() + 
            geom_smooth(method = "lm", se = FALSE) + 
            xlab(paste(input$stat)) +
            ylab("WS") + 
            theme_classic()
        plot2
    })
    rplot_selected <- eventReactive(input$update2, {
        displayTable <- all_data %>% filter(Tm == input$team2) %>%
            select(input$unique_stats)
    })
    output$selected <- renderTable({
        rplot_selected()})
    
    output$lmplot <- renderPlot({rplot_stats()})
    output$words <- renderPlot({rplot_words()})
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", input$team4, ".csv", sep="")
        },
        content = function(file) {
            write.csv({rplot_selected()}, file)
        }
    )

}

shinyApp(ui = ui, server = server)
