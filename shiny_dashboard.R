library(tidyverse)
library(plotly)
per_36 <- read.csv("Data/shiny_data_per_36.csv")[,-1] %>% filter(Tm != "TOT")
per_minute <- read.csv("Data/shiny_data_per_game.csv")[,-1] %>% filter(Tm != "TOT")
teams <- read.csv("Data/team_abbreviations.csv")
per_36 <- per_36 %>% inner_join(teams, by = c('Tm' = "bref"))
per_minute <- per_minute %>% inner_join(teams, by = c('Tm' = "bref"))
unique_teams <- c(unique(per_minute$Team_Name), "League")
unique_stats_36 <- names(per_36)
unique_stats_game <- names(per_minute)
numeric_stats_36 <- per_36 %>% select_if(is.numeric) %>% names()
numeric_stats_game <- per_minute %>% select_if(is.numeric) %>% names()
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(shiny)
library(RColorBrewer)
library(DT)
library(shinyWidgets)

ui <- dashboardPage(
    dashboardHeader(title = "WNBA Player Value"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Player Information", tabName = "player-info", icon = icon("dashboard")),
            menuItem("Salaries", tabName = "Salaries", icon = icon("dollar-sign")),
            menuItem("Statistic Relationships", tabName = "stat-corr", icon = icon("th")),
            menuItem("Distribution", tabName = "stat-dist", icon = icon("chart-bar")),
            menuItem("Data Glossary", tabName = "glossary", icon = icon("book"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "player-info",
                    fluidRow(
                        box(width = 6,
                            column(width = 6,
                                   selectizeInput('team2', 'Choose a team', unique_teams, "League", multiple = TRUE),
                                   selectizeInput("stat_type", "Type of Statistics", c("Per 36 minutes", "Per game")),
                                   conditionalPanel(
                                       condition = "input.stat_type == 'Per 36 minutes'",
                                       selectizeInput('all_stats_minute', 'Stat Options',
                                                      unique_stats_36, selected = "Player", multiple = TRUE)
                                   ),
                                   conditionalPanel(
                                       condition = "input.stat_type == 'Per game'",
                                       selectizeInput('all_stats_game', 'Stat Options',
                                                      unique_stats_game, selected = "Player", multiple = TRUE)
                                   ),
                                   actionButton('update2', 'Update'),
                                   downloadButton("downloadData", "Download")
                            )
                        )
                    ),
                    fluidRow(
                        box(width = 12,
                            DT::dataTableOutput("selected"),
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "Salaries",
                    fluidRow(
                        box(
                            selectizeInput('team', 'Choose a team', unique_teams, "League", multiple = TRUE),
                            actionButton('update1', 'Update')
                        ),
                    ),
                    fluidRow(
                        box(width = 10,
                            plotlyOutput('words')
                        )
                    )
            ),
            
            tabItem(tabName = "stat-corr",
                    fluidRow(
                        box(
                            selectizeInput("stat_type1", "Type of Statistics", c("Per 36 minutes", "Per game")),
                            conditionalPanel(
                                condition = "input.stat_type1 == 'Per 36 minutes'",
                                selectizeInput('stat_36', 'Stat Options',
                                               numeric_stats_36, selected = "WS")
                            ),
                            conditionalPanel(
                                condition = "input.stat_type1 == 'Per game'",
                                selectizeInput('stat_game', 'Stat Options',
                                               numeric_stats_game, selected = "WS")
                            ),
                            actionButton('update3', 'Update'),
                        ),
                        infoBoxOutput("box1"),
                        infoBoxOutput("box2")
                    ),
                    fluidRow(
                        box(width = 9,
                            plotlyOutput('lmplot')
                        )    
                    )
            ),
            tabItem(tabName = "stat-dist",
                    fluidRow(
                        box(
                            selectizeInput("stat_type2", "Type of Statistics", c("Per 36 minutes", "Per game")),
                            conditionalPanel(
                                condition = "input.stat_type2 == 'Per 36 minutes'",
                                selectizeInput('stat2_36', 'Stat Options',
                                               numeric_stats_36, selected = "RAPM")
                            ),
                            conditionalPanel(
                                condition = "input.stat_type2 == 'Per game'",
                                selectizeInput('stat2_game', 'Stat Options',
                                               numeric_stats_game, selected = "RAPM")
                            ),
                            sliderInput("nbins",
                                        "Number of bins:",
                                        min = 5,
                                        max = 30,
                                        value = 20), 
                            actionButton('update4', 'Update'),
                        ),
                        infoBoxOutput("box3"),
                        infoBoxOutput("box4"),
                        infoBoxOutput("box5")
                    ),
                    fluidRow(
                        box(width = 9,
                            plotOutput('stat_dist')
                        )    
                    )
            ),
            tabItem(tabName = "glossary",
                    fluidRow(
                        column(1),
                            box(
                                title = p(icon('book'), 'Data Glossary'),
                                width = 10,
                                collapsible = TRUE,
                                p(strong('Player: '), "First and last name of WNBA player"),
                                p(strong('Season: '), "WNBA season"),
                                p(strong('Age: '), "Player age at the beginning of a certain season"),
                                p(strong("Tm: "), "Team abbreviation"),
                                p(strong("WS: "), "Win shares from Basketball Reference"),
                                p(strong("G: "), "Total games played"),
                                p(strong("GS: "), "Total games started"),
                                p(strong("MP: "), "Total minutes played"),
                                p(strong("FG: "), "Total field goals for the season"),
                                p(strong("FGA: "), "Total field goal attempts for the season"),
                                p(strong("X2P: "), "Total made 2 pointers for the season"),
                                p(strong("X2PA: "), "Total 2 point attempts for the season"),
                                p(strong("X3: "), "Total made 3 pointers for the season"),
                                p(strong("X3PA: "), "Total 3 point attempts for the season"),
                                p(strong("FT: "), "Total free throws made for the season"),
                                p(strong("FTA: "), "Total free throw attempts for the season"),
                                p(strong("DRB: "), "Total defensive rebounds for the season"),
                                p(strong("ORB: "), "Total offensive rebounds for the season"),
                                p(strong("TRB: "), "Total rebounds for the season"),
                                p(strong("AST: "), "Total assists for the season"),
                                p(strong("STL: "), "Total steals for the season"), 
                                p(strong("BLK: "), "Total blocks for the season"),
                                p(strong("TOV: "), "Total turnovers for the season"),
                                p(strong("PF: "), "Total personal fouls for the season"),
                                p(strong("PTS: "), "Total points for the season"),
                                p(strong("FG.: "), "Field goal percentage for the season"),
                                p(strong("FT.: "), "Free throw percentage for the season"),
                                p(strong("X2P.: "), "2 point percentage for the season"),
                                p(strong("X3P.: "), "3 point percentage for the season"),
                                p(strong("eFG.: "), "Effective field goal percentage for the season"),
                                p(strong("APM: "), "Adjusted Plus Minus"),
                                p(strong("RAPM: "), "Regularized Adjusted Plus Minus"), 
                                p(strong("Salary: "), "Season salary for player"), 
                                p(strong("ESPN: "), "ESPN team abbreviation"),
                                p(strong("Team_Name: "), "Team name"),
                                h3(strong("Per 36 minute stats")),
                                p(strong("FGPM: "), "Field goals made per 36 minutes"),
                                p(strong("FGAPM: "), "Field goals attempted per 36 minutes"),
                                p(strong("X2PPM: "), "2 pointers made per 36 minutes"),
                                p(strong("X2PAPM: "), "2 pointers attempted per 36 minutes"),
                                p(strong("X3PPM: "), "3 pointers made per 36 minutes"),
                                p(strong("X3PAPM: "), "3 pointers attempted per 36 minutes"),
                                p(strong("FTPM: "), "Free throws made per 36 minutes"),
                                p(strong("FTAPM: "), "Free throw attempts per 36 minutes"),
                                p(strong("ORBPM: "), "Offensive rebounds per 36 minutes"),
                                p(strong("DRBPM: "), "Defensive rebounds per 36 minutes"),
                                p(strong("ASPM: "), "Assists per 36 minutes"),
                                p(strong("SPM: "), "Steals per 36 minutes"), 
                                p(strong("BPM: "), "Blocks per 36 minutes"),
                                p(strong("TPM: "), "Turnovers per 36 minutes"),
                                p(strong("PFPM: "), "Personal fouls per 36 minutes"),
                                p(strong("PPM: "), "Points per 36 minutes"),
                                h3(strong("Per game stats")),
                                p(strong("FGPG: "), "Field goals made per game"),
                                p(strong("FGAPG: "), "Field goals attempted per game"),
                                p(strong("X2PPG: "), "2 pointers made per game"),
                                p(strong("X2PAPG: "), "2 pointers attempted per game"),
                                p(strong("X3PPG: "), "3 pointers made per game"), 
                                p(strong("X3PAPG: "), "3 pointers attempted per game"),
                                p(strong("FTPG: "), "Free throws made per game"),
                                p(strong("FTAPG: "), "Free throws attempted per game"),
                                p(strong("DRBPG: "), "Defensive rebounds per game"),
                                p(strong("ORBPG: "), "Offensvie rebounds per game"),
                                p(strong("TRBPG: "), "Total rebounds per game"), 
                                p(strong("APG: "), "Assists per game"),
                                p(strong("SPG: "), "Steals per game"),
                                p(strong("BPG: "), "Blocks per game"),
                                p(strong("TPG: "), "Turnovers per game"),
                                p(strong("PFPG: "), "Personal fouls per game"),
                                p(strong("PPG: "), "Points per game")
                            )
                        )
                    )
        )
    )
)

server <- function(input, output, session){
    rplot_words <- eventReactive(input$update1, {
        if(!("League" %in% input$team)){
            all_data <- per_36 %>%
                filter(Team_Name %in% input$team)
        } else {
            all_data <- per_36
        }
        if(length(input$team) <= 8 & !("League" %in% input$team)){
            col_pal <- "Dark2"
        } else {
            col_pal <- "Set3"
        }
        plot1 <-  all_data %>%
            ggplot(aes(Salary, RAPM, text = paste0("Player: ", Player, "<br>", 
                                                   "Salary: ", scales::dollar(Salary), "<br>",
                                                   "RAPM: ", round(RAPM, digits = 3)))) +
            geom_point(aes(color = Team_Name)) +
            xlab("Salary") +
            ylab("RAPM") +
            ggtitle(paste0("RAPM against Player Salary")) + 
            scale_x_continuous(labels = scales::dollar_format()) + 
            theme_minimal()
        plot1 <- plot1 + scale_color_brewer("Team", palette = col_pal)
        plot1
    })
    rplot_stats <- eventReactive(input$update3, {
        if(input$stat_type1 == "Per 36 minutes"){
            all_data <- per_36
            plot2 <- all_data %>%
                ggplot(aes_string(x = input$stat_36, y = 'RAPM')) + 
                geom_point() + 
                geom_smooth(method = "lm", se = FALSE, color = "blue3") + 
                xlab(paste(input$stat_36)) +
                ylab("RAPM") + 
                labs(title = paste0("Linear Regression of RAPM vs ", input$stat_36)) + 
                scale_color_hue(l = 45) +
                theme_minimal()
            plot2
        }else {
            all_data <- per_minute
            plot2 <- all_data %>%
                ggplot(aes_string(x = input$stat_game, y = 'RAPM')) + 
                geom_point() + 
                geom_smooth(method = "lm", se = FALSE, color = "blue3") + 
                xlab(paste(input$stat_game)) +
                ylab("RAPM") + 
                labs(title = paste0("Linear Regression of RAPM vs ", input$stat_game)) + 
                scale_color_hue(l = 45) +
                theme_minimal()
            plot2
        }
    })
    rplot_selected <- eventReactive(input$update2, {
        if(input$stat_type == "Per 36 minutes"){
            all_data <- per_36
            variables <- input$all_stats_minute
        }
        if(input$stat_type == "Per game"){
            all_data <- per_minute
            variables <- input$all_stats_game
        }
        if(!("League" %in% input$team2)){
            displayTable <- all_data %>% filter(Team_Name %in% input$team2) %>%
                dplyr::select(all_of(variables))
        } else {
            displayTable <- all_data  %>%
                dplyr::select(all_of(variables))
        }
    })
    r2 <- eventReactive(input$update3, {
        if(input$stat_type1 == "Per 36 minutes"){
            all_data <- per_36
            model_formula <- as.formula(paste0("RAPM ~ ", input$stat_36))
            model <- lm(model_formula, data = all_data)
            r2 <- summary(model)$r.squared
            r2
        } else{
            all_data <- per_minute
            model_formula <- as.formula(paste0("RAPM ~ ", input$stat_game))
            model <- lm(model_formula, data = all_data)
            r2 <- summary(model)$r.squared
            r2
        }
        r2
    })
    cor_coef <- eventReactive(input$update3, {
        if(input$stat_type1 == "Per 36 minutes"){
            all_data <- per_36
            corr_var <- cor(all_data[,"RAPM"], all_data[,paste(input$stat_36)])
            corr_var
        } else{
            all_data <- per_minute
            corr_var <- cor(all_data[,"RAPM"], all_data[,paste(input$stat_game)])
            corr_var
        }
        corr_var
    })
    rplot_dist <- eventReactive(input$update4, {
        if(input$stat_type2 == "Per 36 minutes"){
            all_data <- per_36 %>% select_if(is.numeric)
            plot <- ggplot(all_data, aes_string(x= input$stat2_36)) + 
                geom_histogram(aes(y = ..density..), 
                               color = 'black',
                               bins = input$nbins,
                               fill = '#a9daff') +
                stat_function(fun = stats::dnorm,
                              args = list(
                                  mean = mean(all_data %>%  dplyr::pull(input$stat2_36), na.rm = TRUE),
                                  sd = stats::sd(all_data %>%  dplyr::pull(input$stat2_36), na.rm = TRUE)
                              ),
                              col = '#317196',
                              size = 2) + 
                xlab(input$stat2_36) +
                ylab('Density') +
                ggtitle(paste0("Histogram and Density Plot of ", input$stat2_36)) + 
                theme_minimal()
            plot
        }else {
            all_data <- per_minute %>% select_if(is.numeric)
            plot <- ggplot(all_data, aes_string(x= input$stat2_game)) + 
                geom_histogram(aes(y = ..density..), 
                               color = 'black',
                               bins = input$nbins,
                               fill = '#a9daff') +
                stat_function(fun = stats::dnorm,
                              args = list(
                                  mean = mean(all_data %>%  dplyr::pull(input$stat2_game), na.rm = TRUE),
                                  sd = stats::sd(all_data %>%  dplyr::pull(input$stat2_game), na.rm = TRUE)
                              ),
                              col = '#317196',
                              size = 2) + 
                xlab(input$stat2_game) +
                ylab('Density') +
                ggtitle(paste0("Histogram and Density Plot of ", input$stat2_game)) + 
                theme_minimal()
            plot
        }
    })
    dist_mean <- eventReactive(input$update4, {
        if(input$stat_type2 == "Per 36 minutes"){
            all_data <- per_36 %>% select_if(is.numeric)
            mu <- mean(all_data[,paste(input$stat2_36)], na.rm = TRUE)
            mu
        }else{
            all_data <- per_minute %>% select_if(is.numeric)
            mu <- mean(all_data[,paste(input$stat2_game)], na.rm = TRUE)
            mu
        }
    })
    dist_sd <- eventReactive(input$update4, {
        if(input$stat_type2 == "Per 36 minutes"){
            all_data <- per_36 %>% select_if(is.numeric)
            std <- sd(all_data[,paste(input$stat2_36)], na.rm = TRUE)
            std
        } else{
            all_data <- per_minute %>% select_if(is.numeric)
            std <- sd(all_data[,paste(input$stat2_game)], na.rm = TRUE)
            std
        }
    })
    dist_median <- eventReactive(input$update4, {
        if(input$stat_type2 == "Per 36 minutes"){
            all_data <- per_36 %>% select_if(is.numeric)
            med <- median(all_data[,paste(input$stat2_36)], na.rm = TRUE)
            med
        }else{
            all_data <- per_minute %>% select_if(is.numeric)
            med <- median(all_data[,paste(input$stat2_game)], na.rm = TRUE)
            med
        }
    })
    output$selected <- renderDataTable({
        datatable(rplot_selected(), rownames = FALSE, options = list(scrollX='400px'))
    })
    
    output$lmplot <- renderPlotly({ggplotly(rplot_stats())})
    output$words <- renderPlotly({ggplotly(rplot_words(), tooltip = c("text"))})
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("player_data.csv", sep="")
        },
        content = function(file) {
            write.csv({rplot_selected()}, file, row.names = FALSE)
        }
    )
    output$box1 <- renderInfoBox({
        
        req(input$stat_36)
        req(input$stat_game)
        
        infoBox(
            "R-squared", 
            round(r2(), digits = 3),
            icon = icon('basketball-ball'),
            color = 'light-blue',
            fill = TRUE
        )
    })
    output$box2 <- renderInfoBox({
        
        req(input$stat_36)
        req(input$stat_game)
        
        infoBox(
            "Correlation Coefficient", 
            round(cor_coef(), digits = 3),
            icon = icon('basketball-ball'),
            color = 'light-blue',
            fill = TRUE
        )
    })
    output$box3 <- renderInfoBox({

        req(input$stat2_36)
        req(input$stat2_game)

        infoBox(
            "Mean",
            round(dist_mean(), digits = 3),
            icon = icon('basketball-ball'),
            color = 'light-blue',
            fill = TRUE
        )
    })
    output$box4 <- renderInfoBox({

        req(input$stat2_36)
        req(input$stat2_game)

        infoBox(
            "Median",
            round(dist_median(), digits = 3),
            icon = icon('basketball-ball'),
            color = 'light-blue',
            fill = TRUE
        )
    })
    output$box5 <- renderInfoBox({

        req(input$stat2_36)
        req(input$stat2_game)

        infoBox(
            "Standard Deviation",
            round(dist_sd(), digits = 3),
            icon = icon('basketball-ball'),
            color = 'light-blue',
            fill = TRUE
        )
    })
    output$stat_dist <- renderPlot(rplot_dist())
}


shinyApp(ui = ui, server = server)
