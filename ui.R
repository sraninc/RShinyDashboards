
#################################################################
# Data Trade Gateway Project
# Social Media Analytics Dashboard
# Authors: Sneha Rani
# Date: 07/25/2024
#################################################################

library(shiny)
library(plotly)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .center-chart {
        display: flex;
        justify-content: center;
      }
    "))
  ),
  titlePanel("Social Media Insights Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("fb_page_id", "Facebook Page ID", value = ""),
      passwordInput("access_token", "Page Access Token", value = ""),
      textInput("ig_user_id", "Instagram User ID", value = ""),
      passwordInput("ig_access_token", "Instagram Access Token", value = ""),
      actionButton("load_data", "Load Data"),
      width = 2 
    ),
    mainPanel(
      width = 10,
      tabsetPanel(
        tabPanel("Facebook",
                 tabsetPanel(
                   tabPanel("Track Social Media Performance",
                            HTML("<br><br>"),
                            fluidRow(
                              column(6, plotlyOutput("fb_post_reach")),
                              column(6, plotlyOutput("fb_post_engagement"))
                            ),
                            HTML("<br><br>"),
                            fluidRow(
                              column(6, plotlyOutput("fb_engagement_rate")),
                              column(5, plotlyOutput("fb_overall_engagement_rate"))
                            ),
                            HTML("<br><br>"),
                            fluidRow(
                              column(6, plotlyOutput("fb_follower_growth")),
                              column(6, plotlyOutput("fb_impressions"))
                            )
                   ),
                   tabPanel("Content Effectiveness",
                            HTML("<br><br>"),
                            fluidRow(
                              column(6, plotlyOutput("fb_top_performing_content")),
                              column(6, plotlyOutput("fb_content_frequency"))
                            ),
                            HTML("<br><br>"),
                            fluidRow(
                              column(12, div(class = "center-chart", plotlyOutput("fb_content_reach_by_type")))
                            )
                   ),
                   tabPanel("Sentiment Analysis",
                            HTML("<br><br>"),
                            fluidRow(
                              column(10, plotlyOutput("sentiment_analysis_fb"))
                            )
                   ),
                   tabPanel("Predictive Analytics",
                            HTML("<br><br>"),
                            fluidRow(
                              column(6, plotlyOutput("fb_predicted_engagement")),
                              column(6, plotlyOutput("fb_predicted_reach"))
                            )
                   )
                 )
        ),
        tabPanel("Instagram",
                 tabsetPanel(
                   tabPanel("Track Social Media Performance",
                            HTML("<br><br>"),
                            fluidRow(
                              column(6, plotlyOutput("ig_post_reach")),
                              column(6, plotlyOutput("ig_post_engagement"))
                            ),
                            HTML("<br><br>"),
                            fluidRow(
                              column(6, plotlyOutput("ig_engagement_rate")),
                              column(5, plotlyOutput("ig_overall_engagement_rate"))
                            ),
                            HTML("<br><br>"),
                            fluidRow(
                              column(6, plotlyOutput("ig_follower_growth")),
                              column(6, plotlyOutput("ig_impressions"))
                            )
                   ),
                   tabPanel("Content Effectiveness",
                            HTML("<br><br>"),
                            fluidRow(
                              column(6, plotlyOutput("ig_top_performing_content")),
                              column(6, plotlyOutput("ig_content_frequency"))
                            ),
                            HTML("<br><br>"),
                            fluidRow(
                              column(12, div(class = "center-chart", plotlyOutput("ig_content_reach_by_type")))
                            )
                   ),
                   tabPanel("Sentiment Analysis",
                            HTML("<br><br>"),
                            fluidRow(
                              column(10, plotlyOutput("sentiment_analysis_ig"))
                            )
                   ),
                   tabPanel("Predictive Analytics",
                            HTML("<br><br>"),
                            fluidRow(
                              column(6, plotlyOutput("ig_predicted_engagement")),
                              column(6, plotlyOutput("ig_predicted_reach"))
                            )
                   )
                 )
        ),
        tabPanel("Debug", verbatimTextOutput("debug_output"))
      ),
      style = "margin-top: 20px;" 
    )
  )
)

