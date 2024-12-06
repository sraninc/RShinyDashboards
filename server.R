
#################################################################
# Data Trade Gateway Project
# Social Media Analytics Dashboard
# Authors: Sneha Rani
# Date: 07/25/2024
#################################################################

library(shiny)
library(httr)
library(jsonlite)
library(lubridate)
library(plotly)
library(syuzhet)
library(forecast)

# Shiny server function
server <- function(input, output, session) {
  
  # Function to fetch data from the Facebook API
  fetch_facebook_posts <- function(page_id, access_token) {
    fb_posts_url <- paste0(
      "https://graph.facebook.com/v20.0/", page_id, 
      "/posts?fields=id,message,created_time,story,status_type,shares,comments,fan_count,followers_count.summary(true),likes.summary(true),insights.metric(post_impressions_unique,post_impressions)&access_token=", 
      access_token
    )
    fb_response <- GET(fb_posts_url)
    fb_data <- fromJSON(content(fb_response, "text"), flatten = TRUE)
    # print(fb_data)
    return(fb_data$data)
  }
  
  # Function to clean and structure the Facebook data
  clean_facebook_data <- function(fb_data) {
    fb_data$PostID <- sapply(fb_data$id, function(x) sub(".*_", "", x))
    fb_data$PostMessage <- fb_data$message
    fb_data$Date <- substr(fb_data$created_time, 1, 10)
    fb_data$ContentType <- gsub("added_|shared_", "", fb_data$status_type)
    fb_data$ContentType[fb_data$ContentType == "mobile_status_update"] <- "miscellaneous"
    fb_data$Likes <- sapply(fb_data$likes.summary.total_count, function(count) ifelse(is.null(count), 0, count))
    fb_data$Shares <- sapply(fb_data$shares.count, function(count) ifelse(is.na(count), 0, count))
    fb_data$Comments <- sapply(fb_data$comments.summary.total_count, function(count) ifelse(is.na(count), 0, count))
    
    # Extracting comments and assigning them to Reviews_FB
    fb_data$Reviews_FB <- sapply(fb_data$comments.data, function(comments) {
      if (!is.null(comments) && length(comments) > 0) {
        comment_texts <- sapply(comments, function(comment) {
          paste(comment, collapse = ", ")
        })
        return(paste(comment_texts, collapse = "; "))
      } else {
        return(NA)
      }
    })
    
    # Removing data before the first ";" and after the last ";"
    fb_data$Reviews_FB <- sapply(fb_data$Reviews_FB, function(review) {
      if (!is.na(review) && grepl(";", review)) {
        review <- sub("^[^;]*;", "", review)  # Remove data before the first ";"
        review <- sub(";[^;]*$", "", review)  # Remove data after the last ";"
      }
      return(review)
    })
    
    # Extracting Reach and Impressions
    fb_data$Reach <- sapply(fb_data$insights.data, function(insights) {
      if (!is.null(insights) && length(insights) > 0) {
        reach_texts <- sapply(insights, function(insight1) {
          paste(insight1, collapse = ", ")
        })
        combined_insights <- paste(reach_texts, collapse = "; ")
        
        # Extract the first number before closing parenthesis ")"
        reach_matches <- regmatches(combined_insights, gregexpr("\\d+(?=\\))", combined_insights, perl = TRUE))
        if (length(reach_matches[[1]]) >= 1) {
          return(as.numeric(reach_matches[[1]][1]))
        } else {
          return(NA)
        }
      } else {
        return(NA)
      }
    })
    
    fb_data$Impressions <- sapply(fb_data$insights.data, function(insights) {
      if (!is.null(insights) && length(insights) > 0) {
        impressions_texts <- sapply(insights, function(insight1) {
          paste(insight1, collapse = ", ")
        })
        combined_insights <- paste(impressions_texts, collapse = "; ")
        
        # Extract the second number before closing parenthesis ")"
        impressions_matches <- regmatches(combined_insights, gregexpr("\\d+(?=\\))", combined_insights, perl = TRUE))
        if (length(impressions_matches[[1]]) >= 2) {
          return(as.numeric(impressions_matches[[1]][2]))
        } else {
          return(NA)
        }
      } else {
        return(NA)
      }
    })
    
    # Create a new data frame with only the needed columns
    clean_data <- data.frame(
      PostID = fb_data$PostID,
      PostMessage = fb_data$PostMessage,
      Date = fb_data$Date,
      ContentType = fb_data$ContentType,
      Likes = fb_data$Likes,
      Shares = fb_data$Shares,
      Comments = fb_data$Comments,
      Reviews_FB = fb_data$Reviews_FB,
      Reach = as.numeric(fb_data$Reach),
      Impressions = as.numeric(fb_data$Impressions),
      stringsAsFactors = FALSE
    )
    
    # Replace NA with 0 in Reach and Impressions
    clean_data$Reach[is.na(clean_data$Reach)] <- 0
    clean_data$Impressions[is.na(clean_data$Impressions)] <- 0
    
    return(clean_data)
  }
  
  observeEvent(input$load_data, {
    fb_data <- fetch_facebook_posts(input$fb_page_id, input$access_token)
    cleaned_fb_data <- clean_facebook_data(fb_data)
    
    fb_data <- cleaned_fb_data
    
    # Print the cleaned fb_data to see the structure and values
    print(fb_data)
    
    if (all(c("Likes", "Shares", "Comments", "Reach", "Impressions") %in% colnames(fb_data))) {
      fb_data$Engagement <- fb_data$Likes + fb_data$Shares + fb_data$Comments
      fb_data$EngagementRate <- with(fb_data, ifelse(Reach > 0, (Likes + Shares + Comments) / Reach * 100, 0))
      fb_data$EngagementRate[is.na(fb_data$EngagementRate)] <- 0 
      fb_data$FollowerGrowth <- rev(cumsum(rev(fb_data$Reach))) 
    } else {
      fb_data$EngagementRate <- fb_data$FollowerGrowth <- NA
    }
    
    comments_text_fb <- fb_data$Reviews_FB
    
    sentiments_fb <- get_nrc_sentiment(comments_text_fb)
    sentiment_summary_fb <- colSums(sentiments_fb[,1:8])
    
    print(sentiment_summary_fb)
    
    output$debug_output <- renderPrint({
      list(
        fb_data = head(fb_data),
        comments_text_fb = head(comments_text_fb)
      )
    })
    
    # Tab 1 - Facebook - Track Social Media Performance
    output$fb_post_reach <- renderPlotly({
      plot_ly(fb_data, x = ~Date, y = ~Reach, type = 'scatter', mode = 'lines', name = 'Post Reach', text = ~paste("Post ID:", PostID)) %>%
        add_lines(x = ~Date, y = ~predict(loess(Reach ~ as.numeric(Date), data = fb_data)), name = 'Trend Line') %>%
        layout(
          title = 'Post Reach Over Time',
          xaxis = list(title = 'Date', tickformat = "%b %Y"),
          yaxis = list(title = 'Reach'),
          legend = list(orientation = 'h', y = -0.2),
          updatemenus = list(
            list(
              type = "buttons",
              active = -1,
              buttons = list(
                list(label = "Post Reach", method = "update", args = list(list(visible = c(TRUE, FALSE)))),
                list(label = "Trend Line", method = "update", args = list(list(visible = c(FALSE, TRUE)))),
                list(label = "All", method = "update", args = list(list(visible = c(TRUE, TRUE))))
              ),
              direction = "down",
              showactive = TRUE
            )
          )
        )
    })
    
    output$fb_post_engagement <- renderPlotly({
      plot_ly(fb_data, x = ~Date, y = ~Likes, name = 'Likes', type = 'scatter', mode = 'lines', text = ~paste("Post ID:", PostID)) %>%
        add_trace(y = ~Shares, name = 'Shares', type = 'scatter', mode = 'lines', line = list(dash = 'dot')) %>%
        add_trace(y = ~Comments, name = 'Comments', type = 'scatter', mode = 'lines', line = list(dash = 'dash')) %>%
        add_lines(x = ~Date, y = ~predict(loess(Likes ~ as.numeric(Date), data = fb_data)), name = 'Likes Trend', line = list(color = 'blue', dash = 'solid')) %>%
        add_lines(x = ~Date, y = ~predict(loess(Shares ~ as.numeric(Date), data = fb_data)), name = 'Shares Trend', line = list(color = 'red', dash = 'dot')) %>%
        add_lines(x = ~Date, y = ~predict(loess(Comments ~ as.numeric(Date), data = fb_data)), name = 'Comments Trend', line = list(color = 'green', dash = 'dash')) %>%
        layout(
          title = '          Post Engagement Over Time',
          xaxis = list(title = 'Date', tickformat = "%b %Y"),
          yaxis = list(title = 'Engagement'),
          legend = list(orientation = 'h', y = -0.2),
          updatemenus = list(
            list(
              type = "buttons",
              active = -1,
              buttons = list(
                list(label = "Likes", method = "update", args = list(list(visible = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)))),
                list(label = "Shares", method = "update", args = list(list(visible = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)))),
                list(label = "Comments", method = "update", args = list(list(visible = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)))),
                list(label = "All", method = "update", args = list(list(visible = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))))
              ),
              direction = "down",
              showactive = TRUE
            )
          )
        )
    })
    
    output$fb_engagement_rate <- renderPlotly({
      plot_ly(fb_data, x = ~Date, y = ~EngagementRate, type = 'scatter', mode = 'lines') %>%
        layout(title = 'Engagement Rate by Reach Over Time', xaxis = list(title = 'Date'), yaxis = list(title = 'Engagement Rate (%)'))
    })
    
    output$fb_overall_engagement_rate <- renderPlotly({
      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = mean(fb_data$EngagementRate, na.rm = TRUE),
        title = list(text = "Overall Engagement Rate by Reach"),
        gauge = list(
          axis = list(range = list(NULL, 100)),
          steps = list(
            list(range = c(0, 50), color = "lightgray"),
            list(range = c(50, 100), color = "gray")
          ),
          threshold = list(
            line = list(color = "red", width = 4),
            thickness = 0.75,
            value = mean(fb_data$EngagementRate, na.rm = TRUE)
          )
        )
      ) %>%
        layout(title = 'Overall Engagement Rate')
    })
    
    output$fb_follower_growth <- renderPlotly({
      plot_ly(fb_data, x = ~Date, y = ~FollowerGrowth, type = 'scatter', mode = 'lines') %>%
        layout(title = 'Follower Trend Over Time', xaxis = list(title = 'Date'), yaxis = list(title = 'Follower Growth'))
    })
    
    output$fb_impressions <- renderPlotly({
      plot_ly(fb_data, x = ~Date, y = ~Impressions, type = 'scatter', mode = 'lines', name = 'Impressions', text = ~paste("Post ID:", PostID)) %>%
        add_lines(x = ~Date, y = ~predict(loess(Impressions ~ as.numeric(Date), data = fb_data)), name = 'Trend Line') %>%
        layout(
          title = 'Impressions Over Time',
          xaxis = list(title = 'Date', tickformat = "%b %Y"),
          yaxis = list(title = 'Impressions'),
          legend = list(orientation = 'h', y = -0.2),
          updatemenus = list(
            list(
              type = "buttons",
              active = -1,
              buttons = list(
                list(label = "Impressions", method = "update", args = list(list(visible = c(TRUE, FALSE)))),
                list(label = "Trend Line", method = "update", args = list(list(visible = c(FALSE, TRUE)))),
                list(label = "All", method = "update", args = list(list(visible = c(TRUE, TRUE))))
              ),
              direction = "down",
              showactive = TRUE
            )
          )
        )
    })
    
    # Tab 2 - Facebook - Content effectiveness
    output$fb_top_performing_content <- renderPlotly({
      plot_ly(fb_data, x = ~PostID, y = ~Engagement, type = 'bar', name = 'Engagement', text = ~paste("Post ID:", PostID)) %>%
        layout(title = 'Top Performing Content', xaxis = list(title = 'Post ID'), yaxis = list(title = 'Engagement'))
    })
    
    output$fb_content_frequency <- renderPlotly({
      plot_ly(fb_data, x = ~Date, type = 'histogram', name = 'Posts Frequency', marker = list(line = list(color = 'black', width = 1))) %>%
        layout(title = 'Content Frequency', xaxis = list(title = 'Date'), yaxis = list(title = 'Number of Posts'))
    })
    
    output$fb_content_reach_by_type <- renderPlotly({
      plot_ly(fb_data, labels = ~ContentType, values = ~Reach, type = 'pie') %>%
        layout(title = 'Content Reach by Type')
    })
    
    # Tab 3 - Facebook - Sentiment Analysis
    output$sentiment_analysis_fb <- renderPlotly({
      plot_ly(x = names(sentiment_summary_fb), y = sentiment_summary_fb, type = 'bar', 
              marker = list(color = rainbow(length(sentiment_summary_fb)))) %>%
        layout(title = 'Sentiment Analysis (on comments made by followers)', xaxis = list(title = 'Sentiment category'), yaxis = list(title = 'Number of posts'))
    })
    
    # Tab 4 - Predictive Analytics - Facebook
    fb_data$Date <- as.Date(fb_data$Date, origin = "1970-01-01") # Ensure Date column is in Date format
    fb_ts <- ts(fb_data$Engagement, start = c(year(min(fb_data$Date)), month(min(fb_data$Date))), frequency = 365)
    fb_fit <- auto.arima(fb_ts)
    fb_forecast <- forecast(fb_fit, h = 30) # Forecast next 30 days
    
    output$fb_predicted_engagement <- renderPlotly({
      plot_ly() %>%
        add_lines(x = fb_data$Date, y = fb_data$Engagement, name = "Observed", 
                  text = ~paste("Date:", format(fb_data$Date, "%b %d, %Y"), "<br>Engagement:", round(fb_data$Engagement, 0)), hoverinfo = 'text') %>%
        add_lines(x = seq.Date(from = max(fb_data$Date) + 1, by = "day", length.out = 30), y = fb_forecast$mean, name = "Predicted", 
                  text = ~paste("Date:", format(seq.Date(from = max(fb_data$Date) + 1, by = "day", length.out = 30), "%b %d, %Y"), "<br>Engagement:", round(fb_forecast$mean, 0)), hoverinfo = 'text') %>%
        layout(title = "Predicted Engagement Over Time", 
               xaxis = list(title = "Date", tickformat = "%b %d, %Y"),
               yaxis = list(title = "Engagement"))
    })
    
    fb_ts_reach <- ts(fb_data$Reach, start = c(year(min(fb_data$Date)), month(min(fb_data$Date))), frequency = 365)
    fb_fit_reach <- auto.arima(fb_ts_reach)
    fb_forecast_reach <- forecast(fb_fit_reach, h = 30)
    
    output$fb_predicted_reach <- renderPlotly({
      plot_ly() %>%
        add_lines(x = fb_data$Date, y = fb_data$Reach, name = "Observed", 
                  text = ~paste("Date:", format(fb_data$Date, "%b %d, %Y"), "<br>Reach:", round(fb_data$Reach, 0)), hoverinfo = 'text') %>%
        add_lines(x = seq.Date(from = max(fb_data$Date) + 1, by = "day", length.out = 30), y = fb_forecast_reach$mean, name = "Predicted", 
                  text = ~paste("Date:", format(seq.Date(from = max(fb_data$Date) + 1, by = "day", length.out = 30), "%b %d, %Y"), "<br>Reach:", round(fb_forecast_reach$mean, 0)), hoverinfo = 'text') %>%
        layout(title = "Predicted Reach Over Time", 
               xaxis = list(title = "Date", tickformat = "%b %d, %Y"),
               yaxis = list(title = "Reach"))
    })
  })
}


