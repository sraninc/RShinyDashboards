library(httr)
library(jsonlite)

# User access token obtained from Facebook Login
user_access_token <- "EAAETnmiGvpQBO5VEuVbdBeBcj5LQ7oe5QKgLGmTGNNZAVRDtZB8ZBG2MR6BpZBrYT60BoNZABpZC2AsaiYIfsEwl0WmfOyjhNh3ZCqVj7gaqo90dw9o8rk9wneNg9aFuZAQ8jAaGSJszMcRoBQZCSkvOpRwBZCIjFvu3LdxSIP841o1voiOUdbRRZBxJKGVgDZB1ZCuyYfRpQVJ4QZAk4VN0CYnv9HZBCkh"

# Step 1: Get Page Access Token
pages_url <- paste0("https://graph.facebook.com/v20.0/me/accounts?access_token=", user_access_token)
pages_response <- GET(pages_url)
pages_info <- fromJSON(content(pages_response, "text"))

# Print response to debug
print(pages_info)

# Extract the Page Access Token for the specific page
page_id <- "370180529500817" 
page_access_token <- NULL

for (page in pages_info$data) {
  if (page$id == page_id) {
    page_access_token <- page$access_token
    break
  }
}

if (is.null(page_access_token)) {
  stop("Page access token not found for the specified page.")
}

cat("Page Access Token:", page_access_token)

# Step 2: Fetch Page Insights using Page Access Token
insights_url <- paste0(
  "https://graph.facebook.com/v12.0/", page_id, 
  "/insights?metric=page_tab_views_login_top_unique&access_token=", page_access_token
)

insights_response <- GET(insights_url)
insights_data <- fromJSON(content(insights_response, "text"))

print(insights_data)
