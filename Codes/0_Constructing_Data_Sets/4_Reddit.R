# install.packages("RedditExtractoR")
# library(RedditExtractoR)
#
# posts <- find_thread_urls(subreddit = "leagueoflegends",
#                           period = "month")
#
# # posts_metadata <- get_thread_content(posts$url) # for the sake of simplicity
#

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

get_pushshift_data <- function(subreddit, start_date, end_date, size = 500) {
  url <- "https://api.pushshift.io/reddit/submission/search/"
  after <- as.integer(as_datetime(start_date))
  before <- as.integer(as_datetime(end_date))

  res <- GET(url, query = list(
    subreddit = subreddit,
    after = after,
    before = before,
    size = size,
    sort = "desc"
  ))

  stop_for_status(res)
  data <- content(res, as = "text", encoding = "UTF-8")
  posts <- fromJSON(data, flatten = TRUE)$data

  if (length(posts) == 0) return(tibble())

  tibble(
    date_utc = as_datetime(posts$created_utc),
    timestamp = posts$created_utc,
    title = posts$title,
    text = posts$selftext,
    subreddit = posts$subreddit,
    comments = posts$num_comments,
    url = posts$url
  )
}

# Example: Reddit posts from June 2022
posts_june2022 <- get_pushshift_data("leagueoflegends", "2022-06-01", "2022-06-30", size = 500)
print(posts_june2022)
