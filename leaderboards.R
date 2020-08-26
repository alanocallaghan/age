library("curl")
library("ggplot2")
library("ggpointdensity")
library("viridis")
c1 <- curl::curl_fetch_memory("https://aoe2.net/api/leaderboard?game=aoe2de&leaderboard_id=3&start=1&count=10000")
json1 <- jsonlite::prettify(rawToChar(c1$content))
jl1 <- jsonlite::fromJSON(json1)
leaderboard <- jl1$leaderboard
c2 <- curl::curl_fetch_memory("https://aoe2.net/api/leaderboard?game=aoe2de&leaderboard_id=3&start=10001&count=10000")
json2 <- jsonlite::prettify(rawToChar(c2$content))
jl2 <- jsonlite::fromJSON(json2)
leaderboard <- rbind(leaderboard, jl2$leaderboard)
c3 <- curl::curl_fetch_memory("https://aoe2.net/api/leaderboard?game=aoe2de&leaderboard_id=3&start=20001&count=10000")
json3 <- jsonlite::prettify(rawToChar(c3$content))
jl3 <- jsonlite::fromJSON(json3)
leaderboard <- rbind(leaderboard, jl3$leaderboard)
c4 <- curl::curl_fetch_memory("https://aoe2.net/api/leaderboard?game=aoe2de&leaderboard_id=3&start=30001&count=10000")
json4 <- jsonlite::prettify(rawToChar(c4$content))
jl4 <- jsonlite::fromJSON(json4)
leaderboard_1v1 <- rbind(leaderboard, jl4$leaderboard)



ggplot(leaderboard_1v1) + aes(rating, games) + geom_pointdensity() + scale_colour_viridis(trans="log10")

ggplot(leaderboard_1v1) +
  aes(rating, games) +
  geom_pointdensity() +
  scale_colour_viridis(trans="log10", name = "Local density") +
  labs(
    x = "1v1 rating", y = "Number of games",
    title = paste(
      "1v1 rating against games played\nfor",
      format(nrow(merged_leaderboards), big.mark = ","), 
      "AoE2 DE players"
    )
  )
ggsave("1v1_games.png", width = 7, height = 7)





c1 <- curl::curl_fetch_memory("https://aoe2.net/api/leaderboard?game=aoe2de&leaderboard_id=4&start=4&count=10000")
json1 <- jsonlite::prettify(rawToChar(c1$content))
jl1 <- jsonlite::fromJSON(json1)
leaderboard <- jl1$leaderboard
c2 <- curl::curl_fetch_memory("https://aoe2.net/api/leaderboard?game=aoe2de&leaderboard_id=4&start=10001&count=10000")
json2 <- jsonlite::prettify(rawToChar(c2$content))
jl2 <- jsonlite::fromJSON(json2)
leaderboard <- rbind(leaderboard, jl2$leaderboard)
c3 <- curl::curl_fetch_memory("https://aoe2.net/api/leaderboard?game=aoe2de&leaderboard_id=4&start=20001&count=10000")
json3 <- jsonlite::prettify(rawToChar(c3$content))
jl3 <- jsonlite::fromJSON(json3)
leaderboard <- rbind(leaderboard, jl3$leaderboard)
c4 <- curl::curl_fetch_memory("https://aoe2.net/api/leaderboard?game=aoe2de&leaderboard_id=4&start=30001&count=10000")
json4 <- jsonlite::prettify(rawToChar(c4$content))
jl4 <- jsonlite::fromJSON(json4)
tg_leaderboard <- rbind(leaderboard, jl4$leaderboard)
c5 <- curl::curl_fetch_memory("https://aoe2.net/api/leaderboard?game=aoe2de&leaderboard_id=4&start=40001&count=10000")
json5 <- jsonlite::prettify(rawToChar(c5$content))
jl5 <- jsonlite::fromJSON(json5)
tg_leaderboard <- rbind(tg_leaderboard, jl5$leaderboard)
c6 <- curl::curl_fetch_memory("https://aoe2.net/api/leaderboard?game=aoe2de&leaderboard_id=4&start=50001&count=10000")
json6 <- jsonlite::prettify(rawToChar(c6$content))
jl6 <- jsonlite::fromJSON(json6)
tg_leaderboard <- rbind(tg_leaderboard, jl6$leaderboard)


ggplot(tg_leaderboard) +
  aes(rating, games) +
  geom_pointdensity() +
  scale_colour_viridis(trans="log10", name = "Local density") +
  labs(
    x = "Team game rating", y = "Number of games",
    title = paste(
      "Team game rating against 1v1 rating\nfor",
      format(nrow(merged_leaderboards), big.mark = ","), 
      "AoE2 DE players"
    )
  )
ggsave("tg_games.png", width = 7, height = 7)




merged_leaderboards <- merge(
    leaderboard_1v1, tg_leaderboard,
    by = "profile_id", suffixes=c("_1v1", "_tg"))
theme_set(theme_bw())


rating_diff <- merged_leaderboards$rating_tg - merged_leaderboards$rating_1v1
ggplot() +
  aes(rating_diff) +
  geom_histogram(bins = nclass.FD(rating_diff), fill = "grey90", colour = "black") +
  labs(x = "Rating difference",
    title = paste(
      "Team game rating minus 1v1 rating\nacross",
      format(nrow(merged_leaderboards), big.mark = ","), 
      "AoE2 DE players"
    )
  ) +
  geom_vline(
    aes(
      xintercept = median(rating_diff),
      color = "Median difference"),
    lty = "dashed") +
  scale_colour_manual(values = "black", name = NULL)
ggsave("rating_difference.png", width = 7, height = 7)


ggplot(merged_leaderboards, aes(rating_1v1, rating_tg)) +
  geom_pointdensity() +
  scale_colour_viridis(trans="log10", name = "Local density") +
  geom_smooth(method = "gam") +
  labs(x = "1v1 rating", y = "Team game rating",
    title = paste(
      "Team game rating against games played\nfor",
      format(nrow(merged_leaderboards), big.mark = ","), 
      "AoE2 DE players\n"
    )
  )
ggsave("tg_1v1_gg.png", width = 7, height = 7)


library("mgcv")
library("visreg")

model <- gam(rating_tg ~ s(rating_1v1, bs = "cs"), data = merged_leaderboards)

visreg(model, gg = TRUE) +
  geom_vline(
    aes(
      colour = c("John's Elo"),
      xintercept = c(1000)
    ),
    linetype = "dashed"
  ) +
  geom_vline(
    aes(
      colour = c("My Elo"),
      xintercept = c(1200)
    ),
    linetype = "dashed"
  ) +
  geom_hline(
    aes(
      yintercept = predict(model, list(rating_1v1=c(1000))),
      colour = c("John's predicted Elo")
    ),
    linetype = "dashed"
  ) +
  geom_hline(
    aes(
      yintercept = predict(model, list(rating_1v1=c(1200))),
      colour = c("My predicted Elo")
    ),
    linetype = "dashed"
  )

ggsave("tg_1v1_gam.png", width = 7, height = 7)
