library("curl")
library("ggplot2")
library("ggpointdensity")
library("viridis")
library("glue")
theme_set(theme_bw())


lead_ids <- c(rm = 3, trm = 4, ew = 13, tew = 14)
boards <- list()

get_board <- function(id) {
  request <- function(id, start) {
    c <- curl::curl_fetch_memory(glue("https://aoe2.net/api/leaderboard?game=aoe2de&leaderboard_id={id}&start={start}&count=10000"))
    json <- jsonlite::prettify(rawToChar(c$content))
    jl <- jsonlite::fromJSON(json)
    jl$leaderboard
  }
  start <- 0
  leaderboard <- request(id, start)
  while (TRUE) {
    current <- request(id, start)
    if (!length(current)) {
      break
    }
    leaderboard <- rbind(leaderboard, current)
    start <- start + 10000
  }
  leaderboard
}

for (board in names(lead_ids)) {
  boards[[board]] <- get_board(lead_ids[[board]])
}

leaderboard_1v1 <- boards[["rm"]]




ggplot(leaderboard_1v1) + aes(rating, games) + geom_pointdensity() + scale_colour_viridis(trans="log10")

ggplot(leaderboard_1v1) +
  aes(rating, games) +
  geom_pointdensity() +
  scale_colour_viridis(trans="log10", name = "Local density") +
  labs(
    x = "1v1 rating", y = "Number of games",
    title = paste(
      "1v1 rating against games played\nfor",
      format(nrow(leaderboard_1v1), big.mark = ","), 
      "AoE2 DE players"
    )
  )
ggsave("1v1_games.png", width = 7, height = 7)

tg_leaderboard <- boards[["trm"]]


ggplot(tg_leaderboard) +
  aes(rating, games) +
  geom_pointdensity() +
  scale_colour_viridis(trans="log10", name = "Local density") +
  labs(
    x = "Team game rating", y = "Number of games",
    title = paste(
      "Team game rating against games played\nfor",
      format(nrow(tg_leaderboard), big.mark = ","), 
      "AoE2 DE players"
    )
  )
ggsave("tg_games.png", width = 7, height = 7)


merged_leaderboards <- merge(
  leaderboard_1v1, tg_leaderboard,
  by = "profile_id", suffixes=c("_1v1", "_tg")
)

save.image("leaderboards.RData")


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
      "Team game rating against 1v1 rating\nfor",
      format(nrow(merged_leaderboards), big.mark = ","), 
      "AoE2 DE players\n"
    )
  )
ggsave("tg_1v1_gg.png", width = 7, height = 7)


ind_filter <- merged_leaderboards$games_1v1 > 20 & merged_leaderboards$games_tg > 20
merged_leaderboards_filter <- merged_leaderboards[ind_filter, ]


rating_diff_filter <- merged_leaderboards_filter$rating_tg - merged_leaderboards_filter$rating_1v1
ggplot() +
  aes(rating_diff_filter) +
  geom_histogram(bins = nclass.FD(rating_diff_filter), fill = "grey90", colour = "black") +
  labs(x = "Rating difference",
    title = paste(
      "Team game rating minus 1v1 rating\nacross",
      format(nrow(merged_leaderboards_filter), big.mark = ","), 
      "AoE2 DE players"
    )
  ) +
  geom_vline(
    aes(
      xintercept = median(rating_diff_filter),
      color = "Median difference"),
    lty = "dashed") +
  scale_colour_manual(values = "black", name = NULL)
ggsave("rating_difference_filter.png", width = 7, height = 7)


merged_leaderboards$mean_games <- rowMeans(merged_leaderboards[, c("games_1v1", "games_tg")])
ggplot(merged_leaderboards, aes(rating_1v1, rating_tg, colour = mean_games)) +
  geom_point() +
  scale_colour_viridis(trans = "log10", name = "Number of games\n(average TG+1v1)") +
  geom_smooth(method = "gam") +
  labs(x = "1v1 rating", y = "Team game rating",
    title = paste(
      "Team game rating against 1v1 rating\nfor",
      format(nrow(merged_leaderboards), big.mark = ","), 
      "AoE2 DE players\n"
    )
  )
ggsave("tg_1v1_gg_ngames.png", width = 7, height = 7)



ggplot(merged_leaderboards_filter, aes(rating_1v1, rating_tg)) +
  geom_pointdensity() +
  scale_colour_viridis(trans = "log10", name = "Local density") +
  geom_smooth(method = "gam") +
  labs(x = "1v1 rating", y = "Team game rating",
    title = paste(
      "Team game rating against 1v1 rating\nfor",
      format(nrow(merged_leaderboards_filter), big.mark = ","), 
      "AoE2 DE players\n"
    )
  )
ggsave("tg_1v1_gg_filter.png", width = 7, height = 7)





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




rm_ew_1v1 <- merge(
  boards[["rm"]], boards[["ew"]],
  by = "profile_id", suffixes = c("_rm", "_ew")
)
# merged_leaderboards$mean_games <- rowMeans(rm_ew_1v1[, c("games_rm", "games_ew")])


ggplot(rm_ew_1v1, aes(rating_rm, rating_ew)) +
  geom_pointdensity(alpha = 0.5) +
  scale_colour_viridis(guide = "none") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  # scale_colour_viridis(trans = "log10", name = "Number of games\n(average TG+1v1)") +
  # geom_smooth(method = "gam") +
  labs(
    x = "Random map rating", y = "Empire wars rating",
    title = paste(
      "1v1 Empire wars rating against RM rating\nfor",
      format(nrow(rm_ew_1v1), big.mark = ","),
      "AoE2 DE players"
    )
  )

ggsave("rm_ew_1v1.png", width = 7, height = 7)
