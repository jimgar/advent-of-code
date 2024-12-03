df <- read.table("2024/01-input.txt", col.names = c("l", "r"))

# Part 1

df$l <- sort(df$l)
df$r <- sort(df$r)

sum(abs(df$r - df$l))

# Part 2

freq <- as.data.frame(table(df$r[df$r %in% df$l]))
names(freq) <- c("l", "f")

df <- merge(df, freq, by = "l", all.x = TRUE, sort = FALSE)

sum(df$l * df$f, na.rm = TRUE)
