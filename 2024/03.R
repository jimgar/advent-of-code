input <- readChar("2024/03-input.txt", file.info("2024/03-input.txt")$size)

# Part 1

extract_mul_pairs <- function(input) {
  pattern <- r"{mul\((\d{1,3},\d{1,3})\)}"
  matches <- gregexpr(pattern = pattern, text = input)
  muls <- unlist(regmatches(input, m = matches))

  pairs <- lapply(
    strsplit(gsub(pattern, "\\1", muls), ","),
    as.integer
  )

  return(pairs)
}

pairs <- extract_mul_pairs(input)

sum(sapply(pairs, prod))

# Part 2
do_input <- paste0("do()", input)

# Extract the chunks bookended by do() and don't()
pattern <- r"{do\(\).+?don't\(\)}"
matches <- gregexpr(pattern = pattern, text = do_input)
target_chunks <- unlist(regmatches(do_input, m = matches))

pairs <- extract_mul_pairs(paste0(target_chunks, collapse = ""))

sum(sapply(pairs, prod))


# ---- Alternative using capture groups and `utils::strcapture` ----

# I prefer my first solution (not that there's much difference!) but remember
# coming across `utils::strcapture` a couple of times as a comment on the
# StackOverflow post linked below. I always wanted to find a use for it, and
# here we finally are!
# https://stackoverflow.com/questions/952275/regex-group-capture-in-r-with-multiple-capture-groups


# Part 1

input <- readChar("2024/03-input.txt", file.info("2024/03-input.txt")$size)

extract_mul_pairs <- function(input) {
  # By using capture groups we can recycle the pattern in the function.
  # Capture groups will be ignored by gregexpr but used by strcapture
  pattern <- r"{mul\((\d{1,3}),(\d{1,3})\)}"
  matches <- gregexpr(pattern = pattern, text = input)
  muls <- unlist(regmatches(input, m = matches))

  pairs <- utils::strcapture(
    pattern = pattern,
    x = muls,
    proto = data.frame(left = integer(), right = integer())
  )

  return(pairs)
}

df <- extract_mul_pairs(input)

sum(df$left * df$right)


# Part 2

do_input <- paste0("do()", input)

# Extract the chunks bookended by do() and don't()
pattern <- r"{do\(\).+?don't\(\)}"
matches <- gregexpr(pattern = pattern, text = do_input)
target_chunks <- unlist(regmatches(do_input, m = matches))

df <- extract_mul_pairs(paste0(target_chunks, collapse = ""))

sum(df$left * df$right)
