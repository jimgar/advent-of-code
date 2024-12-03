input <- readChar("2024/03-input.txt", file.info("2024/03-input.txt")$size)

# Part 1

extract_mul_pairs <- function(input) {
  pattern <- r"{mul\(\d{1,3},\d{1,3}\)}"
  matches <- gregexpr(pattern = pattern, text = input)
  muls <- unlist(regmatches(input, m = matches))

  pairs <- lapply(
    strsplit(gsub(r"{mul\(|\)}", "", muls), ","),
    as.integer
  )

  return(pairs)
}

pairs <- extract_mul_pairs(input)

sum(sapply(pairs, prod))

# Part 2
do_input <- paste0("do()", input)

pattern <- r"{do\(\).+?don't\(\)}"
matches <- gregexpr(pattern = pattern, text = do_input)
clean_input <- paste0(unlist(regmatches(do_input, m = matches)), collapse = "")

pairs <- extract_mul_pairs(clean_input)

sum(sapply(pairs, prod))
