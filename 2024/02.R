reports <- readLines("2024/02-input.txt")

reports <- lapply(reports, \(x) {
  as.integer(strsplit(x, split = " ")[[1]])
})

# Part 1

is_safe <- function(x) {
  diffs <- diff(x)

  # One or more steps are too large
  if (any(abs(diffs) > 3L)) {
    return(FALSE)
  }

  all_decreasing <- all(diffs < 0)
  all_increasing <- all(diffs > 0)

  if (!all_decreasing && !all_increasing) {
    # Must be a fluctuation or steps where there was no change
    return(FALSE)
  }

  # At this point it must be smoothly increasing or decreasing
  return(TRUE)
}

sum(sapply(reports, is_safe))


# Part 2

is_safe_turbo_2000 <- function(x) {
  safe <- is_safe(x)

  if (safe) {
    return(safe)
  }

  # Attempt to make it safe by removing one step at a time
  for (i in seq_along(x)) {
    xx <- x[-i]
    safe <- is_safe(xx)

    # No need to keep computing
    if (safe) {
      return(safe)
    }
  }

  # We tried our best...
  return(FALSE)
}

# Part 2
sum(sapply(reports, is_safe_turbo_2000))
