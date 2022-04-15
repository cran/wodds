#' wodd_format
#'
#' @description wodd_format a private function
#'
#' @param wodd_name string. "S0", "S1", "M". etc
#'
#' @return A string
wodd_format <- function(wodd_name) {
  if(is.character(wodd_name)){
  wodd_name <- gsub("S0", "", wodd_name)
  wodd_name <- gsub("S1", "S", wodd_name)
  wodd_name
  }else{
      stop("wodd_name must be a string")
    }
  }
#' raw_wodd
#'
#' @description raw_wodd a private function
#'
#' @param index int
#'
#' @return A vector
#'
#' @import glue
#' @importFrom purrr map_chr
#'
#' @export
raw_wodd <- function(index) {
  if(is.integer(index)){
  int <- 2^index
  power <- 0
  counted <- FALSE
  if (int >= 16) {
    while (counted == FALSE) {
      if (int >= 16^power) {
        power <- power + 1
      } else {
        counted <- TRUE
        power <- power - 1
      }
    }
  }
  if ((16^(power) * 1) == int) {
    return(wodd_format(glue::glue("S{power}")))
  } else if ((16^(power) * 2) == int) {
    return(wodd_format(glue::glue("S{power}M")))
  } else if ((16^(power) * 4) == int) {
    return(wodd_format(glue::glue("S{power}F")))
  } else if ((16^(power) * 8) == int) {
    return(wodd_format(glue::glue("S{power}E")))
  }
  }
  else{
    stop("index must be an integer")
  }
}
#' make_wodd_name
#'
#' @description make_wodd_name a private function
#'
#' @param index int
#'
#' @return A vector
#'
#' @importFrom purrr map_chr
#'
#' @export
make_wodd_name <- function(index) {
  if(is.integer(index)){
  wodd_depth <- seq(1:index)
  wodd_name <- map_chr(.x = wodd_depth, .f = raw_wodd)
  wodd_name
  }else{
    stop("index must be an integer")
  }
}
#' select_wodd_name_from_table
#'
#' @description select_wodd_name_from_table a private function
#'
#' @param index int
#'
#' @return A vector
#'
#' @importFrom dplyr filter
#' @import tibble
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' select_wodd_name_from_table(1L)
select_wodd_name_from_table <- function(index) {
  if(is.integer(index)){
  if (index > 100) {
    stop("depth > 100 which is larger than precalculated table of wodd names")
  } else {
    first_100_wodds <- c(
      "M", "F", "E", "S", "SM",
      "SF", "SE", "S2", "S2M", "S2F", "S2E", "S3", "S3M", "S3F", "S3E",
      "S4", "S4M", "S4F", "S4E", "S5", "S5M", "S5F", "S5E", "S6", "S6M",
      "S6F", "S6E", "S7", "S7M", "S7F", "S7E", "S8", "S8M", "S8F",
      "S8E", "S9", "S9M", "S9F", "S9E", "S0", "S0M", "S0F", "S0E",
      "S1", "S1M", "S1F", "S1E", "S2", "S2M", "S2F", "S2E", "S3", "S3M",
      "S3F", "S3E", "S4", "S4M", "S4F", "S4E", "S5", "S5M", "S5F",
      "S5E", "S6", "S6M", "S6F", "S6E", "S7", "S7M", "S7F", "S7E",
      "S8", "S8M", "S8F", "S8E", "S9", "S9M", "S9F", "S9E", "S20",
      "S20M", "S20F", "S20E", "S21", "S21M", "S21F", "S21E", "S22",
      "S22M", "S22F", "S22E", "S23", "S23M", "S23F", "S23E", "S24",
      "S24M", "S24F", "S24E", "S25"
    )
    first_100_wodds[1:index]
  }
  }else{
    stop("index must be an integer")
  }
}
#' Get sample size from depth
#'
#' @description Calculates the sample size needed given an alpha level and depth
#'
#' @param d an integer depth
#' @param alpha alpha level such as 0.1, 0.05, 0.01. An alpha of 0.05 would be associated with a 95 percent confidence interval
#' @param conservative a bool. default is FALSE. If TRUE then a conservative (larger) sample size is returned.
#'
#' @return a float sample size
#'
#' @export
#'
#' @examples
#' get_n_from_depth(7L, 0.01)
get_n_from_depth <- function(d, alpha = 0.05, conservative = TRUE){
  if(!is.integer(d)){
    stop("d (depth) must be an integer")
  }else{
    if (!is.null(alpha)){
      if (conservative == FALSE){
        sample_size <- 2^((d-1) + log2(2 * (qnorm(1 - (alpha / 2))^2)))
        floor(sample_size) # a simple round down of the decimals # not necessary just convenience
      }else{
        sample_size <- 2^((d) + log2(2 * (qnorm(1 - (alpha / 2))^2)))
        floor(sample_size) # a simple round down of the decimals # not necessary just convenience
      }
    }
  }
}
#' Get depth from sample size
#'
#' @description Calculates the depth given a sample size and alpha level
#'
#' @param n an integer scalar sample size
#' @param alpha alpha level such as 0.1, 0.05, 0.01. An alpha of 0.05 would be associated with a 95 percent confidence interval
#'
#' @return an integer depth
#'
#' @export
#'
#' @examples
#' get_depth_from_n(1e4L, 0.05)
get_depth_from_n <- function(n, alpha = 0.05){
  if(is.integer(n)){
    if(!is.null(alpha)){
      stopifnot(is.numeric(alpha) && length(alpha) == 1)
      stopifnot(alpha > 0 && alpha < 1)
      k <- as.integer(floor(log2(n) - log2(2 * (qnorm(1 - (alpha / 2))^2))) + 1L)
      k
    }
  }else{
    stop("n must be an integer. Here is an example get_depth_from_n(n=1e4L, 0.05). suffix the number with L to force value to be an integer")
  }
}
#' Calculate whisker odds
#'
#' @description makes whisker odds
#'
#' @param y A vector of values
#' @param alpha the alpha level, such as 0.05 which is the compliment of the confidence interval, such as 0.95
#' @param include_tail_area a binary.
#' If true then include a column of tail area 2^(i)
#' @param include_outliers a binary.
#' If true include a column of outliers beyond the last wodd depth
#' @param include_depth a binary.
#' If true include a column indicating the depth of the letter value
#'
#' @return A dataframe of wodds
#'   \item{lower_value}{lower value}
#'   \item{wodd_name}{Name of wodd}
#'   \item{upper_value}{upper value}
#'
#' @importFrom dplyr select filter bind_rows mutate
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom purrr map_chr
#' @importFrom stats qnorm median quantile
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' wodds(rnorm(1e4, 0, 1))
wodds <- function(y, alpha = 0.05, include_tail_area = FALSE, include_outliers = FALSE,
                  include_depth = FALSE) {
  lower_value <- upper_value <- wodd_name <- NULL
  data <- sort(y)
  n <- length(data)
  alpha <- alpha
  # rule 3
  k <- get_depth_from_n(as.integer(n), 0.05)
  lvl <- (k - 1) * 2
  nq <- lvl - 1
  qs <- rep(0, nq) # initialize array of quantiles
  f <- function(n) {
    (1 + floor(n)) / 2
  }
  for (i in 1:k) {
    # median calculation
    if (i == 1) {
      d <- f(n)
      qs[i - 1] <- 0.5
      d
    } else {
      d <- f(d)
      d
    }
    if (ceiling(d) != floor(d)) {
      l_idx1 <- as.integer(floor(d))
      l_idx2 <- as.integer(ceiling(d))
      u_idx1 <- as.integer(floor(n - d + 1))
      u_idx2 <- as.integer(ceiling(n - d + 1))
      ql <- mean(c(l_idx1 / n, l_idx2 / n))
      qu <- mean(c(u_idx1 / n, u_idx2 / n))
    } else {
      ql <- as.integer(d) / n
      qu <- as.integer(floor(n - d + 1)) / n
    }
    qs[((i - 1) * 2) - 1] <- ql
    qs[(i - 1) * 2] <- qu
    vf <- quantile(data, qs)
  }
  vf <- c(median(data), median(data), vf)
  lower <- vf[seq(1, length(vf), 2)]
  upper <- vf[seq(2, length(vf), 2)]
  depth <- seq(1, k)
  tail_area <- 2^depth
  wodd_depth_name <- wodds::select_wodd_name_from_table(k)
  o_upper <- sort(y[y > max(upper)])
  o_lower <- sort(y[y < min(lower)], decreasing = TRUE)
  o_max_len <- max(length(o_upper), length(o_lower))
  length(o_upper) <- o_max_len
  length(o_lower) <- o_max_len
  length(depth) <- o_max_len + length(depth)
  length(tail_area) <- o_max_len + length(tail_area)
  o_name <- paste0("O", seq(1, o_max_len))
  if (o_max_len > 0){
    df_o <- tibble::tibble(lower_value = o_lower, wodd_name = o_name, upper_value = o_upper)
    df_base <- tibble::tibble(lower_value = lower, wodd_name = wodd_depth_name, upper_value = upper)
    df_with_outliers <- dplyr::bind_rows(df_base, df_o)
  }else{
    df_o <- tibble::tibble(lower_value = NULL, wodd_name = NULL, upper_value = NULL)
    df_base <- tibble::tibble(lower_value = lower, wodd_name = wodd_depth_name, upper_value = upper)
    df_with_outliers <- dplyr::bind_rows(df_base)
  }

  df_with_outliers_depth <- df_with_outliers %>%
    dplyr::mutate(depth = depth) %>%
    dplyr::select(depth, lower_value, wodd_name, upper_value)
  df_with_outliers_depth_tail_area <- df_with_outliers %>%
    dplyr::mutate(depth = depth, tail_area = tail_area) %>%
    dplyr::select(depth, tail_area, lower_value, wodd_name, upper_value)
  if (include_outliers == TRUE) {
    if (include_tail_area == FALSE & include_depth == FALSE) {
      df_with_outliers_depth_tail_area %>% dplyr::select(-tail_area, -depth)
    } else if (include_tail_area == TRUE & include_depth == FALSE) {
      df_with_outliers_depth_tail_area %>% dplyr::select(-depth)
    } else if (include_tail_area == TRUE & include_depth == TRUE) {
      df_with_outliers_depth_tail_area
    }else if (include_tail_area == FALSE & include_depth == TRUE){
      df_with_outliers_depth_tail_area %>% dplyr::select(-tail_area)
    }
  } else {
    df_with_depth_tail_area <- df_with_outliers_depth_tail_area %>% dplyr::filter(!is.na(depth))
    if (include_tail_area == FALSE & include_depth == FALSE) {
      df_with_depth_tail_area %>% dplyr::select(-tail_area, -depth)
    } else if (include_tail_area == TRUE & include_depth == FALSE) {
      df_with_depth_tail_area %>% dplyr::select(-depth)
    } else if (include_tail_area == TRUE & include_depth == TRUE) {
      df_with_depth_tail_area
    }else if (include_tail_area == FALSE & include_depth == TRUE){
      df_with_depth_tail_area %>% dplyr::select(-tail_area)
    }
  }
}
