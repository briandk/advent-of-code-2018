#' # Part 1
#'
#' --- Day 3: No Matter How You Slice It ---
#'
#' The Elves managed to locate the chimney-squeeze prototype fabric for Santa's suit (thanks to someone who helpfully wrote its box IDs on the wall of the warehouse in the middle of the night). Unfortunately, anomalies are still affecting them - nobody can even agree on how to cut the fabric.
#'
#' The whole piece of fabric they're working on is a very large square - at least 1000 inches on each side.
#'
#' Each Elf has made a claim about which area of fabric would be ideal for Santa's suit. All claims have an ID and consist of a single rectangle with edges parallel to the edges of the fabric. Each claim's rectangle is defined as follows:
#'
#'     The number of inches between the left edge of the fabric and the left edge of the rectangle.
#'     The number of inches between the top edge of the fabric and the top edge of the rectangle.
#'     The width of the rectangle in inches.
#'     The height of the rectangle in inches.
#'
#' A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall. Visually, it claims the square inches of fabric represented by # (and ignores the square inches of fabric represented by .) in the diagram below:
#'
#' ...........
#' ...........
#' ...#####...
#' ...#####...
#' ...#####...
#' ...#####...
#' ...........
#' ...........
#' ...........
#'
#' The problem is that many of the claims overlap, causing two or more claims to cover part of the same areas. For example, consider the following claims:
#'
#' #1 @ 1,3: 4x4
#' #2 @ 3,1: 4x4
#' #3 @ 5,5: 2x2
#'
#' Visually, these claim the following areas:
#'
#' ........
#' ...2222.
#' ...2222.
#' .11XX22.
#' .11XX22.
#' .111133.
#' .111133.
#' ........
#'
#' The four square inches marked with X are claimed by both 1 and 2. (Claim 3, while adjacent to the others, does not overlap either of them.)
#'
#' If the Elves all proceed with their own plans, none of them will have enough fabric. How many square inches of fabric are within two or more claims?
#'
#' # Solution

library(dplyr)
library(magrittr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

clean_data <- function(raw_data) {
  return(
    raw_data %>%
      mutate(claim_id = stringr::str_remove(X1, "#")) %>%
      mutate(starting_coordinates = stringr::str_remove(X3, ":")) %>%
      mutate(area = X4) %>%
      select(claim_id, starting_coordinates, area)
  )
}



separate_coordinates_and_area <- function(clean_data) {
  return(
    clean_data %>%
      tidyr::separate(starting_coordinates, into = c("start_x", "start_y")) %>%
      tidyr::separate(area,
                      sep = "x",
                      into = c("extend_x", "extend_y"))
  )
}


split_into_columns <- function(cleaned_data) {
  return(separate_coordinates_and_area(cleaned_data) %>%
            map_df( ~ as.numeric(.)))
}

prepare_data <- function(raw_data) {
  return(
    raw_data %>%
      clean_data() %>%
      split_into_columns()
  )
}

turn_start_extend_into_vector <- function(start, extend) {
  return((start):(start + extend - 1))
}

generate_coordinate_pairs <-
  function(x_start, y_start, x_extend, y_extend) {
    x_values <- turn_start_extend_into_vector(x_start, x_extend)
    y_values <- turn_start_extend_into_vector(y_start, y_extend)
    combined_pairs <- NULL
    for (x in x_values) {
      for (y in y_values) {
        combined_pairs <- c(combined_pairs, paste(x, y, sep = "-"))
      }
    }
    collapsed_combined_pairs <-
      combined_pairs %>% paste(collapse = ",")
    # combined_pairs <- paste(combined_pairs, collapse=",")
    # print(combined_pairs)
    # print(length(combined_pairs))
    # combined_pairs <- x_values %>%
    #   purrr::map( ~ purrr::map2(., y_values, ~ paste(.x, .y, sep = "-"))) %>%
    #   unlist() %>%
    #   paste(collapse = ",") %>%
    #   debug_pipe()
    return(collapsed_combined_pairs)
  }

expand_data_in_a_really_expensive_way_that_takes_a_long_time <- function(cleaned_data) {

  expanded_data <- cleaned_data %>%
    rowwise() %>%
    mutate(
      claim_id_expanded = claim_id,
      start_x_expanded = start_x,
      start_y_expanded = start_y,
      extend_x_expanded = extend_x,
      extend_y_expanded = extend_y,
      coordinate_pairs = generate_coordinate_pairs(start_x, start_y, extend_x, extend_y)
    )
  return(expanded_data)
}

compute_doubly_claimed_coordinates <- function(cleaned_data) {
  return(
    cleaned_data %>%
      count(coordinate_pairs) %>%
      filter(n > 1)

  )
}

melt_data_into_long_format <- function(expanded_data) {
  long_data <- expanded_data %>%
    tidyr::separate_rows(coordinate_pairs, sep = ",") %>%
    select(claim_id, coordinate_pairs)

  return(long_data)

}

get_expanded_data_from_scratch <- function() {
  raw_data <- readr::read_delim("input.txt",
                                col_names = FALSE,
                                delim = " ")
  expanded_data <- raw_data %>%
    prepare_data() %>%
    expand_data_in_a_really_expensive_way_that_takes_a_long_time()
  return(expanded_data)
}

get_long_data <- function() {
  expanded_data <- get_expanded_data_from_scratch()
  long_data <- expanded_data %>% melt_data_into_long_format()
  return(long_data)
}

#' # Part 2

get_coordinate_pairs_that_appear_in_more_than_one_claim <- function(long_data) {

  output <- long_data %>%
    compute_doubly_claimed_coordinates() %>%
    select(coordinate_pairs)

  return(output)
}

# Part 1 answer
#
answer_part_1 <- function(long_data) {
  long_data <- get_long_data()
  answer <- long_data %>%
    compute_doubly_claimed_coordinates() %>%
    dim(.) %>%
    `[`(1)
  long_data %>% save(., file = "long_data.rda")
  print(answer)
  return(answer)
}

check_if_coordinates_are_on_the_naughty_list <- function(coordinates, naughty_list) {
  coordinates_are_naughty <- coordinates %in% naughty_list
  return(coordinates_are_naughty)
}

answer_part_two <- function(expanded_data, long_data) {
  naughty_list <-
    get_coordinate_pairs_that_appear_in_more_than_one_claim(long_data) %>%
    `$`(coordinate_pairs)
  print("naughty list is: ")
  naughty_list %>% head() %>% print()
  print("----------")
  long_data %>%
    filter(str_detect(coordinate_pairs, "10-")) %>%
    head() %>%
    print()
  print("-------")
  # long_data %>%
  #   head() %>%
  #   mutate(is_naughty = check_if_coordinates_are_on_the_naughty_list(coordinate_pairs, naughty_list)) %>%
  #   head() %>%
  #   print()

  # duplicate_filter <- purrr::map(expanded_data$coordinate_pairs %>% head(), function(pairs) {
  #   splits <- pairs %>% str_split(",")
  #   for (coordinates in splits) {
  #     for (overlap in overlaps) {
  #
  #     }
  #   }
  #
  #   output <- TRUE
  #   map(overlaps, function(overlap) {
  #     output <- TRUE
  #     if (pair %>% str_detect(overlap)) {
  #       output <- FALSE
  #     }
  #     return(output)
  #   })
  # })
  # duplicate_filter %>%
  #   unlist() %>%
  #   head() %>%
  #   glimpse() %>%
  #   print()
}



answer_the_questions <- function() {
  expanded_data <- get_expanded_data_from_scratch()
  long_data <- expanded_data %>% melt_data_into_long_format()
  # answer_part_1(long_data)
  answer_part_two(expanded_data = expanded_data,
                  long_data = long_data)
}

answer_the_questions()


# "10-153" %in% repeats$coordinate_pairs %>% print()
#





