#' Get One Phase's Group Reward and Probability Info
#'
#' @param phase_data One of the three phase objects defined by calling
#'   \code{\link{get_phases}}.
#'
#' @importFrom rlang .data
#'
#' @return A dataframe with four columns. It has the reward and probability of
#'   receiving it as a function of each image's group per phase. This can be
#'   compared to the output of \code{\link{get_choices}} or
#'   \code{\link{process_choice_data}}. This function is called by
#'   \code{\link{get_group_info}} which combines the information for all three of
#'   the phases.

get_group_reward_probs <- function(phase_data) {

  phase_data %>%
    dplyr::slice(1) %>%
    dplyr::mutate(data = stringr::str_remove_all(.data$data, "Phase [1|2|3] Group \\{Reward, Probability\\}: ")) %>%
    tidyr::separate(col = .data$data,
                    into = c("1", "2", "3", "4", "5", "6"),
                    extra = "warn",
                    remove = TRUE,
                    sep = "\\},") %>%
    dplyr::select(-row) %>%
    dplyr::mutate(dplyr::across(.cols = -.data$phase, ~ stringr::str_remove(.x, "Group [1|2|3|4|5|6] \\{\\$"))) %>%
    tidyr::pivot_longer(cols = -.data$phase,
                        names_to = "group",
                        values_to = "info") %>%
    dplyr::mutate(info = stringr::str_trim(.data$info),
                  info = stringr::str_remove_all(.data$info, ",|\\}|%")) %>%
    tidyr::separate(col = .data$info,
                    into = c("reward", "probability"),
                    extra = "warn",
                    remove = TRUE,
                    sep = " ") %>%
    dplyr::mutate(group = forcats::as_factor(.data$group),
                  probability = base::as.numeric(.data$probability),
                  probability = .data$probability/100) %>%
    tidyr::drop_na()

}

#' Get Group Reward and Probability Info for All Phases
#'
#' @param data The data output from \code{\link{read_prpt}}
#'
#' @importFrom rlang .data
#'
#' @return A dataframe with 15 rows and seven columns. It has the reward amount
#'   and probability of receiving a reward as a function of each image's group
#'   per phase. This also has the icon to group mapping for the subject. Most
#'   importantly, it has the EU and rank columns. These are used to determine
#'   optimal choice when measuring behavioral performance. This can be used in
#'   conjunction with the output of \code{\link{get_choices}} or
#'   \code{\link{process_choice_data}}.
#'
#' @export

get_group_info <- function(data) {

  get_phases(data)
  phases <- list(phase$one, phase$two, phase$three)

  iconGroupMaps <- data %>%
    dplyr::filter(stringr::str_detect(.data$data, "ICON TO GROUP MAPPING: ")) %>%
    dplyr::select(-.data$time) %>%
    dplyr::mutate(data = stringr::str_remove(.data$data, "ICON TO GROUP MAPPING:"),
                  data = stringr::str_remove(.data$data, "\\{|\\}"),
                  data = stringr::str_remove(.data$data, "\\}")) %>%
    tidyr::separate(col = .data$data,
                    into = c("icon1", "icon2", "icon3", "icon4", "icon5", "icon6"),
                    sep = "\\,") %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "iconNum") %>%
    dplyr::mutate(value = stringr::str_remove_all(.data$value, "\\\"")) %>%
    tidyr::separate(.data$value,
                    into = c("icon", "group"),
                    sep = "\\:") %>%
    dplyr::arrange(.data$group) %>%
    dplyr::transmute(icon = stringr::str_trim(.data$icon),
                     group = base::as.factor(.data$group))

  #warnings suppressed due to there only being three groups in phase 1. It throws a warning but it doesn't matter.
  output <- base::suppressWarnings(dplyr::left_join(iconGroupMaps, purrr::map_df(phases, ~get_group_reward_probs(.x)), by = "group")) %>%
    dplyr::arrange(phase) %>%
    dplyr::select(phase, .data$icon, .data$group, .data$reward, .data$probability) %>%
    dplyr::mutate(EU = base::as.numeric(.data$reward) * .data$probability) %>%
    dplyr::group_by(phase) %>%
    dplyr::mutate(rank = dplyr::min_rank(.data$EU),
                  sign = ifelse(.data$EU > 0, "pos", "neg")) %>%
    dplyr::ungroup()

  return(output)

}
