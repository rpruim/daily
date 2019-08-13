
#' Calendar CSS
#'
#' Emit CSS for calendars
#'
#' @export
calendar_css <- function() {
  path <- system.file("css", "calendar.css", package="rjp")
  lines <- getSrcLines(srcfile(path), 1, 10000)
  paste(lines, collapse = "\n")
}

#' Create HTML Calendar
#'
#' Create HTML Calendar
#'
#' @import magrittr
#' @importFrom lubridate today floor_date mdy weeks month wday mday days
#' @importFrom dplyr mutate arrange filter select group_by ungroup
#' @importFrom dplyr slice pull do right_join bind_rows
#' @importFrom glue glue
#' @importFrom stringr str_split
#' @import ggplot2
#'
#' @param path  path to "daily" file
#' @param start start date
#' @param end   end date
#' @param mask  a numeric vector indicating which days of the week to display. (1 = Sunday)
#'
#' @export
calendar <-
  function(
    path = "daily.txt",
    start = lubridate::today() - lubridate::weeks(2),
    end   = lubridate::today() + lubridate::weeks(4),
    mask = 2:6
  )
  {
    current_date  <- NA
    Cal <- data.frame(date = NA) # , Topic = NA, note = NA)
    lines <- getSrcLines(srcfile(path), 1, 10000)
    state = 0
    current_row <- data.frame(date = current_date)
    done_with_cal <- FALSE
    skip_day <- FALSE
    for (line in lines) {
      if (done_with_cal) break

      # skip commented lines
      if (grepl("^#", line)) {
        next
      }

      # stop processing if we hit endofcal
      if (grepl("^===", line) &&
          grepl("endofcal", line, ignore.case = TRUE)
          ) {
        done_with_cal <- TRUE
      }

      # next day
      if (grepl("^===", line)) {
        if (! skip_day) {
          # if not skipping, add to calendar and bump date
          Cal <-
            dplyr::bind_rows(
              Cal,
              current_row
            )
          new_date <- lubridate::mdy(line, quiet = TRUE)
          if (is.na(new_date)) {
            current_date <- next_cal_day(current_date, mask)
          } else {
            current_date <- new_date
          }
        }
        # whether skipped or not, flush current_date
        skip_day <- FALSE
        current_row <- data.frame(date = current_date)

        # set flag to skip new day's content
        if (grepl("^===skip", line)) {
          skip_day <- TRUE
        }

      }  # end if ^===
      # item to place on calendar of form label:: value
      if (grepl("^[^\\s]+:: ", line)) {
        parts <- stringr::str_split(line, pattern = ":: ", n = 2)[[1]]
        label <- parts[1]; value <- parts[2]
        # check for and process URLs
        parts <- stringr::str_split(value, pattern = "@@", n = 2)[[1]]
        if (length(parts) > 1) {
          value <-
            glue::glue('<a href="{url}">{val}</a>',
                       url = parts[2], val = parts[1])
        } else {
          value <- parts[1]
        }

        if (!is.null(current_row[1, label])) {
          current_row[1, label] <-
            paste0(current_row[1, label], "<br>", value)
        } else {
          current_row[1, label] <- value
        }
      }
    }  # end for line
  Cal <-
    Cal %>%
    filter(!is.na(date), date >= start)

  start <- lubridate::floor_date(start, unit = "week")
  suppressMessages(
    Cal %>%
      right_join(
        data.frame(
          date = start +
            lubridate::days(1: difftime(max(Cal$date), start,
                                        units = "days"))
        )
      ) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(week  = as.numeric(ceiling(difftime(date, start, units = "weeks"))))
    # month = lubridate::month(date, label = TRUE),
    # day   = lubridate::wday(date),
    # wday  = lubridate::wday(date, label = TRUE))
  )
  }

next_cal_day <- function(date, mask = 1:5) {
  wd <- lubridate::wday(date)
  date + lubridate::days(days_til_next(mask)[wd])
}

days_til_next <- function(mask) {
  mask <- c(mask, 7 + mask) # make it two weeks
  sapply(1:7, function(x) min(mask[mask > x]) - x)
}

#' @export
gg_calendar <- function(calendar, show = 1:7, size = 3) {
  ggplot(data = calendar %>% filter(wday(date) %in% show)) +
    geom_text(aes(y = 4, x = -1,
                  label = paste0(lubridate::month(date), "/",  lubridate::mday(date))),
              color = "black", hjust = 0, size = size * .8) +
    geom_text(aes(y = 3, x = 0, label = Topic),
              color = "navy", hjust = 0, size = size) +
    geom_text(aes(y = 2, x = 0, label = Note),
              color = "forestgreen", hjust = 0, size = size) +
    facet_grid(week ~ wday(date)) +
    lims(y = c(0,5), x = c(-1,5)) +
    labs(x = "", y = "") +
    theme_bw() +
    theme(
      axis.text  = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank())
}

#' @export
html_calendar <- function(calendar, show = 1:7, items = NULL) {
  Cal <- calendar %>% filter(wday(date) %in% show)
  Cal <- Cal %>%
    dplyr::group_by(date, week) %>%
    dplyr::do(html_reduce(.)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date)
  last_week <- -Inf
  res <- c("<table border=1 width=95%><tr>")
  for (s in show) {
    res <- c(res, '<col width = "1*">')  # equal widths for each column
  }
  res <- c(res, "<tr>")
  for (wday in c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[show]) {
    res <- c(res, glue::glue("<th>{day}</th>", day = wday))
  }
  for (i in 1:nrow(Cal)) {
    if (Cal[i, "week"] > last_week) {
      res <- c(res, paste("<!-- ", Cal[i, "week"], ">", last_week, "-->"))
      res <- c(res, "</tr><tr>")
      last_week <- Cal[i, "week"]
    }
    res <- c(res, render_day(Cal, i, items))
  }
  res <- c(res, "</tr></table>")
  res %>% paste(collapse = "\n")
}

render_day <- function(calendar, row, items) {
  available_items <- setdiff(names(calendar), c("date", "week"))
  if (is.null(items)) {
    items <- available_items
  } else {
    items <- intersect(items, available_items)
  }

  month <-
    calendar %>%
    dplyr::slice(row) %>%
    dplyr::pull(date) %>%
    lubridate::month()
  res <-
    glue::glue('<td class = "{parity}">{month}/{day}<br>',
               parity = if(month %% 2 == 0) "even" else "odd",
               month = month,
               day   =
                 calendar %>%
                 dplyr::slice(row) %>%
                 dplyr::pull(date) %>%
                 lubridate::mday()
    )
  # res <- c(res, "<table><tr><td>")
  for (i in items) {
    if (! is.na(calendar[row, i])) {
      res <-
        paste0(res,
               glue::glue('<span class = "{item}">{value}</span><br>',
                          item = i, value = calendar[row, i]
               )
        )
    }
  }
  # res <- c(res, "</td></tr></table>")
  res <- paste0(res, "<br></td>")
  res
}

html_reduce <- function(data) {
  lapply(
    data %>% dplyr::select(-date, -week),
    function(x) paste0(unique(na.omit(x)), collapse = "<br>")
  ) %>%
    as.data.frame(stringsAsFactors = FALSE)
}

# calendar(start = mdy("8/15/2018"), mask = c(2,3,5,6)) %>%
#   gg_calendar(show = 2:6)
#
# calendar(start = mdy("8/15/2018"), mask = c(2,3,5,6)) %>%
#   html_calendar()