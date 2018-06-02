plot_square_waffle <- function(.data, variable, label, colors = NULL, reverse = FALSE){
  variable <- enquo(variable)
  col <- pull(.data, !! variable)
  if(reverse) colors <- rev(colors)
  
  if(!is.null(names(colors))) names(colors) <- NULL
  
  if(is.null(colors)){
    fill_scale <- scale_fill_discrete()
  } else {
    fill_scale <- scale_fill_manual(values = colors)
  }
  
  side_length <- col %>%
    na.omit() %>%
    length() %>%
    sqrt() %>%
    ceiling()
  
  col %>%
    as.character() %>%
    sort(decreasing = reverse) %>%
    c(rep(NA, (side_length ^ 2) - length(.))) %>%
    matrix(nrow = side_length, ncol = side_length) %>%
    as.data.frame() %>%
    rownames_to_column(var = "row") %>%
    gather(key = col, value = value, -row) %>%
    mutate(
      col = col %>% gsub("[^0-9]", "", .) %>% as.integer(),
      row = as.integer(row)
    ) %>%
    filter(complete.cases(.)) %>%
    # Add percent to labels
    group_by(value) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    mutate(
      percent = as.integer(round(n / sum(unique(n)) * 100, 0)),
      value = paste0(value, "\n(", percent, "%)")
    ) %>%
    ggplot() +
    geom_tile(
      aes(x = col, y = row, fill = value),
      color = set_names(ocacgt_pal["blanco"], NULL), size = 0.8
    ) +
    labs(
      fill = label
    ) +
    fill_scale +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_equal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom"
    )
}


plot_square_waffle_df <- function(
  .data, variable, label, colors = NULL, reverse = FALSE
){
  variable <- enquo(variable)
  
  # Set color pallete
  if(reverse) colors <- rev(colors)
  
  if(!is.null(names(colors))) names(colors) <- NULL
  
  if(is.null(colors)){
    fill_scale <- scale_fill_discrete()
  } else {
    fill_scale <- scale_fill_manual(values = colors)
  }
  
  .data <- .data %>%
    # Add percent to labels
    group_by(!! variable) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    mutate(
      percent = as.integer(round(n / sum(unique(n)) * 100, 0)),
      n = ifelse(
        test = rep(reverse, n()),
        yes = -n,
        no = n
      ),
      .values = !! variable %>%
        fct_reorder(n)
    ) %>%
    arrange(.values) %>%
    select(-.values)
    
  values <- pull(.data, !! variable)
  
  side_length <- values %>%
    na.omit() %>%
    length() %>%
    sqrt() %>%
    ceiling()
  
  position <- values %>%
    c(rep(NA, (side_length ^ 2) - length(.))) %>%
    matrix(nrow = side_length, ncol = side_length) %>%
    as.data.frame() %>%
    rownames_to_column(var = "row") %>%
    gather(key = col, value = value, -row) %>%
    mutate(
      col = col %>% gsub("[^0-9]", "", .) %>% as.integer(),
      row = as.integer(row)
    ) %>%
    filter(complete.cases(.))
  
  bind_cols(.data, position) %>%
    mutate(
      value = paste0(value, "\n(", percent, "%)")
    ) %>%
    ggplot() +
    geom_tile(
      aes(x = col, y = row, fill = value),
      color = set_names(ocacgt_pal["blanco"], NULL), size = 0.8
    ) +
    labs(
      fill = label
    ) +
    fill_scale +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_equal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom"
    )
}


# Top levels
top <- function(.data, variable, n = 1, alt = "otro"){
  variable <- enquo(variable)
  
  .data %>%
    group_by(!! variable) %>%
    mutate(
      .count = n()
    ) %>%
    ungroup() %>%
    arrange(desc(.count)) %>%
    mutate(
      !! as.character(quo_name(variable)) := 
        ifelse(
          test = !!variable %in% setdiff(unique(!!variable), alt)[seq(1, n)],
          yes = as.character(!!variable),
          no = rep(alt, n())
        ) %>%
        fct_reorder(.count) %>%
        fct_reorder(. != alt)
    ) %>%
    select(-.count)
    # filter(
    #   !!variable %in% unique(!!variable)[seq(1, n)]
    # )
}

