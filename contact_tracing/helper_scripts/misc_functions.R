


col2hex <- function(col, alpha) {
  rgb(t(col2rgb(col)),
      alpha = alpha, maxColorValue = 255
  )
}

#' Function to add column numbers to the names of a column
#' sometimes helpful for arranging
prepend_col_nums <- function(df) {
  names(df) <-
    names(df) %>%
    paste(stringr::str_pad(1:ncol(df), 2, pad = 0), .)
  df
}

faq_function <- function(question, answer, n){
  paste0('accordion(id = "accordion',n, '",
                 accordionItem(title = tags$p("',question,'", style = "font-size: 11px;"),
                               status = "success",
                               collapsed = TRUE,
                               tags$p("', answer, '", style = "font-size: 11px; color: #0b8f53")
                               )
                ),')
}

## add column if it does not exist
force_col_to_exist <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}



split_long_df <- function(df, max_rows) {
  num_rows <- nrow(df)
  # only run if there are more rows than the max
  if (num_rows > max_rows) {
    # create vector of numbers and label the rows
    repeater <- function(number) rep(number, length.out = max_rows)
    repeated_numbers <- map(1:100, ~ repeater(number = .x)) %>% flatten_dbl()
    clipped_numbers_vec <- repeated_numbers[1:num_rows]
    df_labeled <- df %>% bind_cols(clipped_numbers = clipped_numbers_vec)
    
    ## add NA rows so that the last table has the same number as all others ---
    
    ## count how many rows are in last table
    final_table_row_count <- 
      df_labeled %>% 
      filter(clipped_numbers == max(clipped_numbers_vec)) %>% 
      nrow()
    ## therefore, how many should be added
    number_of_rows_to_add <- max_rows - final_table_row_count
    
    ## create data to add
    data_to_add <- 
      matrix(nrow = number_of_rows_to_add, 
             ncol = ncol(df)) %>% 
      data.frame()
    
    names(data_to_add) <- names(df)
    
    ## bind data then replace table label
    df_padded <- 
      df_labeled %>% 
      bind_rows(data_to_add) %>% 
      mutate(clipped_numbers = replace_na(clipped_numbers, max(clipped_numbers_vec)))
    
    
    # split the data based on this vector
    split_dfs <- split(df_padded, df_padded$clipped_numbers)
    
    # append the number of the list to the column names
    for (i in 1:length(split_dfs)) {
      ## strip artificial column clipped-numbers
      split_dfs[[i]] <- split_dfs[[i]] %>% select(-any_of("clipped_numbers"))
      
      ## append only if i is higher than 1
      if (i > 1){
      names(split_dfs[[i]]) <- glue::glue("{names(split_dfs[[i]])} ({i})")
      }
    }
    
    # bind data back together 
    output_df <- split_dfs %>% bind_cols()
    
    return(output_df)
    
  } else {
    return(df)
  }
  
}

## test the function 
# split_long_df(iris, 7)



valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                          icon = NULL, color = "aqua", width = 2, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-sm",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.5);" # make transparent cuz it doesn't work
    ),
    # bs3 pull-right 
    # bs4 float-right
    class = "pull-right float-right"
  )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      shiny::p(subtitle)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-small icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}



html_webshot <- function(chart, vheight = 350, vwidth = 500) {
  html <- here::here("markdown/tempfig.html") 
  png <- here::here("markdown/tempfig.png")
  
  htmlwidgets::saveWidget(chart, html)
  webshot::webshot(url = html,
                    file = png,
                    zoom = 3,
                    vheight = vheight,
                    vwidth = vwidth)

  # read back in as PNG raster
  png_out <- 
    png %>% 
    png::readPNG(info = FALSE) %>% 
    grid::rasterGrob() %>% 
    gridExtra::arrangeGrob()
  
   grid::grid.draw(png_out)

  
}

gt_webshot <- function(gt_object){
  
    gt_object %>% 
    gt::gtsave(path = here::here("markdown"), 
           filename =  "tempfig.png" )
  
  # read back in as PNG raster
  png_out <- 
    here::here("markdown/tempfig.png") %>% 
    png::readPNG(info = FALSE) %>% 
    grid::rasterGrob() %>% 
    gridExtra::arrangeGrob()
  
  grid::grid.draw(png_out)

}




return_html_or_webshot <- function(html_object, report_format){
  
  if (report_format %in% c("pptx", "docx", "pdf")){
      html_webshot(html_object)
      dev.off()
    # no need to return anything. html_webshot prints automatically
    return(NULL)
  }
  
  if (report_format %in% c("html", "shiny")){
    return(html_object)
  }
  
  
}




