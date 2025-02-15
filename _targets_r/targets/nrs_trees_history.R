tar_target(nrs_trees_history, {
  # We're going to work one plot at a time
  # Fetch all trees that have ever been live on a plot
  stashed_plot <- NULL
  
  assemble_history <- function(plots) {
    # fetch_trees - fetch all of the trees that have ever been live on a plot
    all_trees <- fia_trees(fiadb, plots) |>
      filter(CONDID == 1) # only live trees
    
    # add NEXT_TRE_CN to make it easy to traverse tree lineage in either direction
    tree_prev_next <- all_trees |>
      left_join(
        all_trees |>
          select(CN, PREV_TRE_CN) |>
          rename(NEXT_TRE_CN = CN, CN = PREV_TRE_CN),
        by = join_by(CN)
      )
    
    # current trees is the set of trees currently being examined,
    # one for each history of an ingrown tree
    current_trees <- tree_prev_next |>
      semi_join(nrs_trees_ingrowth, by = join_by(CN))
    
    # head trees are the first trees in each history
    # of an ingrown tree
    head_trees <- current_trees |> filter(is.na(PREV_TRE_CN))
    
    # Function to fetch trees matching PREV_TRE_CN
    earlier_trees <- function(.data) {
      tree_prev_next |> semi_join(.data, by = join_by(CN == PREV_TRE_CN))
    }
    
    # Function to fetch trees matching NEXT_TRE_CN
    later_trees <- function(.data) {
      tree_prev_next |> semi_join(.data, by = join_by(CN == NEXT_TRE_CN))
    }
    
    # Advance current_trees
    while ((current_trees <- earlier_trees(current_trees)) |> nrow() > 0) {
      head_trees <- bind_rows(
        head_trees,
        current_trees |> filter(is.na(PREV_TRE_CN))
      )
    }
    
    # Head trees now contains the earliest trees for all ingrown trees.
    # Create a table of the entire history of each tree, of the form
    # CN, FIRST_CN, LAST_CN
    # We do this in three steps:
    # 1. CN, FIRST_CN for the head trees (CN == FIRST_CN)
    # 2. CN, FIRST_CN for trees where PREV_TRE_CN in CN
    # Repeat step 2 until nothing more matches
    # 3. Fill in LAST_CN for everything
    
    current_trees <- head_trees |> filter(!is.na(NEXT_TRE_CN))
    tree_history <- head_trees |> select(CN) |> mutate(FIRST_CN = CN, LAST_CN = CN)
    
    while ((current_trees <- later_trees(current_trees)) |> nrow() > 0) {
      current_cns <- current_trees |> select(CN, PREV_TRE_CN) |> rename(LAST_CN = CN)
      updated_history_rows <- tree_history |>
        left_join(current_cns, by = join_by(LAST_CN == PREV_TRE_CN)) |>
        mutate(LAST_CN = coalesce(LAST_CN.y, LAST_CN)) |>
        select(CN, FIRST_CN, LAST_CN)
      new_history_rows <- current_trees |>
        select(CN, PREV_TRE_CN) |>
        mutate(LAST_CN = CN) |>
        left_join(tree_history |> select(CN, FIRST_CN), by = join_by(PREV_TRE_CN == CN)) |>
        select(CN, FIRST_CN, LAST_CN)
      tree_history <- bind_rows(
        updated_history_rows,
        new_history_rows
      )
    }
  
    # Paste metadata on to history records to be friendly
    tree_history |>
      left_join(
        all_trees |> select(CN, STATECD, COUNTYCD, PLOT, SUBP, TREE, INVYR),
        by = join_by(CN)
      )
  }
  
  plot_ids <- nrs_plots_grown |>
    distinct(STATECD, COUNTYCD, PLOT)
  
  chunk_size <- 100
  
  bind_rows(
    pbapply::pblapply(
      plot_ids |> split(ceiling(1:nrow(plot_ids) / chunk_size)),
      assemble_history
    )
  )
})
