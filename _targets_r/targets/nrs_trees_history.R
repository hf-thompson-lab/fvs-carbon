tar_target(nrs_trees_history, {
  # Get trees with CN, PREV_TRE_CN and  NEXT_TRE_CN to make it easy to traverse
  # tree lineage in either direction
  tree_prev_next <- nrs_trees_grown |>
    select(CN, PREV_TRE_CN) |>
    left_join(
      nrs_trees_grown |>
        select(CN, PREV_TRE_CN) |>
        rename(NEXT_TRE_CN = CN, CN = PREV_TRE_CN),
      by = join_by(CN)
    ) |>
    # Null out PREV_TRE_CN if it points to a tree we don't have access to.
    left_join(
      nrs_trees_grown |> select(CN) |> mutate(PREV_TRE_EXISTS = TRUE),
      by = join_by(PREV_TRE_CN == CN)
    ) |>
    mutate(
      PREV_TRE_CN = if_else(!is.na(PREV_TRE_EXISTS), PREV_TRE_CN, NA)
    ) |>
    select(!PREV_TRE_EXISTS)
    # Don't need to worry about NEXT_TRE_CN, since that join won't have
    # lined up and left dangling references.
  
  # We've limited how far back we look in tree history, but have history
  # all the way to the present. So we'll start with the most recent trees
  # and stitch together history into the past.
  
  # current trees is the set of trees currently being examined,
  # one for each history of a tree
  current_trees <- tree_prev_next |>
    filter(is.na(NEXT_TRE_CN))
    
  # head trees are the first trees in each history
  # We identify tree history by the earliest tree in that line,
  # since that will never change.
  head_trees <- current_trees |> filter(is.na(PREV_TRE_CN))
    
  # Function to fetch trees matching PREV_TRE_CN
  # This is used to walk earlier in time
  earlier_trees <- function(.data) {
    tree_prev_next |> semi_join(.data, by = join_by(CN == PREV_TRE_CN))
  }
  
  # Function to fetch trees matching NEXT_TRE_CN
  # This is used to walk later in time
  later_trees <- function(.data) {
    tree_prev_next |> semi_join(.data, by = join_by(CN == NEXT_TRE_CN))
  }
  
  # Walk back to the beginning of time to find all the head trees
  while ((current_trees <- earlier_trees(current_trees)) |> nrow() > 0) {
    head_trees <- bind_rows(
      head_trees,
      current_trees |> filter(is.na(PREV_TRE_CN))
    )
  }
  
  # Head trees now contains the earliest trees for all tree histories.
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
  
  # tree_history should have a record for every live tree.
  stopifnot(nrs_trees_grown |> anti_join(tree_history, by = join_by(CN)) |> nrow() == 0)
  
  # Paste metadata on to history records to be friendly
  nrs_trees_history <- tree_history |>
    left_join(
      nrs_trees_grown |>
        select(CN, STATECD, COUNTYCD, PLOT, SUBP, TREE, INVYR, SPCD, DIA, HT),
      by = join_by(CN)
    ) |>
    left_join(
      nrs_plots_grown |>
        select(STATECD, COUNTYCD, PLOT, INVYR, MEASYEAR),
      by = join_by(STATECD, COUNTYCD, PLOT, INVYR)
    ) |>
    # Add a flag to indicate that a particular tree is marked as ingrowth in a
    # particular year
    left_join(
      nrs_grm_ingrowth |> select(TRE_CN) |> rename(CN = TRE_CN) |> mutate(ESTAB = TRUE),
      by = join_by(CN)
    ) |>
    mutate(
      ESTAB = if_else(!is.na(ESTAB), ESTAB, FALSE)
    )
})
