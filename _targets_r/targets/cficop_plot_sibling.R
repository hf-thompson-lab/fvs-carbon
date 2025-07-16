tar_target(cficop_plot_sibling, {
  tmp_trees <- qryDWSPCFIPlotVisitTreeDetail |>
    cfi_with_visit_info(tblDWSPCFIPlotVisitsComplete) |>
    cfi_with_tree_info(tblDWSPCFITreesComplete) |>
    cfi_with_plot_info(tblDWSPCFIPlotsComplete) |>
    cfi_abp(cfiabp_trees) |>
    filter(VisitCycle == 1970) |>
    filter(cfi_status_live(VisitTreeStatusCode)) |> # Only Live Trees
    left_join(
      species_crosswalk |> select(SPCD, SCIENTIFIC_NAME),
      by = join_by(SpeciesCode == SPCD)
    ) |>
    mutate(
      TPA = 5,
      BA = VisitTreeDIAM
    )
  
  tmp_by_plot <- tmp_trees |>
    group_by(MasterPlotID) |>
    summarize(
      TPA_Plot = sum(TPA, na.rm = TRUE),
      BA_Plot = sum(BA, na.rm = TRUE),
      .groups = "drop"
    ) |>
    ungroup() |>
    select(MasterPlotID, TPA_Plot, BA_Plot)
  
  tmp_top_species <- c(
    "Acer rubrum", "Quercus rubra", "Quercus velutina", "Pinus strobus"
  )
  
  tmp_plot_fingerprint <- tmp_trees |>
    group_by(MasterPlotID, SCIENTIFIC_NAME) |>
    summarize(
      TPA_Species = sum(TPA, na.rm = TRUE),
      BA_Species = sum(BA, na.rm = TRUE),
      .groups = "keep"
    ) |>
    ungroup() |>
    left_join(tmp_by_plot, by = join_by(MasterPlotID)) |>
    mutate(
      TPA_Frac = TPA_Species / TPA_Plot,
      BA_Frac = BA_Species / BA_Plot
    ) |>
    select(-ends_with("_Species")) |>
  #  filter(BA_Frac > 0.1) |>
    filter(SCIENTIFIC_NAME %in% tmp_top_species) |>
    group_by(MasterPlotID) |>
    mutate(
      Species = do.call(
        paste,
        append(sort(SCIENTIFIC_NAME), list(sep = ", "))
      )
    ) |>
    ungroup() |>
    mutate(SCIENTIFIC_NAME = gsub(" ", "_", SCIENTIFIC_NAME)) |>
    pivot_wider(names_from = "SCIENTIFIC_NAME", values_from = c("TPA_Frac", "BA_Frac"))
  
  tmp_plots_grow_only <- cfigro_plot_harvested |>
    filter(!CFIHarvested) |>
    inner_join(
      cfigro_plot_disturbed |> filter(!CFIDisturbed),
      by = join_by(MasterPlotID)
    ) |>
    select(MasterPlotID)
  
  most_similar_plot <- function(target_plots) {
    target_plots <- target_plots |>
      # Penalize things that differ on whether they have
      # a species on the landscape
      replace_na(list(
        BA_Frac_Pinus_strobus = -1,
        BA_Frac_Acer_rubrum = -1,
        BA_Frac_Quercus_rubra = -1,
        BA_Frac_Quercus_velutina = -1
      ))
  
    lapply(1:nrow(target_plots), \(n) {
      # Minimize RMS difference in BA
      ba_plot <- target_plots[[n, "BA_Plot"]]
      ba_pist <- target_plots[[n, "BA_Frac_Pinus_strobus"]]
      ba_acru <- target_plots[[n, "BA_Frac_Acer_rubrum"]]
      ba_quru <- target_plots[[n, "BA_Frac_Quercus_rubra"]]
      ba_quve <- target_plots[[n, "BA_Frac_Quercus_velutina"]]
      tmp_plot_fingerprint |>
        inner_join(tmp_plots_grow_only, by = join_by(MasterPlotID)) |>
        replace_na(list(
          BA_Frac_Pinus_strobus = -1,
          BA_Frac_Acer_rubrum = -1,
          BA_Frac_Quercus_rubra = -1,
          BA_Frac_Quercus_velutina = -1
        )) |>
        mutate(
          BA_RMSE = sqrt(
            ((BA_Plot - ba_plot) / (BA_Plot + ba_plot))^2 +
              (BA_Frac_Pinus_strobus - ba_pist)^2 +
              (BA_Frac_Acer_rubrum - ba_acru)^2 +
              (BA_Frac_Quercus_rubra - ba_quru)^2 +
              (BA_Frac_Quercus_velutina - ba_quve)^2
          )
        ) |>
        arrange(BA_RMSE) |>
        head(1) |>
        pull(MasterPlotID)    
    }) |>
      unlist()
  }
  
  tmp_plot_fingerprint |>
    mutate(sibling_plot = most_similar_plot(tmp_plot_fingerprint)) |>
    select(MasterPlotID, sibling_plot)
})
