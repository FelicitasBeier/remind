# |  (C) 2006-2024 Potsdam Institute for Climate Impact Research (PIK)
# |  authors, and contributors see CITATION.cff file. This file is part
# |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
# |  AGPL-3.0, you are granted additional permissions described in the
# |  REMIND License Exception, version 1.0 (see LICENSE file).
# |  Contact: remind@pik-potsdam.de

############################# LOAD LIBRARIES #############################

library(dplyr,    quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2,  quietly = TRUE, warn.conflicts = FALSE)
# Functions from the following libraries are loaded using ::
# lucode2, tidyr, mip, quitte, lusweave, withr

############################# BASIC CONFIGURATION #############################

if (!exists("source_include") | !exists("runs") | !exists("folder")) {
  message("Script started from command line.")
  message("ls(): ", paste(ls(), collapse = ", "))
  runs <- if (exists("outputdirs")) unique(sub("-rem-[0-9]*", "", basename(outputdirs))) else NULL
  folder <- "./output"
  lucode2::readArgs("runs", "folder")
} else {
  message("Script was sourced.")
  message("runs  : ", paste(runs, collapse = ", "))
  message("folder: ", paste(folder, collapse = ", "))
}

############################# DEFINE FUNCTIONS ###########################

# Plot dimension specified for 'color' over dimension specified for 'xaxis' as line plot or bar plot
myplot <- function(dat, type = "line", xaxis = "ttot", color = "iteration", scales = "free_y", ylab = NULL, title = NULL) {
  
  # Zero values are not stored in the gdx and are this missing in dat.
  # Add '0' for the missing combinations of iteration, ttot, all_regi.
  dat <- tidyr::complete(dat, iteration, ttot, all_regi, fill = list("value" = 0))
  
  # convert dimension that should be distinguished by color to factors (relevant if years are plotted over iterations)
  dat[[color]] <- as.factor(dat[[color]]) 
  
  text_size <- 10
  scale_color <- as.character(mip::plotstyle(as.character(unique(dat[[color]])), out = "color"))

  p <- ggplot()
  if (type == "line") {
    p <- p + geom_line( mapping = aes(x=!!sym((xaxis)), y=value, color=!!sym(color), group = !!sym(color)), data = dat, linewidth = 1)
    p <- p + geom_point(mapping = aes(x=!!sym((xaxis)), y=value, color=!!sym(color), group = !!sym(color)), data = dat, size = 1)
  } else if (type == "bar"){
    p <- p + geom_col(  mapping = aes(x=!!sym((xaxis)), y=value, fill=!!sym(color),  group = !!sym(color)), data = dat)
  }
    p <- p + facet_wrap(~all_regi, scales=scales) +
    labs(x = NULL, y = ylab, title = title) +
    scale_color_manual(values=scale_color) +
    theme(
      plot.title   = element_text(size = text_size+4),
      strip.text.x = element_text(size = text_size),
      axis.text.y  = element_text(size = text_size),
      axis.title.x = element_text(size = text_size),
      axis.text.x  = element_text(size = text_size)) #+
    #theme_bw()
  return(p)
}

# The main function that compiles all plots
plot_iterations <- function(runname) {

  # ---- Settings ----
  gdxName <- file.path(runname, "fulldata.gdx")
  TWa2EJ <- 31.5576 # TWa to EJ (1 a = 365.25*24*3600 s = 31557600 s)
  sm_tdptwyr2dpgj <- 31.71 # convert [TerraDollar per TWyear] to [Dollar per GJoule]
  GtC_2_MtCO2 <- 44/12 * 1000
  magpieIter <- quitte::read.gdx(gdxName, "magpieIter") |> filter(iteration > 2)

  # ---- Plot: MAgPIE prices for purpose grown bioenergy ----

  var <- "Internal|Price|Biomass|MAgPIE (US$2017/GJ)"

  # modules/30_biomass/magpie/presolve.gms
  par <- "o_p30_pebiolc_pricemag"
  
  dat <- quitte::read.gdx(gdxName, par) |>
    mutate(value = value * sm_tdptwyr2dpgj) |>
    semi_join(magpieIter, by = join_by(iteration))
  
  p_price_mag <- myplot(dat, ylab = "$/GJ", title = paste(runname, var, par, sep = "\n"))

  # ---- Plot: MAgPIE co2luc ----

  # vm_emiMacSector <- readGDX(gdx, "vm_emiMacSector", field = "l", restore_zeros = FALSE)
  # dimSums(vm_emiMacSector[, , "co2luc"]                 , dim = 3) * GtC_2_MtCO2
  var <- "Emi|CO2|+|Land-Use Change (Mt CO2/yr)"

  # core/postsolve.gms:
  # o_vm_emiMacSector_co2luc(iteration,ttot,regi) = vm_emiMacSector.l(ttot,regi,"co2luc");
  
  par <- "o_vm_emiMacSector_co2luc"
  
  dat <- quitte::read.gdx(gdxName, par) |>
    mutate(value = value * GtC_2_MtCO2) |>
    semi_join(magpieIter, by = join_by(iteration))

  p_emi_mag <- myplot(dat, ylab = "Mt CO2/yr", title = paste(runname, var, par, sep = "\n"))


  # ---- Plot: REMIND Production of purpose grown bioenergy ----

  # remind2::reportExtraction.R
  # fuelex     <- readGDX(gdx, name = c("vm_fuExtr", "vm_fuelex"),      field = "l", restore_zeros = FALSE, format = "first_found")
  # fuelex_bio <- fuelex[, t, c("pebiolc", "pebios", "pebioil")]
  # dimSums(fuelex_bio[, , "pebiolc.1"], dim = 3) * TWa_2_EJ
  #   -> "PE|Production|Biomass|+|Lignocellulosic (EJ/yr)"
  #   -> "Primary Energy Production|Biomass|Energy Crops (EJ/yr)" (used also in coupling interface in MAgPIE)
  var <- "Primary Energy Production|Biomass|Energy Crops (EJ/yr)"

  # modules/30_biomass/magpie/postsolve.gms:
  # o_vm_fuExtr_pebiolc = vm_fuExtr.l(ttot,regi,"pebiolc","1");
  
  par <- "o_vm_fuExtr_pebiolc"
  
  dat <- quitte::read.gdx(gdxName, par) |>
    mutate(value = value * TWa2EJ) |>
    semi_join(magpieIter, by = join_by(iteration))
  
  title <- paste(runname, var, par, sep = "\n")

  p_fuelex         <- myplot(dat,                                      ylab = "EJ/yr", title = title)
  p_fuelex_it      <- myplot(dat, xaxis = "iteration", color = "ttot", ylab = "EJ/yr", title = title)
  p_fuelex_it_fix  <- myplot(dat, xaxis = "iteration", color = "ttot", ylab = "EJ/yr", title = title, scales = "fixed")
  p_fuelex_it_2060 <- myplot(dat |> filter(ttot == 2060), type = "bar", 
                                  xaxis = "iteration", color = "ttot", ylab = "EJ/yr", title = title, scales = "fixed")
  

  # ---- Plot: REMIND Demand for purpose grown bioenergy ----

  # remind2::reportPE.R
  # fuelex[,,"pebiolc.1"] + (1-p_costsPEtradeMp[,,"pebiolc"]) * Mport[,,"pebiolc"] - Xport[,,"pebiolc"] -> "PE|Biomass|+++|Energy Crops (EJ/yr)"

  var <- "PE|Biomass|+++|Energy Crops (EJ/yr)"
  
  # core/postsolve.gms:
  # o_PEDem_Bio_ECrops(iteration,ttot,regi) = vm_fuExtr.l(ttot,regi,"pebiolc","1") + (1 - pm_costsPEtradeMp(ttot,regi"pebiolc")) * vm_Mport.l(ttot,regi,"pebiolc") - vm_Xport.l(ttot,regi"pebiolc");
  
  par <- "o_PEDem_Bio_ECrops"
  
  dat <- quitte::read.gdx(gdxName, par) |>
    mutate(value = value * TWa2EJ) |>
    semi_join(magpieIter, by = join_by(iteration))

  title  <- paste(runname, var, par, sep = "\n")

  p_demPE    <- myplot(dat,                                      ylab = "EJ/yr", title = title)
  p_demPE_it <- myplot(dat, xaxis = "iteration", color = "ttot", ylab = "EJ/yr", title = title)

  # ---- Plot: REMIND Price shift factor ----

  # remind2::reportPrices.R
  # p30_pebiolc_pricshift -> "Internal|Price|Biomass|Shiftfactor ()"

  #var <- "Internal|Price|Biomass|Shiftfactor ()"
  # shift_p        <- readGDX(gdx, name = "p30_pebiolc_pricshift", format = "first_found")[, t, ]

  #p_shift <- myplot(reports[r, years, var], ylab = "$/GJ", title = paste(runname, var, sep="\n"))


  # ---- Plot: REMIND Price scaling factor ----

  # remind2::reportPrices.R
  # p30_pebiolc_pricmult -> "Internal|Price|Biomass|Multfactor ()"

  var <- "Internal|Price|Biomass|Multfactor ()"
  
  # mult_p         <- readGDX(gdx, name = "p30_pebiolc_pricmult", format = "first_found")[, t, ]
  # o_p30_pebiolc_pricmult
  
  par <- "o_p30_pebiolc_pricmult"

  dat <- quitte::read.gdx(gdxName, par) |>
    semi_join(magpieIter, by = join_by(iteration))
  
  p_mult <- myplot(dat, title = paste(runname, var, par, sep = "\n"))
  p_mult_it <- myplot(dat |> filter(ttot > 2000), xaxis = "iteration", color = "ttot", title = paste(runname, var, par, sep = "\n"))


  # ---- Plot: REMIND co2 price ----

  var <- "Price|Carbon (US$2017/t CO2)"
  # pm_taxCO2eqSum <- readGDX(gdx, name = "pm_taxCO2eqSum", format = "first_found")[, t, ]
  # pm_taxCO2eqSum * 1000 * 12 / 44
  
  # Alternative, that is already being tracked:
  # pm_taxCO2eq_iter(iteration,ttot,all_regi)
  
  par <- "pm_taxCO2eq_iter"
  
  dat <- quitte::read.gdx(gdxName, par) |>
    mutate(value = value * 1000 * 12 / 44) |>
    semi_join(magpieIter, by = join_by(iteration))
  
  title <- paste(runname, var, par, sep = "\n")

  p_price_carbon      <- myplot(dat, ylab = "$/tCO2", title = title)

  p_price_carbon_it_1 <- myplot(dat |> filter(ttot < 2025),
                                ylab = "$/tCO2", xaxis = "iteration", color = "ttot", title = title)
  p_price_carbon_it_2 <- myplot(dat |> filter(ttot > 2020, ttot <= 2100),
                                ylab = "$/tCO2", xaxis = "iteration", color = "ttot", title = title)

  # ---- Print to pdf ----

  out <- lusweave::swopen(template = "david")

  lusweave::swfigure(out, print, p_price_mag,         sw_option = "height=9,width=16")
  lusweave::swfigure(out, print, p_fuelex,            sw_option = "height=9,width=16")
  lusweave::swfigure(out, print, p_fuelex_it,         sw_option = "height=9,width=16")
  lusweave::swfigure(out, print, p_fuelex_it_fix,     sw_option = "height=9,width=16")
  lusweave::swfigure(out, print, p_fuelex_it_2060,    sw_option = "height=9,width=16")
  lusweave::swfigure(out, print, p_demPE_it,          sw_option = "height=9,width=16")
  lusweave::swfigure(out, print, p_emi_mag,           sw_option = "height=9,width=16")
  lusweave::swfigure(out, print, p_mult,              sw_option = "height=9,width=16")
  lusweave::swfigure(out, print, p_mult_it,           sw_option = "height=9,width=16")
  #lusweave::swfigure(out, print, p_shift,             sw_option = "height=9,width=16")
  lusweave::swfigure(out, print, p_price_carbon,      sw_option = "height=9,width=16")
  lusweave::swfigure(out, print, p_price_carbon_it_1, sw_option = "height=9,width=16")
  lusweave::swfigure(out, print, p_price_carbon_it_2, sw_option = "height=9,width=16")

  filename <- paste0(runname)
  lusweave::swclose(out, outfile = filename, clean_output = TRUE, save_stream = FALSE)
  file.remove(paste0(filename,c(".log",".out")))
  return("Done\n")
}

withr::with_dir(folder, {

  # ---- Search for runs if not provided----
  if (is.null(runs)) {
    message("\nNo run specified by user. Searching for all runs available in this folder:")
    # Find fulldata.gdx files of all runs
    runs <- Sys.glob("C_*/fulldata.gdx")
    # Remove everything but the scenario name from the folder names and remove duplicates
    runs <- unique(sub("/fulldata.gdx","",runs))
    message(paste(runs, collapse = ", "))
    message("")
  }

  # ---- Loop over runs ans plot ----
  for (runname in runs) {
    message("##################### ",runname," #################################")
    ret <- plot_iterations(runname)
    message(ret)
  }

})

