### ** note for KDP: "resume.R" version 08.07.20 ** ###

### ************************************
### PREFACE ----
### ************************************

# ** note for KDP: PREFACE version 0.1 ** #

# configure the script
# NOTE: my machine runs MacOS; paths have not been tested on other platforms
# NOTE: this information and more is stored in section I below

setwd("~/Desktop/") # *gasp*
date_ofrun <- "(December 10, 2019 (12-10-19)" # date script was run
name_scrpt <- "resume.R" # script filename
name_projd <- "resume" # name of project directory
path_projd <- paste(sep = "", getwd(), "/", name_projd) # path to project dir
path_vault <- paste(sep = "", path_projd, "/vault") # path to vault (storage)
PREFACE <- c("date_ofrun","name_scrpt", "name_projd", "path_projd", 
             "path_vault")

# *gasp*
setwd("~/Desktop/")

# paths for outputs stored in the central "/vault" (ofv = output file to vault)
ofv_COSMOS_wksp <- paste(sep = "", path_vault, "/WS_COSMOS.RData")
I.ofv_info <- paste(sep = "", path_vault, "/info_Section_I.txt")
I.ofv_wksp <- paste(sep = "", path_vault, "/WS_Section_I.RData")
T.ofv_plot_teaching <- paste(sep = "", path_vault, "/plot_teaching.pdf")
T.ofv_wksp <- paste(sep = "", path_vault, "/WS_Section_T.RData")
M.ofv_plot_mentorship <- paste(sep = "", path_vault, "/plot_mentorship.pdf")
M.ofv_wksp <- paste(sep = "", path_vault, "/WS_Section_M.RData")
S.ofv_plot_service <- paste(sep = "", path_vault, "/plot_service.pdf")
S.ofv_wksp <- paste(sep = "", path_vault, "/WS_Section_S.RData")
ofv_resume_wksp <- paste(sep = "", path_vault, "/WS_resume.RData")

# save PREFACE workspace
save.image(file = paste(sep = "", path_vault, "/WS_PREFACE.RData"))

# description of sections:
# PREFACE: configure the script
# UNIVERSAL OBJECTS ~ COSMOS: contains items used universally across sections
# SECTION I - Info: gathers path, machine, software, and package information
# SECTION T - Teaching: creates a barplot of teaching experience
# SECTION M - Mentorship: creates a lineplot of length for each mentorship
# SECTION S - Service: creates a piechart with categories of service
# EPILOGUE: save the final workspace

### ************************************
### UNIVERSAL OBJECTS ~ COSMOS ----
### ************************************

# hex codes for a gradient of black/grey/white (left to right = dark to light)
greydient <- c("#000000", "#252525", "#525252",
               "#969696", "#bbbbbb", "#d9d9d9", "#e0e0e0", 
               "#ffffff")

# font family
fnt <- "Courier"

# "#192e67" # Zissou navy
# "#2ca1db" # Zissou blue
# "#f21a00" # Zissou red
# "#fec72A" # Zissou yellow (variant)
# "#489e97" # Zissou teal (variant)
# "#969696" # Zissou grey

# create a vector naming all of the above (useful when saving workspaces)
cosmos <- c("greydient", "fnt")

# save universal objects workspace
COSMOS <- c(cosmos, "COSMOS")
save(list = COSMOS, file = ofv_COSMOS_wksp)

### ************************************
### I - MACHINE/PACKAGE/VERSION Info ----
### ************************************

# NOTE: this section requires objects from the PREFACE to be in the environment

# R packages accessed via namespace:
# benchmarkme

# R packages accessed via require:
require(ggplot2, quietly = T)
require(ggpubr, quietly = T)

# capture R package-related information:
I.Rpac_ctg <- "R package version"
I.Rpackge_a <- data.frame(info = "benchmarkme", section = "I", 
                          category = I.Rpac_ctg, script = name_scrpt, 
                          stringsAsFactors = F)
I.Rpackge_b <- data.frame(info = "dplyr", section = "I; T", 
                          category = I.Rpac_ctg, script = name_scrpt, 
                          stringsAsFactors = F)
I.Rpackge_c <- data.frame(info = "ggplot2", section = "T; M; S", 
                          category = I.Rpac_ctg, script = name_scrpt, 
                          stringsAsFactors = F)
I.Rpackge_d <- data.frame(info = "ggpubr", section = "T; M; S", 
                          category = I.Rpac_ctg, script = name_scrpt, 
                          stringsAsFactors = F)
I.Rpackge_0 <- rbind(I.Rpackge_a, I.Rpackge_b, I.Rpackge_c, I.Rpackge_d)

## *********************************** ## *********************************** ##
## *********************************** ## *********************************** ##

# ** note for KDP: captureVersions() version 0.1 ** #
# function takes an input data.frame with:
# column 'info' listing the package name and returns a data.frame with a ...
# ... new column 'value' listing the version information for the package
# NOTE: input column names are rigid; column 'info' must list package name
captureVersions <- function(data = data.frame) {
  # internal checks to ensure correct input classes
  if (!inherits(data, "data.frame")) {
    stop("input data must be class 'data.frame'")
  }
  # store original input df and format the new df
  new_dat <- data
  new_dat$value <- ""
  for(i in 1:length(new_dat$info)) {
    # create package name variable to be tested to ensure correct inputs/outputs
    pack <- unlist(
      packageDescription(
        new_dat$info[i], fields = c("Package", "Version"))[1])
    # test if package name in input data matches 'pack' variable
    # if TRUE, add package version value to the new data.frame
    if (pack == new_dat$info[i]) {
      new_dat$value[i] <- unlist(
        packageDescription(
          new_dat$info[i], fields = c("Package", "Version"))[2])
    }
    # if FALSE, print error message
    else {
      if (!pack == new_dat$info[i]) {
        stop("'pack' variable returned by packageDescription();
       does not match row value in column 'package' of input data;
       incorrect package version valuermation likely returned")
      }
    }
  }
  return(new_dat)
}
# 
# # example usage:
# new_dataframe <- captureVersions(data = dataframe)

## *********************************** ## *********************************** ##
## *********************************** ## *********************************** ##

# captureVersions() function
I.Rpackge_1 <- captureVersions(I.Rpackge_0)
I.Rpackge <- dplyr::select(I.Rpackge_1, category, info, value, script, section)

# capture Project-related information:
I.project_a <- data.frame(category = "project", 
                          info = "date script was run",
                          value = date_ofrun,
                          script = name_scrpt,
                          section = "all", stringsAsFactors = F)
I.project_b <- data.frame(category = "project", 
                          info = "name of project directory",
                          value = name_projd,
                          script = name_scrpt,
                          section = "all", stringsAsFactors = F)
I.project_0 <- rbind(I.project_a, I.project_b)
I.project <- dplyr::select(I.project_0, category, info, value, script, section)

# capture PATH-related information:
I.path_ctg <- "filepath"
I.path_sec <- "all"
I.pathsto_a <- data.frame(info = "working directory",
                          value = paste(getwd(), "/", sep = ""),
                          category = I.path_ctg, script = name_scrpt,
                          section = I.path_sec, stringsAsFactors = F)
I.pathsto_b <- data.frame(info = "path to project directory",
                          value = paste(path_projd, "/", sep = ""),
                          category = I.path_ctg, script = name_scrpt,
                          section = I.path_sec, stringsAsFactors = F)
I.pathsto_c <- data.frame(info = "path to project's central '/vault/'",
                          value = paste(path_vault, "/", sep = ""),
                          category = I.path_ctg, script = name_scrpt,
                          section = I.path_sec, stringsAsFactors = F)
I.pathsto_0 <- rbind(I.pathsto_a, I.pathsto_b, I.pathsto_c)
I.pathsto <- dplyr::select(I.pathsto_0, category, info, value, script, section)

# capture Machine-related information:
I.mach_ctg <- "machine"
I.mach_sec <- "all"
I.mach_cvr <- 1073741824 # number of bytes in 1GB (used for conversion of RAM)
I.machine_a <- data.frame(info = "OS",
                          value = sessionInfo()$running,
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_b <- data.frame(info = "processor",
                          value = benchmarkme::get_cpu()$model_name,
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_c <- data.frame(info = "number of cores",
                          value = parallel::detectCores(logical = F),
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_d <- data.frame(info = "number of threads",
                          value = parallel::detectCores(logical = T),
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_e <- data.frame(info = "RAM",
                          value = 
                            paste(as.numeric(benchmarkme::get_ram()) / I.mach_cvr,
                                  "GB", sep = ""),
                          category = I.mach_ctg, script = name_scrpt,
                          section = I.mach_sec, stringsAsFactors = F)
I.machine_0 <- rbind(I.machine_a, I.machine_b, I.machine_c, I.machine_d, 
                     I.machine_e)
I.machine <- dplyr::select(I.machine_0, category, info, value, script, section)

# capture R-related information:
I.rlan_ctg <- "base R"
I.rlan_sec <- "all"
I.baseRpl_a <- data.frame(info = "version",
                          value = R.Version()$version.string,
                          category = I.rlan_ctg, script = name_scrpt,
                          section = I.rlan_sec, stringsAsFactors = F)
I.baseRpl_b <- data.frame(info = "nickname",
                          value = R.Version()$nickname,
                          category = I.rlan_ctg, script = name_scrpt,
                          section = I.rlan_sec, stringsAsFactors = F)
I.baseRpl_c <- data.frame(info = "platform",
                          value = R.Version()$platform,
                          category = I.rlan_ctg, script = name_scrpt,
                          section = I.rlan_sec, stringsAsFactors = F)
I.baseRpl_0 <- rbind(I.baseRpl_a, I.baseRpl_b, I.baseRpl_c)
I.baseRpl <- dplyr::select(I.baseRpl_0, category, info, value, script, section)

# capture RStudio-related information:
I.RStudio <- data.frame(info = "version",
                        value = as.character(RStudio.Version()$version),
                        category = "R Studio", script = name_scrpt,
                        section = "all", stringsAsFactors = F)

# rbind all of the above data.frames together and write outputs
I.info <- rbind(I.project, I.machine, I.pathsto, I.baseRpl, I.RStudio, I.Rpackge)

# outputs to the vault
write.table(sep = "\t", row.names = F, x = I.info, file = I.ofv_info)

I.obj <- c(ls(pattern = "I."), PREFACE, "captureVersions")
save(list = I.obj, file = I.ofv_wksp)

### ************************************
### T - Step 1: create the data ----
### ************************************

# Pittsburg State University
T.PSU <- data.frame("course" = c("BIOL371", "BIOL571"), 
                    "description" = c("General Microbiology",
                                      "Pathogenic Bacteriology"),
                    "position" = "UTA",
                    "group" = "a",
                    semesters = c(2, 1), stringsAsFactors = F)

# University of Wyoming
T.WYO <- data.frame("course" = c("MOLB1101", "MICR2021_a", "MICR2021_b"), 
                    "description" = c("The Neanderthal and the Nucleus", 
                                      "General Microbiology", 
                                      "General Microbiology"),
                    "position" = c("GTA", "GTA", "Guest Lecturer"),
                    "group" = c("b", "b", "c"),
                    semesters = c(1, 2, 7), stringsAsFactors = F)

# Colorado State University
T.CSU <- data.frame("course" = "ERHS569", 
                    "description" = "Immunotoxicology",
                    "position" = "Guest Lecturer",
                    "group" = "c",
                    semesters = 1, stringsAsFactors = F)

# Front Range Community College
T.FRC <- data.frame("course" = "BIO204", 
                    "description" = "Microbiology",
                    "position" = "Instructor of Record",
                    "group" = "d",
                    semesters = 2, stringsAsFactors = F)

T.tch <- rbind(T.PSU, T.WYO, T.CSU, T.FRC)

### ************************************
### T - Step 2: define plot parameters/aesethics ----
### ************************************

# bars: fill parameters
T.hex_UTA <- "#f21a00"
T.hex_GTA <- "#67bbe5"
T.hex_lec <- "#142962"
T.hex_Ins <- "#fec72A"
T.hex_typ <- c(T.hex_UTA, T.hex_GTA, T.hex_lec, T.hex_Ins)

# points: color, fill, and shape parameters
T.shp_UTA <- 21
T.shp_GTA <- 13
T.shp_lec <- 21
T.shp_Ins <- 15

T.tch_shp <- T.tch
T.tch_shp$PtsHex[T.tch_shp$group == "a"] <- T.hex_UTA
T.tch_shp$PtsHex[T.tch_shp$group == "b"] <- T.hex_GTA
T.tch_shp$PtsHex[T.tch_shp$group == "c"] <- T.hex_lec
T.tch_shp$PtsHex[T.tch_shp$group == "d"] <- T.hex_Ins

T.tch_shp$PtsFil[T.tch_shp$group == "a"] <- greydient[8]
T.tch_shp$PtsFil[T.tch_shp$group == "b"] <- T.hex_GTA
T.tch_shp$PtsFil[T.tch_shp$group == "c"] <- T.hex_lec
T.tch_shp$PtsFil[T.tch_shp$group == "d"] <- T.hex_Ins

T.tch_shp$PtsShp[T.tch_shp$group == "a"] <- T.shp_UTA
T.tch_shp$PtsShp[T.tch_shp$group == "b"] <- T.shp_GTA
T.tch_shp$PtsShp[T.tch_shp$group == "c"] <- T.shp_lec
T.tch_shp$PtsShp[T.tch_shp$group == "d"] <- T.shp_Ins

T.ypts <- -0.3

# axis parameters
T.xord <- c("BIOL371", "BIOL571", "MOLB1101", "MICR2021_a", "MICR2021_b",
            "ERHS569", "BIO204")
T.xlab <- c("BIOL 371", "BIOL 571", "MOLB 1101", "MICR 2021", "MICR 2021",
            "ERHS 569", "BIO 204\n5 sections")
T.xexp <- c(0, 0.4)
T.ylim <- c(-0.63, 8)
T.ybrk <- c(0, 1, 2, 3, 4, 5, 6, 7)
T.ylab <- c("0", "1", "2", "3", "4", "5", "6", "7")
T.yttl <- "number of semesters"
T.yexp <- c(0, 0)

# sizing parameters
T.sze_bar <- 0.42 # size for widths of bars
T.sze_shp <- 1.42 # size for shapes
T.sze_stk <- 0.55 # size for shape stroke
T.sze_vln <- 0.42 # size for vertical dotted lines separating universities
T.sze_uxt <- 3.33 # size for university label text
T.sze_atly <- 8 # size for title on y axis (before orientation flip)
T.sze_atty <- 7 # size for number scale on y axis (before orientation flip)
T.sze_attx <- 8 # size for course labels on x axis (before orientation flip)
T.sze_ltx <- 7 # size for legend labels

# labels for each university
T.ulab <- data.frame(y = T.ylim[2] - 0.36, stringsAsFactors = F,
                     "label" = c("PSU", "UW", "CSU", "FRCC"),
                     x = c(7.13, 5.2, 2.2, 1.2))

### ************************************
### T - Step 3: create the plot ----
### ************************************

T.gpr <- ggbarplot(data = T.tch, x = "course", y = "semesters", 
                   order = rev(T.xord), fill = "group", color = greydient[1],
                   ylab = T.yttl, font.family = fnt, orientation = "horizontal",
                   width = T.sze_bar) +
  geom_point(inherit.aes = F, data = T.tch_shp, aes(x = course), y = T.ypts, 
             size = T.sze_shp, stroke = T.sze_stk, color = T.tch_shp$PtsHex,
             fill = T.tch_shp$PtsFil, shape = T.tch_shp$PtsShp) +
  geom_vline(xintercept = c(1.5, 2.5, 5.5), linetype = "dotted", 
             color = greydient[5], size = T.sze_vln) +
  geom_text(inherit.aes = F, data = T.ulab, size = T.sze_uxt,
            aes(x = x, y = y, label = label), color = greydient[1], 
            family = fnt, hjust = 0.9) +
  scale_fill_manual(values = T.hex_typ) +
  scale_x_discrete(labels = rev(T.xlab), expand = T.xexp) +
  scale_y_continuous(limits = T.ylim, breaks = T.ybrk, labels = T.ylab, 
                     expand = T.yexp) +
  border(color = greydient[1]) +
  theme(text = element_text(family = fnt, color = greydient[1]),
        line = element_line(lineend = "square", color = greydient[1]),
        axis.title.x = element_text(size = T.sze_atly),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = T.sze_atty),
        axis.text.y = element_text(size = T.sze_attx),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.ontop = T,
        panel.background = element_rect(fill = NA))

### ************************************
### T - Step 4: create a custom legend ----
### ************************************

# create a data.frame for the legend using the shapes data.frame ...
# ... by remove unnecessary information and adding x and y values for plotting
T.tch_lgn_0 <- dplyr::select(T.tch_shp, group, PtsShp, PtsHex, PtsFil)
T.tch_lgn_1 <- dplyr::distinct(T.tch_lgn_0, group, .keep_all = T)
T.tch_lgn <- T.tch_lgn_1
T.tch_lgn$x <- 0
T.tch_lgn$y <- 0

# legend parameters
T.lgn_brk <- c("a", "b", "c", "d")
T.lgn_lab <- c("UTA", "GTA", "Guest Lecturer", "Instructor of Record")
T.lgn_shp <- T.tch_lgn$PtsShp
T.lgn_hex <- T.tch_lgn$PtsHex
T.lgn_fil <- T.tch_lgn$PtsFil

# plot
T.ggp_lgn <- ggplot(data = T.tch_lgn, aes(x = x, y = y)) +
  geom_point(aes(shape = group, color = group, fill = group),
             size = T.sze_shp, stroke = T.sze_stk) +
  scale_shape_manual(name = NULL, breaks = T.lgn_brk, labels = T.lgn_lab,
                     values = T.lgn_shp) +
  scale_color_manual(name = NULL, breaks = T.lgn_brk, labels = T.lgn_lab,
                     values = T.lgn_hex) +
  scale_fill_manual(name = NULL, breaks = T.lgn_brk, labels = T.lgn_lab,
                    values = T.lgn_fil) +
  theme_void(base_family = fnt) +
  theme(legend.position = c(0.5, 0.5),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = NULL),
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.size = unit(0, "mm"),
        legend.box.margin = margin(1, 0, -1, 10.25, "mm"),
        legend.text = element_text(margin = margin(0.5, 1, 0.5, -1, "mm"),
                                   size = T.sze_ltx, color = greydient[1],
                                   hjust = 0)) +
  scale_x_continuous(limits = c(0, 2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) +
  guides(shape = guide_legend(nrow = 2, ncol = 2))

# extract legend and convert from class 'gtable' to 'ggplot'
T.gpr_lgn_0 <- get_legend(T.ggp_lgn)
T.gpr_lgn <- as_ggplot(T.gpr_lgn_0)

### ************************************
### T - Step 5: arrange plot with custom legend ----
### ************************************

T.gga <- ggarrange(T.gpr_lgn, T.gpr, labels = NULL, ncol = 1, nrow = 2, 
                   heights = c(1, 9))

### ************************************
### T - WRITE OUTPUTS ----
### ************************************

# outputs to the vault
ggsave(device = "pdf", dpi = 600, units = "mm", width = 75, height = 75,
       filename = T.ofv_plot_teaching, plot = T.gga)

T.obj <- c(ls(pattern = "T."), PREFACE, COSMOS)
save(list = T.obj, file = T.ofv_wksp)

### ************************************
### M - Step 1: create the data ----
### ************************************

# data.frames for individual mentees
M.mala <- data.frame(stringsAsFactors = F, "mentee" = "Malachi", 
                     y = 01, xbeg = 01, xend = 45, "group" = "c")
M.kyle  <- data.frame(stringsAsFactors = F, "mentee" = "Kyle", 
                      y = 02, xbeg = 06, xend = 13, "group" = "a")
M.reid  <- data.frame(stringsAsFactors = F, "mentee" = "Reid", 
                      y = 03, xbeg = 14, xend = 23, "group" = "a")
M.bail  <- data.frame(stringsAsFactors = F, "mentee" = "Bailey", 
                      y = 04, xbeg = 18, xend = 54, "group" = "a")
M.mjwe  <- data.frame(stringsAsFactors = F, "mentee" = "MJ", 
                      y = 05, xbeg = 32, xend = 47, "group" = "a")
M.Si2F  <- data.frame(stringsAsFactors = F, "mentee" = "Si2Fellows", 
                      y = 06, xbeg = 36, xend = 36, "group" = "c")
M.miTA  <- data.frame(stringsAsFactors = F, "mentee" = "MicroTAs", 
                      y = 07, xbeg = 38, xend = 47, "group" = "c")
M.marc  <- data.frame(stringsAsFactors = F, "mentee" = "Marcus", 
                      y = 08, xbeg = 39, xend = 51, "group" = "a")
M.melB  <- data.frame(stringsAsFactors = F, "mentee" = "Mel", 
                      y = 09, xbeg = 60, xend = 64, "group" = "b")

# data.frame with the above combined
M.ment <- rbind(M.mala, M.kyle, M.reid, M.bail, M.mjwe, M.Si2F, M.miTA, 
                M.marc, M.melB)

# data.frame to plot year labels
M.year <- data.frame("x" = c(12.5, 24.5, 36.5, 48.5, 60.5),
                     "label" = c("2015", "2016", "2017", "2018", "2019"), 
                     stringsAsFactors = F)

M.yint_hln <- 0.10 # y for horizontal line above year labels
M.yyxt <- -0.55 # y for year labels

### ************************************
### M - Step 2: define plot parameters/aesethics ----
### ************************************

# color and shape parameters
M.hex_undg <- "#489e97"
M.hex_grad <- "#969696"
M.hex_othr <- "#fec72A"
M.hex_grp <- c(M.hex_undg, M.hex_grad, M.hex_othr)
M.fil_grp <- c(M.hex_undg, M.hex_grad, M.hex_othr)
M.shp_grp <- c(22, 23, 21)

# axis parameters
M.xlim <- c(0, 65)
M.xexp <- c(0, 0.5)
M.ylim <- c(-0.9, 9.6)
M.ybrk <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
M.ylab <- c("Malachi", "Kyle", "Reid", "Bailey", "MJ", "Si2 Fellows", 
            "Micro TAs", "Marcus", "Mel")
M.yexp <- c(0, 0.3)

# legend parameters
M.lgn_brk <- c("a", "b", "c")
M.lgn_lab <- c("Undergraduate", "Graduate", "Other")

# sizing parameters
M.sze_vln <- 0.42 # size for vertical dotted lines separating years
M.sze_hln <- 0.42 # size for horizontal line above year labels
M.sze_yxt <- 2.88 # size for year label text
M.sze_axt <- 8 # size for mentee labels on y axis
M.sze_ltx <- 7 # size for legend labels
M.sze_shp <- 1.95 # size for mentee shapes

### ************************************
### M - Step 3: create the plot ----
### ************************************

# create plot; order of layers:
# geom_vline() = vertical dotted lines separating years
# geom_hline() = horizontal line above year labels
# x3 geom_text() = year labels
# geom_segment() = segments connecting start and end points
# geom_point() = start point
# geom_point() = end point

M.ggp <- ggplot(data = M.ment) +
  geom_vline(xintercept = c(06, 18, 30, 42, 54), linetype = "dotted", 
             color = greydient[6], size = M.sze_vln) +
  geom_hline(yintercept = M.yint_hln, color = greydient[1], size = M.sze_hln) +
  geom_text(inherit.aes = F, data = M.year, aes(x = x, label = label), 
            y = M.yyxt, family = fnt, size = M.sze_yxt, color = greydient[1]) +
  geom_text(inherit.aes = F, data = M.year, aes(x = x, label = label), 
            y = M.yyxt, family = fnt, size = M.sze_yxt, color = greydient[1]) +
  geom_text(inherit.aes = F, data = M.year, aes(x = x, label = label), 
            y = M.yyxt, family = fnt, size = M.sze_yxt, color = greydient[1]) +
  geom_segment(aes(x = xbeg, xend = xend, y = y, yend = y, color = group),
               linetype = "solid", show.legend = F) +
  geom_point(aes(x = xbeg, y = y, shape = group, color = group, fill = group),
             size = M.sze_shp, stroke = 0) +
  geom_point(aes(x = xend, y = y, shape = group, color = group, fill = group),
             size = M.sze_shp, stroke = 0) +
  scale_color_manual(values = M.hex_grp, breaks = M.lgn_brk, labels = M.lgn_lab, 
                     name = NULL) +
  scale_shape_manual(values = M.shp_grp, breaks = M.lgn_brk, labels = M.lgn_lab, 
                     name = NULL) +
  scale_fill_manual(values = M.fil_grp, breaks = M.lgn_brk, labels = M.lgn_lab, 
                    name = NULL) +
  theme_pubr() +
  border(color = greydient[1]) +
  theme(text = element_text(family = fnt, color = greydient[1]),
        line = element_line(lineend = "square", color = greydient[1]),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = M.sze_axt),
        legend.background = element_rect(fill = greydient[8]),
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.size = unit(2.51, "mm"),
        legend.box.margin = margin(-2, 0, -4, 0, "mm"),
        legend.text = element_text(margin = margin(0, 0.5, 0, -1.5, "mm"), 
                                   size = M.sze_ltx, color = greydient[1],
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.ontop = T,
        panel.background = element_rect(fill = NA)) +
  scale_x_continuous(limits = M.xlim, expand = M.xexp) +
  scale_y_continuous(limits = M.ylim, breaks = M.ybrk, labels = M.ylab, 
                     expand = M.yexp)

### ************************************
### M - WRITE OUTPUTS ----
### ************************************

# outputs to the vault
ggsave(device = "pdf", dpi = 600, units = "mm", width = 125, height = 49,
       filename = M.ofv_plot_mentorship, plot = M.ggp)

M.obj <- c(ls(pattern = "M."), PREFACE, COSMOS)
save(list = M.obj, file = M.ofv_wksp)

### ************************************
### S - Step 1: do it all ----
### ************************************

# create the data
S.serv_0 <- data.frame("num" = c(1, 6, 3, 2), 
                       "org" = c("CSU", "WY", "UW", "CO"), stringsAsFactors = F)


# define plot parameters/aesethics
S.lvls <- c("CSU", "WY", "UW", "CO")
S.serv_1 <- S.serv_0
S.serv_1$org <- factor(S.serv_1$org, levels = S.lvls)
S.hec_CSU <- "#489e97" # Zissou teal (variant)
S.hex_WY <- "#f21a00" # Zissou red
S.hex_UW <- "#fec72A" # Zissou yellow (variant)
S.hex_CO <- "#192e67" # Zissou navy
S.fil_org <- c(S.hec_CSU, S.hex_WY, S.hex_UW, S.hex_CO)
S.pie_lab <- rep("", times = 4)
S.sze_lne <- 1.95

# create the plot
S.gpr <- ggpie(data = S.serv_1, x = "num", label = S.pie_lab, font.family = fnt,
               fill = "org", color = greydient[8], size = S.sze_lne) +
  scale_fill_manual(values = S.fil_org) +
  theme(text = element_text(family = fnt, color = greydient[1]),
        legend.position = "none")

### ************************************
### S - WRITE OUTPUTS ----
### ************************************

# outputs to the vault
ggsave(device = "pdf", dpi = 600, units = "mm", width = 81, height = 81,
       filename = S.ofv_plot_service, plot = S.gpr)

S.obj <- c(ls(pattern = "S."), PREFACE, COSMOS)
save(list = S.obj, file = S.ofv_wksp)

### ************************************
### EPILOGUE ----
### ************************************

# output to the vault
save.image(file = ofv_resume_wksp)
