### ************************************
### PREFACE ----
### ************************************

# *gasp*
setwd("~/Desktop/")

# relative paths for outputs headed for storage in the "vault/"
#ofv_WS <- "TransFaunation/vault/WS_Section_A.RData"

require(ggplot2, quietly = T)
require(ggpubr, quietly = T)

### ************************************
### UNIVERSAL OBJECTS ----
### ************************************

# hex codes for a gradient of black/grey/white (left to right = dark to light)
greydient <- c("#000000", "#252525", "#525252",
               "#969696", "#bbbbbb", "#d9d9d9", "#e0e0e0", 
               "#ffffff")

# font family
fnt <- "Courier"

### ************************************
### SECTION T - Teaching experience ----
### ************************************

# Pittsburg State University
T.dat_PSU <- data.frame("course" = c("BIOL\n571", "BIOL\n371"), 
                        "description" = c("Pathogenic Bacteriology",
                                          "General Microbiology"),
                        "position" = "UTA",
                        "semesters" = c(1, 2),
                        "order" = c(1, 2), stringsAsFactors = F)

# University of Wyoming
T.dat_WYO <- data.frame("course" = c("MOLB\n1101", "MICR\n2021", "MICR\n2021"), 
                        "description" = c("The Neanderthal and the Nucleus", 
                                          "General Microbiology", 
                                          "General Microbiology"),
                        "position" = c("GTA", "GTA", "Lect."),
                        "semesters" = c(1, 2, 7),
                        "order" = c(3, 4, 5), stringsAsFactors = F)

# Colorado State University
T.dat_CSU <- data.frame("course" = "ERHS\n569", 
                        "description" = "Immunotoxicology",
                        "position" = "Lect.",
                        "semesters" = 1,
                        "order" = 6, stringsAsFactors = F)

T.dat <- rbind(T.dat_PSU, T.dat_WYO, T.dat_CSU)

# custom plot parameters & aesthetics
T.ylab <- "number of semesters"
T.ylim <- c(0, 8)
T.sze_wid_bar <- 0.42

# School-specific colors
# T.hex_PSU_red <- "#D1202F"
# T.hex_PSU_ylw <- "#FDCF07"
# T.hex_WYO_bwn <- "#4A3024"
# T.hex_WYO_ylw <- "#FEC72A"
# T.hex_CSU_grn <- "#1F4C2C"
# T.hex_CSU_ylw <- "#C5B94D"
# T.hex_vec1 <- c(T.hex_PSU_ylw, T.hex_PSU_red, 
#                "#ffffff", T.hex_WYO_ylw,T.hex_WYO_bwn, 
#                T.hex_CSU_grn)
# T.hex_vec2 <- c(T.hex_PSU_ylw, T.hex_PSU_red, 
#                 "#000000", T.hex_WYO_ylw, T.hex_WYO_bwn, 
#                 T.hex_CSU_grn)
# limited colors - still too busy
# T.hex_PSU_drk <- "#800e00"
# T.hex_PSU_lyt <- "#f21a00"
# T.hex_WYO_drk <- "#fec72A"
# T.hex_WYO_lyt <- "#fede80"
# T.hex_CSU_drk <- "#192e67"
# T.hex_CSU_lyt <- "#2ca1db"
# T.hex_vec1 <- c(T.hex_PSU_lyt, T.hex_PSU_drk, 
#                 "#ffffff", T.hex_WYO_lyt,T.hex_WYO_drk, 
#                 T.hex_CSU_drk)

# Team Zissou by teaching type (UTA / GTA / Lecturer)
# order = UTA x2 - GTA x 2 - Lecturer x2

# T.hex_UTA <- "#f21a00"
# T.hex_GTA <- "#2ca1db"
# T.hex_LEC <- "#192e67" 

T.hex_UTA <- "#f21a00" # "#ff321a"
T.hex_GTA <- "#67bbe5"
T.hex_LEC <- "#142962" 



T.hex_vec1 <- c(T.hex_UTA, T.hex_UTA, 
                T.hex_GTA, T.hex_GTA, 
                T.hex_LEC, T.hex_LEC)

T.scl_uni <- 0.16
T.scl_txt <- 0.55
T.sze_uni <- 3.33
T.sze_txt <- 2.22
T.lne_hgt <- 0.88
T.sze_axs_txt_x <- 07
T.sze_axs_txt_y <- 09
T.sze_axs_ttl <- 09
T.sze_int_lne <- 0.5

T.uni_labs <- data.frame("label" = c("PSU", "UW", "CSU"), x = c(1.5, 4, 6),
                         y = T.ylim[2] - T.scl_uni, stringsAsFactors = F)

# barplot
T.gpr <- ggbarplot(data = T.dat, font.family = fnt,
                   x = "order", y = "semesters", 
                   fill = "order", color = greydient[1],
                   ylab = T.ylab, ylim = T.ylim, width = T.sze_wid_bar) +
  geom_text(inherit.aes = F, data = T.uni_labs, size = T.sze_uni,
            aes(x = x, y = y, label = label), color = greydient[8], 
            family = fnt) +
  geom_vline(xintercept = c(2.5, 5.5), linetype = "dotted", 
             color = greydient[5], size = T.sze_int_lne) +
  geom_text(aes(x = order, y = semesters + T.scl_txt, label = course), 
            family = fnt, color = greydient[8], size = T.sze_txt, 
            lineheight = T.lne_hgt) +
  geom_text(aes(x = order, y = semesters + T.scl_txt, label = course), 
            family = fnt, color = greydient[8], size = T.sze_txt, 
            lineheight = T.lne_hgt) +
  geom_text(aes(x = order, y = semesters + T.scl_txt, label = course), 
            family = fnt, color = greydient[8], size = T.sze_txt, 
            lineheight = T.lne_hgt) +
  scale_x_discrete(labels = T.dat$position) +
  scale_fill_manual(values = T.hex_vec1) +
  border(color = greydient[1]) +
  theme(text = element_text(family = fnt, color = greydient[8]),
        line = element_line(lineend = "square", color = greydient[1]),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # axis.title.y = element_text(size = T.sze_axs_ttl),
        # axis.title.x = element_blank(),
        # axis.text.x = element_text(size = T.sze_axs_txt_x),
        # axis.text.y = element_text(size = T.sze_axs_txt_y),
        # plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        panel.ontop = T,
        panel.background = element_rect(fill = NA))

# ggsave(device = "pdf", dpi = 600, units = "mm", width = 110, height = 55,
#        filename = "teaching_barplot.pdf", plot = T.gpr)

ggsave(device = "pdf", dpi = 600, units = "mm", width = 75, height = 55,
       filename = "teaching_barplot.pdf", plot = T.gpr)

