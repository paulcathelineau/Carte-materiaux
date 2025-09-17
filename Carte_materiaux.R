################################################################################
# LIBRAIRIES
################################################################################
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(tidyverse)
library(patchwork)

has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)  # pour labels anti-chevauchement

################################################################################
# PARAMÈTRES D'AFFICHAGE (ajustables)
################################################################################

# Aspect des ellipses
width_factor_type   <- 0.20   # épaisseur relative (petits ovales type)
width_factor_domain <- 0.40   # Domain plus “gros” comme Ashby
min_a_log           <- 1e-4   # rayons min (décades) si min≈max
min_b_log           <- 1e-4
ellipse_stroke_type <- 0.8
ellipse_stroke_dom  <- 1.1

# Style
base_text_size      <- 11
label_size_domain   <- 6.0     # taille texte des Domain
fill_alpha_domain   <- 0.4   # opacité des Domain remplis
pad_mult            <- 0.10    # marge en log (10%)

################################################################################
# PATHS
################################################################################
main_path <- "E:/R/15092025_CARTE_MATERIAUX"
xlsx_path <- file.path(main_path, "Data/Data and information for engineering materials.xlsx")
csv_path <- file.path(main_path, "Data/01-08-2025_full_dataset_without_filter.csv")
################################################################################
# LECTURE & COLONNES
################################################################################
df0 <- read_xlsx(xlsx_path)
df1 <- read.csv2(csv_path, sep=",")

required_cols <- c("E_min (GPa)", "E_max (GPa)", "Density_min (Mg/m3)", "Density_max (Mg/m3)")
if (!all(required_cols %in% names(df0))) {
  stop("Colonnes manquantes. Requis : ", paste(required_cols, collapse = ", "))
}

col_type <- if ("Type" %in% names(df0)) "Type" else
  grep("^Type$|Subtype|Material.?type|Category2|Class2|Classe", names(df0),
       ignore.case = TRUE, value = TRUE)[1]
if (is.na(col_type)) stop("Colonne 'Type' introuvable.")

col_domain <- if ("Domain" %in% names(df0)) "Domain" else
  grep("^Domain$|Group|Family|Class|Category", names(df0),
       ignore.case = TRUE, value = TRUE)[1]
if (is.na(col_domain)) stop("Colonne 'Domain' introuvable.")

################################################################################
# PRÉPARATION DES BORNES
################################################################################
bounds <- df0 %>%
  rename(
    Type    = all_of(col_type),
    Domain  = all_of(col_domain),
    E_min   = `E_min (GPa)`,
    E_max   = `E_max (GPa)`,
    Rho_min = `Density_min (Mg/m3)`,
    Rho_max = `Density_max (Mg/m3)`
  ) %>%
  mutate(
    Type   = as.factor(Type),
    Domain = as.factor(Domain),
    across(c(E_min, E_max, Rho_min, Rho_max), ~ suppressWarnings(as.numeric(.x)))
  ) %>%
  group_by(Domain, Type) %>%
  summarise(
    E_min   = min(E_min,   na.rm = TRUE),
    E_max   = max(E_max,   na.rm = TRUE),
    Rho_min = min(Rho_min, na.rm = TRUE),
    Rho_max = max(Rho_max, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(E_min > 0, E_max > 0, Rho_min > 0, Rho_max > 0)

if (nrow(bounds) == 0) stop("Aucun Type avec bornes strictement positives (log-log).")

bounds_dom <- bounds %>%
  group_by(Domain) %>%
  summarise(
    E_min   = min(E_min,   na.rm = TRUE),
    E_max   = max(E_max,   na.rm = TRUE),
    Rho_min = min(Rho_min, na.rm = TRUE),
    Rho_max = max(Rho_max, na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# ELLIPSES ORIENTÉES EN LOG (grand axe = segment min→max)
################################################################################
ellipse_oriented_log <- function(df, id_cols, width_factor) {
  df %>%
    mutate(
      # extrémités du grand axe (espace log10)
      x1 = log10(Rho_min), y1 = log10(E_min),
      x2 = log10(Rho_max), y2 = log10(E_max),
      # centre + direction
      cx = 0.5*(x1 + x2), cy = 0.5*(y1 + y2),
      dx = x2 - x1,       dy = y2 - y1,
      L  = sqrt(dx*dx + dy*dy),
      a  = pmax(L/2, min_a_log),
      ux = ifelse(L > 0, dx/L, 1),
      uy = ifelse(L > 0, dy/L, 0),
      vx = -uy, vy = ux,
      b  = pmax(width_factor * a, min_b_log)
    ) %>%
    select(all_of(id_cols), cx, cy, a, b, ux, uy, vx, vy)
}

type_desc <- ellipse_oriented_log(bounds,     id_cols = c("Domain","Type"), width_factor = width_factor_type)
dom_desc  <- ellipse_oriented_log(bounds_dom, id_cols = c("Domain"),        width_factor = width_factor_domain)

# Polygones (calcul en log → retour en réels)
n_pts  <- 360
theta  <- seq(0, 2*pi, length.out = n_pts)

build_polys <- function(desc, id_cols) {
  desc %>%
    tidyr::crossing(theta = theta) %>%
    mutate(
      lx = cx + a * cos(theta) * ux + b * sin(theta) * vx,
      ly = cy + a * cos(theta) * uy + b * sin(theta) * vy,
      Rho = 10^lx,
      E   = 10^ly
    ) %>%
    select(all_of(id_cols), Rho, E)
}
ell_polys_type <- build_polys(type_desc, c("Domain","Type"))
ell_polys_dom  <- build_polys(dom_desc,  c("Domain"))

################################################################################
# LABELS DOMAIN — centres + excentrage (traitillé, couleur Domain)
################################################################################
dom_labels <- dom_desc %>% mutate(Rho_cent = 10^cx, E_cent = 10^cy)

if (!has_ggrepel) {  # fallback manuel
  k <- nrow(dom_desc)
  dom_labels <- dom_desc %>%
    arrange(cy) %>%
    mutate(
      idx    = row_number(),
      r_off  = 0.22 * pmax(a, b),
      ang    = ifelse(idx %% 2 == 0,  pi/6, -pi/6) + (idx-1) * (pi/(7*max(k,1))),
      lx_lab = cx + r_off * cos(ang) * ux + r_off * sin(ang) * vx,
      ly_lab = cy + r_off * cos(ang) * uy + r_off * sin(ang) * vy,
      Rho_lab = 10^lx_lab, E_lab = 10^ly_lab,
      Rho_cent = 10^cx, E_cent = 10^cy
    )
}

################################################################################
# LIGNES GUIDES "à la Ashby" (vitesse d'onde & indices perf)
################################################################################
# grille de rho pour tracer les droites en coordonnées réelles (log)
rho_seq <- 10^seq(log10(min(ell_polys_dom$Rho, na.rm = TRUE)),
                  log10(max(ell_polys_dom$Rho, na.rm = TRUE)),
                  length.out = 200)

# 1) Vitesse d'onde c (m/s) : E(Pa) = rho(kg/m3)*c^2
#    Avec E en GPa et rho en Mg/m3 => E(GPa) = rho(Mg/m3) * c^2 * 1e-6
c_vals <- c(1e2, 1e3, 1e4)
guides_c <- lapply(c_vals, function(c0) {
  data.frame(group = paste0("c=", format(c0, scientific = FALSE), " m/s"),
             Rho = rho_seq,
             E   = rho_seq * (c0^2) * 1e-6)
}) |> dplyr::bind_rows()

# 2) E/rho = K (module spécifique), droites pente 1
K_vals <- c(0.1, 1, 10, 100)  # GPa per (Mg/m3)
guides_Erho <- lapply(K_vals, function(K) {
  data.frame(group = paste0("E/rho=", K),
             Rho = rho_seq,
             E   = K * rho_seq)
}) |> dplyr::bind_rows()

# 3) sqrt(E)/rho = S  => E = (S*rho)^2 (pente 2)
S_vals <- c(0.3, 1)  # au choix
guides_sqrtErho <- lapply(S_vals, function(S) {
  data.frame(group = paste0("sqrt(E)/rho=", S),
             Rho = rho_seq,
             E   = (S * rho_seq)^2)
}) |> dplyr::bind_rows()

################################################################################
# CADRAGE (marges paddées en log, ratio libre)
################################################################################
x_rng <- range(c(ell_polys_dom$Rho, ell_polys_type$Rho), na.rm = TRUE)
y_rng <- range(c(ell_polys_dom$E,   ell_polys_type$E),   na.rm = TRUE)
lx_rng <- log10(x_rng); ly_rng <- log10(y_rng)
x_span <- diff(lx_rng); y_span <- diff(ly_rng)
if (!is.finite(x_span) || x_span == 0) x_span <- 1
if (!is.finite(y_span) || y_span == 0) y_span <- 1
xlim <- 10^(c(lx_rng[1] - pad_mult * x_span, lx_rng[2] + pad_mult * x_span))
ylim <- 10^(c(ly_rng[1] - pad_mult * y_span, ly_rng[2] + pad_mult * y_span))

################################################################################
# DONNEES CSV
################################################################################
# Nettoyage de MOE et masse.vol, avec gestion des valeurs vides/NA
df1 <- df1 %>%
  mutate(
    # Nettoyage de MOE : remplace les chaînes vides par NA, puis convertit en numérique
    MOE = as.character(MOE) %>%
      replace(., . == "", NA) %>%  # Remplace les chaînes vides par NA
      stringr::str_replace_all("[^0-9.]", "") %>%  # Supprime tout sauf chiffres et points
      as.numeric(),  # Convertit en numérique (les chaînes non valides deviennent NA)
    
    # Même nettoyage pour masse.vol
    `masse.vol` = as.character(`masse.vol`) %>%
      replace(., . == "", NA) %>%
      stringr::str_replace_all("[^0-9.]", "") %>%
      as.numeric()
  )

# Vérification des NA après nettoyage
summary(df1$MOE)       # Doit montrer des valeurs numériques et des NA
summary(df1$`masse.vol`)  # Idem

# Filtrer les lignes où MOE ou masse.vol sont NA
df1_clean <- df1 %>%
  filter(!is.na(MOE) & !is.na(`masse.vol`))

# Vérification du nombre de lignes restantes
nrow(df1_clean)  # Doit être > 0 si des données valides existent
head(df1_clean)  # Aperçu des données nettoyées

# Conversion des unités (sans risque de NA, car déjà filtrés)
df_points <- df1_clean %>%
  mutate(
    MOE_GPa = MOE / 1000,               # MPa → GPa
    masse_vol_Mg_m3 = `masse.vol` / 1000  # kg/m³ → Mg/m³
  )

# Vérification finale
nrow(df_points)  # Doit être égal à nrow(df1_clean)
head(df_points)  # Aperçu des données converties

################################################################################
# PLOT
################################################################################
p <- ggplot() +
  
  # Ellipses Type — petites “pastilles” blanches bord noir (comme Ashby)
  geom_polygon(
    data = ell_polys_type,
    aes(x = Rho, y = E, group = interaction(Domain, Type)),
    fill = "white", color = "black", linewidth = ellipse_stroke_type, alpha = 1
  ) +
  
  # Ellipses Domain — grandes zones remplies (couleur par Domain)
  geom_polygon(
    data = ell_polys_dom,
    aes(x = Rho, y = E, group = Domain, fill = Domain, color = Domain),
    alpha = fill_alpha_domain, linewidth = ellipse_stroke_dom, lineend = "round",
    show.legend = FALSE
  ) +
  
  # Légende in-plot : texte couleur Domain + traitillé (excentré)
  {
    if (has_ggrepel) {
      ggrepel::geom_label_repel(
        data = dom_labels,
        aes(x = Rho_cent, y = E_cent, label = Domain, color = Domain),
        size = label_size_domain, label.size = 0,
        fill = alpha("white", 0.85),
        box.padding = 0.8, point.padding = 0.8, force = 100,
        segment.color = "grey40", segment.size = 0.5, segment.linetype = "dashed",
        min.segment.length = 0.5, max.overlaps = Inf, show.legend = FALSE
      )
    } else {
      list(
        geom_segment(
          data = dom_labels,
          aes(x = Rho_cent, y = E_cent, xend = Rho_lab, yend = E_lab, color = Domain),
          linetype = "dashed", linewidth = 0.35, show.legend = FALSE
        ),
        geom_label(
          data = dom_labels,
          aes(x = Rho_lab, y = E_lab, label = Domain, color = Domain),
          size = label_size_domain, label.size = 0,
          fill = alpha("white", 0.85), show.legend = FALSE
        )
      )
    }
  } +
  
  # Axes LOG–LOG + cadrage
  scale_x_log10(
    breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
    labels = label_number(accuracy = 0.1)
  ) +
  scale_y_log10(
    breaks = 10^(-3:3),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
  
  labs(
    x = expression(rho~"(Mg/"*m^3*")"),
    y = "Young's modulus, E (GPa)",
    title = "Young’s modulus vs Density (style Ashby)"
  ) +
  theme_classic(base_size = base_text_size) +
  theme(
    axis.line = element_line(linewidth = 0.6, colour = "black"),  # barres x et y
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = grid::unit(4, "pt"),
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

# Ajout des points rouges au plot existant (p)
p <- p +
  geom_point(
    data = df_points,
    aes(x = masse_vol_Mg_m3, y = MOE_GPa),
    color = "red",
    size = 2,          # Taille des points (ajustable)
    alpha = 0.7,       # Transparence (optionnel)
    shape = 16         # Forme des points (16 = cercle plein)
  )

print(p)

################################################################################
# EXPORT
################################################################################
ggsave(file.path(main_path, "ashby_style_E_vs_rho.png"),
       p, width = 11, height = 8.5, dpi = 300)

################################################################################
# DOMAINES CIBLÉS : Natural bast/leaf/seed fibre — PLOT UNIQUE
################################################################################

sel_domains <- c("Natural", "Natural bast fibre", "Natural leaf fibre", "Natural seed fibre")

# (robuste aux espaces parasites)
bounds_clean <- bounds %>% mutate(Domain = stringr::str_squish(as.character(Domain)))

# Sous-ensemble Domain ∈ {bast, leaf, seed}
bounds_sel <- bounds_clean %>% filter(Domain %in% sel_domains)
stopifnot(nrow(bounds_sel) > 0)

# Bornes au niveau Domain (pour la grande ellipse de chaque Domain)
bounds_dom_sel <- bounds_sel %>%
  group_by(Domain) %>%
  summarise(
    E_min   = min(E_min,   na.rm = TRUE),
    E_max   = max(E_max,   na.rm = TRUE),
    Rho_min = min(Rho_min, na.rm = TRUE),
    Rho_max = max(Rho_max, na.rm = TRUE),
    .groups = "drop"
  )

# Descripteurs + polygones
type_desc_sel <- ellipse_oriented_log(bounds_sel,     id_cols = c("Domain","Type"), width_factor = width_factor_type)
dom_desc_sel  <- ellipse_oriented_log(bounds_dom_sel, id_cols = c("Domain"),        width_factor = width_factor_domain)

ell_polys_type_sel <- build_polys(type_desc_sel, c("Domain","Type"))
ell_polys_dom_sel  <- build_polys(dom_desc_sel,  c("Domain"))

# Labels (types + domain)
type_labels_sel <- type_desc_sel %>% mutate(Rho_cent = 10^cx, E_cent = 10^cy)
dom_labels_sel  <- dom_desc_sel  %>% mutate(Rho_cent = 10^cx, E_cent = 10^cy)

# Limites (couvrent: 3 Domain + tes points)
lims <- {
  lx <- log10(range(c(ell_polys_dom_sel$Rho, ell_polys_type_sel$Rho, df_points$masse_vol_Mg_m3), na.rm = TRUE))
  ly <- log10(range(c(ell_polys_dom_sel$E,   ell_polys_type_sel$E,   df_points$MOE_GPa),         na.rm = TRUE))
  xs <- diff(lx); ys <- diff(ly); if (!is.finite(xs) || xs==0) xs <- 1; if (!is.finite(ys) || ys==0) ys <- 1
  list(xlim = 10^(c(lx[1]-pad_mult*xs, lx[2]+pad_mult*xs)),
       ylim = 10^(c(ly[1]-pad_mult*ys, ly[2]+pad_mult*ys)))
}

# (facultatif) guides restants : c et E/ρ ; pas de √E/ρ
rho_seq_sel <- 10^seq(log10(min(lims$xlim)), log10(max(lims$xlim)), length.out = 200)
guides_c <- dplyr::bind_rows(
  tibble::tibble(group="c=100 m/s",  Rho=rho_seq_sel, E=rho_seq_sel*(1e2^2)*1e-6),
  tibble::tibble(group="c=1000 m/s", Rho=rho_seq_sel, E=rho_seq_sel*(1e3^2)*1e-6),
  tibble::tibble(group="c=10000 m/s",Rho=rho_seq_sel, E=rho_seq_sel*(1e4^2)*1e-6)
)
guides_Erho <- dplyr::bind_rows(
  tibble::tibble(group="E/rho=0.1", Rho=rho_seq_sel, E=0.1*rho_seq_sel),
  tibble::tibble(group="E/rho=1",   Rho=rho_seq_sel, E=1.0*rho_seq_sel),
  tibble::tibble(group="E/rho=10",  Rho=rho_seq_sel, E=10.0*rho_seq_sel),
  tibble::tibble(group="E/rho=100", Rho=rho_seq_sel, E=100.0*rho_seq_sel)
)

# ==== PLOT ====
p_sel <- ggplot() +
  
  # Ellipses Type (petites pastilles blanches, bord noir)
  geom_polygon(
    data = ell_polys_type_sel,
    aes(x = Rho, y = E, group = interaction(Domain, Type)),
    fill = "white", color = "black", linewidth = ellipse_stroke_type, alpha = 1
  ) +
  
  # Grandes ellipses par Domain (remplies, une parmis 3)
  geom_polygon(
    data = ell_polys_dom_sel,
    aes(x = Rho, y = E, group = Domain, fill = Domain, color = Domain),
    alpha = fill_alpha_domain, linewidth = ellipse_stroke_dom, lineend = "round",
    show.legend = FALSE
  ) +
  
  # Labels des Types (avec traits pointillés)
  {
    if (has_ggrepel) {
      ggrepel::geom_label_repel(
        data = type_labels_sel,
        aes(x = Rho_cent, y = E_cent, label = Type),
        size = 4.2, label.size = 0,
        fill = alpha("white", 0.88), color = "black",
        box.padding = 0.6, point.padding = 0.6, force = 70,
        segment.color = "grey35", segment.size = 0.5, segment.linetype = "dashed",
        min.segment.length = 0.5, max.overlaps = Inf, show.legend = FALSE
      )
    } else {
      geom_text(data = type_labels_sel, aes(Rho_cent, E_cent, label = Type),
                size = 4.2, color = "black")
    }
  } +
  
  # (Option) Labels des Domain — commente si tu ne veux pas ces étiquettes
  {
    if (has_ggrepel) {
      ggrepel::geom_label_repel(
        data = dom_labels_sel,
        aes(x = Rho_cent, y = E_cent, label = Domain, color = Domain),
        size = 5.0, label.size = 0,
        fill = alpha("white", 0.85),
        box.padding = 0.8, point.padding = 0.8, force = 50,
        segment.color = "grey50", segment.size = 0.4, segment.linetype = "dashed",
        min.segment.length = 0.5, max.overlaps = Inf, show.legend = FALSE
      )
    }
  } +
  
  # Tes points palmiers
  geom_point(
    data = df_points,
    aes(x = masse_vol_Mg_m3, y = MOE_GPa),
    color = "red", size = 2, alpha = 0.7, shape = 16
  ) +
  
  # Axes log + cadrage
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10), labels = scales::label_number(accuracy = 0.1)) +
  scale_y_log10(breaks = 10^(-3:3), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  coord_cartesian(xlim = lims$xlim, ylim = lims$ylim, expand = FALSE, clip = "off") +
  labs(
    x = expression(rho~"(Mg/"*m^3*")"),
    y = "Young's modulus, E (GPa)",
    title = 'Young’s modulus vs Density (style Ashby) with only natural material'
  ) +
  # remplace ce bloc de thème sur chaque plot (p_nat, p_sel, etc.)
  theme_classic(base_size = base_text_size) +
  theme(
    axis.line = element_line(linewidth = 0.6, colour = "black"),  # barres x et y
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = grid::unit(4, "pt"),
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

print(p_sel)

# Export
ggsave(file.path(main_path, "ashby_Natural_bast_leaf_seed_types_labels_NOgrid.png"),
       p_sel, width = 11, height = 8.5, dpi = 300)
