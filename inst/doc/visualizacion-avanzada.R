## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.align = "center",
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(walkability)
library(ggplot2)
library(patchwork)
library(sf)

# Cargar la red modelo del Centro Histórico de Cusco
data(streets_cusco)

# Construir el grafo topológico en metros (UTM 19S)
net <- walk_build_network(streets_cusco, verbose = FALSE)

# Identificar nodos pertenecientes al componente gigante conectado
giant_nodes <- net$nodes$node_id[net$nodes$is_giant]
orig_node <- giant_nodes[1]
dest_node <- giant_nodes[50]

## ----isochrone-base-----------------------------------------------------------
iso <- walk_isochrone(net, origin = orig_node, time_min = c(5, 10, 15), rings = TRUE)

# Gráfico por defecto provisto por el paquete
p_iso <- plot(iso)

## ----isochrone-custom---------------------------------------------------------
p_iso_custom <- p_iso +
  theme_bw(base_size = 11, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#005F73"),
    plot.subtitle = element_text(size = 9.5, color = "#0A9396"),
    plot.caption = element_text(size = 8, color = "#6C757D", face = "italic"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    legend.background = element_rect(fill = alpha("#F8F9FA", 0.8), color = "#DEE2E6"),
    panel.grid.major = element_line(color = "#E9ECEF", linetype = "dashed")
  ) +
  labs(
    title = "Isócronas Peatonales Disjuntas (Anillos Concéntricos)",
    subtitle = "Centro Histórico de Cusco: umbrales de 5, 10 y 15 minutos",
    caption = "walkability | Motor Rust paralelizado con Dijkstra y alpha hulls"
  )

p_iso_custom

## ----dark-theme---------------------------------------------------------------
# Aplicar tema oscuro personalizado con alta legibilidad
p_iso_dark <- plot(iso) +
  theme_walkability(dark = TRUE, base_size = 11) +
  labs(
    title = "Isócronas Peatonales en Modo Oscuro",
    subtitle = "Cusco: visualización nocturna para cartografía digital"
  )

p_iso_dark

## ----extra-layers-------------------------------------------------------------
# Simulación de equipamientos de salud y educación
set.seed(42)
sample_pois <- net$nodes[net$nodes$is_giant, ][sample(nrow(net$nodes[net$nodes$is_giant, ]), 12), ]
sample_pois$tipo <- rep(c("Centro de Salud", "Escuela", "Mercado"), each = 4)

# Superponer los equipamientos usando geom_sf() sobre el mapa de isócronas
# Mapeamos la forma (shape) para evitar colisiones con la escala fill de las isócronas
p_iso_pois <- p_iso_custom +
  geom_sf(
    data = sample_pois,
    aes(shape = tipo),
    color = "#9B2226",
    fill = "#EE9B00",
    size = 3.2,
    stroke = 1.0
  ) +
  scale_shape_manual(name = "Equipamiento Urbano", values = c(21, 22, 24)) +
  labs(subtitle = "Anillos de caminabilidad y equipamientos de proximidad integrados")

p_iso_pois

## ----patchwork-duo, fig.width = 10, fig.height = 5----------------------------
# 1. Isócronas
p1 <- p_iso_custom + labs(title = "A. Isócronas de Alcance (5, 10 y 15 min)")

# 2. Morfología e intersecciones
morph <- walk_intersection_density(net)
p2 <- plot(morph) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#005F73"),
    plot.subtitle = element_text(size = 9.5, color = "#0A9396"),
    legend.position = "right"
  ) +
  labs(
    title = "B. Morfología de Intersecciones",
    subtitle = "Valencia topológica: cul-de-sacs, nodos 3-way y 4-way+"
  )

# Composición horizontal con anotación global
panel_auditoria <- (p1 | p2) +
  plot_annotation(
    title = "Diagnóstico Integral de Movilidad y Estructura Vial: Cusco",
    subtitle = "Integración de isócronas peatonales y permeabilidad morfológica",
    caption = "Fuente: OpenStreetMap | Elaborado con walkability y patchwork",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", color = "#001219"),
      plot.subtitle = element_text(size = 11, color = "#495057")
    )
  )

panel_auditoria

## ----patchwork-triptico, fig.width = 11, fig.height = 8-----------------------
# 1. Ruteo óptimo entre dos puntos
path_res <- walk_shortest_path(net, from = orig_node, to = dest_node)
p_ruta <- plot(path_res) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12, color = "#AE2012")) +
  labs(title = "1. Ruteo Óptimo y Circuidad")

# 2. Morfología urbana
p_morf <- plot(morph) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12, color = "#005F73")) +
  labs(title = "2. Permeabilidad e Intersecciones")

# 3. Gradiente de alcance a 10 minutos (Rust multihilo)
reach_res <- walk_compute_reach(net, time_min = 10, verbose = FALSE)
p_alcance <- plot(reach_res) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12, color = "#0A9396")) +
  labs(title = "3. Gradiente Espacial de Longitud Caminable (10 min)")

# Composición: dos paneles superiores y uno inferior panorámico
dashboard_triptico <- (p_ruta | p_morf) / p_alcance +
  plot_layout(heights = c(1.1, 1)) +
  plot_annotation(
    title = "Auditoría Multidimensional de Transitabilidad Activa: Cusco",
    subtitle = "Ruta directa, topología nodal y gradiente de alcance de red a 10 minutos",
    tag_levels = "I",
    caption = "Paquete walkability | R + Rust",
    theme = theme(
      plot.title = element_text(size = 17, face = "bold", color = "#001219"),
      plot.subtitle = element_text(size = 11.5, color = "#333333")
    )
  )

dashboard_triptico

## ----patchwork-escenarios, fig.width = 10, fig.height = 5---------------------
# Simular clausura peatonal de 6 tramos céntricos
edges_to_close <- c(15L, 16L, 17L, 18L, 19L, 20L)
scen <- walk_scenario_close(net, edge_indices = edges_to_close, name = "Peatonalización Centro")

# Comparar alcance de 10 min antes y después de la intervención
comp <- walk_compare_scenarios(base = net, scenario = scen, time_min = 10, verbose = FALSE)

# Gráfico comparativo de impacto en tramos
p_comp <- plot(comp) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#9B2226"),
    legend.position = "right"
  ) +
  labs(
    title = "Impacto en Caminabilidad: Delta de Alcance (km)",
    subtitle = "Tramos rojos punteados: vías peatonalizadas"
  )

p_comp

## ----patchwork-global-styling, fig.width = 10, fig.height = 5-----------------
# Crear dos gráficos sencillos
p_a <- plot(iso)
p_b <- plot(path_res)

# Unificar tipografía y fondo en todos los paneles con '&'
panel_unificado <- (p_a | p_b) &
  theme_void(base_size = 11) &
  theme(
    plot.background = element_rect(fill = "#F4F1DE", color = NA),
    panel.background = element_rect(fill = "#F4F1DE", color = NA),
    plot.title = element_text(face = "bold", size = 12, color = "#3D405B"),
    plot.margin = margin(8, 8, 8, 8)
  )

panel_unificado

