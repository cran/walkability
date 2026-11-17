## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 5,
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(walkability)
library(sf)

# Cargar dataset real empaquetado
data(streets_cusco)

# Construir grafo peatonal limpio y estructurado
net <- walk_build_network(
  data = streets_cusco,
  walk_speed_kmh = 4.5,
  repair = TRUE,
  snap_tolerance_m = 5.0,
  keep_components = "largest",
  verbose = FALSE
)

print(net)

## -----------------------------------------------------------------------------
# Seleccionar dos puntos extremos dentro del componente conectado
nodos_principales <- net$nodes[net$nodes$is_giant, ]
coords <- st_coordinates(nodos_principales)

origen  <- coords[1, 1:2]
destino <- coords[50, 1:2]

ruta <- walk_shortest_path(net, from = origen, to = destino)
print(ruta)

## -----------------------------------------------------------------------------
plot(ruta)

## -----------------------------------------------------------------------------
# Calcular isócrona a 5 minutos a pie desde el punto de origen
iso_5m <- walk_isochrone(net, origin = origen, time_min = 5.0, method = "concave")
print(iso_5m)

## -----------------------------------------------------------------------------
plot(iso_5m)

## -----------------------------------------------------------------------------
morfologia <- walk_intersection_density(net)
print(morfologia)

## -----------------------------------------------------------------------------
plot(morfologia)

## -----------------------------------------------------------------------------
# Simular 5 equipamientos urbanos (p. ej. paradas de transporte público o farmacias)
# ubicados en nodos de la red
set.seed(42)
pois_muestra <- nodos_principales[c(5, 25, 45, 65, 85), ]
pois_muestra$tipo <- c("Salud", "Educación", "Transporte", "Comercio", "Salud")

# Calcular accesibilidad acumulativa en 5 minutos a pie
acc <- walk_compute_access(
  network = net,
  pois = pois_muestra,
  time_min = 5.0,
  category_col = "tipo",
  verbose = FALSE
)

print(acc)

## -----------------------------------------------------------------------------
cols_acc <- grep("^access_", names(acc$nodes), value = TRUE)
head(sf::st_drop_geometry(acc$nodes)[, c("node_id", head(cols_acc, 4))])

## -----------------------------------------------------------------------------
plot(acc)

## -----------------------------------------------------------------------------
# Simular el cierre de dos tramos viales (por ejemplo, para paseo peatonal o mantenimiento)
aristas_cierre <- c(5, 12)
net_proyecto <- walk_scenario_close(
  network = net,
  edge_indices = aristas_cierre,
  name = "Peatonalización Eje Central"
)

# Comparar ambos escenarios en un horizonte de 5 minutos
comparacion <- walk_compare_scenarios(
  base = net,
  scenario = net_proyecto,
  time_min = 5.0,
  verbose = FALSE
)

print(comparacion)

## -----------------------------------------------------------------------------
plot(comparacion)

## -----------------------------------------------------------------------------
# Conversión a objeto sfnetwork
if (requireNamespace("sfnetworks", quietly = TRUE)) {
  sfn <- as_sfnetwork(net)
  print(sfn)
}

# Conversión a objeto tidygraph tbl_graph
if (requireNamespace("tidygraph", quietly = TRUE)) {
  tg <- as_tidygraph(net)
  print(tg)
}

