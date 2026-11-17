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

# Cargar la capa real de Cusco incluida en el paquete
data(streets_cusco)
mis_calles <- streets_cusco

# Diagnóstico de conectividad y detección de undershoots en un radio de 5 metros
diag <- walk_diagnose_network(mis_calles, snap_search_radius_m = 5.0)

# Imprimir resumen de componentes e islas
print(diag)

## -----------------------------------------------------------------------------
plot(diag)

## -----------------------------------------------------------------------------
# Construir grafo peatonal limpio y estructurado
red_limpia <- walk_build_network(
  data = mis_calles,
  crs = 32719,                   # Proyección métrica local (UTM 19S)
  walk_speed_kmh = 4.5,          # Velocidad de diseño (4.5 km/h = 1.25 m/s)
  repair = TRUE,                 # Decisión explícita del usuario de reparar
  snap_tolerance_m = 5.0,        # Conectar extremos a <= 5 metros (corrige el undershoot)
  planarize = TRUE,              # Dividir líneas en intersecciones
  keep_components = "largest",   # Conservar la componente gigante conectada
  verbose = FALSE
)

print(red_limpia)

## -----------------------------------------------------------------------------
# Por defecto se calcula para 10 minutos (time_min = 10.0).
# Para redes barriales o ciudades intermedias/pequeñas, el usuario puede
# ajustar libremente el parámetro (por ejemplo, time_min = 5.0 o 7.5):
metricas <- walk_compute_reach(
  network = red_limpia,
  time_min = 10.0,
  verbose = FALSE
)

print(metricas)

## -----------------------------------------------------------------------------
# Mapa temático de caminabilidad
mapa <- walk_plot_reach(
  reach = metricas,
  palette = "viridis",
  title = "Caminabilidad Peatonal en 10 Minutos",
  subtitle = "Longitud de red accesible a pie desde cada intersección (Centro Histórico de Cusco)",
  dark = TRUE
)

print(mapa)

## -----------------------------------------------------------------------------
circuidad <- walk_compute_circuity(red_limpia, sample_size = 50)
summary(circuidad$mean_circuity)

