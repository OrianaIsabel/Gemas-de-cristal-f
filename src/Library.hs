module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Aspecto = UnAspecto {
  tipoDeAspecto :: String,
  grado :: Number
} deriving (Show, Eq)

tension :: Aspecto
tension = UnAspecto "Tension" 10

incertidumbre :: Aspecto
incertidumbre = UnAspecto "Incertidumbre" 15

peligro :: Aspecto
peligro = UnAspecto "Peligro" 30

type Situacion = [Aspecto]

guerra :: Situacion
guerra = [tension, incertidumbre, peligro]

mejorAspecto :: Aspecto -> Aspecto -> Bool
mejorAspecto mejor peor = grado mejor < grado peor

mismoAspecto :: Aspecto -> Aspecto -> Bool
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto :: Aspecto -> Situacion -> Aspecto
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)

buscarAspectoDeTipo :: String -> Situacion -> Aspecto
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)

reemplazarAspecto :: Aspecto -> Situacion -> Situacion
reemplazarAspecto aspectoBuscado situacion = aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)

-- Punto 1

modificarAspecto :: (Number -> Number) -> Aspecto -> Aspecto
modificarAspecto f aspecto = aspecto {grado = f (grado aspecto)}

mejorSituacion :: Situacion -> Situacion -> Bool
mejorSituacion [aspecto] peor = mejorAspecto aspecto (buscarAspecto aspecto peor)
mejorSituacion (aspecto:aspectos) peor = (mejorSituacion [aspecto] peor) && (mejorSituacion aspectos peor)

modificarSituacion :: (Number -> Number) -> Aspecto -> Situacion -> Situacion
modificarSituacion f aspectoACambiar = reemplazarAspecto (modificarAspecto f aspectoACambiar)

-- Punto 2

data Gema = UnaGema {
    nombre :: String,
    fuerza :: Number,
    personalidad :: Personalidad
} deriving (Show, Eq)

type Personalidad = Situacion -> Situacion

vidente :: Personalidad
vidente = (modificarSituacion (+ (-10)) tension).(modificarSituacion (* 0.5) incertidumbre)

relajada :: Number -> Personalidad
relajada relajamiento = (modificarSituacion (+ relajamiento) peligro).(modificarSituacion (+ (-30)) tension)

zafiro :: Gema
zafiro = UnaGema "Zafiro" 10 vidente

amatista :: Gema
amatista = UnaGema "Amatista" 30 (relajada 15)

perla :: Gema
perla = UnaGema "Perla" 50 vidente

rubi :: Gema
rubi = UnaGema "Rubi" 15 (relajada 3)

rose :: Gema
rose = UnaGema "Rose" 60 (relajada 20)

gemasDeCristal :: [Gema]
gemasDeCristal = [zafiro, amatista, rubi, perla, rose]

-- Punto 3

masFuerte :: Gema -> Gema -> Bool
masFuerte fuerte debil = (fuerza fuerte) >= (fuerza debil)

causaMejorSituacion :: Gema -> Gema -> Situacion -> Bool
causaMejorSituacion mejor peor situacion = mejorSituacion ((personalidad mejor) situacion) ((personalidad peor) situacion)

leGana :: Gema -> Gema -> Situacion -> Bool
leGana ganadora perdedora situacion = (masFuerte ganadora perdedora) && (causaMejorSituacion ganadora perdedora situacion)

-- Punto 4

fusionarNombres :: Gema -> Gema -> String
fusionarNombres gema1 gema2
    | (nombre gema1) /= (nombre gema2) = (nombre gema1) ++ (nombre gema2)
    | otherwise = nombre gema1

reducirTodosLosAspectos :: Number -> Situacion -> Situacion
reducirTodosLosAspectos n situacion = foldr ($) situacion (map (modificarSituacion (+ (-n))) situacion)

fusionarPersonalidades :: Gema -> Gema -> Personalidad
fusionarPersonalidades gema1 gema2 = (personalidad gema2).(personalidad gema1).(reducirTodosLosAspectos 10)

mejoraAlFusionarse :: Gema -> Gema -> Situacion -> Bool
mejoraAlFusionarse gema otra situacion = mejorSituacion ((fusionarPersonalidades gema otra) situacion) ((personalidad gema) situacion)

compatibles :: Gema -> Gema -> Situacion -> Bool
compatibles gema1 gema2 situacion = (mejoraAlFusionarse gema1 gema2 situacion) && (mejoraAlFusionarse gema2 gema1 situacion)

sumaFuerzas :: Gema -> Gema -> Number
sumaFuerzas gema1 gema2 = (fuerza gema1) + (fuerza gema2)

dominante :: Gema -> Gema -> Situacion -> Gema
dominante gema1 gema2 situacion
    | leGana gema1 gema2 situacion = gema1
    | otherwise = gema2

fusionarFuerzas :: Gema -> Gema -> Situacion -> Number
fusionarFuerzas gema1 gema2 situacion
    | compatibles gema1 gema2 situacion = (sumaFuerzas gema1 gema2) * 10
    | otherwise = (fuerza (dominante gema1 gema2 situacion)) * 7

fusion :: Gema -> Gema -> Situacion -> Gema
fusion gema1 gema2 situacion = UnaGema (fusionarNombres gema1 gema2) (fusionarFuerzas gema1 gema2 situacion) (fusionarPersonalidades gema1 gema2)

-- Punto 5

fusionGrupal :: [Gema] -> Situacion -> Gema
fusionGrupal [gema1, gema2] situacion = fusion gema1 gema2 situacion
fusionGrupal (gema1:gema2:resto) situacion = fusionGrupal ((fusionGrupal [gema1, gema2] situacion):resto) situacion