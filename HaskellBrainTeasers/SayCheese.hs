module HaskellBrainTeasers.SayCheese where

moons :: [(String, [String])]
moons =
  [ ("mercury", []),
    ("venus", []),
    ("earth", ["Luna"]),
    ("mars", ["Phobos", "Deimos"]),
    ("jupiter", ["Io", "Europa", "Ganymede", "Callisto"]),
    ("saturn", ["Mimas", "Enceladus", "Tethys", "Dione"]),
    ("uranus", ["Oberon", "Titania", "Umbriel", "Ariel"]),
    ("neptune", ["Proteus", "Larissa", "Galatea", "Despina"])
  ]

main :: IO ()
main =
  do
    let moonPrinter :: [(String, [String])] -> [(String, IO ())]
        moonPrinter [] = []
        moonPrinter ((planet, planetMoons) : otherPlanets) =
          let message =
                if null planetMoons
                  then putStrLn $ planet <> "has no moon"
                  else
                    putStrLn $
                      "The moons of "
                        <> planet
                        <> " are "
                        <> show planetMoons
           in (planet, message) : moonPrinter otherPlanets
        pickJupiter message (planet, moonMessage)
          | planet == "jupiter" = moonMessage
          | otherwise = message
    foldl pickJupiter (putStrLn "starting search...") $ moonPrinter moons
