import Test.Hspec
import TP

main :: IO ()
main = hspec $ do
  describe "TP integrador" $ do
    
    describe "Punto 1." $ do
      describe "Ítem a. Saber si un auto está cerca de otro auto" $ do
        it "Si no son autos distintos y la distancia que hay entre ellos (en valor absoluto) no es menor a 10." $ do
          estanCerca (Auto {color = "rojo", velocidad = 0, distancia = -5}) (Auto {color = "rojo", velocidad = 0, distancia = 5}) `shouldBe` False
        it "Si no son autos distintos y la distancia que hay entre ellos (en valor absoluto) es menor a 10." $ do
          estanCerca (Auto {color = "rojo", velocidad = 0, distancia = -4}) (Auto {color = "rojo", velocidad = 0, distancia = 4}) `shouldBe` False
        it "Si son autos distintos y la distancia que hay entre ellos (en valor absoluto) no es menor a 10." $ do
          estanCerca (Auto {color = "rojo", velocidad = 0, distancia = 20}) (Auto {color = "amarillo", velocidad = 0, distancia = 10}) `shouldBe` False
        it "Si son autos distintos y la distancia que hay entre ellos (en valor absoluto) es menor a 10." $ do
          estanCerca (Auto {color = "rojo", velocidad = 0, distancia = -4}) (Auto {color = "amarillo", velocidad = 0, distancia = 4}) `shouldBe` True
      describe "Ítem b. Saber si un auto va tranquilo en una carrera" $ do
        it "Si tiene algún auto cerca y no les va ganando a todos" $ do
          vaTranquilo (Auto {color = "rojo", velocidad = 0, distancia = 20}) ([Auto {color = "rojo", velocidad = 0, distancia = 20},Auto {color = "amarillo", velocidad = 0, distancia = 25}]) `shouldBe` False
        it "Si tiene algún auto cerca y les va ganando a todos" $ do
          vaTranquilo (Auto {color = "rojo", velocidad = 0, distancia = 20}) ([Auto {color = "rojo", velocidad = 0, distancia = 20},Auto {color = "amarillo", velocidad = 0, distancia = 15}]) `shouldBe` False
        it "Si no tiene ningún auto cerca y no les va ganando a todos" $ do
          vaTranquilo (Auto {color = "rojo", velocidad = 0, distancia = 20}) ([Auto {color = "rojo", velocidad = 0, distancia = 20},Auto {color = "amarillo", velocidad = 0, distancia = 30}]) `shouldBe` False
        it "Si no tiene ningún auto cerca y les va ganando a todos" $ do
          vaTranquilo (Auto {color = "rojo", velocidad = 0, distancia = 20}) ([Auto {color = "rojo", velocidad = 0, distancia = 20},Auto {color = "amarillo", velocidad = 0, distancia = 10}]) `shouldBe` True
      describe "Ítem c. Conocer en qué puesto está un auto en una carrera" $ do
        it "Puesto 1/3" $ do
          puesto (Auto {color = "rojo", velocidad = 0, distancia = 10}) ([Auto {color = "rojo", velocidad = 0, distancia = 10},Auto {color = "amarillo", velocidad = 0, distancia = 9},Auto {color = "verde", velocidad = 0, distancia = 8}]) `shouldBe` 1
        it "Puesto 2/3" $ do
          puesto (Auto {color = "amarillo", velocidad = 0, distancia = 9}) ([Auto {color = "rojo", velocidad = 0, distancia = 10},Auto {color = "amarillo", velocidad = 0, distancia = 9},Auto {color = "verde", velocidad = 0, distancia = 8}]) `shouldBe` 2
        it "Puesto 3/3" $ do
          puesto (Auto {color = "verde", velocidad = 0, distancia = 8}) ([Auto {color = "rojo", velocidad = 0, distancia = 10},Auto {color = "amarillo", velocidad = 0, distancia = 9},Auto {color = "verde", velocidad = 0, distancia = 8}]) `shouldBe` 3
  
    describe "Punto 2." $ do
      it "Ítem a. Hacer que un auto corra durante un determinado tiempo." $ do
          correr 5 (Auto {color = "rojo", velocidad = 1, distancia = -4}) `shouldBe` (Auto {color = "rojo", velocidad = 1, distancia = 1})
          correr 4 (Auto {color = "celeste", velocidad = 2, distancia = 2}) `shouldBe` (Auto {color = "celeste", velocidad = 2, distancia = 10})
          correr 3 (Auto {color = "violeta", velocidad = 3, distancia = 3}) `shouldBe` (Auto {color = "violeta", velocidad = 3, distancia = 12})
          correr 2 (Auto {color = "negro", velocidad = 4, distancia = 10}) `shouldBe` (Auto {color = "negro", velocidad = 4, distancia = 18})
      describe "Ítem b." $ do
        describe "Apartado i. Alterar la velocidad de un auto." $ do
          it "Si la velocidad actual se altera multiplicándola por 100" $ do
            alterarVelocidadAuto (*100) (Auto {color = "rojo", velocidad = 10, distancia = -4})  `shouldBe` (Auto {color = "rojo", velocidad = 1000, distancia = -4})
          it "Si la velocidad actual se altera sumándole 60" $ do
            alterarVelocidadAuto (+60) (Auto {color = "celeste", velocidad = 7, distancia = 12})  `shouldBe` (Auto {color = "celeste", velocidad = 67, distancia = 12})
          it "Si la velocidad actual se altera dividiéndola por: 5" $ do
            alterarVelocidadAuto (`div` 5) (Auto {color = "amarillo", velocidad = 10, distancia = 32})  `shouldBe` (Auto {color = "amarillo", velocidad = 2, distancia = 32})
          it "Si la velocidad actual se altera restándole 500" $ do
            alterarVelocidadAuto (+(-500)) (Auto {color = "negro", velocidad = 50, distancia = 41})  `shouldBe` (Auto {color = "negro", velocidad = -450, distancia = 41})
        describe "Apartado ii. Bajar la velocidad de un auto." $ do
          it "Si la resta da 0 o más" $ do
            bajarVelocidadAuto 100 (Auto {color = "rojo", velocidad = 200, distancia = -4})  `shouldBe` (Auto {color = "rojo", velocidad = 100, distancia = -4})
          it "Si la resta da menos de 0" $ do
            bajarVelocidadAuto 25 (Auto {color = "celeste", velocidad = 10, distancia = -4})  `shouldBe` (Auto {color = "celeste", velocidad = 0, distancia = -4})
          it "Si se le resta una velocidad negativa" $ do
            bajarVelocidadAuto (-200) (Auto {color = "violeta", velocidad = 100, distancia = -4}) `shouldBe` (Auto {color = "violeta", velocidad = 300, distancia = -4})
  
    describe "Punto 3. - PowerUps" $ do
      it "Ítem a. terremoto" $ do
        terremoto (Auto {color = "verde", velocidad = 100, distancia = 50}) ([Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]) `shouldBe` [Auto {color = "amarillo", velocidad = 0, distancia = 45},Auto {color = "azul", velocidad = 10, distancia = 55},Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "violeta", velocidad = 40, distancia = 60}]
      it "Ítem b. miguelitos" $ do
        miguelitos 5 (Auto {color = "verde", velocidad = 100, distancia = 50}) ([Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]) `shouldBe` [Auto {color = "rojo", velocidad = 55, distancia = 40},Auto {color = "amarillo", velocidad = 35, distancia = 45},Auto {color = "verde", velocidad = 95, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]
        miguelitos 50 (Auto {color = "verde", velocidad = 100, distancia = 50}) ([Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]) `shouldBe` [Auto {color = "rojo", velocidad = 10, distancia = 40},Auto {color = "amarillo", velocidad = 0, distancia = 45},Auto {color = "verde", velocidad = 50, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]
      it "Ítem c. jet pack" $ do
          jetPack 5 (Auto {color = "verde", velocidad = 100, distancia = 50}) ([Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]) `shouldBe` [Auto {color = "verde", velocidad = 100, distancia = 1050},Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]

    describe "Punto 4." $ do
      describe "Ítem a. simularCarrera" $ do
        it "carrera simulada del item C." $ do
         simularCarrera ([Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]) (["verde" `usaPowerUp` terremoto]) `shouldBe` [(1,"violeta"),(2,"azul"),(3,"verde"),(4,"amarillo"),(5,"rojo")]
      describe "Ítem b. Generar los eventos de una carrera" $ do
        it "Apartado i. correnTodos" $ do
          correnTodos 20 ([Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]) `shouldBe` [Auto {color = "rojo", velocidad = 60, distancia = 1240},Auto {color = "amarillo", velocidad = 40, distancia = 845},Auto {color = "verde", velocidad = 100, distancia = 2050},Auto {color = "azul", velocidad = 60, distancia = 1255},Auto {color = "violeta", velocidad = 40, distancia = 860}]
        it "Apartado ii. usaPowerUp" $ do
          usaPowerUp "verde" terremoto ([Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]) `shouldBe` [Auto {color = "amarillo", velocidad = 0, distancia = 45},Auto {color = "azul", velocidad = 10, distancia = 55},Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "violeta", velocidad = 40, distancia = 60}]
      it "Ítem c. Prueba de lo desarrollado" $ do
        simularCarrera carreraDeEjemplo eventosDeEjemplo `shouldBe` [(1,"azul"),(2,"blanco"),(3,"negro"),(4,"rojo")]

    describe "Punto 5. - Misil teledirigido" $ do
      it "Si la velocidad del auto a afectar no es menor a 50" $ do
        misilTeledirigido "rojo" (Auto {color = "verde", velocidad = 100, distancia = 50}) ([Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]) `shouldBe` [Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]
      it "Si la velocidad del auto a afectar es menor a 50 y no le va ganando al auto que usó el power up" $ do    
        misilTeledirigido "amarillo" (Auto {color = "verde", velocidad = 100, distancia = 50}) ([Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]) `shouldBe` [Auto {color = "amarillo", velocidad = 10, distancia = 45},Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]
      it "Si la velocidad del auto a afectar es menor a 50 y le va ganando al auto que usó el power up" $ do
        misilTeledirigido "violeta" (Auto {color = "verde", velocidad = 100, distancia = 50}) ([Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55},Auto {color = "violeta", velocidad = 40, distancia = 60}]) `shouldBe` [Auto {color = "violeta", velocidad = 10, distancia = 65},Auto {color = "rojo", velocidad = 60, distancia = 40},Auto {color = "amarillo", velocidad = 40, distancia = 45},Auto {color = "verde", velocidad = 100, distancia = 50},Auto {color = "azul", velocidad = 60, distancia = 55}]