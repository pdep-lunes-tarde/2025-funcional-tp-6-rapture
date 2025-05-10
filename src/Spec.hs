module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

pollosHermanos = 
    Hamborguesa {
        precioBase = 30,
        ingredientes = [Pan, Lechuga, Cheddar, Pollo, Cheddar, Pollo, Pan]
    }

correrTests :: IO ()
correrTests = hspec $ do
    punto1
    -- punto2
    -- punto3

punto1 =
    describe "Punto 1" $ do
        describe "agrandarHamborguesa" $ do
            it "Agrandar una hamborguesa devuelve a la hamborguesa con una carne, pollo o pati vegano extra" $ do
                ingredientes (agrandarHamborguesa cuartoDeLibra) `shouldBe` [Pan,Cheddar,Carne,Carne,Pan]
                ingredientes (agrandarHamborguesa pollosHermanos) `shouldBe` [Pan, Lechuga, Cheddar, Pollo, Pollo, Cheddar, Pollo, Pan]
        
        describe "agregarIngrediente" $ do
            it "Agregar un ingrediente cualquiera a una hamborguesa devuelve a la hamborguesa con dicho ingrediente" $ do
                ingredientes (agregar Panceta pollosHermanos) `shouldBe` [Pan, Lechuga, Cheddar, Panceta, Pollo, Cheddar, Pollo, Pan]
                ingredientes (agregar Carne pollosHermanos) `shouldBe` [Pan, Lechuga, Cheddar, Carne, Pollo, Cheddar, Pollo, Pan]
                
        describe "descuento" $ do
            it "Aplicarle un descuento a una hamborguesa devuelve a la hamborguesa con el descuento aplicado sobre su precio base" $ do
                precioBase (aplicarDescuento 50 pollosHermanos) `shouldBe` 15

        describe "pdepBurger" $ do
            it "El precio de esta deliciosa hamborguesa es de $110" $ do
                precioDeHamborguesa pdepBurga `shouldBe` 110
        
        describe "dobleCuarto" $ do
            it "El precio de esta rica hamborguesa es de $84" $ do
                precioDeHamborguesa dobleCuarto `shouldBe` 84


-- punto2 = implementame
-- punto3 = implementame