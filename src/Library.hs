module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | PatiVegano | Lechuga
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente PatiVegano = 10

data Hamborguesa = Hamborguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)


cuartoDeLibra =
    Hamborguesa {
        precioBase = 20,
        ingredientes = [Pan, Cheddar, Carne, Pan]
    }

precioDeHamborguesa :: Hamborguesa -> Number
precioDeHamborguesa hamborguesa =
    precioBase hamborguesa + (sum.map precioIngrediente) (ingredientes hamborguesa)

-- **agrandar**: si había carne se agrega carne, si había pollo se agrega pollo, si había ambos da igual cuál se agregue.
agrandarHamborguesa :: Hamborguesa -> Hamborguesa
agrandarHamborguesa hamborguesa
    | any (== Carne) (ingredientes hamborguesa) = agregar Carne hamborguesa
    | any (== Pollo) (ingredientes hamborguesa) = agregar Pollo hamborguesa
    | any (== PatiVegano) (ingredientes hamborguesa) = agregar PatiVegano hamborguesa
    | otherwise =
        hamborguesa

-- **agregarIngrediente**: recibe un ingrediente y una hambrugesa lo agrega a la hamburguesa.
agregar :: Ingrediente -> Hamborguesa -> Hamborguesa
agregar ingredienteNuevo hamborguesa = 
    hamborguesa {ingredientes = agregarIngrediente ingredienteNuevo (ingredientes hamborguesa)}

    where 
        agregarIngrediente _ [] = []
        agregarIngrediente nuevoIngrediente (cabezaIngrediente : colaIngrediente)
            | cabezaIngrediente == Carne || cabezaIngrediente == Pollo || cabezaIngrediente == PatiVegano = 
                nuevoIngrediente : cabezaIngrediente : colaIngrediente
            -- (agrega el ingrediente encima de la carne/el pollo, porque sería una aberración poner una lechuga, por ejemplo, sobre el pan inferior)
            | otherwise = 
                cabezaIngrediente : agregarIngrediente nuevoIngrediente colaIngrediente

-- **descuento**: recibe un % de descuento, y devuelve la hamburguesa con ese descuento aplicado al precio base.
aplicarDescuento :: Number -> Hamborguesa -> Hamborguesa
aplicarDescuento porcentaje hamborguesa
    | porcentaje <= 0 = 
        error"¡¡Ladrón!!"
    | otherwise = 
        hamborguesa{precioBase = calcularDescuento porcentaje (precioBase hamborguesa)}

    where
        calcularDescuento porcentaje precio = 
            precio * (1 - porcentaje/100)

-- **pdepBurger**: un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento. Su precio final deberia ser 110.
pdepBurga = aplicarDescuento 20 . agregar Cheddar . agregar Panceta . agrandarHamborguesa . agrandarHamborguesa $ cuartoDeLibra

-- **dobleCuarto** = es un cuarto de libra con carne y cheddar. El precio final deberia ser 84.
dobleCuarto = agregar Cheddar . agrandarHamborguesa $ cuartoDeLibra