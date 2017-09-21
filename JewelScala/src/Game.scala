import java.util.Random;
import java.io.PrintWriter;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
/*
 * CAMBIOS:
 * 	-> Separado el programa en varias clases .scala
 * 	-> Menú prinicpal del juego
 * 	-> Conteo de combinaciones y puntuación 
 * 
 * HOY:
 * 	-> Detectar si se hacen combinaciones la mover
 *  -> Borrar hasta que no queden combinaciones
 *  -> Documentar
 */

/**
 * Clase que implementa la funcionalidad principal del juego
 */
class Game{
   println("Hello, I'm GAME!")
   
   //Para activar el modo espera
   var espera= false
   
   /**
    * Pone la configuración inicial del juego e 
    * inicia el bucle principal del mismo.
    */
   def iniciarJuego(){
     //val filas = 3;
     //val columnas = 3;
     //val dif = 1;
     
     println("Bienvenido al juego!\nPor favor, Introduce la dificultad(1-3): ")
     
     val dif = readInt()
     
     println("Has seleccionado: " + dif)
     
     //---
     println("Desea activar el modo espera? (s/n)")
     val x = readChar()
     espera = x=='s'
     if(espera) println("Modo espera seleccionado!")
     //---
     
     if(dif > 3 || dif < 1)
     {
       println("Dificultad no válida, pasando a dificultad por defecto: 1")
     }
     
     val filas = dif match {
       case 1 => 7
       case 2 => 11
       case 3 => 15
       case _ => 9
     }
     
     val columnas = dif match {
       case 1 => 9
       case 2 => 17
       case 3 => 27
       case _ => 9
     }
     
     println("Generando tablero de " + filas + "x"
         + columnas +" dificultad " + dif +"...");
     val tableroNuevo = generarTablero(filas,columnas,dif);
     
     bucleJuego(tableroNuevo);
   }
   
   /**
    * Bucle principal del juego
    */
   def bucleJuego(tablero : Tablero){
     imprimirTableroPlus(tablero)
    
     //Almaceno en una variable si hay combinaciones en el tablero
     val hayCombinaciones = detectarCombinacionesTablero(tablero, 0)
     println("Quedan Combinaciones : " + hayCombinaciones)
     //Si hay combinaciones, llamo a borrarCombinaciones para
     //eliminarlas
     if(hayCombinaciones)
     {
      //println("Has seleccionado 'Buscar combinaciones'...")
      //println("Buscando combinaciones...")
      println("Borrando combinaciones...")
      val tableroNuevo = borrarCombinacionesTablero(tablero)
      //DEBUG
      println("Antes de borrar: ")
      imprimirTablero(tablero)
      println("Despues de borrar: ")
      imprimirTablero(tableroNuevo)
      //FIN_DEBUG
      bucleJuego(tableroNuevo) 
     }
     //Si no, dejo al usuario elegir la acción a realizar
     else
     {
         println(">>>>>> Seleccione una opción...\n" 
             +">>>1. Mover Ficha"
             //+"\n>>>2. Buscar Combinaciones."
             +"\n>>>3. Movimiento Automático (IA)."
             +"\n>>>8. Guardar Partida."
             +"\n>>>9. Cargar Partida."
             +"\n>>>0. Salir.")
         val eleccion = readInt()
         eleccion match
         {
           case 1 =>
             println("Has seleccionado 'Mover ficha'...")
             val tableroNuevo = moverFichaInterfaz(tablero)
             bucleJuego(tableroNuevo)
             /*
           case 2 =>
             println("Has seleccionado 'Buscar combinaciones'...")
             println("Buscando combinaciones...")
             val tableroNuevo = borrarCombinacionesTablero(tablero)
             //DEBUG
             println("Antes de borrar: ")
             imprimirTablero(tablero)
             println("Despues de borrar: ")
             imprimirTablero(tableroNuevo)
             //FIN_DEBUG
             bucleJuego(tableroNuevo)
             */
           case 3 =>
             val ia = new IA(this)
             println("Has seleccionado 'Movimiento Automático'...")
             println("Llamando IA...")
             val tableroNuevo = ia.llamarIA(tablero)
             bucleJuego(tableroNuevo)
           case 8 =>
             println("Has seleccionado 'Guardar Partida'...")
             println("Guardando tablero...")
             guardarTablero(tablero)
             println("Tablero guardado...")
             bucleJuego(tablero)
           case 9 =>
             println("Has seleccionado 'Cargar Partida'...")
             println("Cargando tablero...")
             val tableroNuevo = cargarTablero(tablero)
             println("Tablero cargado...")
             bucleJuego(tableroNuevo)
           case 0 =>
             println("Saliendo...")
             System.exit(0)
           case _ =>
             println("Opción seleccionada no válida. Inténtelo de nuevo.")
             bucleJuego(tablero)
         }
     
     }
   }
   
   /**
    * Funciones del tablero
    */
   
   /**
    * Genera un tablero con las filas 
    * y columnas especificadas
    */
   def generarTablero(filas : Int, columnas : Int, dificultad : Int) : Tablero = {
     new Tablero(generarTableroRec(filas * columnas, dificultad),
         filas, columnas, dificultad, 0, 0)
   }
   
   /**
    * Utilizado por generarTablero para realizar su funcion.
    * Esta función realiza el trabajo de crear la lista concatenando
    * las fichas
    */
   def generarTableroRec(cantidad : Int, dificultad : Int) : List[Int] = {
     if(cantidad == 0) {
       Nil
     } else {
       (new Random().nextInt(2+2*dificultad) + 1) :: generarTableroRec(cantidad-1,dificultad)
     }
   }
   
   /**
    * Genera un tablero vacío con las ficlas y columnas especificadas
    */
   def generarTableroVacio(filas : Int, columnas : Int, dificultad : Int) : Tablero = {
       new Tablero(generarTableroRecVacio(filas * columnas, dificultad)
           , filas, columnas, dificultad, 0, 0)
   }
   
   /**
    * Utilizado por generarTableroVacio para realizar su funcion.
    * Esta función realiza el trabajo de crear la lista concatenando
    * las fichas vacías.
    */
   def generarTableroRecVacio(cantidad : Int, dificultad : Int) : List[Int] = {
     if(cantidad == 0) {
       Nil
     } else {
       0 :: generarTableroRecVacio(cantidad-1,dificultad)
     }
   }
   
   
   /**
    * Similar a imprimirTablero, pero mostrando más información,
    * como la puntuación y las combinaciones
    */
   def imprimirTableroPlus(tablero: Tablero) = {
     println("_________________________________\n")
     println("Combinaciones: "+tablero.combinaciones)
     println("Puntuación: "+tablero.puntuacion)
     imprimirTablero(tablero)
   }
   
   /**
    * Imprime por consola el tablero especificado
    */
   def imprimirTablero(tablero : Tablero) : Tablero = {
     println(imprimirTableroRec(tablero.tablero,tablero.columnas));
     tablero
     
   }
   
   /**
    * Utilizado por imprimirTablero para imprimir correctamente
    * las filas del tablero
    */
   def imprimirTableroRec(tablero : List[Int], columnas : Int) : String = {
     if(tablero.length == 0) {
       ""
     } else {
       if(tablero.length % columnas == 0) {
         "\n" + equivalenciaNumeroColor(tablero.head) + " " + imprimirTableroRec(tablero.tail, columnas)
       } else {
         equivalenciaNumeroColor(tablero.head) + " " + imprimirTableroRec(tablero.tail, columnas)  
       }
       
     }
   }
   
   /**
    * Función que dado un número nos devulve el valor equivalente.
    */
   def equivalenciaNumeroColor(valor : Int) : String = {
     valor match {
       case 1 => "A"
       case 2 => "R"
       case 3 => "N"
       case 4 => "V"
       case 5 => "P"
       case 6 => "M"
       case 7 => "G"
       case 8 => "B"
       case _ => "-" // "Vacío_"
     }
   }
   
   /**
    * Operaciones para transformación de coordenadas
    */
   
     
   /**
    * Transforma una posicion del tablero a filas y columnas
    */
   def transPosicion(tablero:Tablero, pos:Int): List[Int] = {
     List(pos / tablero.columnas , pos % tablero.columnas)
   }
   
   /**
    * Dada fila y columna devuelve la posicion en
    * el array que es el tablero
    */
   def transCoordenadas(tablero:Tablero, fila:Int, col:Int) : Int =
   {
       fila*tablero.columnas+col
   }
   
   /**
    * prueba. Solo muestra por pantalla. Para debug de transPosicion()
    */
   def transListaPos(tablero:Tablero, lista:List[Int]) : Unit ={
     if(!lista.isEmpty)
     {
       println(transPosicion(tablero, lista.head))
       transListaPos(tablero, lista.tail)
     }
   }
   
   /**
    * Funciones movimiento de fichas
    */
   
   /**
    * Pone la ficha con el color indicado en la posición del tablero indicado.
    */
   def poner(color : Int, fila : Int, columna : Int, tablero : Tablero) : Tablero = {
        new Tablero(ponerRec(tablero.tablero,0,color,fila,columna,tablero)
            ,tablero.filas, tablero.columnas, tablero.dificultad
            ,tablero.puntuacion, tablero.combinaciones)   
   }
   
   /**
    * Pone una ficha aleatoria en la coordenada indicada del tablero.
    */
   def ponerAleatorio(fila : Int, columna : Int, tablero : Tablero) : Tablero = {
       new Tablero(ponerRec(tablero.tablero,0
           ,(new Random().nextInt(2+2*tablero.dificultad) + 1)
           ,fila,columna,tablero),tablero.filas, tablero.columnas
           ,tablero.dificultad, tablero.puntuacion, tablero.combinaciones)
   }
   
   /**
    * Utilizado por poner() para concatenar de forma adecuada todas las gemas del tablero
    * tras insertar la ficha.
    */
   def ponerRec(listaAux : List[Int], indice : Int, color : Int, fila : Int, columna : Int, tablero : Tablero) : List[Int] = {
       // Si el indice se está pasando, devolvemos Nil para detener la recursividad.
       if(indice > tablero.columnas + tablero.filas*tablero.columnas) {
           Nil
       } else {
           if(listaAux.length == 0) {
               Nil
           } else {
               // Si el indice es el que esta apuntando la fila con la columna...
               if(indice == columna + fila*tablero.columnas) {
                   // Entonces cambiamos, es decir, en vez de adjuntar el valor que existe, le ponemos el nuevo
                   color :: ponerRec(listaAux.tail,indice + 1,color,fila,columna,tablero)
               } else {
                   // Entonces lo dejamos igual.
                   listaAux.head :: ponerRec(listaAux.tail,indice + 1,color,fila,columna,tablero)
               }
           }        
       }
   }
   
   /*
    * MOVIMIENTOS DE FICHAS
    */
   
   /**
    * Intercambia dos fichas en un tablero, y devuelve el 
    * tablero resultante del intercambio.
    * La primera casilla estará definida por fila1 y columna1
    * y la segunda por fila2 y columna2
    */
   def swap(tablero: Tablero, fila1: Int, columna1: Int,fila2: Int, columna2: Int) : Tablero = {
       //copia el valor de la primera ficha
       //val aux = tablero.tablero(fila1*tablero.columnas+columna1)
       //pongo la segunda ficha en el lugar de la primera
       val tableron1 = poner(tablero.tablero(fila2*tablero.columnas+columna2),fila1,columna1,tablero)
       //pongo la primera ficha en el lugar de la segunda
       val tableron2 = poner(tablero.tablero(fila1*tablero.columnas+columna1),fila2,columna2,tableron1)
       return tableron2
       
   }
   
   /**
    * Interfaz para swap para mover fichas a casillas adyacentes,
    * utilizando swap()
    *   Devuleve el tablero resultante de mover la ficha
   */
   def moverFicha(tablero:Tablero,fila:Int,columna:Int) : Tablero = {
     //si se sale del tablero no movemos la ficha
     if(fila >= tablero.filas || columna >= tablero.filas){
       println("Las coordenadas introducidas no son adecuadas para el tablero actual")
       return tablero
     }
     //preguntamos a donde quiere mover
     else{
       println("Selecciona dirección:\n1. Arriba\n2. Derecha\n3. Izquierda\n4. Abajo\n>>> ")
       val dir = readInt()
       //println("Has introducido: " + dir)
       dir match{
         //ARRIBA
         case 1 => 
            if(fila>0){
              //mover
              val tableroNuevo = swap(tablero,fila,columna,fila-1,columna)
              
              //Comprobamos si el movimiento realiza una combinacion.
              val haceCombinacion = detectarCombinacionesCambio(tableroNuevo,fila,columna,fila-1,columna)
              //Si la realiza lo permitimos
              if(haceCombinacion)
              {
                tableroNuevo
              }
              //Si no hace combinación, descartamos
              //el movimiento y devolvemos el tablero inicial
              //sin el cambio
              else
              {
                println("Ese movimiento no realiza ninguna combinación!\nDescartando...")
                tablero
              }
              
            }
            else{
              println("ERROR: No se puede mover hacia arriba una ficha en la primera fila")
              //devuelvo el mismo tablero, no hago nada
              return tablero
            }
         //DERECHA
         case 2 =>
           if(columna<tablero.columnas-1){
              //mover
              val tableroNuevo = swap(tablero,fila,columna,fila,columna+1)   
              
              //Comprobamos si el movimiento realiza una combinacion.
              //Lo comprobamos en el tablero resultante de hacer el movimiento
              val haceCombinacion = detectarCombinacionesCambio(tableroNuevo,fila,columna,fila,columna+1)
              //Si la realiza lo permitimos
              if(haceCombinacion)
              {
                tableroNuevo
              }
              //Si no hace combinación, descartamos
              //el movimiento y devolvemos el tablero inicial
              //sin el cambio
              else
              {
                println("Ese movimiento no realiza ninguna combinación!\nDescartando...")
                tablero
              }
              
            }
            else{
              println("ERROR: No se puede mover hacia la derecha una ficha en la última columna")
              //devuelvo el mismo tablero, no hago nada
              return tablero
            }
         //IZQUIERDA   
         case 3 =>
           if(columna>0){
              //mover
              val tableroNuevo = swap(tablero,fila,columna,fila,columna-1)
              
              //Comprobamos si el movimiento realiza una combinacion.
              //Lo comprobamos en el tablero resultante de hacer el movimiento
              val haceCombinacion = detectarCombinacionesCambio(tableroNuevo,fila,columna,fila,columna-1)
              //Si la realiza lo permitimos
              if(haceCombinacion)
              {
                tableroNuevo
              }
              //Si no hace combinación, descartamos
              //el movimiento y devolvemos el tablero inicial
              //sin el cambio
              else
              {
                println("Ese movimiento no realiza ninguna combinación!\nDescartando...")
                tablero
              }
              
            }
            else{
              println("ERROR: No se puede mover hacia la izquierda una ficha en la primera columna")
              //devuelvo el mismo tablero, no hago nada
              return tablero
            }
         //ABAJO   
         case 4 =>
           if(fila<tablero.filas-1){
              //mover
              val tableroNuevo = swap(tablero,fila,columna,fila+1,columna)
              
              //Comprobamos si el movimiento realiza una combinacion.
              //Lo comprobamos en el tablero resultante de hacer el movimiento
              val haceCombinacion = detectarCombinacionesCambio(tableroNuevo,fila,columna,fila+1,columna)
              //Si la realiza lo permitimos
              if(haceCombinacion)
              {
                tableroNuevo
              }
              //Si no hace combinación, descartamos
              //el movimiento y devolvemos el tablero inicial
              //sin el cambio
              else
              {
                println("Ese movimiento no realiza ninguna combinación!\nDescartando...")
                tablero
              }
              
            }
            else{
              println("ERROR: No se puede mover hacia abajo una ficha en la última fila")
              //devuelvo el mismo tablero, no hago nada
              return tablero
            }
            
         case _ => 
           println("La entrada no es válida: "+ dir)
           //vuelvo a preguntar
           moverFicha(tablero,fila,columna)
       }
     }
   }
   
   /**
    * Interfaz adicional para moverFicha para comprobar si los 
    * movimientos que realizamos con moverFicha son "legales"
    */
   def moverFichaInterfaz(tablero: Tablero) : Tablero =
   {
     println("Seleccione coordenadas de la ficha a mover:\n>>>Fila: ")
       val fila = readInt()
     println(">>>Columna: ")
       val col = readInt()
       
     //Comprobacion de si las coordenadas son incorrectas
     if(fila < 0 || col < 0 || fila >= tablero.filas || col >= tablero.columnas)
     {
       print("Coordenadas introducidas no válidas")
       //vuelvo a lanzar la funcion
       moverFichaInterfaz(tablero)
     }
     //si son correctams, realizo el movimiento
     else
     {
       //movemos la ficha
       moverFicha(tablero,fila,col)
       
     }
   }
   
   /**
    * BORRADO FICHAS
    */
   
   /**
    * Borra la ficha en la fila y columna especificada.
    * Devuelve el tablero resultante de realizar la eliminación de la ficha
    */
   def borrarFicha(fila: Int, columna : Int, tablero : Tablero, gravedadEspecial:Int) : Tablero =
   {
       val tableroEditado = poner(-1, fila, columna, tablero)
       // tendriamos que mirar por aqui si toda la columna es -1... por ahora siempre bajaremos las fichas superiores.
       //val haciaDerecha = 0 // esta variable le tendriamos que asignar comprobarColumna (si la columna entera es combinacion) en vez de 0
       val haciaDerecha = gravedadEspecial
       println("Imprimo el tablero con el -1 en su posicion")
       imprimirTablero(tableroEditado)
       if (espera) Thread.sleep(750)
       
       val tableroMovido = subirFicha(tableroEditado,fila,columna,haciaDerecha)
       println("Imprimo el tablero con el -1 movido donde tiene que estar")
       imprimirTablero(tableroMovido)
       if (espera) Thread.sleep(750)
       
       println("Imprimo el tablero habiendo sustituido el -1 por un valor al azar")
       if(haciaDerecha == 0) {
         // lo hemos subido arriba del todo, asi que sustituimos la ficha de arriba del todo
         val tableroFinal = ponerAleatorio(0, columna, tableroMovido)
         imprimirTablero(tableroFinal)
         if (espera) Thread.sleep(750)
         tableroFinal
       } else {
         //lo hemos movido a la izquierda del todo, asi que sustituimos la ficha de la izquierda del todo
         val tableroFinal = ponerAleatorio(fila, 0, tableroMovido)
         imprimirTablero(tableroFinal)
         if (espera) Thread.sleep(750)
         tableroFinal
       }
   }
   
   /**
    * Función que implementa la gravedad para mover una ficha hacia arriba(gravedad normal)
    * o hacia la izquierda si borramos una columna entera (gravedad especial).
    * 
    * cuantasQuedan = cuantas quedan por mover hacia la dirección.
    */
   def subirFicha(tablero : Tablero, fila: Int, columna : Int, direccion : Int) : Tablero = {
       if(direccion == 0) {
           if(fila == 0) {
             tablero
           } else {
             subirFicha(swap(tablero, fila, columna, fila - 1, columna),fila - 1, columna, 0)  
           }
       } else {
           if(columna == 0) {
             tablero
           } else {
             subirFicha(swap(tablero, fila, columna, fila, columna - 1),fila, columna - 1, 1)         
           }
       }
   }
   
   /**
    * Funcion que utilizaremos para borrar combinaciones de 3 o más fichas. 
    * detectarCombinaciones nos devuelve listas con las fichas que forman las 
    * diferentes combinaciones en el tablero. Esta función recibirá una lista
    * de casillas como argumento y borrará las fichas en las mismas.
    * 
    * ATENCION: EN ESTA FUNCION TAMBIEN INCREMENTAREMOS EL CONTADOR DEL JUEGO
    * 
    * @param tablero: tablero del juego
    * @param combinacion: lista con las casillas que forman la combinación
    * @return Devuelve un tablero con la combinación borrada
    */
   def borrarCombinacion(tablero:Tablero, combinacion : List[Int], gravedadEspecial:Int) : Tablero = {
     //si quedan fichas por borrar, las borramos
     if(!combinacion.isEmpty)
     {
        //BORRAMOS LA FICHA 
        //AQUI CON UN IF -> si la longitud a borrar es igual 
        //a la longitud de las columnas del tablero, borraremos las
        //fichas de esa columna con la gravedad especial.
        //Deberemos modificar a borrarFicha para que reciba como
        //parámetro si debe utilizar la gravedad especial o no
        if(combinacion.length == tablero.filas && combinacion(1)-combinacion(0)>1
            || gravedadEspecial == 1)
        {
            //transPosicion(tablero, combinacion.head)(0) -> fila
            //transPosicion(tablero, combinacion.head)(1) -> columna
            imprimirTablero(tablero)
            val nuevoTablero = borrarFicha(transPosicion(tablero, combinacion.head)(0) //Fila
                ,transPosicion(tablero, combinacion.head)(1)        //Columna
                ,tablero,1)//1 como ultimo parametro pues la gravedad es especila, hacia la izquierda
            
            //AUMENTO PUNTUACION. 25 puntos por cada ficha
            val nuevaPuntuacion = nuevoTablero.puntuacion + 25
            val otroTablero = new Tablero(nuevoTablero.tablero
                ,nuevoTablero.filas, nuevoTablero.columnas
                ,nuevoTablero.dificultad
                ,nuevaPuntuacion
                ,nuevoTablero.combinaciones)
                
            //llamada recursiva
            //1 como ultimo parametro pues la gravedad es especial, hacia la izquierda
            borrarCombinacion(otroTablero, combinacion.tail,1)
        }
        //Como else tendremos el borrado normal
        else
        {
            //transPosicion(tablero, combinacion.head)(0) -> fila
            //transPosicion(tablero, combinacion.head)(1) -> columna
            imprimirTablero(tablero)
            val nuevoTablero = borrarFicha(transPosicion(tablero, combinacion.head)(0) //Fila
                ,transPosicion(tablero, combinacion.head)(1)        //Columna
                ,tablero,0)//0 como ultimo parametro pues la gravedad es normal
            
            //AUMENTO PUNTUACION. 25 puntos por cada ficha
            val nuevaPuntuacion = nuevoTablero.puntuacion + 25
            val otroTablero = new Tablero(nuevoTablero.tablero
                ,nuevoTablero.filas, nuevoTablero.columnas
                ,nuevoTablero.dificultad
                ,nuevaPuntuacion
                ,nuevoTablero.combinaciones)
                
            //llamada recursiva
            //0 como ultimo parametro pues la gravedad es normal
            borrarCombinacion(otroTablero, combinacion.tail,0)
        }
     }
     //si no, acabamos la recursividad y devolvemos el tablero
     else
     {
       tablero
     }
   }
   
   
   /**
    * COMBINACIONES FICHAS PARA BORRAR
    */
   
   /**
    * Devuelve una lista  con las casillas consecutivas de una fila que contienen
    * la misma gema. La primera casilla será la que tenga el indice "pos" especificado
    * en los parámetros
    */
   def detectarCombinacionesFilasRecRec(tablero:Tablero, pos:Int) : List[Int] = {
     //si la siguiente posicion es igual a la actual, y no estoy en la ultima columna
     if( !(pos%(tablero.columnas)==tablero.columnas-1) && tablero.tablero(pos+1) == tablero.tablero(pos))
     {
       pos :: detectarCombinacionesFilasRecRec(tablero,pos+1)
     }
     else
     {
       pos :: Nil
     }
     
   }
   
   /**
    * Devuelve una lista  con las casillas consecutivas de una columna que contienen
    * la misma gema. La primera casilla será la que tenga el indice "pos" especificado
    * en los parámetros
    */
   def detectarCombinacionesColumnasRecRec(tablero:Tablero, pos:Int) : List[Int] = {
     //si la siguiente posicion es igual a la actual, y no estoy en la ultima fila
     //if( (pos < (tablero.filas*(tablero.columnas-1))) && tablero.tablero(pos+tablero.columnas) == tablero.tablero(pos))
     if( (pos + tablero.columnas  < (tablero.filas*tablero.columnas)-1) 
         && tablero.tablero(pos+tablero.columnas) == tablero.tablero(pos))
     {
       pos :: detectarCombinacionesColumnasRecRec(tablero,pos+tablero.columnas)
     }
     else
     {
       pos :: Nil
     }
     
   }
   
   /**
    * Detecta todas las combinaciones en filas del tablero y las borra.
    * También aumenta el contador de combinaciones hechas
    */
   //Borra las combinaciones directamente
   def borrarCombinacionesFilasRec(tablero:Tablero, indice:Int) : Tablero = {
     //SI aún me quedan casillas por visitar
     if(indice < (tablero.filas*tablero.columnas)-1)
     {
       val res = detectarCombinacionesFilasRecRec(tablero,indice)
       
       //Si la combinaciones es de tres o mas, muestro y borro
       if(res.length >=3)
       {
         println("Posiciones a borrar para indice = " + indice +"\\"
             + equivalenciaNumeroColor(tablero.tablero(indice))+ " Borramos : " +res)
         
         //DEBUG
         println("Coordenadas transformadas")
         transListaPos(tablero,res)
         println("--------------------------------")
         
         //Insertar codigo para borrar
         val nuevoTablero = borrarCombinacion(tablero, res, 0)
         
         //Incremento el contador de combinaciones hechas
         val otroTablero = new Tablero(nuevoTablero.tablero
            ,nuevoTablero.filas, nuevoTablero.columnas
            ,nuevoTablero.dificultad
            ,nuevoTablero.puntuacion
            ,nuevoTablero.combinaciones+1)
         
         //DEBUG
         println("Antes de borrar "+ res)
         imprimirTablero(tablero)
         println("Despues de borrar " + res )
         imprimirTablero(otroTablero)
         
         //llamada recursiva con las fichas borradas
         borrarCombinacionesFilasRec(otroTablero, indice+1)
         
       }
       //si no,
       //paso a la siguiente itereacion
       else
       {
         borrarCombinacionesFilasRec(tablero, indice+1)
       }
     }
     //si no, acabo y devuelvo tablero
     else
     {
       println("fin de detectar combinaciones")
       //devuelvo el tablero final
       tablero
     }
   }
   
    /**
    * Detecta todas las combinaciones en columnas del tablero y las borra.
    * También aumenta el contador de combinaciones hechas
    */
   //Borra las combinaciones directamente
   def borrarCombinacionesColumnasRec(tablero:Tablero, indice:Int) : Tablero = {
     //SI aún me quedan casillas por visitar
     if(indice < (tablero.filas*tablero.columnas)-1)
     {
       val res = detectarCombinacionesColumnasRecRec(tablero,indice)
       
       //Si la combinaciones es de tres o mas, muestro y borro
       if(res.length >=3)
       {
         println("Posiciones a borrar para indice = " + indice +"\\"
             + equivalenciaNumeroColor(tablero.tablero(indice))+ " Borramos : " +res)
         
         //DEBUG
         println("Coordenadas transoformadas")
         transListaPos(tablero,res)
         println("--------------------------------")

         //Insertar codigo para borrar
         val nuevoTablero = borrarCombinacion(tablero, res, 0)
         
         //Incremento el contador de combinaciones hechas
         val otroTablero = new Tablero(nuevoTablero.tablero
            ,nuevoTablero.filas, nuevoTablero.columnas
            ,nuevoTablero.dificultad
            ,nuevoTablero.puntuacion
            ,nuevoTablero.combinaciones+1)
         
         
         //DEBUG
         println("Antes de borrar "+ res)
         imprimirTablero(tablero)
         println("Despues de borrar " + res )
         imprimirTablero(otroTablero)
         
         //llamada recursiva con las fichas borradas
         borrarCombinacionesColumnasRec(otroTablero, indice+1)
             
       }
       //si no,
       //paso a la siguiente itereacion
       else
       {
         borrarCombinacionesColumnasRec(tablero, indice+1)
       }
     }
     //si no, acabo y devuelvo tablero
     else
     {
       println("fin de detectar combinaciones")
       //devuelvo el tablero final
       tablero
     }
   }
   
   /**
    * Interfaz simple para detectar combinaciones en todo el tablero.
    * Devuelve el tablero tras haber borrado todas las combinaciones.
    * Se utilizará la función detectarCombinacionesTablero() para
    * detectar si existen combianciones en el tablero. De ser así,
    * se llamará a esta función para borrarlas.
    */
   def borrarCombinacionesTablero(tablero:Tablero) : Tablero = {
     println("Detectando filas...")
     val otroTablero = borrarCombinacionesFilasRec(tablero, 0)
     
     println("Detectando columnas...")
     val otroTableroMas = borrarCombinacionesColumnasRec(otroTablero,0)
     return otroTableroMas
   }
   
   //----------SOLO DETECTAR_NO BORRAR
   
   
   /**
    * Detecta si al hacer un cambio se crea una combinación en la fila,
    * dada la fila y columna de la casilla que hemos intercambiado.
    */
   def detectarCombinacionesFila(tablero:Tablero, fila:Int, col:Int) : Boolean = {
     //si la ficha de la izquierda tiene el mismo valor, empezamos desde ella
     //es decir, nos posicionamos a la izquierda del todo
     if(col>0 
         && tablero.tablero(transCoordenadas(tablero,fila,col))
             == tablero.tablero(transCoordenadas(tablero,fila,col-1))
       )
     {
       detectarCombinacionesFila(tablero,fila,col-1)      
     }
     //si no, estaremos ya a la izquierda del todo, por lo que
     //empezamos a contar
     else
     {
       val combinacion = detectarCombinacionesFilasRecRec(tablero
           ,transCoordenadas(tablero,fila,col))
       //si la ongitud de la combinacion es mayor o igual a 3, significa que
       //hemos hecho una combinación
       //println("FILA: La combinación encontrada es: " + combinacion)  
       if(combinacion.length>=3)
       {
         true
       }
       else
       {
         false
       }
     }
     
   }
   
   /**
    * Detecta si al hacer un cambio se crea una combinación en la columna,
    * dada la fila y columna de la casilla que hemos intercambiado.
    */
   def detectarCombinacionesColumna(tablero:Tablero, fila:Int, col:Int) : Boolean = {
     //si la ficha de arriba tiene el mismo valor, empezamos desde ella
     //es decir, nos posicionamos arriba del todo
     if(fila>0 
         && tablero.tablero(transCoordenadas(tablero,fila,col))
             == tablero.tablero(transCoordenadas(tablero,fila-1,col))
       )
     {
       detectarCombinacionesColumna(tablero,fila-1,col)      
     }
     //si no, estaremos ya arriba del todo, por lo que
     //empezamos a contar
     else
     {
       val combinacion = detectarCombinacionesColumnasRecRec(tablero,transCoordenadas(tablero,fila,col))
       //si la longitud de la combinacion es mayor o igual a 3, significa que
       //hemos hecho una combinación
       //println("COL: La combinación encontrada es: " + combinacion)
       if(combinacion.length>=3)
       {
         true
       }
       else
       {
         false
       }
     }
     
   }
   
   
   /**
    * Detecta si hacer un cambio acarreará la creación
    * de una combinación.
    * 
    * Esta función será llamada con el tablero creado
    * tras el cambio ("swap") como parámetro.
    * 
    * Comprobaremos si de las dos fichas que se han intercambiado,
    * alguna crea una combinación de filas o columnas de 3 o más
    */
   def detectarCombinacionesCambio(tablero:Tablero
       ,fila1:Int,col1:Int,fila2:Int,col2:Int): Boolean = 
   {
     return (
         detectarCombinacionesFila(tablero,fila1,col1)
         ||
         detectarCombinacionesFila(tablero,fila2,col2)
         ||
         detectarCombinacionesColumna(tablero,fila1,col1)
         ||
         detectarCombinacionesColumna(tablero,fila2,col2)
     )
   }
   
   /**
    * Detecta si hay combinaciones en un tablero dado.
    * Esta función la utilizaremos para detectar si debemos
    * llamar al método borrarCombinacionesTablero()
    */
   def detectarCombinacionesTablero(tablero:Tablero, posicion: Int) : Boolean = {
     //Si estoy en la última posición, significa que no se ha encontrado
     //ninguna combinación pues si no la función ya habría acabado y no
     //legaría a este punto
     if(posicion >= (tablero.filas*tablero.columnas)-1)
     {
       false //No hay ninguna combinación en el tablero
     }
     //Si estoy en una casilla cualquiera, compruebo si para 
     //esa casilla existe una combinación de algún tipo
     else if(detectarCombinacionesFila(tablero
               ,transPosicion(tablero,posicion)(0)//fila
               ,transPosicion(tablero,posicion)(1))//columna
             ||
             detectarCombinacionesColumna(tablero
               ,transPosicion(tablero,posicion)(0)//fila
               ,transPosicion(tablero,posicion)(1))//columna
             )
     {
       //DEBUG
       println("En la pos: " + posicion +" hay una comb.")
       true //Si es asi, existen combinaciones en el tablero
     }
     //Si no se da ninguna de las situadiones anteriores,
     //continuo iterando a través del tablero
     else
     {
       detectarCombinacionesTablero(tablero, posicion+1)
     }
   }
   
   
   /*
    * Métodos para aplicar el sistema de guardado.
    */
   
   /**
    * Guarda un fichero para representar el tablero y poder cargar el mismo tablero y la puntuación que llevamos.
    */
   def guardarTablero(tablero:Tablero) : Unit = {
     val out = new PrintWriter("partida.txt");
     out.println(tableroToString(tablero));
     out.close();
   }
   
   /**
    * Carga en el programa el tablero que se encuentra
    * guardado en el fichero partida.txt en la carpeta 
    * raíz del programa
    */
  def cargarTablero(tablero:Tablero) : Tablero = {
     
     val archivo = new File("partida.txt")
     if(archivo.exists()) {
       // Creamos el reader
       val reader = new BufferedReader(new FileReader(archivo));
       // Obtenemos el string que representa al tablero y su info.
       val string = reader.readLine();
       // Convertimos ese string en una lista, separando sus campos según el ' ' (espacio)
       val lista = string.split(" ").map(_.toInt).toList;
       // El primer elemento serán las filas, el segundo las columnas, el tercero la dificultad,
       // el cuarto la puntuacion, el quinto las combinaciones. A partir del sexto (incluido),
       // cada valor representará a la ficha en cada posición. Por lo tanto, habrá 5 + filas*columnas
       // elementos en la lista.
     
       // Quitamos los primeros 5 elementos (asi que solo quedarán los que representan las fichas)
       val soloTablero = lista.drop(5)
     
       new Tablero(soloTablero,lista(0),lista(1),lista(2),lista(3),lista(4));
       
     } else {
       println("No se ha encontrado la partida guardada.")
       tablero
     }
   }
   
   /**
    * Convierte el tablero a un String para
    * poder almacenarlo como texto en un fichero
    */
   def tableroToString(tablero : Tablero) : String = {
      val val1 = tablero.filas + " " + tablero.columnas + " " + tablero.dificultad + " " + tablero.puntuacion + " " + tablero.combinaciones;
      val val2 = val1 + listaToString(tablero.tablero);
      println("Imprimiendo lo que vamos a devolver con tableroToString");
      println(val2);
      val2
   }
   
   /**
    * Convierte la lista de enteros dada a un String.
    */
   def listaToString(lista : List[Int]) : String = {
     if(lista.isEmpty) {
       ""
     } else {
       " " + lista.head + listaToString(lista.tail)
     }
   }
   
   
   
}
