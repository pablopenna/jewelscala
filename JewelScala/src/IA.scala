

class IA(game : Game) {
  
  /**
    * INTELIGENCIA ARTIFICIAL
    */
   
   /**
    * Ejecuta las diferentes funciones relacionadas
    * con la IA de forma que se detecta la mejor
    * jugada a realizar posible y se hace la misma,
    * obteniendo el tablero resultante a partir de la
    * llamada a esta funcion
    */
   def llamarIA(tablero:Tablero) : Tablero =
   {
     //DERECHA
     println("\n>>>Probando IA der...")
     val jugadaDer = iaDerRec1(tablero,0,0,0,0,0,0)
     println("La MEJOR jugada a derechas es : " + jugadaDer + "| LONG : " +jugadaDer(4))
     
     //IZQUIERDA
     println("\n>>>Probando IA izq...")
     val jugadaIzq = iaIzqRec1(tablero,(tablero.filas*tablero.columnas)-1,0,0,0,0,0)
     println("La MEJOR jugada a izquierdas es : " + jugadaIzq + "| LONG : " +jugadaIzq(4))
     
     //ABAJO
     println("\n>>>Probando IA abajo...")
     val jugadaAbajo = iaDownRec1(tablero,0,0,0,0,0,0)
     println("La MEJOR jugada hacia abajo es : " + jugadaAbajo + "| LONG : " +jugadaAbajo(4))
     
     //ARRIBA
     println("\n>>>Probando IA arriba...")
     val jugadaArriba = iaUpRec1(tablero,(tablero.filas*tablero.columnas)-1,0,0,0,0,0)
     println("La MEJOR jugada hacia arriba es : " + jugadaArriba + "| LONG : " +jugadaArriba(4))
     
     //Obetener la mejor jugada de las 4
     val mejorJugada = maxJugada( 
         maxJugada(jugadaDer, jugadaIzq) 
         , maxJugada(jugadaAbajo, jugadaArriba)  )
         
     println("Jugada DEFINITIVA : " + mejorJugada)
     
     println("\nAntes de:")
     game.imprimirTablero(tablero)
     val nuevoTablero = ejecutarJugada(tablero,mejorJugada)
     println("\nDespues de:")
     game.imprimirTablero(nuevoTablero)
     
     return nuevoTablero
   }
   
   //Se le pasan dos jugadas y devuelve la mejor.
   //Se da por sentado que el formato de las listas
   //que forman las jugadas es el siguiente:
   // [ fila1 | columna1 | fila2 | columna2 | longitud jugada ]
   //por lo que en jugada(4) tendremos la longitud de la jugada
   def maxJugada(jugada1 : List[Int], jugada2: List[Int]) : List[Int] = 
   {
      if(jugada1(4) >= jugada2(4))
      {
        jugada1
      }
      else
      {
        jugada2
      }
   }
   
   /**
    * Recibe una jugada con el siguiente formato:
    * [ fila1 | columna1 | fila2 | columna2 | longitud jugada ]
    * y la ejecuta en el tablero especificado, devolviendo el
    * tablero resultante
    */
   def ejecutarJugada(tablero:Tablero, jugada: List[Int]) : Tablero =
   {
     val nuevoTablero = game.swap(tablero, jugada(0), jugada(1), jugada(2), jugada(3))
     return nuevoTablero
   }
   //-------------------------------------------------------------------------------
   //DERECHA
   /**
    * @param tablero: tablero a través del cual se 
    * buscarán las jugadas
    * @param indice: indice de la casilla para la cual buscaremos
    * jugada en la iteración actual
    * @param fila1: fila de la primera casilla
    * @param columna1: columna de la primera casilla
    * @param fila2 : fila de la segunda casilla
    * @param columna2: columna de la segunda casilla
    * @param maxLong: longitud de la mejor jugada encontrada hasta ahora
    * 
    * Itera a través del todo el tablero barriendo hacia la
    * derecha, en busca de la mejor jugada posible
    * 
    * Cuando la funcion haya acabado de iterar,
    * la mejor jugada la realizaremos intecambiando las fichas
    * en [fila1,columna1] y [fila2,columna2]. 
    * Tendremos que devolverlo como una lista   */
   def iaDerRec1(tablero: Tablero, indice: Int
       , fila1 : Int, columna1 : Int
       , fila2 : Int, columna2 : Int
       , maxLong : Int) : List[Int] = 
   {
     if(indice < (tablero.filas*tablero.columnas)-1 )
     {
           val fichaActual = tablero.tablero(indice)
           //me situa en la primera casilla a la derecha
           //que tenga un valor diferente
           val contador = iaDerRec2(tablero,indice+1,fichaActual,1)
           //posicion de la primera casilla con valor diferente.
           val casillaDiff = indice + contador
           val filaDiff = game.transPosicion(tablero, casillaDiff)(0)
           val colDiff = game.transPosicion(tablero, casillaDiff)(1)
           
           //legados a este punto estaré con casillaDiff indicandome 
           //una joya diferente
           //o al final de linea
           //si estamos al final de linea no hacemos nada
           if(
               (contador >=2 
                   || 
                 (colDiff <= tablero.columnas-2
                 && colDiff > 0
                 && (tablero.tablero(casillaDiff-1)
                   ==tablero.tablero(casillaDiff+1))
                 )
               )
               //seguridad
               && filaDiff < tablero.filas
               && colDiff < tablero.columnas
               && filaDiff >= 0
               && colDiff >=0
               //fin seguridad
               && tablero.tablero(casillaDiff)!=fichaActual)
           {
                 //println("->Comprobando para ["
                 //   +filaDiff + "," + colDiff 
                 //   + "] con contador = " + contador)
                 //------------------------------------------------   
                 //miro arriba
                 if( (filaDiff > 0)
                     &&
                     (tablero.tablero(
                     game.transCoordenadas(tablero,filaDiff-1,colDiff))
                     == fichaActual) )
                 {
                   //println("swap arriba")
                   //calcular verdadera longitud de la jugada
                   //contador mas 1, pues si realizamos el swap tendriamos
                   //tres fichas alineadas.
                   val longJugada = iaDerRec3(tablero,casillaDiff+1,fichaActual,contador+1)
                   //println("La verdadera longitud para indice = "+ indice + " es : " + longJugada)
                   //guardar jugada 
                   //llamada recursiva
                   //---
                   //si la jugada es mejor, la guardamos
                   if(longJugada > maxLong)
                   {
                     //println("Nueva mejor jugada!")
                     //llamada recursiva, guardando la jugada actual
                     iaDerRec1(tablero,indice+1
                       ,filaDiff,colDiff,filaDiff-1,colDiff
                       ,longJugada)
                   }
                   else
                   {
                     //llamada recursiva, descartando jugada actual
                     iaDerRec1(tablero,indice+1
                       ,fila1,columna1,fila2,columna2
                       ,maxLong)
                   }
                 }
                 //------------------------------------------------
                 //miro abajo
                 else if( (filaDiff + 1 < tablero.filas)
                     &&
                     (tablero.tablero(
                     game.transCoordenadas(tablero,filaDiff+1,colDiff))
                     == fichaActual) )
                 {
                   //println("swap abajo")
                   //calcular verdadera longitud de la jugada
                   //contador mas 1, pues si realizamos el swap tendriamos
                   //tres fichas alineadas.
                   val longJugada = iaDerRec3(tablero,casillaDiff+1,fichaActual,contador+1)
                   //println("La verdadera longitud para indice = "+ indice + " es : " + longJugada)
                   //guardar jugada 
                   //llamada recursiva
                   //---
                   //si la jugada es mejor, la guardamos
                   if(longJugada > maxLong)
                   {
                     //println("Nueva mejor jugada!")
                     //llamada recursiva, guardando la jugada actual
                     iaDerRec1(tablero,indice+1
                       ,filaDiff,colDiff,filaDiff+1,colDiff
                       ,longJugada)
                   }
                   else
                   {
                     //llamada recursiva, descartando jugada actual
                     iaDerRec1(tablero,indice+1
                       ,fila1,columna1,fila2,columna2
                       ,maxLong)
                   }
                 }
                 //------------------------------------------------
                 //miro derecha
                 else if( (colDiff + 1 < tablero.columnas)
                     &&
                     (tablero.tablero(
                     game.transCoordenadas(tablero,filaDiff,colDiff+1))
                     == fichaActual)
                     &&
                     contador >=2 )
                 {
                   //println("swap derecha")
                   //calcular verdadera longitud de la jugada
                   //si hacemos el movimiento en la misma direccion
                   //en la que estamos buscando, la longitud de la 
                   //jugada será siempre contador + 1
                   val longJugada = contador + 1
                   //println("La verdadera longitud para indice = "+ indice + " es : " + longJugada)
                   //guardar jugada 
                   //llamada recursiva
                   //---
                   //si la jugada es mejor, la guardamos
                   if(longJugada > maxLong)
                   {
                     //println("Nueva mejor jugada!")
                     //llamada recursiva, guardando la jugada actual
                      return iaDerRec1(tablero,indice+1
                       ,filaDiff,colDiff,filaDiff,colDiff+1
                       ,longJugada);
                   }
                   else
                   {
                     //llamada recursiva, descartando jugada actual
                     return iaDerRec1(tablero,indice+1
                       ,fila1,columna1,fila2,columna2
                       ,maxLong);
                   }
                 //------------------------------------------------
                 //Si llego aqui, no hay jugada
                 //mirando arriba, abajo y derecha
                 }
                 else
                 {
                 //llamada recursiva, no hay jugada 
                 iaDerRec1(tablero,indice+1
                   ,fila1,columna1,fila2,columna2
                   ,maxLong)
                 }
         }
         //No se cumplen los requisitos para
         //que haya una jugada posible
         else
         {
           //llamada recursiva, no hay jugada 
           iaDerRec1(tablero,indice+1
               ,fila1,columna1,fila2,columna2
               ,maxLong)
         }
     }
     //ya he inspeccionado todo
     //devuelvo resultado
     else
     {
       List(fila1,columna1,fila2,columna2,maxLong)
     }
   }
   
   /**
    * Indica la longitud de una combinacion. Se utiliza
    * antes de realizar ningún cambio, para detectar
    * posibles configuraciones de fichas que puedan
    * llegar a componer una posible jugada.
    * 
    * @param indice: posicion a comprobar
    * @param contador: cuenta el numero de fichas 
    * iguales que llevo
    * 
    * Para saber cual es la primera ficha diferente, sumare 
    * al indice de iaDerRec1 el contador, lo que me dara el 
    * offset de la primera ficha con valor diferente.
    * 
    * En la llamada incial de esta funcion, contador tendra valor 1
    * e indice tendra el valor de indice + 1 (empiezo contando en
    * la siguiente casilla, con contador mas 1 pues donde he 
    * empezado a contar ya tiene el valor deseado)
    * 
    * @return contador
    */
   def iaDerRec2(tablero:Tablero, indice:Int, valorActual:Int
       ,contador:Int) :Int = 
   {
       //Si la ficha en la que estoy tiene
       // el mismo valor, no esta en la ultima columna
       // y no esta en la primera columna => contador++
       if(!(indice%(tablero.columnas)==tablero.columnas-1) && 
           !(indice%(tablero.columnas)==0)
           //seguridad
           && indice < tablero.filas * tablero.columnas 
           && indice >=0
           //fin seguridad
           && tablero.tablero(indice) == valorActual)
       {
         iaDerRec2(tablero,indice+1,valorActual,contador+1)
       }
       //si no,devuelvo el contador
       else
       {
         contador
       }
   }
   
   /**
    * Indica la longitud de una combinacion una vez se ha realizado la jugada.
    * Obtenemos la longitud de la combinacion resultante de hacer los cambios
    * adecuados. A diferencia de iaDerRec2, tiene en cuenta las fichas con
    * el mismo valor aunque se encuentren en la última columna
    */
   def iaDerRec3(tablero:Tablero, indice:Int, valorActual:Int
       ,contador:Int) :Int = 
   {
       //Si la ficha en la que estoy tiene
       // el mismo valor, no esta en la ultima columna
       // y no esta en la primera columna => contador++
       if(!(indice%(tablero.columnas)==tablero.columnas-1) && 
           !(indice%(tablero.columnas)==0)
           //seguridad
           && indice < tablero.filas * tablero.columnas 
           && indice >=0
           //fin seguridad
           && tablero.tablero(indice) == valorActual)
       {
         iaDerRec3(tablero,indice+1,valorActual,contador+1)
       }
       //si no,devuelvo el contador
       else
       {
         if((indice%(tablero.columnas)==tablero.columnas-1) 
             //seguridad
             && indice < tablero.filas * tablero.columnas 
             && indice >=0
             //fin seguridad 
             && tablero.tablero(indice) == valorActual)
         {
           return (contador + 1)
         }
         else
         {
           return contador
         }
       }
   }
   
   //-------------------------------------------------------------------------------
   //IZQUIERDA
   //profundidad 1
   //Itera a traves de todo el tablero
   /**
    * @param tablero: tablero a través del cual se 
    * buscarán las jugadas
    * @param indice: indice de la casilla para la cual buscaremos
    * jugada en la iteración actual
    * @param fila1: fila de la primera casilla
    * @param columna1: columna de la primera casilla
    * @param fila2 : fila de la segunda casilla
    * @param columna2: columna de la segunda casilla
    * @param maxLong: longitud de la mejor jugada encontrada hasta ahora
    * 
    * Itera a través del todo el tablero barriendo hacia la
    * izquierda, en busca de la mejor jugada posible
    * 
    * Cuando la funcion haya acabado de iterar,
    * la mejor jugada la realizaremos intecambiando las fichas
    * en [fila1,columna1] y [fila2,columna2]. 
    * Tendremos que devolverlo como una lista   */
   def iaIzqRec1(tablero: Tablero, indice: Int
       , fila1 : Int, columna1 : Int
       , fila2 : Int, columna2 : Int
       , maxLong : Int) : List[Int] = 
   {
     if(indice < (tablero.filas*tablero.columnas) 
         && indice > 0)
     {
           val fichaActual = tablero.tablero(indice)
           //me situa en la primera casilla a la izquierda
           //que tenga un valor diferente
           val contador = iaIzqRec2(tablero,indice-1,fichaActual,1)
           //posicion de la primera casilla con valor diferente.
           val casillaDiff = indice - contador
           val filaDiff = game.transPosicion(tablero, casillaDiff)(0)
           val colDiff = game.transPosicion(tablero, casillaDiff)(1)
           
           //legados a este punto estaré con k indicandome 
           //una joya diferente
           //o al final de linea
           //si estamos al final de linea no hacemos nada
           //columnas-1 por la tranformacion, ya que empezamos 
           //contando columnas desde 0
           if(
               (contador >=2 
                   || 
                 (colDiff <= tablero.columnas-2
                 && colDiff > 0
                 && (tablero.tablero(casillaDiff-1)
                   ==tablero.tablero(casillaDiff+1))
                 )
               )
               //seguridad
               && filaDiff < tablero.filas
               && colDiff < tablero.columnas
               && filaDiff >= 0
               && colDiff >=0
               //fin seguridad
              && tablero.tablero(casillaDiff)!=fichaActual)
           {
                 //println("->Comprobando para ["
                 //   +filaDiff + "," + colDiff 
                 //   + "] con contador = " + contador)
                 //------------------------------------------------   
                 //miro arriba
                 if( (filaDiff > 0)
                     &&
                     (tablero.tablero(
                     game.transCoordenadas(tablero,filaDiff-1,colDiff))
                     == fichaActual) )
                 {
                   //println("swap arriba")
                   //calcular verdadera longitud de la jugada
                   //contador mas 1, pues si realizamos el swap tendriamos
                   //tres fichas alineadas.
                   val longJugada = iaIzqRec3(tablero,casillaDiff-1,fichaActual,contador+1)
                   //println("La verdadera longitud para indice = "+ indice + " es : " + longJugada)
                   //guardar jugada 
                   //llamada recursiva
                   //---
                   //si la jugada es mejor, la guardamos
                   if(longJugada > maxLong)
                   {
                     //println("Nueva mejor jugada!")
                     //llamada recursiva, guardando la jugada actual
                     iaIzqRec1(tablero,indice-1
                       ,filaDiff,colDiff,filaDiff-1,colDiff
                       ,longJugada)
                   }
                   else
                   {
                     //llamada recursiva, descartando jugada actual
                     iaIzqRec1(tablero,indice-1
                       ,fila1,columna1,fila2,columna2
                       ,maxLong)
                   }
                 }
                 //------------------------------------------------
                 //miro abajo
                 else if( (filaDiff + 1 < tablero.filas)
                     &&
                     (tablero.tablero(
                     game.transCoordenadas(tablero,filaDiff+1,colDiff))
                     == fichaActual) )
                 {
                   //println("swap abajo")
                   //calcular verdadera longitud de la jugada
                   //contador mas 1, pues si realizamos el swap tendriamos
                   //tres fichas alineadas.
                   val longJugada = iaIzqRec3(tablero,casillaDiff-1,fichaActual,contador+1)
                   //println("La verdadera longitud para indice = "+ indice + " es : " + longJugada)
                   //guardar jugada 
                   //llamada recursiva
                   //---
                   //si la jugada es mejor, la guardamos
                   if(longJugada > maxLong)
                   {
                     //println("Nueva mejor jugada!")
                     //llamada recursiva, guardando la jugada actual
                     iaIzqRec1(tablero,indice-1
                       ,filaDiff,colDiff,filaDiff+1,colDiff
                       ,longJugada)
                   }
                   else
                   {
                     //llamada recursiva, descartando jugada actual
                     iaIzqRec1(tablero,indice-1
                       ,fila1,columna1,fila2,columna2
                       ,maxLong)
                   }
                 }
                 //------------------------------------------------
                 //miro izquierda
                 else if( (colDiff > 0)
                     &&
                     (tablero.tablero(
                     game.transCoordenadas(tablero,filaDiff,colDiff-1))
                     == fichaActual)
                     &&
                     contador >=2 )
                 {
                   //println("swap izquierda")
                   //calcular verdadera longitud de la jugada
                   //si hacemos el movimiento en la misma direccion
                   //en la que estamos buscando, la longitud de la 
                   //jugada será siempre contador + 1
                   val longJugada = contador + 1
                   //println("La verdadera longitud para indice = "+ indice + " es : " + longJugada)
                   //guardar jugada 
                   //llamada recursiva
                   //---
                   //si la jugada es mejor, la guardamos
                   if(longJugada > maxLong)
                   {
                     //println("Nueva mejor jugada!")
                     //llamada recursiva, guardando la jugada actual
                      return iaIzqRec1(tablero,indice-1
                       ,filaDiff,colDiff,filaDiff,colDiff-1
                       ,longJugada);
                   }
                   else
                   {
                     //llamada recursiva, descartando jugada actual
                     return iaIzqRec1(tablero,indice-1
                       ,fila1,columna1,fila2,columna2
                       ,maxLong);
                   }
                 //------------------------------------------------
                 //Si llego aqui, no hay jugada
                 //mirando arriba, abajo e izquierda
                 }
                 else
                 {
                 //llamada recursiva, no hay jugada 
                 iaIzqRec1(tablero,indice-1
                   ,fila1,columna1,fila2,columna2
                   ,maxLong)
                 }
         }
         //No se cumplen los requisitos para
         //que haya una jugada posible
         else
         {
           //llamada recursiva, no hay jugada 
           iaIzqRec1(tablero,indice-1
               ,fila1,columna1,fila2,columna2
               ,maxLong)
         }
     }
     //ya he inspeccionado todo
     //devuelvo resultado
     else
     {
       List(fila1,columna1,fila2,columna2,maxLong)
     }
   }
   
   /**
    * Indica la longitud de una combinacion. Se utiliza
    * antes de realizar ningún cambio, para detectar
    * posibles configuraciones de fichas que puedan
    * llegar a componer una posible jugada.
    * 
    * @param indice: posicion a comprobar
    * @param contador: cuenta el numero de fichas 
    * iguales que llevo
    * 
    * Para saber cual es la primera ficha diferente, sumare 
    * al indice de iaIzqRec1 el contador, lo que me dara el 
    * offset de la primera ficha con valor diferente.
    * 
    * En la llamada incial de esta funcion, contador tendra valor 1
    * e indice tendra el valor de indice - 1 (empiezo contando en
    * la siguiente casilla hacia la izquierda, con contador más 1 
    * pues donde he empezado a contar ya tiene el valor deseado)
    * 
    * @return contador
    */
   def iaIzqRec2(tablero:Tablero, indice:Int, valorActual:Int
       ,contador:Int) :Int = 
   {
       //Si la ficha en la que estoy tiene
       // el mismo valor, no esta en la ultima columna
       // y no esta en la primera columna => contador++
       if(!(indice%(tablero.columnas)==tablero.columnas-1)
           && !(indice%(tablero.columnas)==0)
           //seguridad
           && indice < tablero.filas * tablero.columnas 
           && indice >=0
           //fin seguridad
           && tablero.tablero(indice) == valorActual)
       {
         iaIzqRec2(tablero,indice-1,valorActual,contador+1)
       }
       //si no,devuelvo el contador
       else
       {
         contador
       }
   }
   /**
    * Indica la longitud de una combinacion una vez se ha realizado la jugada.
    * Obtenemos la longitud de la combinacion resultante de hacer los cambios
    * adecuados. A diferencia de iaIzqRec2, tiene en cuenta las fichas con
    * el mismo valor aunque se encuentren en la primera columna
    */
   def iaIzqRec3(tablero:Tablero, indice:Int, valorActual:Int
       ,contador:Int) :Int = 
   {
       //Si la ficha en la que estoy tiene
       // el mismo valor, no esta en la ultima columna
       // y no esta en la primera columna => contador++
       if(!(indice%(tablero.columnas)==tablero.columnas-1)
           && !(indice%(tablero.columnas)==0)
           //seguridad
           && indice < tablero.filas * tablero.columnas 
           && indice >=0
           //fin seguridad
           && tablero.tablero(indice) == valorActual)
       {
         iaIzqRec3(tablero,indice-1,valorActual,contador+1)
       }
       //si no,devuelvo el contador
       else
       {
         if((indice%(tablero.columnas)==0) 
             //seguridad
             && indice < tablero.filas * tablero.columnas 
             && indice >=0
             //fin seguridad
             && tablero.tablero(indice) == valorActual)
         {
           return (contador + 1)
         }
         else
         {
           return contador
         }
       }
   }
   
   //-------------------------------------------------------------------------------
   //ABAJO
   //profundidad 1
   //Itera a traves de todo el tablero
   /**
    * @param tablero: tablero a través del cual se 
    * buscarán las jugadas
    * @param indice: indice de la casilla para la cual buscaremos
    * jugada en la iteración actual
    * @param fila1: fila de la primera casilla
    * @param columna1: columna de la primera casilla
    * @param fila2 : fila de la segunda casilla
    * @param columna2: columna de la segunda casilla
    * @param maxLong: longitud de la mejor jugada encontrada hasta ahora
    * 
    * Itera a través del todo el tablero barriendo hacia abajo,
    * en busca de la mejor jugada posible.
    * 
    * Cuando la funcion haya acabado de iterar,
    * la mejor jugada la realizaremos intecambiando las fichas
    * en [fila1,columna1] y [fila2,columna2]. 
    * Tendremos que devolverlo como una lista   */
   def iaDownRec1(tablero: Tablero, indice: Int
       , fila1 : Int, columna1 : Int
       , fila2 : Int, columna2 : Int
       , maxLong : Int) : List[Int] = 
   {
     if(indice < (tablero.filas*tablero.columnas)-1 
         && indice >= 0)
     {
           val fichaActual = tablero.tablero(indice)
           //me situa en la primera casilla a la derecha
           //que tenga un valor diferente
           val contador = iaDownRec2(tablero,indice+tablero.columnas,fichaActual,1)
           //posicion de la primera casilla con valor diferente.
           val casillaDiff = indice + contador*tablero.columnas
           val filaDiff = game.transPosicion(tablero, casillaDiff)(0)
           val colDiff = game.transPosicion(tablero, casillaDiff)(1)
           //DEBUG
           /*
           println("WARNING")
           println("indice " + indice)
           println("contador " + contador)
           println("casillaDiff " + casillaDiff)
           println("filaDiff " + filaDiff)
           println("columnaDiff " + colDiff)
           */
           
           //legados a este punto estaré con k indicandome 
           //una joya diferente
           //o al final de linea
           //si estamos al final de linea no hacemos nada
           //columnas-1 por la tranformacion, ya que empezamos 
           //contando columnas desde 0
           if(
               (contador >=2 
                   || 
                 (filaDiff <= tablero.filas-2
                 && filaDiff > 0
                 && (tablero.tablero(casillaDiff-tablero.columnas)
                   ==tablero.tablero(casillaDiff+tablero.columnas))
                 )
               )
              //seguridad
               && filaDiff < tablero.filas
               && colDiff < tablero.columnas
               && filaDiff >= 0
               && colDiff >=0
               //fin seguridad
              && tablero.tablero(casillaDiff)!=fichaActual)
           {
                 //println("->Comprobando para ["
                 //   +filaDiff + "," + colDiff 
                 //   + "] con contador = " + contador)
                 //------------------------------------------------   
                 //miro derecha
                 if( (colDiff + 1 < tablero.columnas)
                     &&
                     (tablero.tablero(
                     game.transCoordenadas(tablero,filaDiff,colDiff+1))
                     == fichaActual))
                 {
                   //println("swap derecha")
                   //calcular verdadera longitud de la jugada
                   val longJugada = iaDownRec2(tablero,casillaDiff+tablero.columnas,fichaActual,contador+1)
                   //println("La verdadera longitud para indice = "+ indice + " es : " + longJugada)
                   //guardar jugada 
                   //llamada recursiva
                   //---
                   //si la jugada es mejor, la guardamos
                   if(longJugada > maxLong)
                   {
                     //println("Nueva mejor jugada!")
                     //llamada recursiva, guardando la jugada actual
                      return iaDownRec1(tablero,indice+1
                       ,filaDiff,colDiff,filaDiff,colDiff+1
                       ,longJugada);
                   }
                   else
                   {
                     //llamada recursiva, descartando jugada actual
                     return iaDownRec1(tablero,indice+1
                       ,fila1,columna1,fila2,columna2
                       ,maxLong);
                   }
                 }
                 //------------------------------------------------   
                 //miro izquierda
                 else if( (colDiff > 0)
                     &&
                     (tablero.tablero(
                     game.transCoordenadas(tablero,filaDiff,colDiff-1))
                     == fichaActual) )
                 {
                   //println("swap izquierda")
                   //calcular verdadera longitud de la jugada
                   val longJugada = iaDownRec2(tablero,casillaDiff+tablero.columnas,fichaActual,contador+1)
                   //println("La verdadera longitud para indice = "+ indice + " es : " + longJugada)
                   //guardar jugada 
                   //llamada recursiva
                   //---
                   //si la jugada es mejor, la guardamos
                   if(longJugada > maxLong)
                   {
                     //println("Nueva mejor jugada!")
                     //llamada recursiva, guardando la jugada actual
                      return iaDownRec1(tablero,indice+1
                       ,filaDiff,colDiff,filaDiff,colDiff-1
                       ,longJugada);
                   }
                   else
                   {
                     //llamada recursiva, descartando jugada actual
                     return iaDownRec1(tablero,indice+1
                       ,fila1,columna1,fila2,columna2
                       ,maxLong);
                   }
                 }
                 //------------------------------------------------
                 //miro abajo
                 else if( (filaDiff + 1 < tablero.filas)
                     &&
                     (tablero.tablero(
                     game.transCoordenadas(tablero,filaDiff+1,colDiff))
                     == fichaActual)
                     &&
                     contador >=2  )
                 {
                   //println("swap abajo")
                   //calcular verdadera longitud de la jugada
                   //contador mas 1, pues si realizamos el swap tendriamos
                   //una ficha mas alineada, ya que la que hemos intercambiado
                   //es diferente.
                   //si hacemos el movimiento en la misma direccion
                   //en la que estamos buscando, la longitud de la 
                   //jugada será siempre contador + 1
                   val longJugada = contador + 1
                   //println("La verdadera longitud para indice = "+ indice + " es : " + longJugada)
                   //guardar jugada 
                   //llamada recursiva
                   //---
                   //si la jugada es mejor, la guardamos
                   if(longJugada > maxLong)
                   {
                     //println("Nueva mejor jugada!")
                     //llamada recursiva, guardando la jugada actual
                     iaDownRec1(tablero,indice+1
                       ,filaDiff,colDiff,filaDiff+1,colDiff
                       ,longJugada)
                   }
                   else
                   {
                     //llamada recursiva, descartando jugada actual
                     iaDownRec1(tablero,indice+1
                       ,fila1,columna1,fila2,columna2
                       ,maxLong)
                   }
                 }
                 //------------------------------------------------
                 //Si llego aqui, no hay jugada
                 //mirando izquierda, abajo y derecha
                 else
                 {
                 //llamada recursiva, no hay jugada 
                 iaDownRec1(tablero,indice+1
                   ,fila1,columna1,fila2,columna2
                   ,maxLong)
                 }
         }
         //No se cumplen los requisitos para
         //que haya una jugada posible
         else
         {
           //llamada recursiva, no hay jugada 
           iaDownRec1(tablero,indice+1
               ,fila1,columna1,fila2,columna2
               ,maxLong)
         }
     }
     //ya he inspeccionado todo
     //devuelvo resultao
     else
     {
       List(fila1,columna1,fila2,columna2,maxLong)
     }
   }
   
   /**
    * Se utiliza para indicar la longitud de una combinación.
    * 
    * @param indice: posicion a comprobar
    * @param contador: cuenta el numero de fichas 
    * iguales que llevo
    * 
    * Para saber cual es la primera ficha diferente, sumare 
    * al indice de iaDerDown1 el contador * tablero.columnas, 
    * lo que me dara el offset de la primera ficha con valor diferente.
    * 
    * En la llamada incial de esta funcion, contador tendra valor 1
    * e indice tendra el valor de indice + tablero.columnas (empiezo contando en
    * la casilla inferior, con contador mas 1 pues donde he 
    * empezado a contar ya tiene el valor deseado)
    * 
    * @return contador
    */
   def iaDownRec2(tablero:Tablero, indice:Int, valorActual:Int
       ,contador:Int) :Int = 
   {
       //Si la ficha en la que estoy tiene
       //el mismo valor, y el indice no esta
       //no esta fuera del tablero => contador++
       if(game.transPosicion(tablero, indice)(0) < tablero.filas
           && tablero.tablero(indice) == valorActual)
       {
         iaDownRec2(tablero,indice+tablero.columnas,valorActual,contador+1)
       }
       //si no,devuelvo el contador
       else
       {
         contador
       }
   }
   
   //-------------------------------------------------------------------------------
   //ARRIBA
   //profundidad 1
   //Itera a traves de todo el tablero
   /**
    * @param tablero: tablero a través del cual se 
    * buscarán las jugadas
    * @param indice: indice de la casilla para la cual buscaremos
    * jugada en la iteración actual
    * @param fila1: fila de la primera casilla
    * @param columna1: columna de la primera casilla
    * @param fila2 : fila de la segunda casilla
    * @param columna2: columna de la segunda casilla
    * @param maxLong: longitud de la mejor jugada encontrada hasta ahora
    * 
    * Itera a través del todo el tablero barriendo hacia arriba,
    * en busca de la mejor jugada posible.
    * 
    * Cuando la funcion haya acabado de iterar,
    * la mejor jugada la realizaremos intecambiando las fichas
    * en [fila1,columna1] y [fila2,columna2]. 
    * Tendremos que devolverlo como una lista   */
   def iaUpRec1(tablero: Tablero, indice: Int
       , fila1 : Int, columna1 : Int
       , fila2 : Int, columna2 : Int
       , maxLong : Int) : List[Int] = 
   {
     if(indice < (tablero.filas*tablero.columnas) 
         && indice >= 0)
     {
           val fichaActual = tablero.tablero(indice)
           //me situa en la primera casilla a la derecha
           //que tenga un valor diferente
           val contador = iaUpRec2(tablero,indice-tablero.columnas,fichaActual,1)
           //posicion de la primera casilla con valor diferente.
           val casillaDiff = indice - contador*tablero.columnas
           val filaDiff = game.transPosicion(tablero, casillaDiff)(0)
           val colDiff = game.transPosicion(tablero, casillaDiff)(1)
           //DEBUG
           /*
           println("WARNING")
           println("indice " + indice)
           println("contador " + contador)
           println("casillaDiff " + casillaDiff)
           println("filaDiff " + filaDiff)
           println("columnaDiff " + colDiff)
           */
           
           //legados a este punto estaré con k indicandome 
           //una joya diferente
           //o al final de linea
           //si estamos al final de linea no hacemos nada
           //columnas-1 por la tranformacion, ya que empezamos 
           //contando columnas desde 0
           
           //CAMBIAR IF ->  filaDiff <= tablero.filas-2 && filaDiff > 0 sacar fuera del segundo termino del or
           if(
               (contador >=2 
                   || 
                 (filaDiff <= tablero.filas-2
                 && filaDiff > 0
                 && (tablero.tablero(casillaDiff-tablero.columnas)
                   ==tablero.tablero(casillaDiff+tablero.columnas))
                 )
               )
              //seguridad
               && filaDiff < tablero.filas
               && colDiff < tablero.columnas
               && filaDiff >= 0
               && colDiff >=0
               //fin seguridad
              && tablero.tablero(casillaDiff)!=fichaActual
              
             )
           {
                 //println("->Comprobando para ["
                 //   +filaDiff + "," + colDiff 
                 //   + "] con contador = " + contador)
                 //------------------------------------------------   
                 //miro derecha
                 if( (colDiff + 1 < tablero.columnas)
                     &&
                     (tablero.tablero(
                     game.transCoordenadas(tablero,filaDiff,colDiff+1))
                     == fichaActual))
                 {
                   //println("swap derecha")
                   //calcular verdadera longitud de la jugada
                   val longJugada = iaUpRec2(tablero,casillaDiff-tablero.columnas,fichaActual,contador+1)
                   //println("La verdadera longitud para indice = "+ indice + " es : " + longJugada)
                   //guardar jugada 
                   //llamada recursiva
                   //---
                   //si la jugada es mejor, la guardamos
                   if(longJugada > maxLong)
                   {
                     //println("Nueva mejor jugada!")
                     //llamada recursiva, guardando la jugada actual
                      return iaUpRec1(tablero,indice-1
                       ,filaDiff,colDiff,filaDiff,colDiff+1
                       ,longJugada);
                   }
                   else
                   {
                     //llamada recursiva, descartando jugada actual
                     return iaUpRec1(tablero,indice-1
                       ,fila1,columna1,fila2,columna2
                       ,maxLong);
                   }
                 }
                 //------------------------------------------------   
                 //miro izquierda
                 else if( (colDiff > 0)
                     &&
                     (tablero.tablero(
                     game.transCoordenadas(tablero,filaDiff,colDiff-1))
                     == fichaActual) )
                 {
                   //println("swap izquierda")
                   //calcular verdadera longitud de la jugada
                   val longJugada = iaUpRec2(tablero,casillaDiff-tablero.columnas,fichaActual,contador+1)
                   //println("La verdadera longitud para indice = "+ indice + " es : " + longJugada)
                   //guardar jugada 
                   //llamada recursiva
                   //---
                   //si la jugada es mejor, la guardamos
                   if(longJugada > maxLong)
                   {
                     //println("Nueva mejor jugada!")
                     //llamada recursiva, guardando la jugada actual
                      return iaUpRec1(tablero,indice-1
                       ,filaDiff,colDiff,filaDiff,colDiff-1
                       ,longJugada);
                   }
                   else
                   {
                     //llamada recursiva, descartando jugada actual
                     return iaUpRec1(tablero,indice-1
                       ,fila1,columna1,fila2,columna2
                       ,maxLong);
                   }
                 }
                 //------------------------------------------------
                 //miro arriba
                 else if( (filaDiff > 0)
                     &&
                     (tablero.tablero(
                     game.transCoordenadas(tablero,filaDiff-1,colDiff))
                     == fichaActual)
                     &&
                     contador >=2  )
                 {
                   //println("swap arriba")
                   //calcular verdadera longitud de la jugada
                   //contador mas 1, pues si realizamos el swap tendriamos
                   //una ficha mas alineada, ya que una de las que
                   //hemos intercambiado es diferente.
                   //si hacemos el movimiento en la misma direccion
                   //en la que estamos buscando, la longitud de la 
                   //jugada será siempre contador + 1
                   val longJugada = contador + 1
                   //println("La verdadera longitud para indice = "+ indice + " es : " + longJugada)
                   //guardar jugada 
                   //llamada recursiva
                   //---
                   //si la jugada es mejor, la guardamos
                   if(longJugada > maxLong)
                   {
                     //println("Nueva mejor jugada!")
                     //llamada recursiva, guardando la jugada actual
                     iaUpRec1(tablero,indice-1
                       ,filaDiff,colDiff,filaDiff-1,colDiff
                       ,longJugada)
                   }
                   else
                   {
                     //llamada recursiva, descartando jugada actual
                     iaUpRec1(tablero,indice-1
                       ,fila1,columna1,fila2,columna2
                       ,maxLong)
                   }
                 }
                 //------------------------------------------------
                 //Si llego aqui, no hay jugada
                 //mirando izquierda, arriba y derecha
                 else
                 {
                 //llamada recursiva, no hay jugada 
                 iaUpRec1(tablero,indice-1
                   ,fila1,columna1,fila2,columna2
                   ,maxLong)
                 }
         }
         //No se cumplen los requisitos para
         //que haya una jugada posible
         else
         {
           //llamada recursiva, no hay jugada 
           iaUpRec1(tablero,indice-1
               ,fila1,columna1,fila2,columna2
               ,maxLong)
         }
     }
     //ya he inspeccionado todo
     //devuelvo resultado
     else
     {
       List(fila1,columna1,fila2,columna2,maxLong)
     }
   }
   
   /**
    * Se utiliza para indicar la longitud de una combinación.
    * 
    * @param indice: posicion a comprobar
    * @param contador: cuenta el numero de fichas 
    * iguales que llevo
    * 
    * Para saber cual es la primera ficha diferente, restaré 
    * al indice de iaUpRec1 el contador * tablero.columnas, 
    * lo que me dara el offset de la primera ficha con valor diferente.
    * 
    * En la llamada incial de esta funcion, contador tendra valor 1
    * e indice tendra el valor de indice - tablero.columnas (empiezo contando en
    * la casilla superior, con contador más 1 pues donde he 
    * empezado a contar ya tiene el valor deseado)
    * 
    * @return contador
    */
   def iaUpRec2(tablero:Tablero, indice:Int, valorActual:Int
       ,contador:Int) :Int = 
   {
       //Si la ficha en la que estoy tiene
       //el mismo valor, y el indice no esta
       //no esta fuera del tablero => contador++
       if(//transPosicion(tablero, indice)(0) > 0
           indice >=0
           && tablero.tablero(indice) == valorActual)
       {
         iaUpRec2(tablero,indice-tablero.columnas,valorActual,contador+1)
       }
       //si no,devuelvo el contador
       else
       {
         contador
       }
   }

   
  
}