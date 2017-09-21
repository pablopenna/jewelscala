package gui

import javax.swing._
import java.awt.GridLayout
import java.awt.FlowLayout
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Color
import java.awt.Insets
import java.awt.event._
import juego.Tablero
import juego.Game
import juego.IA

class TableroGUI(val game:Game) {
  // 15*27 es el número máximo de botones que pueden existir en el juego.
  // Preparamos el máximo de botones que se generarán en el tablero, pero no los añadimos
  // al panel de la interfaz todavía.
  val listaBotones = List.tabulate(15*27)(x => {
       val boton = new JButton("")
       boton.setBackground(Color.WHITE)
       
       boton.putClientProperty("indice", x)
       boton.putClientProperty("pulsado",0)
       boton.setPreferredSize(new Dimension(45,45))
       boton.setMinimumSize(new Dimension(45,45))
       boton.setMargin(new Insets(0, 0, 0, 0));
       
       boton
  })
  
  val labelPuntuacion = new JLabel("0")
  val labelCombinaciones = new JLabel("0")
  
  // Añadimos el listener a cada uno de los botones. Uso un for especial de
  // Scala para facilitar la sintaxis, sería perfectamente intercambiable
  // por un bucle recursivo que itere por toda la lista...
  for(boton <- listaBotones) {
    boton.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        val botonPulsado = e.getSource().asInstanceOf[JButton]
        estadoTablero.estado match {
          case 0 => { 
            // Estado 0: Acabamos de pulsar una pieza, sin haber pulsado una justo antes.
            botonPulsado.putClientProperty("pulsado",1)
            botonPulsado.setBackground(botonPulsado.getBackground().darker())
            //botonPulsado.setText("a" + botonPulsado.getClientProperty("indice"))
            estadoTablero = new AyudanteGUI(estadoTablero.tablero,1)
          }
          case 1 => { 
            // Estado 1: Hemos pulsado una pieza antes, y ahora hemos pulsado otra.
            buscarIndiceBotonPulsado(listaBotones) match {
              case -1 => {
                // Volvemos al estado 0 para pedir otra vez operación
                estadoTablero = new AyudanteGUI(estadoTablero.tablero,0) 
              }
              case x => {
                // Nos hemos asegurado de que hemos pulsado un botón antes.
                // Asi que vamos a probar a intercambiar las posiciones.
                val botonPulsadoAntes = listaBotones(x)
                botonPulsadoAntes.setBackground(botonPulsadoAntes.getBackground().brighter())
                botonPulsadoAntes.putClientProperty("pulsado", 0)
                  
                val indiceBotonAnterior = botonPulsadoAntes.getClientProperty("indice").asInstanceOf[Int]
                val indiceBotonActual = botonPulsado.getClientProperty("indice").asInstanceOf[Int]
                if(indiceBotonAnterior == indiceBotonActual) {
                  // Hemos movido a la misma posición... no hace falta hacer nada.
                  estadoTablero = new AyudanteGUI(estadoTablero.tablero,0)
                } else {
                  // Finalmente aqui hemos movido una ficha.
                  
                  val nuevoTablero = game.moverFicha(estadoTablero.tablero,indiceBotonAnterior,indiceBotonActual)
                  // Borramos todas las combinaciones (En combo, si se diese el caso),
                  // y actualizamos el tablero y el estado para recibir otra instrucción
                  // tras haber borrado las combinaciones.
                  
                  val tableroSinCombinaciones = game.borrarHastaNoPoder(nuevoTablero)
                  
                  cambiarColorDeCeldasDiferentes(estadoTablero.tablero,tableroSinCombinaciones)
                  estadoTablero = new AyudanteGUI(tableroSinCombinaciones,0)
                  
                  labelPuntuacion.setText("" + estadoTablero.tablero.puntuacion)
                  labelCombinaciones.setText("" + estadoTablero.tablero.combinaciones)
                }
                
              }
            }
            
          }
          case _ => // no hacemos nada.
        }
           
      }
    })
  }
  
  /**
   * Función que busca el índice del botón que tenga el atributo "pulsado" a 1.
   */
  def buscarIndiceBotonPulsado(listaBotones : List[JButton]) : Int = {
    if(listaBotones.isEmpty) {
      -1
    } else {
      if(listaBotones.head.getClientProperty("pulsado") == 1) {
        listaBotones.head.getClientProperty("indice").asInstanceOf[Int]
      } else {
        buscarIndiceBotonPulsado(listaBotones.tail)
      }  
    }
  }
  
  // La unica var del programa. Guarda el tablero actual y 
  // una variable de 'estado' para guiar a los eventos de los botones, de
  // modo que se sepa que acciones se pueden realizar en cada momento.
  var estadoTablero = new AyudanteGUI(new Tablero(0 :: Nil,1,1,1,0,0),0)

  // Quita todos los botones del panel, y ajusta el layout según el valor de filas y columnas.
  // Seguidamente, añade tantos botones como filas por columnas haya.
  def refrescarPanel(panel : JPanel, filas : Int, columnas : Int) : Unit = {
    panel.removeAll()  
    panel.setLayout(new GridLayout(filas,columnas))
    añadirCantidadBotones(panel,listaBotones,filas*columnas)
    panel.setPreferredSize(new Dimension(45*columnas,45*filas))
  }
  
  // Muestra en el panel la cantidad de botones pasada como parametro.
  def añadirCantidadBotones(panel : JPanel, listaBotones : List[JButton], cuantos : Int) : Unit = {
    if(listaBotones.isEmpty || cuantos <= 0) {
      return
    } else {
      panel.add(listaBotones.head)
      añadirCantidadBotones(panel, listaBotones.tail, cuantos - 1)
    }
  }
  
  // Asignamos el tablero con su estado inicial, además de pintar cada botón con el color de cada pieza.
  def plasmarTablero(tablero:Tablero) : Unit = {
    estadoTablero = new AyudanteGUI(tablero,0)
    val piezas = tablero.tablero
    
    plasmarTableroRec(piezas,0)
    labelPuntuacion.setText("" + estadoTablero.tablero.puntuacion)
    labelCombinaciones.setText("" + estadoTablero.tablero.combinaciones)
  }
  
  // Método recursivo para ir cambiando el color de cada pieza en la GUI
  def plasmarTableroRec(piezas:List[Int], indice:Int) : Unit = {
    if(piezas.isEmpty) {
      Nil
    }
    else
    {
      cambiarColores(piezas.head,indice,false)
      plasmarTableroRec(piezas.tail,indice+1)
    }
      
  }
  
  // Compara dos tableros diferentes y únicamente cambia aquellas piezas diferentes.
  def cambiarColorDeCeldasDiferentes(tableroAntiguo:Tablero, tableroNuevo:Tablero) : Unit = {
    val piezasAntiguas = tableroAntiguo.tablero
    val piezasNuevas = tableroNuevo.tablero
    
    cambiarColorDeCeldasDiferentesRec(piezasAntiguas,piezasNuevas,0)
  }
  
  def cambiarColorDeCeldasDiferentesRec(piezasAntiguas:List[Int], piezasNuevas:List[Int], indice:Int) : Unit = {
    if(piezasAntiguas.isEmpty || piezasNuevas.isEmpty) {
      Nil
    }
    else
    {
      if(piezasNuevas.head != piezasAntiguas.head) {
        cambiarColores(piezasNuevas.head,indice,false)  
      }
      cambiarColorDeCeldasDiferentesRec(piezasAntiguas.tail,piezasNuevas.tail,indice+1)
    }
  }
  
  // Prepara todos los elementos de la interfaz (inicializaciones, jerarquias, etc)
  def preparar(filas:Int, columnas:Int) : Unit = {
    
    val ventana = new JFrame("JewelScala Game");
    
    val cuantosBotones = filas*columnas;
    
    val panelBarraSuperior = new JPanel()
    
    panelBarraSuperior.setLayout(new BorderLayout())
    
    val panelAcciones = new JPanel()
    panelAcciones.setLayout(new FlowLayout())
    val botonMoverAuto = new JButton("Movimiento IA")
    
    // Botón que al pulsarlo, provocará un movimiento calculado por la IA
    botonMoverAuto.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
          val ia = new IA(game)
          val tableroMovido = ia.llamarIA(estadoTablero.tablero)
          val tableroFinal = game.borrarHastaNoPoder(tableroMovido)
          cambiarColorDeCeldasDiferentes(estadoTablero.tablero,tableroFinal)
          estadoTablero = new AyudanteGUI(tableroFinal,0)
          labelPuntuacion.setText("" + estadoTablero.tablero.puntuacion)
          labelCombinaciones.setText("" + estadoTablero.tablero.combinaciones)
      }
    })
    
    panelAcciones.add(botonMoverAuto)
    
    val panelGuardarCargar = new JPanel()
    panelGuardarCargar.setLayout(new FlowLayout())
    
    val botonCargar = new JButton("Cargar")
    val botonGuardar = new JButton("Guardar")
    
    
    
    panelGuardarCargar.add(botonCargar)
    panelGuardarCargar.add(botonGuardar)
    
    val panelPuntuacion = new JPanel()
    val panelWrapper = new JPanel()
    
    panelPuntuacion.add(new JLabel("Puntuación: "))
    panelPuntuacion.add(labelPuntuacion)
    panelPuntuacion.add(new JSeparator(SwingConstants.VERTICAL))
    panelPuntuacion.add(new JLabel("Combinaciones: "))
    panelPuntuacion.add(labelCombinaciones)
    panelPuntuacion.setLayout(new FlowLayout())
    
    panelBarraSuperior.add(panelPuntuacion,BorderLayout.CENTER)
    panelBarraSuperior.add(panelGuardarCargar,BorderLayout.EAST)
    panelBarraSuperior.add(panelAcciones,BorderLayout.WEST)
    panelWrapper.setLayout(new BorderLayout())
    
    val panelPiezas = new JPanel()
    
    refrescarPanel(panelPiezas,filas,columnas)  
    
    botonCargar.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        //cargamos aqui  
        val tableroCargado = game.cargarTablero(estadoTablero.tablero)
        refrescarPanel(panelPiezas,tableroCargado.filas,tableroCargado.columnas)
        plasmarTablero(tableroCargado)
      }
    })
    
    botonGuardar.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
          // guardamos aqui
        game.guardarTablero(estadoTablero.tablero)
      }
    })
    
    panelWrapper.add(panelBarraSuperior,BorderLayout.NORTH)
    panelWrapper.add(panelPiezas,BorderLayout.CENTER)
    ventana.add(panelWrapper)
    
    ventana.setPreferredSize(panelPiezas.getPreferredSize())
    ventana.setMinimumSize(panelPiezas.getPreferredSize())
    ventana.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    
    ventana.pack()
    ventana.setLocationRelativeTo(null)
    ventana.setVisible(true)
  }
  
  // Cambia el color de un botón, segun el valor.
  // El parámetro 'retraso' aporta un pequeño retraso al provocar que
  // los hilos que llamen a esta función tengan que esperar que otro hilo termine.
  def cambiarColores(valor:Int, indice:Int, retraso:Boolean) : Unit = {
    val texto = valor match {
      case 1 => "A"
      case 2 => "R"
      case 3 => "N"
      case 4 => "V"
      case 5 => "P"
      case 6 => "M"
      case 7 => "G"
      case 8 => "B"
      case _ => "-" // "Vacío"
    }
    
    val color = valor match {
      case 1 => new Color(124, 174, 255)
      case 2 => new Color(242, 92, 109)
      case 3 => new Color(242, 182, 92)
      case 4 => new Color(134, 241, 91)
      case 5 => new Color(192, 219, 211)
      case 6 => new Color(186, 118, 232)
      case 7 => new Color(160, 159, 160)
      case 8 => new Color(255, 255, 255)
      case _ => new Color(0,0,0) // "Vacío"
    }
    
    val boton = listaBotones(indice)
    
    if(!retraso) {
      boton.setText(texto)
      boton.setBackground(color)
    } else {
      this.synchronized {
        boton.setText(texto)
        boton.setBackground(color)
      }
    }
  }
  
  // Crea un temporizador (un hilo) que cambia el color de una pieza
  // sin interferir con el rendimiento del programa
  def cambiarColoresConHilo(valor:Int, indice:Int) : Unit = {
    
    val temporizador = new Timer(100, new ActionListener() {
      def actionPerformed(evt:ActionEvent) = {
        cambiarColores(valor,indice,true)
      }
    })
    temporizador.setRepeats(false)
    temporizador.start()
  }
  
  // Muestra un mensaje de error en forma de pop-up
  def mostrarError(mensaje:String) : Unit = {
    JOptionPane.showMessageDialog(null, mensaje, "Error", JOptionPane.ERROR_MESSAGE)
  }
  
}