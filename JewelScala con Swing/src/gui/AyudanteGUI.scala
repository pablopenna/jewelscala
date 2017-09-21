package gui

import juego.Tablero

/**
 * Clase que se usará para encapsular al tablero que se esta usando
 * en este momento, tanto un número que representa el estado de la interfaz:
 * es decir, si está en la fase en la que está esperando a que el usuario
 * introduzca la primera ficha a mover, o si esta esperando que introduzca
 * la segunda ficha a mover.
 */
class AyudanteGUI(val tablero:Tablero, val estado:Int) {
  
}