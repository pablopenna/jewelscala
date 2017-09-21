

package gui

import javax.swing._
import java.awt.GridLayout
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.border.EmptyBorder
import juego.Game

class BienvenidaGUI {
  val panel = new JPanel()
  val frame = new JFrame()
  panel.setLayout(new GridLayout(8,1))
  
  val title = new JLabel("JewelScala")
  title.setFont(title.getFont().deriveFont(32.0f))
  title.setHorizontalAlignment(SwingConstants.CENTER)
  panel.add(title)
  
  val nuevaPartidaLabel = new JLabel("Nueva partida")
  nuevaPartidaLabel.setFont(title.getFont().deriveFont(16.0f))
  nuevaPartidaLabel.setHorizontalAlignment(SwingConstants.CENTER)
  val dificultadLabel = new JLabel("Dificultad")
  dificultadLabel.setHorizontalAlignment(SwingConstants.CENTER)
  panel.add(nuevaPartidaLabel)
  panel.add(dificultadLabel)
  
  val jugadoYaLabel = new JLabel("¿Has jugado ya?")
  jugadoYaLabel.setFont(title.getFont().deriveFont(16.0f))
  jugadoYaLabel.setHorizontalAlignment(SwingConstants.CENTER)
  
  val botonFacil = new JButton("Fácil")
  botonFacil.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        val game = new Game(1)
        game.iniciarJuego()
        frame.dispose()
      }
  })
  val botonNormal = new JButton("Normal")
  botonNormal.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        val game = new Game(2)
        game.iniciarJuego()
        frame.dispose()
      }
  })
  val botonDificil = new JButton("Difícil")
  botonDificil.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        val game = new Game(3)
        game.iniciarJuego()
        frame.dispose()
      }
  })
  
  val botonCargarPartida = new JButton("Cargar partida")
  botonCargarPartida.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        val game = new Game(0)
        val tableroCargado = game.cargarTablero(null)
        if(tableroCargado == null) {
          mostrarError("No se ha encontrado ninguna partida guardada.")
        } else {
          game.tableroGUI.preparar(tableroCargado.filas,tableroCargado.columnas)
          game.tableroGUI.plasmarTablero(tableroCargado)
          frame.dispose()
        }
      }
  })
  
  panel.add(botonFacil)
  panel.add(botonNormal)
  panel.add(botonDificil)
  panel.add(jugadoYaLabel)
  panel.add(botonCargarPartida)
  panel.setBorder(new EmptyBorder(16,16,16,16))
  frame.add(panel)
  frame.setTitle("JewelScala Launcher")
  frame.validate()
  frame.pack()
  frame.setLocationRelativeTo(null)
  frame.setResizable(false)
  frame.setVisible(true)
  
  def mostrarError(mensaje:String) : Unit = {
    JOptionPane.showMessageDialog(null, mensaje, "Error", JOptionPane.ERROR_MESSAGE)
  }
  
}