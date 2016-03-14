package net.entelijan

import java.awt.BorderLayout
import java.awt.Dimension
import javax.swing.JFrame
import javax.swing.JPanel
import doctus.swing._
import doctus.core.template._

object MorphingSwing extends App {

  val top = new JFrame()
  val panel = DoctusComponentFactory.component

  val canvas = DoctusTemplateCanvasSwing(panel)
  val sched = DoctusSchedulerSwing

  val cp = new JPanel
  cp.setLayout(new BorderLayout)
  cp.add(panel, BorderLayout.CENTER)

  top.setContentPane(cp)
  top.setTitle("Morphing")
  top.setSize(new Dimension(900, 700))
  top.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  top.setVisible(true)

  // Common to all platforms
  val templ = MorphingDoctusTemplate(canvas)
  DoctusTemplateController(templ, sched, canvas)

}

