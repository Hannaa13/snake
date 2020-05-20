import java.awt.event.{ActionEvent, KeyEvent, KeyListener}
import java.awt.{Color, Component, Dimension}
import javax.swing.{JFrame, JPanel, Timer}

object Main extends App {
  var listSnake: List[(Int, Int)] = List((2, 2))
  var lengthSnake = 1
  var point = newRandomPoint
  var direction = (1, 0)

  def newRandomPoint = {
    (random(59), random(39))
  }

  def changeDirection(list: List[(Int, Int)], d: (Int, Int)) = {
    ((list.head._1 + d._1, list.head._2 + d._2) :: list).take(lengthSnake)
  }

  def move = {
    listSnake = changeDirection(listSnake, direction)
    listSnake match {
      case list if list.head == point =>
        lengthSnake = lengthSnake + 1
        point = newRandomPoint
        drawJFrame(new RandomPoint(point))
        frame.getContentPane.add(new Point)
      case head :: tail if tail.contains(head) => System.exit(0)
      case _ => drawJFrame(new Point)
    }
  }

  val listener: KeyListener = new KeyListener {

    def keyPressed(e: KeyEvent): Unit = {
      println("[keyPressed]")
      direction = e.getKeyCode match {
        case KeyEvent.VK_A => (-1, 0)
        case KeyEvent.VK_D => (1, 0)
        case KeyEvent.VK_S => (0, 1)
        case KeyEvent.VK_W => (0, -1)
        case KeyEvent.VK_Q =>
          System.exit(0)
          throw new Exception("Game Over")
        case _ => (1, 0)
      }
      println(lengthSnake)
      println(listSnake)
    }

    override def keyReleased(keyEvent: KeyEvent): Unit = ()

    override def keyTyped(keyEvent: KeyEvent): Unit = ()
  }

  def start(): Unit = {
    val timer = new Timer(100, (e: ActionEvent) => {
      move
    })
    timer.start()
  }

  val frame = new JFrame()
  frame.setSize(new Dimension(600, 400))
  frame.setLocationRelativeTo(null)
  frame.setBackground(Color.BLACK)
  frame.setVisible(true)
  frame.addKeyListener(listener)
  drawJFrame(new Point)
  drawJFrame(new RandomPoint(point))

  class Point extends JPanel {

    import java.awt.Graphics

    override def paint(g: Graphics): Unit = {
      g.setColor(Color.GREEN)
      listSnake.foreach(xy =>
        g.fillOval(xy._1 * 10, xy._2 * 10, 10, 10))

    }
  }

  private def drawJFrame(c: Component): Unit = {
    frame.getContentPane.add(c)
    frame.revalidate()
    frame.repaint()

  }


  class RandomPoint(point: =>  (Int, Int)) extends JPanel {

    import java.awt.Graphics

    override def paint(g: Graphics): Unit = {
      g.setColor(Color.RED)
      g.fillOval(point._1 * 10, point._2 * 10, 10, 10)
    }
  }

  private def random(i: Int): Int = {
    val r = (Math.random() * i).toInt
    if (r % 10 == 0) r
    else random(i)

  }

  start()
}