package timing

import java.util.Calendar

object utils {

  /**  Measure a block of code and show a simple 
   *   message
   */
  def time[A](message: String)(block: => A): A = {
    val start = System.nanoTime()
    println(s"Timing $message")
    val result = block
    val end = System.nanoTime()

    val differenceInMs = (end - start) / 1e6
    println(s"Duration = $differenceInMs ms")

    result
  }
}
