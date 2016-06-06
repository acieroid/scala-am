import scala.util.parsing.input.Position
trait Expression[E] {
  def pos(e: E): Position
}
