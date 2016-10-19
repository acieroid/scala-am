import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import scala.language.implicitConversions

object JSON {
  implicit def storeToJSON[Addr : Address, Abs : JoinLattice](s: Store[Addr, Abs]): JValue =
    s.keys
      .filter(a => !implicitly[Address[Addr]].isPrimitive(a)) /* Addresses of primitive do not provide us any useful information */
      .foldLeft(JObject())((acc, k) => ((k.toString -> s.lookup(k).get.toString) ~ acc))
  implicit def kstoreToJSON[KontAddr : KontAddress](ks: KontStore[KontAddr]): JValue =
    ks.keys.foldLeft(JObject())((acc, k) => ((k.toString -> ks.lookup(k).toList.map(_.toString)) ~ acc))
  implicit def unitToJSON(x: Unit): JValue = ""
}
