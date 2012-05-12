/**
 *
 */
package ca.eandb.sortable
import com.twitter.json.Json

/**
 * @author Brad Kimmel
 *
 */
final class Listing(
    val title : String,
    val manufacturer : String,
    val currency : String,
    val price : Float) {
  
  var product : Option[Product] = None

  def this(fields: Map[String, String]) =
    this(
        fields("title"),
        fields("manufacturer"),
        fields("currency"),
        fields("price").toFloat)
  
  def this(any: Any) =
    this(any.asInstanceOf[Map[String, String]])

  def toJson =
    (Json build {
      val fields = Map(
        "title" -> title,
        "manufacturer" -> manufacturer,
        "currency" -> currency,
        "price" -> price)
      product match {
        case Some(x) => fields + { "product_name" -> x.name }
        case None => fields
      }
    }) toString

}