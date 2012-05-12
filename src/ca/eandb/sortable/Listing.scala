/**
 *
 */
package ca.eandb.sortable
import com.twitter.json.Json
import com.twitter.json.JsonSerializable

/**
 * An entity object representing a listing.
 * @author Brad Kimmel
 */
final class Listing(
    val title : String,
    val manufacturer : String,
    val currency : String,
    val price : Float) extends JsonSerializable {
  
  /** An optional <code>Product</code> associated with this listing. */
  var product : Option[Product] = None

  /**
   * Creates a <code>Listing</code> from a <code>Map</code> containing its
   * properties.
   * @param fields A <code>Map</code> containing the values for the fields of
   *   the <code>Listing</code>.
   */
  def this(fields: Map[String, String]) =
    this(
        fields("title"),
        fields("manufacturer"),
        fields("currency"),
        fields("price").toFloat)
  
  /** Creates a <code>Listing</code> from an object convertible to a
   * <code>Map[String, String]</code>.
   * @param any An object convertible to a <code>Map[String, String]</code>. 
   */
  def this(any: Any) =
    this(any.asInstanceOf[Map[String, String]])

  override def toJson =
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