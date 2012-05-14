/**
 *
 */
package ca.eandb.sortable
import scala.util.parsing.json.JSONObject

/**
 * An entity object representing a listing.
 * @author Brad Kimmel
 */
final class Listing(
    val title : String,
    val manufacturer : String,
    val currency : String,
    val price : Float) {
  
  /** An optional <code>Product</code> associated with this listing. */
  var product : Option[Product] = None

  /**
   * Creates a <code>Listing</code> from a <code>JSONObject</code> containing
   * its properties.
   * @param json A <code>JSONObject</code> containing the values for the fields
   *   of the <code>Listing</code>.
   */
  def this(json: JSONObject) =
    this(
        json.obj("title").toString,
        json.obj("manufacturer").toString,
        json.obj("currency").toString,
        json.obj("price").toString.toFloat)
        
  def toJSON : JSONObject = toJSON(true)

  /** Gets a JSON representation of this <code>Listing</code>. */
  def toJSON(recurse : Boolean) =
    new JSONObject({
      val fields = Map(
        "title" -> title,
        "manufacturer" -> manufacturer,
        "currency" -> currency,
        "price" -> price)
      if (recurse) {
        product match {
          case Some(x) => fields + { "product_name" -> x.name }
          case None => fields
        }
      } else fields
    })

}