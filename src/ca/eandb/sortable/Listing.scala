/**
 *
 */
package ca.eandb.sortable
import scala.util.parsing.json.JSONObject
import scala.collection.immutable.Map

/**
 * An entity object representing a listing.
 * @author Brad Kimmel
 */
final case class Listing(
    val title: String,
    val manufacturer: String,
    val currency: String,
    val price: Float,
    val productName: Option[String] = None) {
        
  /**
   * Associates a <code>Product</code> with this <code>Listing</code>.
   * @param product The <code>Product</code> to associate.
   */
  def withProductName(productName: String): Listing =
    copy(productName = Some(productName))
  
  /**
   * Determines if this <code>Listing</code> has a matching <code>Product</code>
   * associated with it.
   */
  def isMatched = productName isDefined
        
  /** Gets a JSON representation of this <code>Listing</code>. */
  def toJSON: JSONObject = toJSON(true)

  /**
   * Gets a JSON representation of this <code>Listing</code>.
   * @param recurse A value indicating weather to show the linked product.
   * @return The <code>JSONObject</code> representing this
   *   <code>Listing</code>.
   */
  def toJSON(showProduct: Boolean) =
    new JSONObject({
      val fields = Map(
        "title" -> title,
        "manufacturer" -> manufacturer,
        "currency" -> currency,
        "price" -> price)
      if (showProduct) {
        productName match {
          case Some(x) => fields + ( "product_name" -> x )
          case None => fields
        }
      } else fields
    })

}

object Listing {
  
  /**
   * Creates a <code>Listing</code> from a <code>JSONObject</code> containing
   * its properties.
   * @param json A <code>JSONObject</code> containing the values for the fields
   *   of the <code>Listing</code>.
   */
  def fromJSON(json: JSONObject) = Listing(
    json.obj("title").toString,
    json.obj("manufacturer").toString,
    json.obj("currency").toString,
    json.obj("price").toString.toFloat)
  
}