/**
 *
 */
package ca.eandb.sortable
import scala.util.parsing.json.JSONObject
import scala.util.parsing.json.JSONArray

/**
 * An entity object representing a product.
 * @author Brad Kimmel
 */
final class Product(
    val name : String,
    val manufacturer : String,
    val model : String,
    val family : String,
    val announcedDate : String) {
  
  var listings = List.empty[Listing]
  
  /**
   * Creates a <code>Product</code> from a <code>JSONObject</code> containing
   * its properties
   * @param json A <code>JSONObject</code> indicating the values of the fields
   * 	for the <code>Product</code>.
   */
  def this(json: JSONObject) =
    this(
        json.obj("product_name").toString,
        json.obj("manufacturer").toString,
        json.obj("model").toString,
        json.obj get "family" match {
          case Some(family) => family.toString
          case None => ""
        },
        json.obj("announced-date").toString)

  /** Gets a JSON representation of this <code>Listing</code>. */
  def toJSON =
    new JSONObject({
      val fields = Map(
        "product_name" -> name,
        "manufacturer" -> manufacturer,
        "model" -> model,
        "family" -> family,
        "announced-date" -> announcedDate)
      if (!listings.isEmpty)
        fields + { "products" -> new JSONArray(listings.map(_.toJSON(false))) }
      else fields
    })

  override def toString = name

}