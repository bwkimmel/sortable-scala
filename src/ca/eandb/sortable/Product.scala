/**
 *
 */
package ca.eandb.sortable
import scala.util.parsing.json.JSONObject
import scala.util.parsing.json.JSONArray
import scala.collection.immutable.Map

/**
 * An entity object representing a product.
 * @author Brad Kimmel
 */
final case class Product(
    name: String,
    manufacturer: String,
    model: String,
    family: String,
    announcedDate: String,
    listings: List[Listing] = Nil) extends JSONEntity {
  
  /**
   * Associates a <code>Listing</code> with this <code>Product</code>.
   * @param listing The <code>Listing</code> to associate.        
   */
  def addListing(listing: Listing): Product =
    copy(listings = listing :: listings)

  /** Gets a JSON representation of this <code>Listing</code>. */
  def toJSON: JSONObject = {
    val fields = Map(
      "product_name" -> name,
      "manufacturer" -> manufacturer,
      "model" -> model,
      "family" -> family,
      "announced-date" -> announcedDate)
    new JSONObject(
      if (!listings.isEmpty)
        fields + ( "listings" -> new JSONArray(listings.map(_.toJSON(false))) )
      else fields)
  }
  
  /**
   * Creates a <code>Tuple</code> that can be used as a <code>Map</code> entry
   * to look up this <code>Product</code> by its name.
   */
  def toMapEntry: (String, Product) = name -> this

  override def toString: String = name

}

object Product {
    
  /**
   * Creates a <code>Product</code> from a <code>JSONObject</code> containing
   * its properties
   * @param json A <code>JSONObject</code> indicating the values of the fields
   * 	for the <code>Product</code>.
   */
  def fromJSON(json: JSONObject): Product = Product(
    json.obj("product_name").toString,
    json.obj("manufacturer").toString,
    json.obj("model").toString,
    json.obj get "family" match {
      case Some(family) => family.toString
      case None => ""
    },
    json.obj("announced-date").toString)

}