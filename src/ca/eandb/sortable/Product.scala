/**
 *
 */
package ca.eandb.sortable
import com.twitter.json.Json

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
  
  /**
   * Creates a <code>Product</code> from a <code>Map</code> indicating its
   * properties.
   * @param fields A <code>Map</code> indicating the values of the fields
   * 	for the <code>Product</code>.
   */
  def this(fields: Map[String, String]) =
    this(
        fields("product_name"),
        fields("manufacturer"),
        fields("model"),
        fields get "family" match {
          case Some(family) => family
          case None => ""
        },
        fields("announced-date"))
  
  /**
   * Creates a <code>Product</code> from an object that is convertable to a
   * <code>Map[String, String]</code>.
   * @param any An object convertable to a <code>Map[String, String]</code>.
   * @see this(Map[String, String])
   */
  def this(any: Any) =
    this(any.asInstanceOf[Map[String, String]])
    
  def toJson =
    (Json build Map(
	  "product_name" -> name,
	  "manufacturer" -> manufacturer,
	  "model" -> model,
	  "family" -> family,
	  "announced-date" -> announcedDate)) toString

  override def toString = name

}