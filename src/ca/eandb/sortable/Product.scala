/**
 *
 */
package ca.eandb.sortable

/**
 * @author Brad Kimmel
 *
 */
final class Product(
    val name : String,
    val manufacturer : String,
    val model : String,
    val family : String,
    val announcedDate : String) {
  
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
  
  def this(any: Any) =
    this(any.asInstanceOf[Map[String, String]])

}