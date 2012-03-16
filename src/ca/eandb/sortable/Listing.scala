/**
 *
 */
package ca.eandb.sortable

/**
 * @author Brad Kimmel
 *
 */
final class Listing(
    val title : String,
    val manufacturer : String,
    val currency : String,
    val price : Float) {
  
  def this(fields: Map[String, String]) =
    this(
        fields("title"),
        fields("manufacturer"),
        fields("currency"),
        fields("price").toFloat)
  
  def this(any: Any) =
    this(any.asInstanceOf[Map[String, String]])

}