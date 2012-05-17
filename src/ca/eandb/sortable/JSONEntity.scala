/**
 *
 */
package ca.eandb.sortable
import scala.util.parsing.json.JSONType

/**
 * Represents an object that may be converted to a <code>JSONType</code>.
 * @author Brad Kimmel
 */
trait JSONEntity {

  /**
   * Convert the object to a <code>JSONType</code>.
   * @return a JSON representation of the object.
   */
  def toJSON: JSONType
  
}