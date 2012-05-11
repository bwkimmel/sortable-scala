/**
 *
 */
package ca.eandb.sortable

import java.text.Normalizer

/**
 * String-related utility methods.
 * @author Brad Kimmel
 */
object StringUtil {
  
  /**
   * Normalizes a string by removing features that should not be considered
   * as differentiating between two strings (such as accents and case).
   * @param s The <code>String</code> to normalize.
   * @return A canonical representation of <code>String</code>.
   */
  def normalize(s: String) : String =
    Normalizer.normalize(s, Normalizer.Form.NFD)
    	.replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    	.toLowerCase()
    	.replaceAll("[^a-z0-9]+", " ")
    	.replaceAll("([a-z])([0-9])", "$1 $2")
    	.replaceAll("([0-9])([a-z])", "$1 $2")

}