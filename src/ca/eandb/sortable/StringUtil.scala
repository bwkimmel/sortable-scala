/**
 *
 */
package ca.eandb.sortable

import java.text.Normalizer

/**
 * @author brad
 *
 */
object StringUtil {
  
  def normalize(s: String) : String =
    Normalizer.normalize(s, Normalizer.Form.NFD)
    	.replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    	.toLowerCase()
    	.replaceAll("[^a-z0-9]+", " ")
    	.replaceAll("([a-z])([0-9])", "$1 $2")
    	.replaceAll("([0-9])([a-z])", "$1 $2")

}