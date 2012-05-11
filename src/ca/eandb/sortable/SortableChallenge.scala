/**
 *
 */
package ca.eandb.sortable

import scala.io._
import com.twitter.json._
import java.io.FileWriter
import java.io.PrintStream

/**
 * @author Brad Kimmel
 *
 */
object SortableChallenge {
  def main(args: Array[String]) = {
    
    val builder = new ProductTrieBuilder
    var start = System.currentTimeMillis()
    Source.fromFile("products.txt").getLines.foreach(line =>
        builder += new Product(Json.parse(line)))
    var end = System.currentTimeMillis()
    println(end - start)
    
    val matcher = new ListingMatcher(builder)
    start = System.currentTimeMillis()
    
    Source.fromFile("listings.txt").getLines.foreach(line =>
      matcher.matchListing(new Listing(Json.parse(line))))
    
    end = System.currentTimeMillis()
    
    println(end - start)
    
    
    val out = new PrintStream("output.txt")
    out.println("---MANUFACTURER---")
    out.println(builder.manufacturerTrie)
    out.println("---MODEL---")
    out.println(builder.modelTrie)
    out.close()

  }
  
}