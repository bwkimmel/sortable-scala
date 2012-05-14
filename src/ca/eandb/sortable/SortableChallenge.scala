/**
 *
 */
package ca.eandb.sortable

import scala.io._
import com.twitter.json._
import java.io.FileWriter
import java.io.PrintStream
import scala.util.parsing.json.JSON
import scala.util.parsing.json.JSONObject

/**
 * An application that matches product listings to a collection of known
 * products.  This application implements a coding challenge described at
 * <a href="http://sortable.com/blog/coding-challenge/">http://sortable.com/blog/coding-challenge/</a>.
 * 
 * @author Brad Kimmel
 */
object SortableChallenge {
  
  /** Indicates whether we should only print unmatched listings. */
  private val printMisses = System.getProperty("ca.eandb.sortable.printMisses", "false").toBoolean
  
  /**
   * Indicates whether we should group the results by listing, rather than by
   * product.
   */
  private val groupByListing = printMisses || 
    System.getProperty("ca.eandb.sortable.groupByListing", "false").toBoolean
  
  /**
   * Usage: ca.eandb.sortable.SortableChallenge  <products_file> [<listings_file> 
   *        [<output_file>]]
   * Matches listings against a collection of products.
   *
   *   <products_file> - A file containing a list of products formatted as JSON
   *                     objects, one per line.
   *   <listings_file> - A file containing a collection of listings formatted as
   *                     JSON objects, one per line.  If not specified, stdin is
   *                     used.  A dash (-) may also be specified for stdin.
   *   <output_file>   - A file to which to write the results.  If not specified,
   *                     stdout is used.  A dash (-) may also be specified for
   *                     stdout.
   *                     
   * @param args The program arguments as specified above.
   */
  def main(args: Array[String]) = {
    
    // check that we have a valid number of arguments.
    if (args.length < 1 || args.length > 3) {
      usage
      System.exit(1);
    }
    
    print("Parsing products... ")
    var start = System.currentTimeMillis()
    val products = Source.fromFile(args(0)).getLines.flatMap(line =>
      JSON parseRaw line match {
        case Some(json : JSONObject) => Some(new Product(json))
        case _ => None
      }).toList		// convert iterator to list now so we can time it
    var end = System.currentTimeMillis()
    printf("%dms", end - start)
    println

    val listingsFile = 
      if (args.length > 1 && !(args(1) equals "-"))
        Source.fromFile(args(1))
      else Source.fromInputStream(System.in)
    
    print("Parsing listings... ")
    start = System.currentTimeMillis()
    var listings = listingsFile.getLines.flatMap(line =>
      JSON parseRaw line match {
        case Some(json : JSONObject) => Some(new Listing(json))
        case _ => None
      }).toList		// convert iterator to list now so we can time it
    end = System.currentTimeMillis()
    printf("%dms", end - start)
    println
    
    // build the data structures necessary to process the listings
    print("Building product data structures... ")
    start = System.currentTimeMillis()
    val builder = new ProductTrieBuilder
    products.foreach(builder += _)
    end = System.currentTimeMillis()
    printf("%dms", end - start)
    println

    val out =
      if (args.length > 2 && !(args(2) equals "-"))
        new PrintStream(args(2))
      else System.out

    print("Matching listings... ")
    start = System.currentTimeMillis()
    val matcher = new ListingMatcher(builder)
    var totalCount = 0		// total number of listings
    var matchCount = 0		// number of matching listings
    
    listings.foreach(listing => {
      matcher.matchListing(listing) match {
        case Some(product) => { listing.linkProduct(product); matchCount += 1 }
        case None => ()
      }
      totalCount += 1
    })
    end = System.currentTimeMillis()
    printf("%dms", end - start)
    println
        
    print("Printing results... ")
    start = System.currentTimeMillis()
    if (printMisses)
      listings = listings.filterNot(_.isMatched)
    if (groupByListing)
      listings.foreach(out println _.toJSON)
    else
      products.foreach(out println _.toJSON)
    out.close
    end = System.currentTimeMillis()
    printf("%dms", end - start)
    println

    printf("Matched %d of %d listings.", matchCount, totalCount);
    println

  }
  
  /** Print the usage information for this application. */
  private def usage = {
    Console.printf("Usage: %s <products_file> [<listings_file> [<output_file>]]", SortableChallenge.getClass().getName());
    Console.println();
    Console.println("Matches listings against a collection of products.");
    Console.println();
    Console.println("  <products_file> - A file containing a list of products formatted as JSON");
    Console.println("                    objects, one per line.");
    Console.println("  <listings_file> - A file containing a collection of listings formatted as");
    Console.println("                    JSON objects, one per line.  If not specified, stdin is");
    Console.println("                    used.  A dash (-) may also be specified for stdin.");
    Console.println("  <output_file>   - A file to which to write the results.  If not specified,");
    Console.println("                    stdout is used.  A dash (-) may also be specified for");
    Console.println("                    stdout.");
  }
  
}