/**
 *
 */
package ca.eandb.sortable

import scala.io._
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
  def main(args: Array[String]) {
    
    // check that we have a valid number of arguments.
    if (args.length < 1 || args.length > 3) {
      usage
      System.exit(1);
    }
    
    print("Parsing products... ")
    var start = System.currentTimeMillis()
    var products = Map.empty ++ Source.fromFile(args(0)).getLines.flatMap(line =>
      JSON parseRaw line match {
        case Some(json : JSONObject) => Some(Product.fromJSON(json).toMapEntry)
        case _ => None
      })		// convert iterator to list now so we can time it
    var end = System.currentTimeMillis()
    println("%dms".format(end - start))

    val listingsFile = 
      if (args.length > 1 && !(args(1) == "-"))
        Source.fromFile(args(1))
      else Source.fromInputStream(System.in)
    
    print("Parsing listings... ")
    start = System.currentTimeMillis()
    val listings: Iterable[Listing] = listingsFile.getLines.flatMap(line =>
      JSON parseRaw line match {
        case Some(json : JSONObject) => Some(Listing.fromJSON(json))
        case _ => None
      }).toList		// convert iterator to list now so we can time it
    end = System.currentTimeMillis()
    println("%dms".format(end - start))
    
    // build the data structures necessary to process the listings
    print("Building product data structures... ")
    start = System.currentTimeMillis()
    val builder = new ProductTrieBuilder
    products.values.foreach(builder.addProduct)
    end = System.currentTimeMillis()
    println("%dms".format(end - start))

    val out =
      if (args.length > 2 && !(args(2) == "-"))
        new PrintStream(args(2))
      else System.out

    print("Matching listings... ")
    start = System.currentTimeMillis()
    val matcher = new ListingMatcher(builder)
    var totalCount = 0		// total number of listings
    var matchCount = 0		// number of matching listings
    
    val results = listings.map(unmatched => {
      totalCount += 1;
      matcher.matchListing(unmatched) match {
        case Some(name) => {
          matchCount += 1;
          val listing = unmatched withProductName name
          val product = products(name).addListing(listing)      
          products += product.toMapEntry
          listing
        }
        case None => unmatched
      }
    })
    end = System.currentTimeMillis()
    println("%dms".format(end - start))
        
    print("Printing results... ")
    start = System.currentTimeMillis()
    val toPrint =
      if (printMisses) results.filterNot(_.isMatched)
      else if (groupByListing) results
      else products.values
    toPrint.foreach(out println _.toJSON)
    out.flush()
    end = System.currentTimeMillis()
    println("%dms".format(end - start))

    println("Matched %d of %d listings.".format(matchCount, totalCount));

  }
  
  /** Print the usage information for this application. */
  private def usage {
    println("Usage: %s <products_file> [<listings_file> [<output_file>]]" format SortableChallenge.getClass().getName());
    println("Matches listings against a collection of products.");
    println();
    println("  <products_file> - A file containing a list of products formatted as JSON");
    println("                    objects, one per line.");
    println("  <listings_file> - A file containing a collection of listings formatted as");
    println("                    JSON objects, one per line.  If not specified, stdin is");
    println("                    used.  A dash (-) may also be specified for stdin.");
    println("  <output_file>   - A file to which to write the results.  If not specified,");
    println("                    stdout is used.  A dash (-) may also be specified for");
    println("                    stdout.");
  }
  
}