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
    
    // Read the products from the products file and build the data
    // structures necessary to process the listings.
    val builder = new ProductTrieBuilder
    var start = System.currentTimeMillis()
    Source.fromFile(args(0)).getLines.foreach(line =>
        builder += new Product(Json.parse(line)))
    var end = System.currentTimeMillis()
    
    printf("Time required to build product data structures: %dms", end - start);
    println()
    
    val matcher = new ListingMatcher(builder)
    val listings = 
      if (args.length > 1 && !(args(1) equals "-"))
    	Source.fromFile("listings.txt")
      else Source.fromInputStream(System.in);

    start = System.currentTimeMillis()
    listings.getLines.foreach(line =>
      matcher.matchListing(new Listing(Json.parse(line))))
    end = System.currentTimeMillis()
    
    printf("Time required to analyse listings: %dms", end - start);
    println()
    
    val out =
      if (args.length > 2 && !(args(2) equals "-"))
        new PrintStream(args(2))
      else System.out;
    out.println("---MANUFACTURER---")
    out.println(builder.manufacturerTrie)
    out.println("---MODEL---")
    out.println(builder.modelTrie)
    out.close()

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