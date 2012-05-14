/**
 *
 */
package ca.eandb.sortable

import scala.collection.mutable._
import ca.eandb.sortable.ProductTrieBuilder._

/**
 * An object that processes a set of product <code>Listing</code>s and matches
 * them with a <code>Product</code>.
 * @author Brad Kimmel
 */
final class ListingMatcher(val builder: ProductTrieBuilder) {
  
  /**
   * Gets the <code>Product</code> that the specified <code>Listing</code>
   * refers to, or <code>None</code> if a unique <code>Product</code> can not
   * be determined.
   * @param listing The <code>Listing</code> to examine.
   * @return The matching <code>Product</code> if there is a unique
   *   <code>Product</code> that matches <code>listing</code>, or
   *   <code>None</code> if no unique <code>Product</code> is found.
   */
  def matchListing(listing: Listing) : Option[Product] = {
    val manufacturerProducts = matchAll(builder.manufacturerTrie, listing.manufacturer, null, false)
    if (manufacturerProducts != null)
      matchOne(builder.modelTrie, cleanTitle(listing.title), manufacturerProducts, true)
    else None
  }
  
  /**
   * Modifies the title of a listing to improve matching results.
   *   - Normalize the string (see StringUtil.normalize).
   *   - Remove everything after the word "for" (or its French translation,
   *     "pour"), becasuse what follows is not the product itself.
   * @param title The title of a <code>Listing</code>.
   * @return The modified title as described above.
   */
  private def cleanTitle(title: String) =
    StringUtil.normalize(title)
      .replaceFirst(" for .*", "")
      .replaceFirst(" pour .*", "")

  /**
   * Matches the specified string against the <code>Product</code>s stored in
   * the specified <code>Trie</code>.
   * @param root The <code>Trie</code> node at the root of the trie to use to
   * 		match against.
   * @param s The <code>String</code> to match against.
   * @param filter A <code>Map</code> of <code>Product</code>s used to filter
   * 		the results.  If present, the specified trie will be treated as if
   * 		it only contained products in this <code>Map</code>.  The keys are
   * 		the <code>Product</code>s, the values indicate whether the
   *        corresponding match represents a maximal match.
   * @param useMaximalFlag A value indicating whether the matching should
   * 		return only maximal matches if there would otherwise be multiple
   * 		matching products.
   * @return A <code>Map</code> containing all of the <code>Product</code>s
   * 		that match.
   */
  private def matchAll(
      trie: ProductTrie, 
      s: String, 
      filter: ProductMap, 
      useMaximalFlag: Boolean) : ProductMap = {
    
    /* Attempt to match all of the sequences of consecutive words against
     * against the provided trie.  For example, if s is "The quick brown
     * fox", we want to consider the following for possible matches:
     * 
     *   - "The", "quick", "brown", "fox"
     *   - "Thequick", "quickbrown", "brownfox"
     *   - "Thequickbrown", "quickbrownfox"
     *   - "Thequickbrownfox"
     * 
     * The reason for this logic is so that we can match against listings
     * where the model number is split into multiple words in the listing,
     * but not in the product data, or vice versa -- or to allow for the
     * listing to contain only a partial model number (for example,
     * "Panasonic FP 7" instead of "Panasonic DMC-FP7").
     * 
     * To accomplish this, we keep track of a queue of positions (cursors)
     * within the trie.  We iterate through the list of words, and for each
     * word, we attempt to match that word using each cursor (as well as
     * the root node, which is added as a cursor each time through the
     * loop).  If we find a descendant matching the word, then we:
     * 
     *   1) Check to see if the descendant has products associated with it.
     *      If there are, we keep track of the set of products associated
     *      with this node in a map (matches).  If a filter was provided,
     *      it is employed here.
     *   2) Add the descendant to the queue as a new cursor.
     *   
     * We also only want to consider longest matches.  That is, if a
     * substring of a match also matches, we want to ignore the matches for
     * the substring.  The reason for this rule is so that for pairs of 
     * products whose model name differs from another only by the addition
     * of more characters, if those characters are present, we do not want
     * to report a match against the other product.  For example, consider
     * 
     *   Pentax WG-1
     *   Pentax WG-1 GPS
     *   
     * If a listing contained the words "WG-1 GPS", without this rule both
     * products would match this listing, resulting in the algorithm
     * reporting no certain match.
     * 
     * We accomplish this by removing the matches for all ancestors
     * when inserting a new matching node into the map.  Because we are
     * adding matches in breadth-first order, we can do this within the
     * loop rather than as a separate tree-traversal at the end.
     */
    
    // maps matching nodes in the trie to their corresponding products
    var matches: Map[ProductTrie, ProductMap] = Map.empty
    
    /**
     * Trace subsequences of consecutive words in the trie and add the
     * matching products to the <code>matches</code> map.
     * @param cursors The current set of <code>Trie</code> nodes from which
     *   to trace the next word.
     * @param words The current tail of the list of words to insert.
     */
    def matchCursors(
        cursors: List[ProductTrie],
        words: List[String]) : Unit = {
      
      // Advance cursors by next word, discard any cursors that fall off
      // the trie, and add the root node back as a new cursor.
      val nextCursors : List[ProductTrie] = 
        trie :: cursors.flatMap(_.find(words.head))
        
      // keep track of matching cursors
      nextCursors.tail.foreach(node => node.data match {
        case Some(x) => {
          var products = if (filter != null) x.filterKeys(filter contains) else x 
          if (products.nonEmpty) {
            matches += { node -> products }
            node.foreachAncestor(matches remove); // only keep maximal matches
          }
        }
        case None => ()
      })
          
      // advance to next word
      if (words.tail.nonEmpty) matchCursors(nextCursors, words.tail)
    }

    // Split the search string into its component words and search for
    // sequences of consecutive words in the trie.
    val words = StringUtil.normalize(s).split(" ")
    if (words.nonEmpty) matchCursors(List(trie), words.toList)
    
    /* Now that we have a collection of possible matches, we must resolve
     * them to a minimal set of matches (ideally only one).  The following
     * possibilities should be considered:
     * 
     *   1) There may be conflicting matches against multiple products.
     *      That is, one trie node matches against exactly one product, and
     *      another node matches against exactly one different product.
     *      More generally, two trie nodes may match but the intersection
     *      of the sets of matching products is empty.
     *      
     *      If this occurs, it is likely because the listing refers to an
     *      accessory (such as a battery, case, etc) that may be used for
     *      multiple products.  For example:
     *      
     *      "Battery pack to be used with Canon EOS 5D, 7D, or T2i cameras"
     *      
     *      Notice that a listing like this one would not be caught by the
     *      "for/pour" rule in the preprocessing stage.
     *      
     *   2) Depending on the word separation for the model number in the
     *      product database vs the listings, and whether a family name is
     *      included or not may cause quirks in the matching.  For example,
     *      consider the following products:
     *      
     *       (a) "Canon EOS Rebel T1i"
     *       (b) "Canon EOS Rebel T2i"
     *       (c) "Canon Rebel T3i"
     *       
     *      Here, "EOS" probably should be included in (c), but it is not.
     *      If the listing says "Canon EOS Rebel T3i", then EOS will match
     *      against (a), (b), (and probably others), but "Rebel T3i" will
     *      result in a unique match (c).  Even though the resulting
     *      intersection of matching sets will be empty, we want the latter
     *      match to take precedence because it is a unique match.
     *      
     *  The following rules are therefore used to resolve the matches:
     *  
     *   1) If there are any nodes which match against only one product,
     *      then we return that product as long as ALL such nodes match
     *      against the same product.
     *   2) If all matching nodes match against multiple products, then we
     *      return a set containing only those products which are matched
     *      by every node.  Note that, because we have eliminated matching
     *      nodes that were ancestors of other matching nodes, conflicts
     *      between such pairs of nodes do not affect the results.  We only
     *      consider maximal matches.
     */
    var results : ProductMap = null;
    var foundSingleton = false;
    matches.values.foreach(products => {
      if (!foundSingleton && products.size == 1) {
        foundSingleton = true;
        results = products;
      } else {
        if (foundSingleton) {
          // if we've already found a singleton, only consider other
          // singletons from here on.
          if (products.size == 1) results.filterKeys(products contains);
        } else { // !foundSingleton
          if (results == null) {
            results = products
          } else {
            results.filterKeys(products contains);
          }
        }
      }
    })
    
    /* If there are still multiple matching products, eliminate all those
     * matches which are not maximal (i.e., for which there is some suffix
     * that could be appended to the match to make a longer match).  This
     * is to handle the possibility that every string that matches one
     * product also matches another.  For example, consider:
     * 
     *   (a) Pentax WG-1
     *   (b) Pentax WG-1 GPS
     * 
     * There is no string that matches (a) that would not also match (b).
     * Consider the following listing:
     * 
     *   "PENTAX Optio WG-1 14 MP Rugged Waterproof Digital Camera"
     *   
     * Without any further consideration, this would match both (a) and (b)
     * and thus the program would report no unique match.  The logic we are
     * using here is that "WG-1" constitutes a maximal match for (a), but
     * there is something that *could* be appended to that substring to
     * create a longer match for (b).  In this case, we accept (a) and
     * reject (b).
     */
    if (useMaximalFlag && results != null && results.size > 1)
      results.filter(_._2)
    else results
    
  }
  
  /**
   * Matches the specified string with at most one <code>Product</code>.
   * @param root The <code>Trie</code> node at the root of the trie to use to
   * 		match against.
   * @param s The <code>String</code> to match against.
   * @param filter A <code>Map</code> of <code>Product</code>s used to filter
   * 		the results.  If present, the specified trie will be treated as if
   * 		it only contained products in this <code>Map</code>.
   * @param useMaximalFlag A value indicating whether the matching should
   * 		return only maximal matches if there would otherwise be multiple
   * 		matching products.
   * @return The matching <code>Product</code>, if there is exactly one, or
   * 		<code>None</code> if zero or more than one <code>Product</code>
   * 		matches.
   */
  private def matchOne(
      trie: ProductTrie, 
      s: String, 
      filter: ProductMap, 
      useMaximalFlag: Boolean) : Option[Product] = {
    val products = matchAll(trie, s, filter, useMaximalFlag)
    if (products != null && products.size == 1)
      Some(products.head._1)
    else None
  }

}