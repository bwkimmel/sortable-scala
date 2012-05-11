/**
 *
 */
package ca.eandb.sortable

import scala.collection.mutable._
import ca.eandb.sortable.ProductTrieBuilder._

/**
 * @author brad
 *
 */
final class ListingMatcher(val builder: ProductTrieBuilder) {
  
  def matchListing(listing: Listing) : Option[Product] = {
    val manufacturerProducts = matchAll(builder.manufacturerTrie, listing.manufacturer, null, false)
    if (manufacturerProducts != null)
      matchOne(builder.modelTrie, cleanTitle(listing.title), manufacturerProducts, true)
    else None
  }
  
  private def cleanTitle(title: String) =
    StringUtil.normalize(title)
      .replaceFirst(" for .*", "")
      .replaceFirst(" pour .*", "")

  private def matchAll(
      trie: ProductTrie, 
      s: String, 
      filter: ProductMap, 
      useMaximalFlag: Boolean) : ProductMap = {
    
    var matches: Map[ProductTrie, ProductMap] = Map.empty
    
    def matchCursors(
        cursors: List[ProductTrie],
        words: List[String]) : Unit = {
      val nextCursors : List[ProductTrie] = 
        trie :: cursors.flatMap(_.find(words.head))
        
      nextCursors.tail.foreach(node => node.data match {
        case Some(x) => {
          var m = x
          if (filter != null) {
            m = m.filterKeys(filter contains)
          }
          if (m.nonEmpty) {
            matches += { node -> m }
          }
        }
        case None => ()
      })
          
      
      if (words.tail.nonEmpty) matchCursors(nextCursors, words.tail)
    }

    matchCursors(List(trie), s.split(" ").toList)
    
    matches.headOption match {
      case Some(x) => x._2
      case None => null
    }
    
    
  }
      
  private def matchOne(
      trie: ProductTrie, 
      s: String, 
      filter: ProductMap, 
      useMaximalFlag: Boolean) : Option[Product] = {
    val products = matchAll(trie, s, filter, useMaximalFlag)
    if (products != null && products.nonEmpty)
      Some(products.head._1)
    else None
  }
        
  
}