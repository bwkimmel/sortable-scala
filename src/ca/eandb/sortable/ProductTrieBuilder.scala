/**
 *
 */
package ca.eandb.sortable

import ca.eandb.sortable.ProductTrieBuilder._
import scala.collection.mutable._

/**
 * @author brad
 *
 */
final class ProductTrieBuilder {
  
  private val allNumbers = "^[0-9]*$".r.pattern
  private val allLetters = "^[a-z]*$".r.pattern
  
  val manufacturerTrie = new ProductTrie
  val modelTrie = new ProductTrie
  
  def += (product: Product) = {
    addStrings(manufacturerTrie, product, false, product.manufacturer)
    addStrings(modelTrie, product, true, product.name)
    addStrings(modelTrie, product, true,
        if (product.family.nonEmpty) 
          product.family + " " + product.model
        else product.model)
  }
  
  private def addStrings(root: ProductTrie, product: Product, isModel: Boolean, value: String) = {
     val words = StringUtil.normalize(value).split(" ").toList
     
     def acceptString(anyLetters: Boolean, anyNumbers: Boolean, totalLength: Int) =
       (totalLength > 1) &&
       ((!isModel) || (totalLength <= 3) || anyNumbers) &&
       ((totalLength >= 4) || anyLetters)
     
     def addChain(
         words: List[String], 
         ancestor: Option[ProductTrie] = None,
         trie: ProductTrie = root,
         anyLetters: Boolean = false,
         anyNumbers: Boolean = false,
         totalLength: Int = 0): Unit = {
       if (words nonEmpty) {
         val tip = trie insert words.head
         var nextAncestor = ancestor
         val nextAnyLetters = anyLetters || !allNumbers.matcher(words head).matches()
         val nextAnyNumbers = anyNumbers || !allLetters.matcher(words head).matches()
         val nextTotalLength = totalLength + words.head.length
         if (acceptString(nextAnyLetters, nextAnyNumbers, nextTotalLength)) {
           tip.data = Some(tip.data.getOrElse(Map.empty) + { product -> true })
           
           nextAncestor = Some(tip)

           ancestor match {
             case Some(prev) => prev.data = Some(prev.data.get + { product -> false })
             case None => ()
           }
         } 
         addChain(words tail, nextAncestor, tip, nextAnyLetters, nextAnyNumbers, nextTotalLength)
       }
     }
       
     words.tails.foreach(addChain(_))
    
  }

}

object ProductTrieBuilder {
    
  type ProductMap = scala.collection.Map[Product, Boolean]
  type ProductTrie = Trie[ProductMap]

}