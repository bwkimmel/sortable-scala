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
  
  /**
   * Adds the sepcified product to the tries.
   * @param product The <code>Product</code> to add.
   */
  def += (product: Product) = {

    /* Add the manufacturer string to a separate trie. */
    processField(manufacturerTrie, product, false, product.manufacturer)

	/* Some product entries have the family, while others what have what
	 * looks to be the "family" as part of the model.  Still others may
	 * only include the "family" within the product_name field.  For
	 * example, in the provided product list, the Canon "EOS" family seems
	 * particularly inconsistent about where the word "EOS" is found.  To
	 * capture all possible cases, we include several possibilities that
	 * may represent the whole model name.  Note that these only indicate
	 * which strings *might* be matches for a given product -- so there's
	 * no harm in adding "too much" information here.
	 */
    processField(modelTrie, product, true, product.name)
    processField(modelTrie, product, true, product.model)
    processField(modelTrie, product, true,
      if (product.family.nonEmpty)
        product.family + " " + product.model
      else product.model)

  }
  
  private def processField(root: ProductTrie, product: Product, isModel: Boolean, value: String) = {
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