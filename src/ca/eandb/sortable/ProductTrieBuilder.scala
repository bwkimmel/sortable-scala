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
  
  /** A pattern that matches strings of only numbers. */
  private val allNumbers = "^[0-9]*$".r.pattern
  
  /** A pattern that matches strings of only letters. */
  private val allLetters = "^[a-z]*$".r.pattern
  
  /** The manufacturer <code>Trie</code>. */
  val manufacturerTrie = new ProductTrie
  
  /** The model <code>Trie</code>. */
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
  
	/**
	 * Inserts substrings of the provided string that are to be considered as
	 * matches into the trie and associate the specified product with the nodes
	 * corresponding to the ends of those substrings. 
	 * @param root The root <code>TrieNode</code> of the trie to insert into.
	 * @param product The <code>Product</code> to associate with the substrings
	 * 		of <code>value</code>.
	 * @param isModel A value indicating if this string is for the model trie
	 * 		(affects the rules used to judge whether a substring is considered
	 * 		to be a match).
	 * @param value The <code>String</code> whose substrings to insert into the
	 * 		trie.
	 */
  private def processField(root: ProductTrie, product: Product, isModel: Boolean, value: String) = {
    
     /* Split the string into its component words and insert the concatenation
      * of every consecutive subsequence of those words into the trie, subject
      * to some additional rules described below.
      */
     val words = StringUtil.normalize(value).split(" ").toList
     
     /* Function indicating whether the specified substring should be inserted
      * into the trie.
      */
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