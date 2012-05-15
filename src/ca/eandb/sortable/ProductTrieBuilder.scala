/**
 *
 */
package ca.eandb.sortable

import ca.eandb.sortable.ProductTrieBuilder._
import scala.collection.mutable._

/**
 * A builder that creates tries to use to match against the model name and the
 * manufacturer for each of the <code>Product</code>s provided.  After all
 * <code>Product</code>s have been added, each <code>Trie</code> node
 * corresponding to a match for one or more products will have a
 * <code>ProductTrieBuilder.ProductMap</code> in its data field.  The
 * strings to be associated with a given product include the concatenations of
 * all sequences of consecutive words within:
 * 
 * 	- For the manufacturer trie: the <code>manufacturer</code> field.
 *  - For the model trie:
 *    - the "model" field, or
 *    - the "product_name" field, or
 *    - the "family" field concatenated with the "model" field.
 *    
 * except for certain strings which are judged not likely to be proper matches
 * (see comments in {@link #processField(Trie, Product, Boolean, String)}
 * below).
 * 
 * @see Trie#data
 * @author Brad Kimmel
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
  def += (product: Product) {

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
  private def processField(root: ProductTrie, product: Product, isModel: Boolean, value: String) {
    
     /* Split the string into its component words and insert the concatenation
      * of every consecutive subsequence of those words into the trie, subject
      * to some additional rules described below.
      */
     val words = StringUtil.normalize(value).split(" ").toList
     
     /* Function indicating whether the specified substring should be inserted
      * into the trie.  This helps eliminate some false positives.
      * 
      * - Don't consider single-character matches.  For example, for
      *   the "Pentax K-r", we don't want to consider "K" or "r" in
      *   isolation to be a match.
      * - Don't consider long words (> 3 characters) of only letters
      *   to be a valid match for a model name.  Some model names
      *   have common dictionary words in them, which may erroneously
      *   match one of the words in the listing (e.g., "Canon EOS
      *   Kiss Digital X3" -- the word "Digital" may trigger many
      *   false positives).  Ideally, we want to allow model names
      *   that include only letters (such as IXUS, ELPH).  As a
      *   compromise, we are assuming 4+ letters in a row to be a
      *   likely dictionary word.  A better solution might be to 
      *   search against a word list (such as the unix word list).
      * - Don't consider short strings of only numbers to be a match.
      */
     def acceptString(anyLetters: Boolean, anyNumbers: Boolean, totalLength: Int) =
       (totalLength > 1) &&
       ((!isModel) || (totalLength <= 3) || anyNumbers) &&
       ((totalLength >= 4) || anyLetters)
     
     /**
      * Adds a list of words to the trie starting from the specified node. The
      * node corresponding to the end of each word is associated with the
      * product. 
      * @param words A <code>List</code> of the words to add.
      * @param ancestor The <code>Trie</code> node corresponding to the end of
      *   the previous word.  After inserting the next word, the product
      *   association for <code>ancestor</code> is marked as non-maximal.
      * @param trie The <code>Trie</code> node at which to insert the words.
      * @param anyLetters A value indicating if there are any letters already
      *   inserted for this word chain.
      * @param anyNumbers A value indicating if there are any numbers already
      *   inserted for this word chain.
      * @param totalLength The sum of the lengths of the words already inserted
      *   for this word chain.
      */
     def addChain(
         words: List[String], 
         ancestor: Option[ProductTrie] = None,
         trie: ProductTrie = root,
         anyLetters: Boolean = false,
         anyNumbers: Boolean = false,
         totalLength: Int = 0) {
       if (words nonEmpty) {
         
		/* Add the word to the tip of the word chain in the trie, but
		 * only associate the product with it if it passes certain tests
		 * below.
		 */
         val tip = trie insert words.head
         
         // update word chain stats
         var nextAncestor = ancestor
         val nextAnyLetters = anyLetters || !allNumbers.matcher(words head).matches()
         val nextAnyNumbers = anyNumbers || !allLetters.matcher(words head).matches()
         val nextTotalLength = totalLength + words.head.length
         
         if (acceptString(nextAnyLetters, nextAnyNumbers, nextTotalLength)) {
           
           // associate the product with the current trie node.
           tip.data = Some(tip.data.getOrElse(Map.empty) + { product.name -> true })
           
           nextAncestor = Some(tip)

           // ancestor is no longer maximal
           ancestor match {
             case Some(prev) => prev.data = Some(prev.data.get + { product.name -> false })
             case None => ()
           }
         }
         
         // append the remainder of the words from the current trie node.
         addChain(words tail, nextAncestor, tip, nextAnyLetters, nextAnyNumbers, nextTotalLength)
       }
     }
       
     /* Insert the word chain corresponding to each tail of the list of words.
      * This will ensure that every subsequence of consecutive words (that fits
      * the criteria above) is associated with the product.
      */
     words.tails.foreach(addChain(_))
    
  }

}

object ProductTrieBuilder {
  
  /* Some type aliases. */
  
  /**
   * A set of <code>Product</code>s associated with a <code>Trie</code> node.
   * The key is the <code>Product</code>.  The value indicates if the node
   * corresponds to a maximal match.
   */
  type ProductMap = scala.collection.Map[String, Boolean]
  
  /**
   * A <code>Trie</code> for associating strings with sets of
   * <code>Product</code>s.
   */
  type ProductTrie = Trie[ProductMap]

}