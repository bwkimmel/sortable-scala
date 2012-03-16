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
    
//    val trie = new Trie[Boolean]
//    
//    trie insert "abc" data = Some(true)
//    trie insert "abd" data = Some(true)
//    trie insert "all" data = Some(true)
//    trie insert "xyz" data = Some(true)
//    
//    println(trie)
//    exit(0)
    
    val builder = new ProductTrieBuilder
    val start = System.currentTimeMillis()
    Source.fromFile("products.txt").getLines.foreach(line =>
        builder += new Product(Json.parse(line)))
    val end = System.currentTimeMillis()
    println(end - start)
    
    val out = new PrintStream("output.txt")
    out.println("---MANUFACTURER---")
    out.println(builder.manufacturerTrie)
    out.println("---MODEL---")
    out.println(builder.modelTrie)
    out.close()
    
    val map = collection.mutable.Map.empty[String, String]
    map("abc") = "def"
    map("ghi") = "jkl"
    println(map)
  }
  
}