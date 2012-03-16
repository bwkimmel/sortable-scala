/**
 *
 */
package ca.eandb.sortable

/**
 * @author Brad Kimmel
 *
 */
final class Trie[T] {
  
  private var children: Map[Char, Trie[T]] = Map.empty
  val data: Set[T] = Set.empty
  
  def insert(c: Char) : Trie[T] = {
    val child = new Trie[T]
    children += c -> child
    child
  }
  
  def insert(s: String) : Trie[T] = (this /: s)(_.insert(_))

  def find(c: Char) : Option[Trie[T]] = children get c
  def find(s: String) : Option[Trie[T]] =
    if (s isEmpty) Some(this)
    else find(s head) match {
      case Some(t) => t find s.tail
      case None => None
    }
  
}