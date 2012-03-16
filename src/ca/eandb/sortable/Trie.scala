/**
 *
 */
package ca.eandb.sortable

import scala.collection.mutable._

/**
 * @author Brad Kimmel
 *
 */
final class Trie[T](val parent: Trie[T] = null, val char: Char = '\0') {
  
  private var children: Map[Char, Trie[T]] = Map.empty
  var data: Option[T] = None
  
  def get(default: => T) : T = data match {
    case Some(x) => x
    case None => { data = Some(default); data.get }
  }
  
  def insert(c: Char) : Trie[T] = children get c match {
    case Some(child) => child
    case None => { 
      val child = new Trie[T](this, c)
      children(c) = child
      child
    }
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