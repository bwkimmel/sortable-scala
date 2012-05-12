/**
 *
 */
package ca.eandb.sortable

import scala.collection.immutable._

/**
 * Represents a node in a trie (see {@link http://en.wikipedia.org/wiki/Trie}),
 * a tree structure that stores strings.  Each node in the trie represents a
 * string prefix.  Strings with a common prefix will share a common path from
 * the root of the tree until the point at which the strings diverge.
 * 
 * TODO This could be made more space-efficient by combining chains of nodes
 *      with only one child into a single object.  To accomplish this, we would
 *      refactor this class by extracting its interface to one with this name
 *      (i.e., Trie would become an interface).  The code in this class
 *      would move to a concrete "SingleTrieNode" class and a new
 *      "CompactTrieNodeChain" class would be created that stores the string
 *      representing the chain  The single "data" field would have to become a 
 *      map (mapping the index to the corresponding data object).  A new
 *      class "ChainTrieNode" would then implement the Trie interface and
 *      would have fields for its corresponding CompactTrieNodeChain object as
 *      well as an index into the chain.  Instances of ChainTrieNode would only
 *      be created on-demand.
 * 
 * @author Brad Kimmel
 */
final class Trie[T](val parent: Trie[T] = null, val char: Char = '\0') {
  
  /** A <code>Map</code> used to look up the children for this trie node. */
  private var children: Map[Char, Trie[T]] = Map.empty
  
  /** An optional data object associated with this node. */
  var data: Option[T] = None
  
  /**
   * Inserts a child <code>Trie</code> node into the trie.
   * @param c The <code>char</code> identifying the new child.
   * @return The new child <code>Trie</code> node, or the existing child if
   * 		one already exists corresponding to <code>c</code>.
   */
  def insert(c: Char) : Trie[T] = children get c match {
    case Some(child) => child
    case None => { 
      val child = new Trie[T](this, c)
      children += { c -> child }
      child
    }
  }
  
  /**
   * Inserts a chain of descendant <code>Trie</code> nodes into the trie.
   * @param s The <code>String</code> identifying the path to insert.
   * @return The new descendant <code>Trie</code> node at the end of the path,
   * 		or the existing descendant if one already exists corresponding to
   * 		<code>s</code>.
   */
  def insert(s: String) : Trie[T] = (this /: s)(_.insert(_))
  
  /**
   * Determines if this <code>Trie</code> node is the root of a trie. 
   * @return A value indicating if this <code>Trie</code> node is the root of
   * 		a trie.
   */
  def isRoot = (parent == null);

  /**
   * Finds a child of this <code>Trie</code> node.
   * @param c The <code>Char</code> identifying which child to find.
   * @return The specified child <code>Trie</code> node, or <code>None</code>
   * 		if no such child exists.
   */
  def find(c: Char) = children get c
  
  /**
   * Finds a descendant of this <code>Trie</code> node.
   * @param s The <code>String</code> identifying the path to follow.
   * @return The specified descendant <code>Trie</code> node, or
   * 		<code>None</code> if no such descendant exists.
   */
  def find(s: String) : Option[Trie[T]] =
    if (s isEmpty) Some(this)
    else find(s head) match {
      case Some(t) => t find s.tail
      case None => None
    }
  
  /**
   * Applies a function <code>f</code> to this <code>Trie</code> node and all
   * of its ancestors.
   * @param f The function to apply.
   */
  def foreachAncestorOrSelf(f : Trie[T] => Unit) : Unit = {
    f(this)
    foreachAncestor(f)
  }

  /**
   * Applies a function <code>f</code> to all of this <code>Trie</code> node's
   * ancestors.
   */
  def foreachAncestor(f : Trie[T] => Unit) : Unit =
    if (!isRoot) parent.foreachAncestorOrSelf(f)

  /**
   * Gets the prefix string representing the path from the root node to this
   * node.
   * @param a Accumulates the result.  The prefix string will be prepended
   *   to this.
   * @return The prefix string representing the path from the root node to
   *   this node, with <code>a</code> appended.
   */
  private def pathString(a: String = "") : String =
    if (parent != null)
      parent.pathString(char + a)
    else a
      
  override def toString =
    (data match {
      case Some(x) => this.pathString(" -> " + x.toString + "\n")
      case None => ""
    }) + children.foldLeft("")((s, e) => s + e._2.toString)
     
}