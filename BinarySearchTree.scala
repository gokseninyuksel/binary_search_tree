import java.util.concurrent.PriorityBlockingQueue
import scala.annotation.tailrec

case object BinarySearchTree extends App{

  /**
   * An trait for making implicit comparable vals for creating binary trees
   * that store various types.
   * @tparam A
   */
  trait Comparable[A]{
    def biggerThan(a:A,b:A):Boolean
    def smallerThan(a:A,b:A): Boolean
    def print(a:A): String
    def equals(a:A,b:A): Boolean
  }

  /**
   * Implicit integer comparable declaration
   */
  implicit val intComparable: Comparable[Int] = new Comparable[Int] {
    override def biggerThan(a: Int, b: Int): Boolean = a > b

    override def smallerThan(a: Int, b: Int): Boolean = b < a

    override def print(a: Int): String = a.toString

    override def equals(a:Int, b:Int): Boolean = a == b
  }

  /**
   * Binary search tree trait, which is covariant in A.
   * That means it is able to take the parameter B that is B <: A
   *
   * @tparam A
   */
  sealed trait BinarySearchTree[+A]{
    /**
     * abstract function for returning left part of binary search tree.
     * @return left binary search tree
     */
    def left: Option[BinarySearchTree[A]] = this match{
      case Node(_,l,_) => Some(l)
      case Empty => None
    }
    /**
     * abstract function for returning right part of binary search tree.
     * @return right binary search tree
     */
    def right: Option[BinarySearchTree[A]] = this match{
      case Node(_,_,r) => Some(r)
      case Empty => None
    }
    /**
     * abstract function for returning the value of node
     * @return value of node
     */
    def value: Option[A] = this match{
      case Node(n,_,_) => Some(n)
      case Empty => None
    }
  }
  case class Node[A] (a:A,leftN: BinarySearchTree[A], rightN: BinarySearchTree[A]) extends BinarySearchTree[A]
  case object Empty extends BinarySearchTree[Nothing]

  /**
   * Given the value v, this function inserts the v in appropriate node of the binary search tree
   * @param binarySearchTree
   * @param v
   * @tparam A
   * @return binary tree with the inserted node
   */

  def insert[A: Comparable](binarySearchTree: BinarySearchTree[A])(v:A): Option[BinarySearchTree[A]] = binarySearchTree match{
    case n : Node[A] => if(implicitly[Comparable[A]].biggerThan(v,n.value.get)) Some(n.copy(rightN = insert(n.right.get)(v).get))
                                                                       else Some(n.copy(leftN = insert(n.left.get)(v).get))
    case Empty => Some(N(v,Empty,Empty))
  }
  /**
   * Given the value list list, this function inserts all the elements in the list,
   * and returns the binary search tree corresponding to the list.
   * @param binarySearchTree
   * @param v
   * @tparam A
   * @return binary tree with the inserted node
   */
  def fromList[A:Comparable](list: List[A]) :Option[BinarySearchTree[A]] = {
    /**
     * Helper method that works as a accumulator given Empty binary search tree.
     * @param list
     * @param binarySearchTree
     * @return
     */
    @tailrec
    def helper(list:List[A])(binarySearchTree: BinarySearchTree[A]) : Option[BinarySearchTree[A]] = list match{
      case Nil => Some(binarySearchTree)
      case x :: xs => helper(xs)(insert(binarySearchTree)(x).get)
  }
    helper(list)(Empty)
}

  /**
   * Using bread first strategy, return the size of the binary search tree. O(logN)
   * where N is number of elements in the tree.
   * @param binarySearchTree
   * @tparam A
   * @return
   */
  def size[A: Comparable](binarySearchTree: BinarySearchTree[A]): Int = binarySearchTree match{
    case Node(_,l,r) => 1 + size(l) + size(r)
    case Empty => 0
  }
  def isValid[A:Comparable](binarySearchTree: BinarySearchTree[A])(v:A): Boolean = binarySearchTree match{
    case n: Node[A] => implicitly[Comparable[A]].equals(n.value.get,v) && isValid(n.left.get)(n.value.get) && isValid(n.right.get)(n.value.get)
    case Empty => true
  }
  def N[A](v:A, l:BinarySearchTree[A],r: BinarySearchTree[A]): BinarySearchTree[A] = Node(v,l,r)
  val binarySearchTree : BinarySearchTree[Int] = fromList(List(8,6,10,3,7,12)).get
  print(binarySearchTree)
}
