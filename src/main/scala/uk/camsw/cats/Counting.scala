package uk.camsw.cats

import cats.Monoid

object Counting extends App {

  /**
    * This is a simple fold that counts the occurrences of characters within a string
    * (ignores case as not particularly interesting!)
    */
  def countChars(s: String): Map[Char, Int] = {
    s.toCharArray.toList.foldLeft(Map.empty[Char, Int])((acc, c) => {
      if (acc contains c) acc + (c -> (acc(c) + 1))
      else acc + (c -> 1)
    })
  }

  println(s"countChars:\t\t\t\t\t\t${countChars("hello world")}")
  //Map(e -> 1,   -> 1, l -> 3, h -> 1, r -> 1, w -> 1, o -> 2, d -> 1)

  /**
    * Int is a member of the Monoid type class
    * A monoid has:
    * An empty case
    * An (associative) append (or combine) operation
    * Left identity and right identity (x + empty = x, empty + x = x)
    *
    * Map is also a member of the Monoid type class.
    * Its append operation merges two maps, utilising the monoid instance for the type of the value
    *   e.g.   Map(a -> 1) combine Map(a -> 2) = Map(a -> 3)
    * Thus we can 'simplify' (totally debatable) the above function to...
    */
  def countCharsUsingMonoid(s: String)(implicit M: Monoid[Map[Char, Int]]): Map[Char, Int] = {
    s.toCharArray.toList.foldLeft(Map.empty[Char, Int])((acc, c) =>
      M.combine(acc, Map(c -> 1))
    )
  }

  // In order for the below to compile we must have monoid instances for Int and Map in scope
  import cats.instances.int._
  import cats.instances.map._
  println(s"countCharsUsingMonoid:\t\t\t${countCharsUsingMonoid("hello world")}")

  /**
    * Cats provide an extension method syntax so we can use the symbols
    * |+| is the monoid append.  Note we don't need to inject the monoid instance for Map, but the instance must still be in-scope for this to compile
    */
  def countCharsUsingMonoidSyntax(s: String): Map[Char, Int] = {
    import cats.syntax.monoid._

    s.toCharArray.toList.foldLeft(Map.empty[Char, Int])((acc, c) =>
      acc |+| Map(c -> 1)
    )
  }

  println(s"countCharsUsingMonoidSyntax:\t${countCharsUsingMonoidSyntax("hello world")}")

  /**
    * That's all good, but once we utilise the monoid then we've basically defined a function that can be used for
    * any type for which a Monoid instance exists
    * .... without losing our type-safety
    * Furthermore because a monoid must be associative we can break down complex ops and perform them in parallel
    */
  def countCharsGeneric[A](s: String, z: A)(implicit M: Monoid[A]): Map[Char, A] = {
    import cats.syntax.monoid._

    s.toCharArray.toList.foldLeft(Map.empty[Char, A])((acc, c) =>
      acc |+| Map(c -> z)
    )
  }

  import cats.instances.float._
  // Now we can simply pass the float we want to append for each character, the compiler will find the Monoid[Float] type class instance and now we can count chars using floats
  println(s"countCharsGeneric (float):\t\t${countCharsGeneric("hello world", 1.0f)}")
  // Map(e -> 1.0,   -> 1.0, l -> 3.0, h -> 1.0, r -> 1.0, w -> 1.0, o -> 2.0, d -> 1.0)

  /**
    * Because we aren't using the Monoid instance directly (we just require there is one in scope for any 'A'), we can use
    * a more concise syntax to define a constraint for the type 'A'
    */
  def countCharsGeneric2[A: Monoid](s: String, z: A): Map[Char, A] = {
    import cats.syntax.monoid._   // Obviously you probably wouldn't import this here, I'm just trying to demonstrate what's required where by keeping it local

    s.toCharArray.toList.foldLeft(Map.empty[Char, A])((acc, c) =>
      acc |+| Map(c -> z)
    )
  }
  println(s"countCharsGeneric2 (float):\t\t${countCharsGeneric2("hello world", 1.0f)}")
  // Works as before


  /**
    * Cats provides instances for tuples, of course the types within the tuple must have monoid instances in scope
    * This is cool because now our function can update both elements in our tuple2 in a single parse, with no changes to code
    * Type inference frin the 'append' operand is enough to create the Monoid[(String, Double)] and make it available to the function
    */

  import cats.instances.tuple._
  import cats.instances.string._
  import cats.instances.double._
  println(s"countCharsGeneric2 (tuple):\t\t${countCharsGeneric2("hello world", ("*", 1d))}")
  // Map(e -> (*,1.0),   -> (*,1.0), l -> (***,3.0), h -> (*,1.0), r -> (*,1.0), w -> (*,1.0), o -> (**,2.0), d -> (*,1.0))


  /**
    * I can provide my own monoid instance for a type.  I think it's normal to have multiple monoid instances for a type and bring the correct one into
    * scope according to what you want to do
    * For example:
    *   additionIntMonoid[ empty = 0, append = + ]
    *   multipleIntMonoid[ empty = 1, append = * ]
    *
    * Here we create a monoid that tells us which chars appear once and only once
    */
  implicit val oneOccurrenceMonoidInstance: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x && !y
  }

  // Now I can use the function with my custom monoid instance.
  // Note that because we are using type classes I can provide conformance to an interface for classes I
  // don't have the source for.  No adapters - ad-hoc polymorphism!
  // Of course I could create a Monoid[....] for any type, providing it conforms to the monoid laws of identity and associativity
  println(s"countCharsGeneric2 (one only):\t\t${countCharsGeneric2("hello world", true)}")
  // Map(e -> true,   -> true, l -> false, h -> true, r -> true, w -> true, o -> false, d -> true)

  /**
    * And of course because everything has a monoid instance, we could include the actual count as part of a tuple at no extra cost!
    */
  println(s"countCharsGeneric2 (count and one only):\t\t${countCharsGeneric2("hello world", (1, true))}")
  // Map(e -> (1,true),   -> (1,true), l -> (3,false), h -> (1,true), r -> (1,true), w -> (1,true), o -> (2,false), d -> (1,true))

  /**
    * Or a tuple3 with an asterix count as well
    */
  println(s"countCharsGeneric2 (count and one only):\t\t${countCharsGeneric2("hello world", (1, true, "*"))}")
  // Map(e -> (1,true,*),   -> (1,true,*), l -> (3,false,***), h -> (1,true,*), r -> (1,true,*), w -> (1,true,*), o -> (2,false,**), d -> (1,true,*))
}
