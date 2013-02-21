package process

/**
 * MultiOrdering is a variant of Ordering that makes it easier to compare
 * by multiple field. You just have to instantiate the transforms, which is
 * a list of transformations from the original type to something that has
 * an Ordering. Each of these is applied in turn, until we get something
 * that provides some difference.
 * 
 * TODO: this still isn't quite as clean and concise as I'd like -- you
 * have to declare a fair amount of excess junk to use it. But it's much
 * better than doing it by hand.
 */
trait MultiOrdering[T] extends Ordering[T] {
    def compare(x:T, y:T):Int = doCompare(x, y, transforms)
    def doCompare(x:T, y:T, comps:List[CompF[_]]):Int = {
      comps match {
        case comp :: rest => {
          val byComp = compareOne(x, y, comp)
          if (byComp == 0)
            doCompare(x, y, rest)
          else
            byComp
        }
        case Nil => 0
      }
    }
    
    type Transformer[U] = (T => U)
    type CompF[U] = (T => U, Ordering[U])
    type TransformList = List[CompF[_]]

    def t[U](t:Transformer[U])(implicit arg0:math.Ordering[U]):CompF[U] = (t, arg0)
    val transforms:TransformList
    def compareOne[U](x:T, y:T, pair:CompF[U]):Int = {
      comp(x, y, pair._1)(pair._2)
    }
    def comp[U](x:T, y:T, f:Transformer[U])(implicit arg0:math.Ordering[U]):Int = {
      arg0.compare(f(x), f(y))
    }
}