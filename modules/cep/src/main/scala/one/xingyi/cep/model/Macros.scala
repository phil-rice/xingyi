package one.xingyi.cep.model

import one.xingyi.cep._
import one.xingyi.core.builder.HasAggregator

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.reflect._
import scala.reflect.macros.{Context, blackbox, whitebox}


object Macros {
  def desugar(a: Any): String = macro desugarImpl

  def desugarImpl(c: Context)(a: c.Expr[Any]) = {
    import c.universe._

    val s = show(a.tree)
    c.Expr(
      Literal(Constant(s))
    )
  }

  //TODO So this didn't work the way i expected. The stringfield is remake... so we could do this differently and get rid of the stringfield macro, making the name here
  def assignmentImpl(c: whitebox.Context)(value: c.Expr[String]): c.Expr[StringField] = {
    import c.universe._
    val stringField = c.Expr[StringField](c.prefix.tree)
    val hasAggregator = c.Expr[HasAggregator[StringField]](c.prefix.tree)
    reify {
      {
        val s = stringField.splice
        new StringFieldWithValue(s.event, s.name, v => value.splice)(hasAggregator.splice.aggregator)
      }
    }
  }
//  def whereImpl(c: whitebox.Context)(value: c.Expr[Boolean]): c.Expr[WhereFn] = {
//    import c.universe._
//    val stringField = c.Expr[StringField](c.prefix.tree)
//    c.Expr(
//      q"""{values: StringMap => $value;}""")
//  }


  def statePipelineImpl(c: blackbox.Context)(block: c.Expr[StatePipeline]): c.Expr[UserState] = {
    import c.universe._
    val preprocess = (c.Expr[Processor](c.prefix.tree))

    val enclosingValName = definingValName(c, methodName => s"""$methodName must be directly assigned to a val, such as `val x = $methodName("details of pipelines")`.""")
    val name = c.Expr[String](Literal(Constant(enclosingValName)))
    //    val freshName = TermName(c.freshName("result$"))
    reify {
      preprocess.splice.newState(new UserState(name.splice, List()), List(block.splice))
    }
  }
  def statePipelinesImpl(c: blackbox.Context)(block: c.Expr[List[StatePipeline]]): c.Expr[UserState] = {
    import c.universe._
    val preprocess = (c.Expr[Processor](c.prefix.tree))

    val enclosingValName = definingValName(c, methodName => s"""$methodName must be directly assigned to a val, such as `val x = $methodName("details of pipelines")`.""")
    val name = c.Expr[String](Literal(Constant(enclosingValName)))
    //    val freshName = TermName(c.freshName("result$"))
    reify {
      preprocess.splice.newState(new UserState(name.splice, List()), block.splice)
    }
  }
  def stringFieldImpl(c: blackbox.Context): c.Expr[StringField] = {
    import c.universe._
    val hasAggregator = (c.Expr[HasAggregator[StringField]](c.prefix.tree))
    val withFields = (c.Expr[EventWithFields](c.prefix.tree))

    val enclosingValName = definingValName(c, methodName => s"""$methodName must be directly assigned to a val, such as `val x = $methodName[Int]("description")`.""")
    val name = c.Expr[String](Literal(Constant(enclosingValName)))
    reify {
      new SimpleStringField(withFields.splice, name.splice)(hasAggregator.splice.aggregator)
    }
  }


  def definingValName(c: blackbox.Context, invalidEnclosingTree: String => String): String = {
    import c.universe.{Apply => ApplyTree, _}
    val methodName = c.macroApplication.symbol.name
    def processName(n: Name): String =
      n.decodedName.toString.trim // trim is not strictly correct, but macros don't expose the API necessary
    @tailrec def enclosingVal(trees: List[c.Tree]): String = {
      trees match {
        case ValDef(_, name, _, _) :: _ => processName(name)
        case (_: ApplyTree | _: Select | _: TypeApply) :: xs => enclosingVal(xs)
        // lazy val x: X = <methodName> has this form for some reason (only when the explicit type is present, though)
        case Block(_, _) :: DefDef(mods, name, _, _, _, _) :: _ if mods.hasFlag(Flag.LAZY) =>
          processName(name)
        case _ =>
          c.error(c.enclosingPosition, invalidEnclosingTree(methodName.decodedName.toString))
          "<error>"
      }
    }
    enclosingVal(enclosingTrees(c).toList.asInstanceOf[List[c.Tree]])
  }
  def enclosingTrees(c: blackbox.Context): Seq[c.Tree] =
    c.asInstanceOf[reflect.macros.runtime.Context]
      .callsiteTyper
      .context
      .enclosingContextChain
      .map(_.tree.asInstanceOf[c.Tree])
}