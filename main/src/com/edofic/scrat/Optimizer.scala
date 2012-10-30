package com.edofic.scrat

import com.edofic.scrat.Tokens._
import TreeOps.Modification
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * User: andraz
 * Date: 10/23/12
 * Time: 11:54 AM
 */
object Optimizer {
  val simplifyFunctionCalls: Modification = {
    case DotAccess(List(f: FunctionCall)) => f
  }
  val recursiveModifications = simplifyFunctionCalls  :: Nil

  def phase1(exp: Expression) = recursiveModifications.foldLeft(exp)((tree, func) => TreeOps.applyRecursiveModificaton(exp)(func))

  def phase2(exp: Expression ) = tailcalls(exp)

  def apply(exps: List[Expression]): List[Expression] = exps map apply
  def apply(exp: Expression): Expression = (phase1 _ andThen phase2)(exp)

  def tailcalls(root: Expression): Expression=
    TreeOps.namedFunctions(root::Nil).foldLeft(root){(exp,func)=>
      TreeOps.applyRecursiveModificaton(exp){
        case f: FunctionDef if f == func => tailcall(f)
      }
    }

  def tailcall(f: FunctionDef): Expression = {
    val FunctionDef(name, argNames, body) = f
    val ends = TreeOps.endpoints(body)
    val modifiedBody = body map { exp =>
      TreeOps.applyRecursiveModificaton(exp){
        case call @ FunctionCall(id, args) if ends.contains(call) && (id==name.get) => Repeat(argNames zip args.lst)
      }
    }
    val newBody = if(body==modifiedBody) body else Tokens.Loop(modifiedBody)::Nil
    FunctionDef(name, argNames, newBody)
  }
}
