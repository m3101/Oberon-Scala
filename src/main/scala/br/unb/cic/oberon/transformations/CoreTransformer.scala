package br.unb.cic.oberon.transformations

import br.unb.cic.oberon.ast._
import scala.collection.mutable.ListBuffer
import br.unb.cic.oberon.ast.Procedure
import br.unb.cic.oberon.visitor.OberonVisitor

class CoreVisitor() extends OberonVisitor[Statement] {  
  
  var addedVariables: List[VariableDeclaration] = Nil

  def visit(stmt: Statement): Statement = stmt match {
    case SequenceStmt(stmts) =>
      SequenceStmt(flatSequenceOfStatements(SequenceStmt(transformListStmts(stmts)).stmts))

    case LoopStmt(stmt) =>
      WhileStmt(BoolValue(true), stmt.accept[Statement,CoreVisitor](this))

    case RepeatUntilStmt(condition, stmt) =>
      WhileStmt(BoolValue(true), SequenceStmt(List(stmt.accept[Statement,CoreVisitor](this), IfElseStmt(condition, ExitStmt(), None))).accept[Statement,CoreVisitor](this))

    case ForStmt(initStmt, condition, block) =>
      SequenceStmt(List(initStmt.accept[Statement,CoreVisitor](this), WhileStmt(condition, block.accept[Statement,CoreVisitor](this))))

    case IfElseIfStmt (condition, thenStmt, elsifStmt, elseStmt) =>
      IfElseStmt (condition, thenStmt.accept[Statement,CoreVisitor](this), Some(transformElsif(elsifStmt, elseStmt)))

    case CaseStmt(exp, cases, elseStmt) => transformCase(exp, cases, elseStmt)

    case WhileStmt(condition, stmt) =>
      WhileStmt(condition, stmt.accept[Statement,CoreVisitor](this))

    case _ => stmt
  }

  private var nextCaseId = 0

  private def getNextCaseId(): Int = {
    val returnId = nextCaseId
    nextCaseId += 1
    returnId
  }

  private def transformCase(exp: Expression, cases: List[CaseAlternative], elseStmt: Option[Statement]): Statement = {
    val coreElseStmt = if (elseStmt.isEmpty) None else Some(elseStmt.get.accept[Statement,CoreVisitor](this))

    // TODO corrigir comportamento para outras expressões

    val caseExpressionId = exp match {
      case VarExpression(name) => name
      case _ => f"case_exp#${getNextCaseId}"
    }
    val caseExpressionEvaluation = AssignmentStmt(VarAssignment(caseExpressionId), exp)

    def casesToIfElseStmt(cases: List[CaseAlternative]): IfElseStmt =
      cases match {
        case SimpleCase(condition, stmt) :: Nil =>
          val newCondition =
            EQExpression(VarExpression(caseExpressionId), condition)
          IfElseStmt(newCondition, stmt.accept[Statement,CoreVisitor](this), coreElseStmt)

        case SimpleCase(condition, stmt) :: tailCases =>
          val newCondition =
            EQExpression(VarExpression(caseExpressionId), condition)
          val newElse = Some(casesToIfElseStmt(tailCases))
          IfElseStmt(newCondition, stmt.accept[Statement,CoreVisitor](this), newElse)

        case RangeCase(min, max, stmt) :: Nil =>
          val newCondition = AndExpression(
            LTEExpression(min, VarExpression(caseExpressionId)),
            LTEExpression(VarExpression(caseExpressionId), max)
          )
          IfElseStmt(newCondition, stmt.accept[Statement,CoreVisitor](this), coreElseStmt)

        case RangeCase(min, max, stmt) :: tailCases =>
          val newCondition = AndExpression(
            LTEExpression(min, VarExpression(caseExpressionId)),
            LTEExpression(VarExpression(caseExpressionId), max)
          )
          val newElse = Some(casesToIfElseStmt(tailCases))
          IfElseStmt(newCondition, stmt.accept[Statement,CoreVisitor](this), newElse)

        case _ => throw new RuntimeException("Invalid CaseStmt without cases")
      }
    
    exp match {
      case VarExpression(_) => casesToIfElseStmt(cases)
      case _ =>
        addedVariables = VariableDeclaration(caseExpressionId, IntegerType) :: addedVariables
        SequenceStmt(List(caseExpressionEvaluation, casesToIfElseStmt(cases)))
    }
  }

  private def transformElsif (elsifStmts: List[ElseIfStmt], elseStmt: Option[Statement]): Statement = {
    val coreElseStmt = if (elseStmt.isEmpty) None else Some(elseStmt.get.accept[Statement,CoreVisitor](this))
    val currentElsif = elsifStmts.head

    if (elsifStmts.tail.isEmpty) {
      IfElseStmt(currentElsif.condition, currentElsif.thenStmt.accept[Statement,CoreVisitor](this), coreElseStmt)
    } else {
      val nextElsif = Some(transformElsif(elsifStmts.tail, coreElseStmt))
      IfElseStmt(currentElsif.condition, currentElsif.thenStmt.accept[Statement,CoreVisitor](this), nextElsif)
    }
  }

  private def transformListStmts(stmtsList: List[Statement], stmtsCore: ListBuffer[Statement] = new ListBuffer[Statement]): List[Statement] = {

    if (!stmtsList.isEmpty){
      stmtsCore += stmtsList.head.accept[Statement,CoreVisitor](this);
      stmtsCore :: transformListStmts(stmtsList.tail, stmtsCore)
    }
    else {
      return Nil
    }

    stmtsCore.toList
  }

  private def transformProcedureListStatement(listProcedures: List[Procedure]): List[Procedure] = {

    var listProceduresCore = ListBuffer[Procedure]()

    for (procedure <- listProcedures){
      addedVariables = Nil
      val coreStmt = procedure.stmt.accept[Statement,CoreVisitor](this)
      listProceduresCore += Procedure(name = procedure.name,
        args = procedure.args,
        referenceMap = procedure.referenceMap,
        returnType = procedure.returnType,
        constants = procedure.constants,
        variables = procedure.variables ++ addedVariables,
        stmt = coreStmt)
    }
    listProceduresCore.toList
  }

  def flatSequenceOfStatements(stmts: List[Statement]): List[Statement] =
    stmts match {
      case SequenceStmt(ss) :: rest => flatSequenceOfStatements(ss) ++ flatSequenceOfStatements(rest)
      case s :: rest => s :: flatSequenceOfStatements(rest)
      case Nil => List()
    }

  def transformModule(module: OberonModule): OberonModule = {
    
    val stmtprocedureList = transformProcedureListStatement(module.procedures)
    val stmtcore = module.stmt.get.accept[Statement,CoreVisitor](this)

     val coreModule = OberonModule(
      name = module.name,
      submodules = module.submodules,
      userTypes = module.userTypes,
      constants = module.constants,
      variables = module.variables ++ addedVariables,
      procedures = stmtprocedureList,
      stmt = Some(stmtcore)
    )

    coreModule
  }

}

object CoreChecker extends OberonVisitor[Unit] {
  private var isCore = true

  def visit(stmt: Statement): Unit = {

    if (!isCore) {
      return
    }

    stmt match {
      case SequenceStmt(stmts) => transformListStmts(stmts)
      case LoopStmt(stmt) => isCore = false
      case RepeatUntilStmt(condition, stmt) => isCore = false
      case ForStmt(initStmt, condition, block) => isCore = false

      // Condicionais
      case IfElseIfStmt (condition, thenStmt, elsifStmt, elseStmt) => isCore = false
      case CaseStmt(exp, cases, elseStmt) => isCore = false

      // Outros casos com SequenceStmt
      case WhileStmt(condition, whileStmt) => whileStmt.accept[Unit,OberonVisitor[Unit]](this)
      case IfElseStmt(condition, thenStmt, elseStmt) => thenStmt.accept[Unit,OberonVisitor[Unit]](this);
        elseStmt match {
          case Some(f) => Some(elseStmt.get.accept[Unit,OberonVisitor[Unit]]((this)))
          case None => ()
        }

      case _ => ()
    }
  }

  private def transformListStmts(stmtsList: List[Statement], stmtsCore: ListBuffer[Statement] = new ListBuffer[Statement]): Unit = {
    if (stmtsList.nonEmpty){
      stmtsList.head.accept[Unit,OberonVisitor[Unit]](this);
      transformListStmts(stmtsList.tail)
    }
  }

  private def checkProcedureStmts(listProcedures: List[Procedure]): Unit = {
    for (procedure <- listProcedures){
      procedure.stmt.accept[Unit,OberonVisitor[Unit]](this)
    }
  }

  def isModuleCore(module: OberonModule): Boolean = {
    isCore = true
    module.stmt.get.accept[Unit,OberonVisitor[Unit]](this)
    if (!isCore){
      isCore
    }
    else{
      module.procedures.foreach{x : Procedure => x.stmt.accept[Unit,OberonVisitor[Unit]](this)}
      isCore
    }
  }
}
