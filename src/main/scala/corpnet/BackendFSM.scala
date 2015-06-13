package corpnet

import java.io.File
import java.nio.file.Path

import akka.actor.FSM
import corpnet.BackendFSMProtocol.BackendState
import corpnet.chart.Plotter
import corpnet.generator._
import corpnet.io.IOUtil
import corpnet.layout.LayoutStyle._
import corpnet.layout.NodeStyle.NodeStyle
import corpnet.layout._
import corpnet.measure.{Modularity, ModularityParameters, ModularityResult}
import corpnet.network.{Network, NetworkParameters}
import corpnet.power.BonacichPower.ComputationMethod
import corpnet.power.{BonacichPower, PowerAnalysis}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object BackendFSMProtocol {

  sealed trait BackendState
  case object BackendIdle extends BackendState
  case object BackendProcessing extends BackendState

  sealed trait BackendRequest
  case class DeleteNode(node: Node) extends BackendRequest
  case class DragEdge(edge: Edge) extends BackendRequest
  case class InsertSubordinate(node: Node) extends BackendRequest
  case object InsertTopManager extends BackendRequest
  case class GeneratePerfectTree(parameters: PerfectTreeParameters) extends BackendRequest
  case class GenerateRandomTree(parameters: RandomTreeParameters) extends BackendRequest
  case class GenerateSocialNetwork(parameters: SocialNetworkParameters) extends BackendRequest
  case object RecalculatePowers extends BackendRequest
  case class SaveNetwork(file: File) extends BackendRequest
  case class LoadNetwork(file: File) extends BackendRequest
  case class ConfigureForceLayout(parameters: ForceLayoutParameters) extends BackendRequest
  case class ConfigureNetwork(parameters: NetworkParameters) extends BackendRequest
  case object NewNetwork extends BackendRequest
  case class SetRecalculateOption(selected: Boolean) extends BackendRequest
  case class SetLayoutStyle(style: LayoutStyle) extends BackendRequest
  case class SetNodeStyle(style: NodeStyle) extends BackendRequest
  case object Undo extends BackendRequest
  case object PlotPowers extends BackendRequest
  case class PlotHistogram(bins: Int) extends BackendRequest
  case class CalculateModularity(parameters: ModularityParameters) extends BackendRequest
  case class ExportTSV(path: Path, parameters: ModularityParameters) extends BackendRequest

  sealed trait BackendResult
  case class ActionSuccessful(appState: UndoStack) extends BackendResult
  case object ActionFailed extends BackendResult
}

class BackendFSM extends FSM[BackendState, UndoStack] {
  import BackendFSMProtocol._

  startWith(BackendIdle, UndoStack(Main.maxUndoLevels))

  when(BackendIdle) {
    case Event(DeleteNode(node), stack)               ⇒ executeUpdate(stack, deleteNode(stack.head, node))
    case Event(DragEdge(edge), stack)                 ⇒ executeUpdate(stack, dragEdge(stack.head, edge))
    case Event(InsertSubordinate(node), stack)        ⇒ executeUpdate(stack, insertSubordinate(stack.head, node))
    case Event(InsertTopManager, stack)               ⇒ executeUpdate(stack, insertTopManager(stack.head))
    case Event(GeneratePerfectTree(params), stack)    ⇒ executeUpdate(stack, generatePerfectTree(stack.head, params))
    case Event(GenerateRandomTree(params), stack)     ⇒ executeUpdate(stack, generateRandomTree(stack.head, params))
    case Event(GenerateSocialNetwork(params), stack)  ⇒ executeUpdate(stack, generateSocialNetwork(stack.head, params))
    case Event(RecalculatePowers, stack)              ⇒ doRecalculate(stack)
    case Event(SaveNetwork(file), stack)              ⇒
      saveNetwork(stack.head, file); stay()
    case Event(LoadNetwork(file), stack)              ⇒ executeUpdate(stack, loadNetwork(stack.head, file))
    case Event(ConfigureForceLayout(params), stack)   ⇒ executeAndRecalculate(stack, updateForceLayout(stack.head, params))
    case Event(ConfigureNetwork(params), stack)       ⇒ executeUpdate(stack, updateNetwork(stack.head, params))
    case Event(NewNetwork, stack)                     ⇒ executeUpdate(stack, newNetwork(stack.head))
    case Event(SetRecalculateOption(selected), stack) ⇒ stay using setAutoRecalculateOption(stack, selected)
    case Event(SetLayoutStyle(style), stack)          ⇒ executeAndReLayout(stack, Future(setLayoutStyle(stack, style)))
    case Event(SetNodeStyle(style), stack)            ⇒ executeAndRecalculate(stack, Future(setNodeStyle(stack, style)))
    case Event(Undo, stack)                           ⇒ undo(stack)
    case Event(PlotHistogram(bins), stack)            ⇒
      plotHistogram(stack.head, bins); stay()
    case Event(PlotPowers, stack)                     ⇒
      plotPowers(stack.head); stay()
    case Event(CalculateModularity(params), stack)    ⇒ execute(stack, calculateModularity(stack.head, params))
    case Event(ExportTSV(path, parameters), stack)    ⇒ exportTSV(stack.head, path, parameters); stay()
  }

  when(BackendProcessing) {
    case Event(_: BackendRequest, _) ⇒
      Logger.info("Unable to execute action while processing")
      stay()
    case Event(ActionFailed, stack) ⇒
      goto(BackendIdle) using stack
    case Event(ActionSuccessful(stack), _) ⇒
      refreshUI(stack.head)
      goto(BackendIdle) using stack
  }

  def refreshUI(state: ApplicationState) = {
    Main.changeState(state)
    Main.redraw()
  }

  def executeUpdate(stack: UndoStack, action: Future[ApplicationState]) = {
    def resetPowerAndModularity(state: ApplicationState) =
      state.copy(bonacichPower = None, powerAnalysis = None, modularityResult = None)

    executeAndRecalculate(stack, action.map(resetPowerAndModularity))
  }

  def executeAndRecalculate(stack: UndoStack, action: Future[ApplicationState]) =
    execute(stack, action.flatMap(recalculate))

  def executeAndReLayout(stack: UndoStack, action: Future[ApplicationState]) =
    execute(stack, action.flatMap(buildLayout))

  def execute(stack: UndoStack, action: Future[ApplicationState]) = {
    action onComplete {
      case Success(state) ⇒ self ! ActionSuccessful(stack.push(state))
      case Failure(_)     ⇒ self ! ActionFailed
    }
    goto(BackendProcessing) using stack
  }

  def recalculate(state: ApplicationState): Future[ApplicationState] = {
    if (state.network.graph.nodes.size == 0)
      Future(state.copy(treeLayout = None, forceLayout = None))
    else {
      val f = for {
        state1 ← recalculatePowers(state)
        state2 ← analyzeNetwork(state1)
        state3 ← buildLayout(state2)
      } yield {
        state3
      }
      state.nodeStyle match {
        case NodeStyle.community ⇒
          f.flatMap(state ⇒ calculateModularity(state, state.modularityParameters))
        case NodeStyle.power ⇒
          f
      }
    }
  }

  def doRecalculate(stack: UndoStack) = {
    recalculate(stack.head) onComplete {
      case Success(newState) ⇒ self ! ActionSuccessful(stack.updateHead(newState))
      case Failure(_)        ⇒ self ! ActionFailed
    }
    goto(BackendProcessing) using stack
  }

  def undo(stack: UndoStack) =
    stack.pop() match {
      case Some((state, smallerStack)) ⇒
        Logger.info(s"UNDO: ${state.label}")
        refreshUI(smallerStack.head)
        stay using smallerStack
      case None ⇒
        stay using stack
    }

  def dragEdge(state: ApplicationState, edge: Edge): Future[ApplicationState] = {
    val f = Future {
      val network = Network.toggleNonReportingEdge(state.network, edge._1, edge._2)
      state.copy(network = network, label = s"Dragged edge [$edge]")
    }
    f onComplete {
      case Failure(e) ⇒ Logger.error(s"Failed to update edge: $e")
      case Success(_) ⇒ Logger.debug(s"Dragged edge [$edge]")
    }
    f
  }

  def insertTopManager(state: ApplicationState): Future[ApplicationState] = {
    val f = Future {
      val (network, _) = Network.insertTopManager(state.network)
      state.copy(network = network, label = s"Inserted top manager")
    }
    f onComplete {
      case Failure(e) ⇒ Logger.error(s"Failed to insert top manager: $e")
      case Success(_) ⇒ Logger.debug(s"Inserted top manager")
    }
    f
  }

  def insertSubordinate(state: ApplicationState, node: Node): Future[ApplicationState] = {
    val f = Future {
      val (network, _) = Network.insertSubordinate(state.network, node)
      state.copy(network = network, label = s"Inserted subordinate to node $node")
    }
    f onComplete {
      case Failure(e) ⇒ Logger.error(s"Failed to insert subordinate to node $node: $e")
      case Success(_) ⇒ Logger.debug(s"Inserted subordinate to node $node")
    }
    f
  }

  def deleteNode(state: ApplicationState, node: Node): Future[ApplicationState] = {
    val f = Future {
      val network = Network.deleteNode(state.network, node)
      state.copy(network = network, label = s"Deleted node $node")
    }
    f onComplete {
      case Failure(e) ⇒ Logger.error(s"Failed to delete node $node: $e")
      case Success(_) ⇒ Logger.debug(s"Deleted node $node")
    }
    f
  }

  def generatePerfectTree(state: ApplicationState, params: PerfectTreeParameters): Future[ApplicationState] = {
    val f = Future {
      Logger.info(s"Generating perfect tree with $params ...")
      val network = PerfectTreeBuilder.generate(params)
      state.copy(network = network, perfectTreeParameters = params, label = "Generated perfect tree")
    }
    f onComplete {
      case Failure(e)        ⇒ Logger.error(s"Failed to generate perfect tree: ${e.getMessage}")
      case Success(newState) ⇒ Logger.info("Finished generating perfect tree")
    }
    f
  }

  def generateRandomTree(state: ApplicationState, params: RandomTreeParameters): Future[ApplicationState] = {
    val f = Future {
      Logger.info(s"Generating random tree with $params ...")
      val network = RandomTreeBuilder.generate(params)
      state.copy(network = network, randomTreeParameters = params, label = "Generated random tree")
    }
    f onComplete {
      case Failure(e)        ⇒ Logger.error(s"Failed to generate random tree: ${e.getMessage}")
      case Success(newState) ⇒ Logger.info("Finished generating random tree")
    }
    f
  }

  def generateSocialNetwork(state: ApplicationState, params: SocialNetworkParameters): Future[ApplicationState] = {
    val f = Future {
      Logger.info(s"Generating social network with $params ...")
      val (network, edges) = SocialNetworkGenerator.generate(state.network, params)
      val newState = state.copy(network = network, socialNetworkParameters = params, label = "Generated social network")
      Logger.info(s"Finished generating social network with $edges edges")
      newState
    }
    f onFailure {
      case e ⇒ Logger.error(s"Failed to generate random tree: ${e.getMessage}")
    }
    f
  }

  def loadNetwork(state: ApplicationState, file: File): Future[ApplicationState] = {
    val f = Future {
      Logger.debug(s"Loading network from $file")
      val network = IOUtil.loadNetwork(file)
      state.copy(network = network, label = s"Loaded network from $file")
    }
    f onFailure {
      case e ⇒ Logger.error(s"Failed to load network from [$file]: $e")
    }
    f
  }

  def saveNetwork(state: ApplicationState, file: File): Unit = {
    val f = Future {
      Logger.info(s"Saving network to $file")
      IOUtil.save(state.network, file)
    }
    f onFailure {
      case e ⇒ Logger.error(s"Failed to save network to [$file]: $e")
    }
  }

  def newNetwork(state: ApplicationState): Future[ApplicationState] = {
    val f = Future {
      Logger.debug(s"New network")
      state.copy(network = Network(), label = "New network")
    }
    f
  }

  def updateNetwork(state: ApplicationState, parameters: NetworkParameters): Future[ApplicationState] = {
    val f = Future {
      Logger.debug(s"Changing network parameters to $parameters")
      val network = Network.updateParameters(state.network, parameters)
      state.copy(network = network, label = s"Changed parameters to $parameters")
    }
    f onFailure {
      case e ⇒ Logger.error(s"Failed to update parameters: ${e.getMessage}")
    }
    f
  }

  def updateForceLayout(state: ApplicationState, parameters: ForceLayoutParameters): Future[ApplicationState] =
    Future {
      Logger.debug(s"Changing forceLayout parameters to $parameters")
      state.copy(forceLayoutParameters = parameters, label = s"Changed parameters to $parameters")
    }

  def setAutoRecalculateOption(stack: UndoStack, selected: Boolean) = {
    val state = stack.head.copy(recalculatePowersOnGraphUpdate = selected)
    stack.updateHead(state)
  }

  def setLayoutStyle(stack: UndoStack, style: LayoutStyle) = {
    stack.head.copy(layoutStyle = style)
  }

  def setNodeStyle(stack: UndoStack, style: NodeStyle) = {
    stack.head.copy(nodeStyle = style)
  }

  def recalculatePowers(state: ApplicationState): Future[ApplicationState] = {
    val f = Future {
      if (state.recalculatePowersOnGraphUpdate) {
        val n = state.network.graph.nodes.size
        Logger.debug(s"Calculating Bonacich power for $n nodes...")
        val bonacichPower = BonacichPower.calculate(state.network.graph, state.network.parameters, ComputationMethod.Exact)
        state.copy(bonacichPower = bonacichPower)
      } else {
        state
      }
    }
    f onComplete {
      case Failure(e) ⇒
        Logger.error(s"BonacichPower calculation failed: ${e.getMessage}")
      case Success(newState) ⇒
        Logger.debug("Finished calculating Bonacich power")
        newState.bonacichPower.foreach(bp ⇒ Logger.info(bp.toString))
    }
    f
  }

  def analyzeNetwork(state: ApplicationState): Future[ApplicationState] = {
    val f = Future {
      state.bonacichPower match {
        case Some(power) ⇒
          val n = state.network.graph.nodes.size
          Logger.debug(s"Analyzing network stability for $n nodes...")
          val tree = NetworkTreeBuilder.buildTree(state.network.graph)
          val powerAnalysis = PowerAnalysis.analyze(power.nodePower, tree)
          Logger.info(powerAnalysis.toString)
          state.copy(powerAnalysis = Some(powerAnalysis))
        case None ⇒
          Logger.error("Need to recalculate Bonacich power before network stability analysis can be performed.")
          state
      }
    }
    f onComplete {
      case Failure(e) ⇒
        Logger.error(s"Analyze network stability calculation failed: ${e.getMessage}")
      case Success(newState) ⇒
        Logger.debug("Finished analyzing network stability")
    }
    f
  }

  def buildLayout(state: ApplicationState): Future[ApplicationState] = {
    val f = Future {
      state.layoutStyle match {
        case LayoutStyle.Tree ⇒
          Logger.debug("Rebuilding tree layout..")
          val treeLayout = TreeLayoutBuilder.buildTreeLayout(state.network, state.treeLayoutParameters)
          state.copy(treeLayout = Some(treeLayout))
        case LayoutStyle.Force ⇒
          Logger.debug("Rebuilding force layout..")
          val forceLayout = ForceLayoutBuilder.build(state.network.graph, state.forceLayoutParameters, state.forceLayout)
          state.copy(forceLayout = Some(forceLayout))
      }
    }
    f onComplete {
      case Failure(e) ⇒
        Logger.error(s"Rebuild layout failed: ${e.getMessage}")
      case Success(newState) ⇒
        Logger.debug("Finished rebuilding layout")
    }
    f
  }

  def plotPowers(state: ApplicationState): Unit = {
    val f = Future {
      state.bonacichPower match {
        case Some(bonacichPower) ⇒
          Logger.info("Plotting powers")
          Plotter.plotPowers(bonacichPower.nodePower, state.network.graph.levels)
        case None ⇒
          Logger.error("Need to recalculate powers before plotting")
      }
    }
    f onFailure {
      case e ⇒ Logger.error(s"Failed to plot powers: $e")
    }
  }

  def plotHistogram(state: ApplicationState, bins: Int): Unit = {
    val f = Future {
      state.bonacichPower match {
        case Some(bonacichPower) if bins > 0 ⇒
          Logger.info("Plotting histogram")
          Plotter.plotHistogram(bonacichPower.nodePower, bins)
        case Some(_) if bins <= 0 ⇒
          Logger.error("bins must be greater than zero")
        case None ⇒
          Logger.error("Need to recalculate powers before plotting")
      }
    }
    f onFailure {
      case e ⇒ Logger.error(s"Failed to plot histogram: $e")
    }
  }

  def calculateModularity(state: ApplicationState, parameters: ModularityParameters): Future[ApplicationState] = {
    val f = Future {
      Logger.info("Calculating modularity...")
      val result = Modularity.run(state.network, parameters)
      result.foreach(result ⇒ Logger.info(result.toString))
      state.copy(modularityResult = result, modularityParameters = parameters)
    }
    f onFailure {
      case e ⇒ Logger.error(s"Failed to calculate modularity: $e")
    }
    f
  }

  def exportTSV(state: ApplicationState, path: Path, parameters: ModularityParameters): Unit = {
    val f = Future {
      IOUtil.exportTSVToPath(state.network, parameters, path)
    }
    f onComplete {
      case Success(path) ⇒ Logger.info(s"Exported TSV to $path")
      case Failure(e)    ⇒ Logger.error(s"Failed to export TSV: $e")
    }
  }
}
