package corpnet

import corpnet.generator._
import corpnet.layout.LayoutStyle._
import corpnet.layout.NodeStyle.NodeStyle
import corpnet.layout._
import corpnet.measure.{ModularityParameters, ModularityResult}
import corpnet.network.Network
import corpnet.power.{BonacichPower, PowerAnalysis}

case class ApplicationState(
  label: String = "Initial state",
  network: Network = Network(),
  bonacichPower: Option[BonacichPower] = None,
  powerAnalysis: Option[PowerAnalysis] = None,
  forceLayoutParameters: ForceLayoutParameters = ForceLayoutParameters(),
  treeLayoutParameters: TreeLayoutParameters = TreeLayoutParameters(),
  perfectTreeParameters: PerfectTreeParameters = PerfectTreeParameters(),
  randomTreeParameters: RandomTreeParameters = RandomTreeParameters(),
  socialNetworkParameters: SocialNetworkParameters = SocialNetworkParameters(),
  forceLayout: Option[ForceLayout] = None,
  treeLayout: Option[TreeLayout] = None,
  recalculatePowersOnGraphUpdate: Boolean = true,
  layoutStyle: LayoutStyle = LayoutStyle.Tree,
  nodeStyle: NodeStyle = NodeStyle.power,
  modularityParameters: ModularityParameters = ModularityParameters(),
  modularityResult: Option[ModularityResult] = None)
