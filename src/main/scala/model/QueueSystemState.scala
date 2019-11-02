package model

import utils.Avg
import utils.RichDouble._

case class QueuedSystemState(system: QueuedSystem, t: Double, rejected: Int, computed: Int, processing: Int, queued: Int, totalDequeued: Int, stats: Stats) {
  override def toString: String = {
    s"[λ: ${system.inEvents}, μ: ${system.outEvents}, m: ${system.m}, l: ${system.l}] --> {t: ${t.round(4)} | " +
      s"processing: $processing | queued: $queued | computed: $computed | rej: $rejected | totalDequeued: $totalDequeued\nstats: $stats"
  }
}

case class Stats(rejectedRatio: Double = 0, avgK: Double = 0, utilization: Double = 0, avgTheta: Avg = Avg(),
                 waitRatio: Double = 0, avgQueued: Double = 0, avgEta: Avg = Avg(), avgEpsilon: Avg = Avg()) {
  override def toString: String = {
    s"(̅Πₚ = ${rejectedRatio.round(4)}, ̅Πᵣ = ${waitRatio.round(4)}, K̅ = ${avgK.round(4)}, " +
      s"̅θ = ${avgTheta.value.round(4)}, ̅η:  ${avgEta.value.round(4)}, ̅ε: ${avgEpsilon.value.round(4)}, ρ = ${utilization.round(4)}, " +
      s"avgQueue: ${avgQueued.round(4)})"
  }
}
