// See LICENSE for license details.

package midas.stage

import firrtl.options.Shell

trait GoldenGateCli { this: Shell =>
  parser.note("Golden Gate Compiler Options")
  Seq(ConfigPackageAnnotation,
      ConfigStringAnnotation,
      firrtl.checks.SkipChecks,
      firrtl.stage.FirrtlFileAnnotation,
      firrtl.stage.OutputFileAnnotation,
      firrtl.stage.FirrtlSourceAnnotation,
      firrtl.EmitCircuitAnnotation)
    .map(_.addOptions(parser))
}