// See LICENSE for license details.

package midas.widgets

import chisel3._
import chisel3.util._
import chisel3.experimental.IO
import freechips.rocketchip.config.{Field}
import freechips.rocketchip.diplomacy.InModuleBody

import midas.core.{StreamSourceParameters, StreamSinkParameters}

/**
  * Bridge Streams serve as means to do bulk transport from BridgeDriver to
  * BridgeModule and vice versa.  Abstractly, they can be thought of as a 512b
  * wide latency-insensitive channel (i.e., queue).
  *
  * The two mixins in this file implement the two directions of
  * producer-consumer relationships: [[StreamFromHostCPU]] add a stream in
  * which the driver is the producer and the BridgeModule is the consumer,
  * [[StreamToHostCPU]] does the converse. BridgeModules can mix in one or both
  * of these traits, to implement streams in either direction.
  *
  * Limitations:
  * - Streams are 512b wide. Bridge modules and drivers must manually handle
  *   width adaptations
  * - Bridges are limited to one stream in each direction. Bridge designers
  *   must multiplex multiple streams onto a single physical stream themselves.
  */

/**
  * Constants defined here apply to interfaces exposed directly to the bridges
  * and thus the user.
  */
object BridgeStreamConstants {
  // The width, in bits, of the decoupled UInt payload presented to the bridge.
  val streamWidth = 512

  def streamChiselType = DecoupledIO(UInt(streamWidth.W))
}

/**
  * Does the dirty work of providing an index to each stream, and checking it's requested name is unique.
  *
  * Instead of making these truly global data structures, they are snuck in to
  * the bridges via p, using the [[ToCPUStreamAllocatorKey]] and
  * [[FromCPUStreamAllocatorKey]].
  *
  * It's worth noting this is the sort of thing diplomacy would handle for us,
  * if I put in the time to defined node types for these streams. That might be
  * worth doing in a future PR.
  */
private [midas] class StreamAllocator {
  private var idx = 0;
  private val namespace = firrtl.Namespace()

  /**
    * Uniquifies the name for a stream, and returns its index
    */
  def allocate(desiredName: String): (String, Int) = {
    val allocatedId = idx;
    idx = idx + 1
    // This is imposed only so that the generated header's output names are
    // predictable (so they can be referred to statically in driver source).
    // Once a bridge's driver parameters are better encapsulated (i.e., in their
    // own structs / protobuf), this can be relaxed.
    // In practice, this shouldn't trip since the names are prefixed with the
    // bridge names which are already uniquified.
    require(!namespace.contains(desiredName), 
      s"Stream name ${desiredName}, already allocated. Requested stream names must be unique.")
    (namespace.newName(desiredName), allocatedId)
  }
}

// TODO: make a streams package, and make these package private so they can only
// be accessed by the traits defined here? 
case object ToCPUStreamAllocatorKey extends Field[StreamAllocator](new StreamAllocator)
case object FromCPUStreamAllocatorKey extends Field[StreamAllocator](new StreamAllocator)

/**
  *  Adds a stream interface that will be dequeued from by the BridgeModule.
  */
trait StreamFromHostCPU { self: Widget =>
  // It may not make sense to keep this common under all stream engine
  // implementations, but for now this minimize the diff in bridge module code.
  def fromHostCPUQueueDepth: Int

  val (fromHostStreamName, fromHostStreamIdx) = p(FromCPUStreamAllocatorKey)
    .allocate(s"${getWName.toUpperCase}_from_cpu_stream")

  def streamSinkParams = StreamSinkParameters(
    fromHostStreamName,
    fromHostStreamIdx,
    fromHostCPUQueueDepth)

  private val _streamDeq = InModuleBody {
    val streamFromHostCPU = IO(Flipped(BridgeStreamConstants.streamChiselType))
    streamFromHostCPU
  }

  // This hides some diplomacy complexity from the user in cases where the
  // implicit conversion from the wrapped value to the decoupled does not work.
  def streamDeq = _streamDeq.getWrappedValue

  appendHeaderFragment { _ => Seq(
      CppGenerationUtils.genConstStatic(s"${fromHostStreamName}_idx", UInt32(fromHostStreamIdx)),
      CppGenerationUtils.genConstStatic(s"${fromHostStreamName}_depth", UInt32(fromHostCPUQueueDepth))
    )
  }
}

/**
  *  Adds a stream interface that will be enqueued to by the BridgeModule.
  */
trait StreamToHostCPU { self: Widget =>
  def toHostCPUQueueDepth: Int

  val (toHostStreamName, toHostStreamIdx) = p(ToCPUStreamAllocatorKey)
    .allocate(s"${getWName.toUpperCase}_to_cpu_stream")

  def streamSourceParams = StreamSourceParameters(
    toHostStreamName,
    toHostStreamIdx,
    toHostCPUQueueDepth)

  private val _streamEnq = InModuleBody {
    val streamToHostCPU = IO(BridgeStreamConstants.streamChiselType)

    streamToHostCPU
  }

  appendHeaderFragment { _ => Seq(
      CppGenerationUtils.genConstStatic(s"${toHostStreamName}_idx", UInt32(toHostStreamIdx)),
      CppGenerationUtils.genConstStatic(s"${toHostStreamName}_depth", UInt32(toHostCPUQueueDepth))
    )
  }

  // This hides some diplomacy complexity from the user in cases where the
  // implicit conversion from the wrapped value to the decoupled does not work.
  def streamEnq = _streamEnq.getWrappedValue
}
