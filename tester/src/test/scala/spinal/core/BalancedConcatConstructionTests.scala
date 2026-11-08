package spinal.core

import java.nio.file.Files

import org.apache.commons.io.FileUtils
import spinal.core.internals._
import spinal.tester.SpinalAnyFunSuite

class BalancedConcatConstructionTests extends SpinalAnyFunSuite {
  private object Constructed extends RuntimeException with scala.util.control.NoStackTrace

  class Inner extends Bundle {
    val x = Bits(3 bits)
    val y = Bits(1 bits)
  }

  class Outer extends Bundle {
    val lo = Bits(2 bits)
    val inner = new Inner
    val hi = Bits(4 bits)
  }

  class Fields extends SpinalStruct {
    val a = Bits(2 bits)
    val b = Bits(3 bits)
    val c = Bits(1 bits)
    override private[core] def isEqualToSim(that: Any): Bool = isEqualTo(that)
  }

  class NineFields extends SpinalStruct {
    val f0 = Bits(1 bits)
    val f1 = Bits(1 bits)
    val f2 = Bits(1 bits)
    val f3 = Bits(1 bits)
    val f4 = Bits(1 bits)
    val f5 = Bits(1 bits)
    val f6 = Bits(1 bits)
    val f7 = Bits(1 bits)
    val f8 = Bits(1 bits)
    override private[core] def isEqualToSim(that: Any): Bool = isEqualTo(that)
  }

  class EmptyBundle extends Bundle
  class EmptyStruct extends SpinalStruct {
    override private[core] def isEqualToSim(that: Any): Bool = isEqualTo(that)
  }

  test("nested, empty and delayed widths generate in all backends") {
    for (mode <- Seq(Verilog, SystemVerilog, VHDL)) {
      val directory = Files.createTempDirectory("spinal-balanced-concat-")
      val config = SpinalConfig(mode = mode, targetDirectory = directory.toString, verbose = false)
      try {
        def circuit = new Component {
          val outer = in(new Outer)
          val packed = out(outer.asBits)
          val reversed = out(outer.asReversedBits)
          assert(packed.getWidth == 10)
          assert(reversed.getWidth == 10)
          assert(Cat(Seq.empty[Data]).getWidth == 0)
          assert((new EmptyBundle).asBits.getWidth == 0)

          val delayed = Bits()
          val combined = Cat(Seq(delayed, B(1, 2 bits)))
          delayed := B(3, 3 bits)
          assert(combined.getWidth == 5)
          val delayedResult = out(Bits(5 bits))
          delayedResult := combined
        }
        if (mode == VHDL) SpinalVhdlBoot.singleShot(config)(circuit)
        else SpinalVerilogBoot.singleShot(config)(circuit)
        val generated = Files.list(directory)
        try assert(generated.findAny().isPresent)
        finally generated.close()
      } finally FileUtils.deleteDirectory(directory.toFile)
    }
  }

  test("SpinalStruct packs fields from last to first") {
    val directory = Files.createTempDirectory("spinal-balanced-struct-")
    val config = SpinalConfig(mode = SystemVerilog, targetDirectory = directory.toString, verbose = false)
    config.phasesInserters += { phases =>
      val original = phases.head
      phases(0) = new Phase {
        override def hasNetlistImpact = original.hasNetlistImpact
        override def impl(pc: PhaseContext): Unit = {
          original.impl(pc)
          throw Constructed
        }
      }
    }
    try {
      intercept[Constructed.type] {
        SpinalVerilogBoot.singleShot(config)(new Component {
          val struct = new Fields
          val packed = struct.asBits
          assert(packed.getWidth == 6)
          assert((new EmptyStruct).asBits.getWidth == 0)

          def leaves(node: Expression): Seq[Expression] = node match {
            case data: BaseType if data.hasOnlyOneStatement => leaves(data.head.source)
            case cat: Operator.Bits.Cat => leaves(cat.left) ++ leaves(cat.right)
            case data: BaseType => Seq(data)
            case other => fail(s"Unexpected struct expression: $other")
          }
          val found = leaves(packed)
          assert(found == Seq(struct.c, struct.b, struct.a))

          def catDepth(node: Expression): Int = node match {
            case data: BaseType if data.hasOnlyOneStatement => catDepth(data.head.source)
            case cat: Operator.Bits.Cat => 1 + math.max(catDepth(cat.left), catDepth(cat.right))
            case _: BaseType => 0
            case other => fail(s"Unexpected struct expression: $other")
          }
          assert(catDepth((new NineFields).asBits) == 4)
        })
      }
    } finally FileUtils.deleteDirectory(directory.toFile)
  }
}
