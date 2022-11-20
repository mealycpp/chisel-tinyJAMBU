/*
 * Dummy tester to start a Chisel project.
 *
 * Author: Martin Schoeberl (martin@jopdesign.com)
 *
 */

package empty

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FSR_640_Tester extends AnyFlatSpec with ChiselScalatestTester {
  // val key = BigInt("0123456789ABCDEF", 16)
  val key = BigInt("0F0E0D0C0B0A09080706050403020100", 16)
  println("key is: ")
  println(key)
  "Edge_Detector" should "get edge" in {
    test(new tick()) { dut =>
      dut.io.in.poke(0)
      println("output is: ")
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()
      dut.io.in.poke(1)
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(1)
      dut.clock.step()
      dut.io.in.poke(1)
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()
      dut.io.in.poke(1)
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()
      dut.io.in.poke(1)
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()
      dut.io.in.poke(1)
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()
      dut.io.in.poke(1)
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()

      dut.io.in.poke(0)
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      dut.clock.step()
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)
      println(dut.io.out_tick.peek().litValue)
      dut.io.out_tick.expect(0)

    }
  }
  "FSR_640" should "work" in {
    test(new FSR_640(640)) { dut =>
      // val key = BigInt("00102030405060708090A0B0C0D0E0F0", 16)
      // val key = BigInt("00102030405060708090A0B0C0D0E0F", 16)
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.state_out.expect(BigInt("b505cbd12f65bbf2db809a68c8f4465d", 16))

    }
  }
  "FSR_640_1" should "work" in {
    test(new FSR_640(5)) { dut =>
      // val key = BigInt("00102030405060708090A0B0C0D0E0F", 16)
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.state_out.expect(BigInt("f8000000000000000000000000000000", 16))
    }
  }
  "FSR_640_2" should "work" in {
    test(new FSR_640(1)) { dut =>
      // val key = BigInt("00102030405060708090A0B0C0D0E0F", 16)
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.state_out.expect(BigInt("80000000000000000000000000000000", 16))
    }
  }
  "FSR_640_3" should "work" in {
    test(new FSR_640(3)) { dut =>
      // val key = BigInt("00102030405060708090A0B0C0D0E0F", 16)
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.state_out.expect(BigInt("e0000000000000000000000000000000", 16))
    }
  }
  "FSR_640_128" should "not fail" in {
    test(new FSR_640(128)) { dut =>
      // val key = BigInt("00102030405060708090A0B0C0D0E0F", 16)
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.state_out.expect(BigInt("8b375e6940ef826b8b46251bfcfdfeff", 16))
    }
  }
  "FSR_640_640" should "not fail" in {
    test(new FSR_640(640)) { dut =>
      // val key = BigInt("00102030405060708090A0B0C0D0E0F", 16)
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.state_out.expect(BigInt("b505cbd12f65bbf2db809a68c8f4465d", 16))
    }
  }
  "FSR_128_0_Rounds" should "not fail" in {
    test(new FSR_N_Reg()) { dut =>
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.steps.poke(0)
      dut.io.start.poke(1)
      dut.io.done.expect(0)
      // dut.io.start.poke(0)
      dut.clock.step()
      dut.io.done.expect(1)
      dut.clock.step()
      dut.io.done.expect(1)
      dut.clock.step()
      dut.io.done.expect(1)
      dut.clock.step()
      dut.io.done.expect(1)
      dut.clock.step()
      dut.io.done.expect(1)
      dut.io.state_out.expect(BigInt("0", 16))
    }
  }
  "FSR_128_1_Rounds" should "not fail" in {
    test(new FSR_N_Reg()) { dut =>
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.steps.poke(1)
      dut.io.start.poke(1)
      dut.clock.step()
      dut.io.done.expect(0)
      dut.io.start.poke(0)
      dut.clock.step()
      dut.io.done.expect(1)
      dut.io.state_out.expect(BigInt("8b375e6940ef826b8b46251bfcfdfeff", 16))
    }
  }
  "FSR_128_2_Rounds" should "not fail" in {
    test(new FSR_N_Reg()) { dut =>
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.steps.poke(2)
      dut.io.start.poke(1)
      dut.clock.step()
      dut.io.done.expect(0)
      dut.io.start.poke(0)
      dut.clock.step()
      dut.clock.step()
      dut.io.done.expect(1)
      dut.io.state_out.expect(BigInt("79f9a9369f3d060a1e9b66d7c23fd1a5", 16))
    }
  }
  "FSR_128_3_Rounds" should "not fail" in {
    test(new FSR_N_Reg()) { dut =>
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.steps.poke(3)
      dut.io.start.poke(1)
      dut.clock.step()
      dut.io.done.expect(0)
      dut.io.start.poke(0)
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.io.done.expect(1)
      dut.io.state_out.expect(BigInt("1ba3261111d42ab61d7a943dc5ab80a7", 16))
    }
  }
  "FSR_128_4_Rounds" should "not fail" in {
    test(new FSR_N_Reg()) { dut =>
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.steps.poke(4)
      dut.io.start.poke(1)
      dut.clock.step()
      dut.io.done.expect(0)
      dut.io.start.poke(0)
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.io.done.expect(1)
      dut.io.state_out.expect(BigInt("94dcd4d9273b69a4b6518555185e8605", 16))
    }
  }
  "FSR_128_5_Rounds" should "not fail" in {
    test(new FSR_N_Reg()) { dut =>
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.steps.poke(5)
      dut.io.start.poke(1)
      dut.clock.step()
      dut.io.done.expect(0)
      dut.io.start.poke(0)
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.io.done.expect(1)
      dut.io.state_out.expect(BigInt("b505cbd12f65bbf2db809a68c8f4465d", 16))
    }
  }
  "FSR_128_6_Rounds" should "not fail" in {
    test(new FSR_N_Reg()) { dut =>
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.steps.poke(6)
      dut.io.start.poke(1)
      dut.clock.step()
      dut.io.done.expect(0)
      dut.io.start.poke(0)
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.io.done.expect(1)
      dut.io.state_out.expect(BigInt("c5c6eda1546fd8c05b6186c6e749f5ed", 16))
    }
  }
  "FSR_128_Multiple_Rounds" should "not fail" in {
    test(new FSR_N_Reg()) { dut =>
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.steps.poke(6)
      dut.io.start.poke(1)
      dut.io.done.expect(0)
      dut.clock.step()
      dut.io.start.poke(0)
      dut.io.done.expect(0)
      for (a <- 1 to 20) {
        dut.clock.step()
        println(dut.io.done.peek().litValue)
      }
      dut.io.done.expect(1)
      dut.io.state_out.expect(BigInt("c5c6eda1546fd8c05b6186c6e749f5ed", 16))
    }

    // test(new FSR_N_Reg()) { dut =>
    //   dut.io.key.poke(key)
    //   dut.io.state.poke(0)
    //   dut.io.steps.poke(6)
    //   dut.io.start.poke(1)
    //   dut.clock.step()
    //   dut.io.done.expect(1)
    //   dut.io.start.poke(0)
    //   dut.io.done.expect(0)
    //   dut.clock.step()
    //   dut.clock.step()
    //   dut.clock.step()
    //   dut.clock.step()
    //   dut.clock.step()
    //   dut.clock.step()
    //   dut.io.done.expect(1)
    //   dut.io.state_out.expect(BigInt("c5c6eda1546fd8c05b6186c6e749f5ed", 16))

    //   dut.io.key.poke(key)
    //   dut.io.state.poke(0)
    //   dut.io.steps.poke(1)
    //   dut.io.start.poke(1)
    //   dut.clock.step()
    //   dut.io.done.expect(0)
    //   dut.io.start.poke(0)
    //   dut.io.done.expect(0)
    //   dut.clock.step()
    //   dut.io.done.expect(1)
    //   dut.io.state_out.expect(BigInt("8b375e6940ef826b8b46251bfcfdfeff", 16))

    //   dut.io.key.poke(key)
    //   dut.io.state.poke(0)
    //   dut.io.steps.poke(6)
    //   dut.io.start.poke(1)
    //   dut.clock.step()
    //   dut.io.done.expect(0)
    //   dut.io.start.poke(0)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)

    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)
    //   dut.clock.step()
    //   println(dut.io.done.peek().litValue)

    //   dut.io.done.expect(1)
    //   dut.io.state_out.expect(BigInt("c5c6eda1546fd8c05b6186c6e749f5ed", 16))
    // }
  }
  val nonce = BigInt("0B0A09080706050403020100", 16)
  "Init" should "match" in {
    test(new initialization_tinyJAMBU_once()) { dut =>
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.start.poke(1)
      dut.io.nonce.poke(BigInt("03020100", 16))
      dut.clock.step()
      dut.io.start.poke(0)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.clock.step()
      println(dut.io.done.peek().litValue)
      dut.io.done.expect(1)
    }
    test(new initialization_tinyJAMBU()) { dut =>
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.nonce.poke(nonce)
      dut.io.start.poke(1)
      dut.io.done.expect(0)
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.clock.step()
      dut.io.done.expect(1)
      dut.io.state_out.expect(BigInt("EA5DC9488E487009BFB6ADC697CF5A5B", 16))
    }
  }
}
