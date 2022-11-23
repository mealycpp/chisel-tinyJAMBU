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
  val nonce = BigInt("0B0A09080706050403020100", 16)
  println("key is: ")
  println(key)
  "Process_ad_once" should "be correct" in {
    test(new process_AD_once()) {dut =>
      // in C, this is simulating an adlength of 4 (32 bits) with state of 0
      // ad is also 0
      dut.io.ad.poke(0)
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.start.poke(1)
      for (i <- 1 to 20) {
        dut.clock.step()
        println(dut.io.done.peek().litValue)
      }
      dut.io.done.expect(1)
      dut.io.state_out.expect(BigInt("6C8A71DA04A3341F64D2A3C9CF009E65", 16))
    }
  }
  "init_key" should "work" in {
    test(new init_key()) { dut =>
      dut.io.key.poke(key)
      dut.io.start.poke(1)
      dut.clock.step()
      dut.io.done.expect(0)
      for (i <- 1 to 20) {
        dut.clock.step()
        println(dut.io.done.peek().litValue)
      }
      dut.io.done.expect(1)
      dut.io.state_out.expect(BigInt("9c435a6f66b7df88481f57e066a437e1", 16))
    }
  }
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
      dut.io.steps.poke(5)
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
      dut.io.state_out.expect(BigInt("b505cbd12f65bbf2db809a68c8f4465d", 16))
      dut.io.start.poke(0)
      dut.clock.step()
      // run with 5 rounds
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.steps.poke(5)
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
      dut.io.state_out.expect(BigInt("b505cbd12f65bbf2db809a68c8f4465d", 16))
      dut.io.start.poke(0)
      dut.clock.step()
      // run with 5 rounds
      dut.io.key.poke(key)
      dut.io.state.poke(0)
      dut.io.steps.poke(5)
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
      dut.io.state_out.expect(BigInt("b505cbd12f65bbf2db809a68c8f4465d", 16))
      dut.io.start.poke(0)
      dut.clock.step()

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
  "Init_once" should "work" in {
    test(new initialization_tinyJAMBU_once()) { dut =>
      dut.io.key.poke(key)
      // state after 8 steps
      dut.io.state.poke(BigInt("9c435a6f66b7df88481f57e066a437e1", 16))
      dut.io.nonce.poke(BigInt("03020100", 16))
      dut.io.start.poke(1)
      for (i <- 1 to 20) {
        println(dut.io.done.peek().litValue)
        println(dut.io.single_initialization_out.peek().litValue.toString(16))
        dut.clock.step()
      }
      dut.io.done.expect(1)
      println(dut.io.single_initialization_out.peek().litValue.toString(16))
      dut.io.single_initialization_out.expect(
        BigInt("1605DB68DB240E22742896C7C0B23058", 16)
      )
      dut.io.start.poke(0)
      dut.clock.step()

      // run "second" init
      dut.io.key.poke(key)
      // state after 8 steps
      dut.io.state.poke(BigInt("1605DB68DB240E22742896C7C0B23058", 16))
      dut.io.nonce.poke(BigInt("07060504", 16))
      dut.io.start.poke(1)
      for (i <- 1 to 20) {
        println(dut.io.done.peek().litValue)
        println(dut.io.single_initialization_out.peek().litValue.toString(16))
        dut.clock.step()
      }
      dut.io.done.expect(1)
      println(dut.io.single_initialization_out.peek().litValue.toString(16))
      dut.io.single_initialization_out.expect(
        BigInt("23BFBB10D84618992FE6A1AC755EBA34", 16)
      )
      dut.io.start.poke(0)
      dut.clock.step()
      // run "third" init
      dut.io.key.poke(key)
      // state after 8 steps
      dut.io.state.poke(BigInt("23BFBB10D84618992FE6A1AC755EBA34", 16))
      dut.io.nonce.poke(BigInt("0B0A0908", 16))
      dut.io.start.poke(1)
      for (i <- 1 to 20) {
        println(dut.io.done.peek().litValue)
        println(dut.io.single_initialization_out.peek().litValue.toString(16))
        dut.clock.step()
      }
      dut.io.done.expect(1)
      println(dut.io.single_initialization_out.peek().litValue.toString(16))
      dut.io.single_initialization_out.expect(
        BigInt("EA5DC9488E487009BFB6ADC697CF5A5B", 16)
      )
      dut.io.start.poke(0)
      dut.clock.step()
    }
  }
  "test_nonce" should "work" in {
    test(new whole_frame_bit()) { dut =>
      // third
      var state = BigInt("23BFBB10D84618992FE6A1AC755EBA34", 16)
      // A turned into B
      var state_out = BigInt("23BFBB10D84618992FE6A1BC755EBA34", 16)
      dut.io.in.poke(BigInt("1", 16))
      dut.io.state.poke(state)
      dut.io.out.expect(state_out)
      // first
      state = BigInt("9C435A6F66B7DF88481F57E066A437E1", 16)
      state_out = BigInt("9C435A6F66B7DF88481F57F066A437E1", 16)
      dut.io.in.poke(BigInt("1", 16))
      dut.io.state.poke(state)
      dut.io.out.expect(state_out)

    }
  }
  "Init_once_third" should "work" in {
    test(new initialization_tinyJAMBU_once()) { dut =>
      dut.io.key.poke(key)
      // state after 8 steps
      dut.io.state.poke(BigInt("23BFBB10D84618992FE6A1AC755EBA34", 16))
      dut.io.nonce.poke(BigInt("0B0A0908", 16))
      dut.io.start.poke(1)
      for (i <- 1 to 20) {
        println(dut.io.done.peek().litValue)
        println(dut.io.single_initialization_out.peek().litValue.toString(16))
        dut.clock.step()
      }
      dut.io.done.expect(1)
      println(dut.io.single_initialization_out.peek().litValue.toString(16))
      dut.io.single_initialization_out.expect(
        BigInt("EA5DC9488E487009BFB6ADC697CF5A5B", 16)
      )
      dut.io.start.poke(0)
      dut.clock.step()
    }
  }
  "Init_all" should "work" in {
    test(new initialization_tinyJAMBU()) { dut =>
      dut.io.key.poke(key)
      dut.io.nonce.poke(nonce)
      dut.io.start.poke(1)
      // dut.clock.step()
      // dut.io.done.expect(0)
      for (i <- 1 to 40) {
        print("state is: ")
        println(dut.io.state_out.peek().litValue.toString(16))
        print("done is: ")
        println(dut.io.done.peek().litValue)
        dut.clock.step()
      }
      dut.io.done.expect(1)
      println(dut.io.state_out.peek().litValue.toString(16))
      dut.io.state_out.expect(BigInt("EA5DC9488E487009BFB6ADC697CF5A5B", 16))
    }
  }

}
