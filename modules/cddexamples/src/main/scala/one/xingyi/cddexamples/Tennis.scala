/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.cddexamples

import one.xingyi.cddengine._
import one.xingyi.cddmustache.Mustache
import one.xingyi.cddscenario.InternetDocument
class Tennis {
  val definition = InternetDocument("CodingDojo", "http://codingdojo.org/cgi-bin/wiki.pl?KataTennis")
  val wikipedia = InternetDocument("Wikipedia", "http://en.wikipedia.org/wiki/Tennis_score")
  val changeRequest = InternetDocument("CR24", "http://en.wikipedia.org/wiki/Tennis_score")

  val lookup = Map(0 -> "love", 1 -> "fifteen", 2 -> "thirty", 3 -> "forty")

  type TennisUseCase = UseCase2[Int, Int, String]

  val ucLeftWins = new TennisUseCase("left winning") {
    scenario(4, 0) produces "left won" when { (l, r) => (l - r) >= 2 && l >= 4 }
    scenario(4, 1) produces "left won"
    scenario(4, 2) produces "left won" reference wikipedia
    scenario(5, 3) produces "left won" title "this is some stuff"
  }
  //  reference("2.1", definition).
  val ucRightWins = new TennisUseCase("Receiver winning") {
    scenario(0, 4) produces "right won" when { (l: Int, r: Int) => (r - l) >= 2 && r >= 4 }
    scenario(1, 4) produces "right won"
    scenario(2, 4) produces "right won"
    scenario(3, 5) produces "right won"
    scenario(40, 42) produces "right won"
  }
  val ucRunningScore = new TennisUseCase("Running score") {
    //  reference("2.10", definition)
    scenario(2, 3) produces "thirty, forty" because { case (l, r) if l < 4 && r < 4 => s"${lookup(l)}, ${lookup(r)}" }
    scenario(2, 1) produces "thirty, fifteen"
  }
  val ucXXAll = new TennisUseCase("When both have the same running score") {
    //                 reference("2.11", definition).
    scenario(0, 0) produces "love all" because { case (l, r) if l == r && l < 3 => s"${lookup(l)} all" }
    scenario(2, 2) produces "thirty all"

  }
  val ucDeuce = new TennisUseCase("Deuce") {
    //  reference("5", definition).
    scenario(3, 3) produces "deuce" when { (l, r) => l >= 3 && r >= 3 && l == r }
    scenario(4, 4) produces "deuce"
    scenario(6, 6) produces "deuce"
  }

  val ucAdvantage = new TennisUseCase("Advantage") {
    //  reference("3", definition).
    scenario(5, 4) produces "advantage left" when { (l, r) => l >= 3 && r >= 3 && l == r + 1 }
    scenario(6, 5) produces "advantage left" //.reference("2").
    scenario(4, 3) produces "advantage left"

    scenario(4, 5) produces "advantage right" when { (l, r) => l >= 3 && r >= 3 && r == l + 1 }
    scenario(5, 6) produces "advantage right"
  }
  val tennis = Engine(ucLeftWins or ucRightWins or ucRunningScore or ucXXAll or ucDeuce or ucAdvantage)


  def dump = {
    println(tennis(0, 0))
    println(tennis(0, 1))
    println(tennis(1, 1))
    println(tennis(2, 1))
    println(tennis(3, 1))
    println(tennis(3, 2))
    println(tennis(3, 3))
    println(tennis(4, 4))
    println(tennis(4, 5))
    println(tennis(5, 5))
    println(tennis(6, 5))
    println(tennis(7, 5))
  }
}


object Tennis extends Tennis with App {
  import Mustache._

import one.xingyi.json4s.Json4sWriter._
  implicit def v[P, R] = new SimpleValidation[P, R]
  tennis.tools.printTraceAboutAdding("tennis")
  tennis.tools.printPages("tennis")
  dump
}

