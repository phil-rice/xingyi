/** Copyright (c) 2018, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
package one.xingyi.core.simpleServer

import java.io.{BufferedReader, InputStream, InputStreamReader, OutputStream}

import scala.annotation.tailrec

object Streams {
  def readAll(stream: InputStream): String = {
    val in = new BufferedReader(new InputStreamReader(stream))
    try {
      val result = new StringBuilder
      @tailrec
      def recurse(): String = {
        val inputLine = in.readLine()
        if (inputLine == null)
          result.toString
        else {
          if (result.size >0)
            result.append("\n")
          result.append(inputLine)
          recurse()
        }
      }
      recurse()
    } finally in.close()
  }


  def sendAll(stream: OutputStream, bytes: Array[Byte]): Unit = {
    try
      stream.write(bytes)
    finally stream.close()
  }
}
