package org.rg.su3

import java.io.{File, PrintWriter}
import java.nio.file.{Paths, Files}
import java.nio.charset.Charset

object IO:

  /**
   * print something to a file (overwrite if exist)
   * @param f : file to write
   * @param op : function used to write on file
   * @example
   *     printToFile(File(full_path))( l => l.println )
   */
  def printToFile(f: File)(op: PrintWriter => Unit): Unit =
      println(s"writing ${f.getPath}")
      val p = PrintWriter(f)
      try
        op(p)
      finally
        p.close()

  /** print a full text as is in a file (overwrite if exist)
   *
   * @param charSet
   * @param path
   * @param text
   * @example
   *    writeFile(StandardCharsets.UTF_8)(full_path)(full_text)
   */
  def writeFile(charSet: Charset)(path: String)(text: String): Unit =
      Files.write(Paths.get(path), text.getBytes(charSet))