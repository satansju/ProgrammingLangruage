package miniscala.parser

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import miniscala.AbstractMachine.Executable

object Files {

  def save(code: Executable, filename: String): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(filename))
    oos.writeObject(code)
    oos.close()
  }

  def load(filename: String): Executable = {
    val ois = new ObjectInputStream(new FileInputStream(filename))
    val code = ois.readObject.asInstanceOf[Executable]
    ois.close()
    code
  }

}
