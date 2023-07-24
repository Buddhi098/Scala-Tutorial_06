object CaesarCipher {
  def encrypt(text: String, shift: Int): String = {
  val base = if (text(0).isUpper) 'A' else 'a'
  if (text.length()==1)
    return text
  else
    val restString = text.substring(1)
    return (if (text(0).isLetter) ((text(0) - base + shift) % 26 + base).toChar else text(0)) + encrypt(restString , shift)
}


  def decrypt(text: String, shift: Int): String = {
    encrypt(text, 26 - shift) 
  }


  def cipher(text: String, shift: Int, mode: String): String = {
    mode match {
      case "encrypt" => encrypt(text, shift)
      case "decrypt" => decrypt(text, shift)
      case _ => throw new Error("Invalid mode. Use 'encrypt' or 'decrypt'.")
    }
  }

  def main(args: Array[String]): Unit = {
    val message = "Buddhi Nadeeshan*/"
    val shift = 3
    val encryptedMessage = cipher(message, shift, "encrypt")
    val decryptedMessage = cipher(encryptedMessage, shift, "decrypt")

    println("Original Message: " + message)
    println("Encrypted Message: " + encryptedMessage)
    println("Decrypted Message: " + decryptedMessage)
  }
}
